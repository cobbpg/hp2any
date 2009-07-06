{-| The purpose of the core library is to make profiling data easy to
access for others. It is hardly more than some glue code that hides
the actual profile format from the application. It also provides basic
data management for applications that don't want to deal with it
themselves. Also, it provides some facilities to make remote profiling
easier. -}

module Profiling.Heap.Read
    ( ProfileReader
    , ProfilingType(..)
    , ProfilingCommand
    , ProfilingInfo
    , ProfilingStop
    , readProfile
    , profile
    , profileCallback
    , costCentreName
    , costCentreNames
    , toList
    , intervalToList
    , maxTime
    ) where

-- The imperative bits
import Control.Applicative
import Control.Monad
import Control.Concurrent
import Data.IORef
import System.Directory
import System.FilePath
import System.IO
import System.Process

-- Data structures
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as S
import Data.List
import Data.Maybe
import qualified Data.IntMap as IM
import qualified Data.Map as M
import Data.Trie (Trie)
import qualified Data.Trie as T
import Profiling.Heap.Types

-- Networking
import Network
import Profiling.Heap.Network

-- Stuff needed only to create a time stamp
import Data.Time.LocalTime (getZonedTime)
import Data.Time.Format (formatTime)
import System.Locale (defaultTimeLocale)

-- * Communication

{-| The simplest case to handle is the traditional method of taking
the profiling output of an earlier run and turn it into an easy to
query structure. This is done by passing 'readProfile' the log created
by the heap profiler. -}

readProfile :: FilePath -> IO Profile
readProfile file = do
  let parse hdl stime prof = do
        stop <- hIsEOF hdl
        if not stop then do
            (stime',prof') <- accumProfile stime prof <$> S.hGetLine hdl
            parse hdl stime' prof'
          else return prof

  hdl <- openFile file ReadMode
  prof <- parse hdl Nothing emptyProfile
  return prof

{-| Since we want to possibly look at this information during the run,
we might need an action that returns the current state. -}

type ProfileReader = IO Profile

{-| There are two basic ways of profiling: local and remote.  Local
profiling means that we directly manage the process we are monitoring.
In the case of remote profiling we connect to a server that streams
profiling information and acts as a proxy between the process to
profile and our program.  The type of profiling also determines the
kind of information available to us after initiating the process, so
we need generic labels to distinguish the alternatives. -}

data ProfilingType loc rem = Local { local :: loc }
                           | Remote { remote :: rem }

{-| When we start profiling, we need a process descriptor for the
local case or a server address (of the form \"address:port\") in the
remote case.  The creation of the process descriptor is aided by the
"Profiling.Heap.Process" module. -}

type ProfilingCommand = ProfilingType CreateProcess String

{-| In the local case we are given the handle of the process
monitored.  Asking for a remote profile gives us a handle we can use
to communicate with the proxy via the common protocol defined in the
"Profiling.Heap.Network" module. -}

type ProfilingInfo = ProfilingType ProcessHandle Handle

{-| It is useful to have an action that stops the reading thread
without touching the slave process, especially in the remote case. -}

type ProfilingStop = IO ()

{-| In order to perform real-time profiling, we need to fire up the
program to analyse and create an accumulator in the background that we
can look at whenever we want using the reading action returned by the
starter function 'profile'. -}

profile :: ProfilingCommand -> IO (ProfileReader,ProfilingStop,ProfilingInfo)
profile prog = do
  let getCmd p = case cmdspec p of
                   ShellCommand cmd -> cmd
                   RawCommand prg args -> intercalate " " (prg:args)
  zt <- getZonedTime
  ref <- newIORef emptyProfile
               { profJob = case prog of 
                             Local desc -> getCmd desc
                             Remote addr -> addr
               -- The time format is deliberately different from the
               -- one currently used in heap profiles. Changing it
               -- doesn't hurt anyone, and it makes more sense this
               -- way, so there.
               , profDate = formatTime defaultTimeLocale "%F %H:%M:%S %Z" zt
               }
  (stop,info) <- profileCallback prog $ \pkg -> do
    prof <- readIORef ref
    case pkg of
      SinkSample t smp   -> writeIORef ref $ prof
                            { profData = M.insert t smp (profData prof) }
      SinkId ccid ccname -> writeIORef ref $ prof
                            { profNames = IM.insert ccid ccname (profNames prof) }
      _                  -> return ()
  return (readIORef ref,stop,info)

{-| The 'profileCallback' function initiates an observation without
maintaining any internal data other than the name mapping, passing
profile samples to the callback as they come. It returns the handle of
the new process or the remote connection as well as the thread stopper
action. -}

profileCallback :: ProfilingCommand -> ProfileSink -> IO (ProfilingStop,ProfilingInfo)
profileCallback (Local prog) sink = do
  dir <- getCurrentDirectory
  let hpPath = fromMaybe dir (cwd prog) ++
               '/' : (takeFileName . execPath . cmdspec) prog ++ ".hp"
      -- Yes, this is extremely naive, but it will do for the time being...
      execPath (ShellCommand cmd) = takeWhile (/=' ') cmd
      execPath (RawCommand path _) = path

  -- We have to delete the .hp file and wait for the process to create it.
  catch (removeFile hpPath) (const (return ()))
  (_,_,_,phdl) <- createProcess prog

  -- Question: do we want an alternative single-threaded interface?
  tid <- forkIO $ do
    maybeHpFile <- tryRepeatedly (openFile hpPath ReadMode) 50 10000

    let hpFile = fromJust maybeHpFile
        pass buf idmap smp = do
          case S.elemIndex '\n' buf of
            -- If there's a whole line in the buffer and we still
            -- have the green light, we'll parse it and notify the
            -- callback (sink) if necessary.
            Just len -> do
              let (line,rest) = S.splitAt len buf
                  -- Getting rid of the line break after the first
                  -- line.
                  next = pass (S.drop 1 rest)

              case parseHpLine line of
                -- Initialising a new empty sample.
                BeginSample _ -> next idmap []
                -- Sending non-empty sample and forgetting it.
                EndSample t -> do
                  when (not (null smp)) $ sink (SinkSample t smp)
                  next idmap []
                -- Adding a cost to the current sample and checking if
                -- we already know the name of the cost centre.
                Cost ccname cost -> do
                  let (newid,ccid,idmap') = addCCId idmap ccname
                  when newid $ sink (SinkId ccid ccname)
                  next idmap' ((ccid,cost):smp)
                _ -> next idmap smp

            -- If there's no line known to be full while the other
            -- process is still running, we keep trying to fetch more
            -- data.
            Nothing -> do
              -- Checking if there is still hope for more data.
              slaveCode <- getProcessExitCode phdl

              if slaveCode == Nothing then do
                  eof <- hIsEOF hpFile
                  if eof then do
                      threadDelay 100000
                      pass buf idmap smp
                    else do
                      newChars <- S.hGetNonBlocking hpFile 0x10000
                      pass (S.append buf newChars) idmap smp
                -- The other process ended, let's notify the callback.
                else sink SinkStop

    when (maybeHpFile /= Nothing) $ pass S.empty T.empty []

  return (profileStop tid sink,Local phdl)

profileCallback (Remote server) sink = do
  -- Yeah, we might need some error handling here...
  let (addr,_:port) = span (/=':') server
      portNum :: Int
      portNum = read port
  hdl <- connectTo addr ((PortNumber . fromIntegral) portNum)
  hSetBuffering hdl LineBuffering

  let readLoop = do
        -- We assume line buffering here. Also, if there seems to be
        -- any error, the profile reader is stopped.
        msg <- catch (readMsg <$> hGetLine hdl) (const (return (Just StrStop)))
        case msg >>= getStream of
          Just profSmp -> do
            sink profSmp
            when (profSmp /= SinkStop) readLoop
          Nothing -> readLoop

  tid <- forkIO readLoop
  return (profileStop tid sink,Remote hdl)

profileStop :: ThreadId -> ProfileSink -> IO ()
profileStop tid sink = do
  killThread tid
  -- The sink is notified asynchronously, since it might be a blocking
  -- operation (like the MVar operations used by the grapher).
  forkIO (sink SinkStop)
  return ()

tryRepeatedly :: IO a -> Int -> Int -> IO (Maybe a)
tryRepeatedly act n d | n < 1     = return Nothing
                      | otherwise = catch (Just <$> act) (const retry)
    where retry = do threadDelay d
                     tryRepeatedly act (n-1) d

-- * Querying functions

costCentreName :: Profile -> CostCentreId -> Maybe CostCentreName
costCentreName p k = IM.lookup k (profNames p)

costCentreNames :: Profile -> [(CostCentreId,CostCentreName)]
costCentreNames = IM.assocs . profNames

toList :: Profile -> [(Time,ProfileSample)]
toList = M.assocs . profData

-- Note: this is not an inclusive interval
intervalToList :: Profile -> Time -> Time -> [(Time,ProfileSample)]
intervalToList p t1 t2 = M.assocs $ fst $ M.split t2 $ snd $ M.split t1 $ profData p

-- Note: minTime could be provided, but it requires an extra field
-- (the grapher keeps track of it explicitly, since it uses the
-- callback interface).
maxTime :: Profile -> Time
maxTime = fst . M.findMax . profData

-- * Parsing profiler output

{-

JOB "command"
DATE "date"
SAMPLE_UNIT "..."
VALUE_UNIT "..."
BEGIN_SAMPLE t1
ccname1<tab>cost1
ccname2<tab>cost2
...
END_SAMPLE t1
BEGIN_SAMPLE t2
...
-}

data ParseResult = Unknown
                 | Job String
                 | Date String
                 | BeginSample Time
                 | EndSample Time
                 | Cost CostCentreName Cost

parseHpLine :: ByteString -> ParseResult
parseHpLine line
    | S.null cost = head ([val | (key,val) <- results, key == cmd] ++ [Unknown])
    | otherwise   = Cost ccname (read . S.unpack . S.tail $ cost)
    where (ccname,cost) = S.span (/='\t') line
          (cmd,sparam) = S.span (/=' ') line
          param = S.unpack (S.tail sparam)
          results = if S.null sparam then [] else
                        [(S.pack "JOB",Job (read param)),
                         (S.pack "DATE",Date (read param)),
                         (S.pack "BEGIN_SAMPLE",BeginSample (read param)),
                         (S.pack "END_SAMPLE",EndSample (read param))]

accumProfile :: Maybe Time -> Profile -> ByteString -> (Maybe Time,Profile)
accumProfile time prof line = case parseHpLine line of
  Job s            -> (Nothing,prof { profJob = s })
  Date s           -> (Nothing,prof { profDate = s })
  BeginSample t    -> (Just t,prof)
  EndSample _      -> (Nothing,prof)
  Cost ccname cost -> let (newid,ccid,pnsi') = addCCId (profNamesInv prof) ccname
                      in (time,
                          prof
                          { profData = M.insertWith (++) (fromJust time) [(ccid,cost)] (profData prof)
                          , profNames = if newid then IM.insert ccid ccname (profNames prof) else profNames prof
                          , profNamesInv = pnsi'
                          })
  Unknown          -> (Nothing,prof)

addCCId :: Trie CostCentreId -> CostCentreName -> (Bool, CostCentreId, Trie CostCentreId)
addCCId idmap ccname = if ccid /= T.size idmap then (False,ccid,idmap)
                       else (True,ccid,T.insert ccname ccid idmap)
    where ccid = fromMaybe (T.size idmap) (T.lookup ccname idmap)

-- Some tests --

-- For the time being, we assume that getCurrentDirectory returns the
-- dir of the cabal file, because we love emacs.

-- Callback test (also: stopTest <- fst <$> _test1)
_test1 :: IO (ProfilingStop,ProfilingInfo)
_test1 = do
  dir <- getCurrentDirectory
  profileCallback (Local (shell (dir++"/test/tester")) { cwd = Just (dir++"/test") }) print

-- Accumulation test
_test2 :: IO ()
_test2 = do
  dir <- getCurrentDirectory
  (reader,_,_) <- profile (Local (shell (dir++"/test/tester")) { cwd = Just (dir++"/test") })
  replicateM_ 5 $ do
    prof <- reader
    print prof
    threadDelay 1000000

-- Archive test
_test3 :: IO Profile
_test3 = do
  dir <- getCurrentDirectory
  readProfile $ dir ++ "/test/example.hp"
