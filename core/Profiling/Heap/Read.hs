{-| The purpose of the core library is to make profiling data easy to
access for others. It is hardly more than some glue code that hides
the actual profile format from the application. It also provides basic
data management for applications that don't want to deal with it
themselves. -}

module Profiling.Heap.Read where

-- The imperative bits
import Control.Applicative ((<$>))
import Control.Monad (when, replicateM_)
import Control.Concurrent (forkIO, threadDelay)
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
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Data.Map (Map)
import qualified Data.Map as M
import Data.Trie (Trie)
import qualified Data.Trie as T

-- Stuff needed only to create a time stamp
import Data.Time.LocalTime (getZonedTime)
import Data.Time.Format (formatTime)
import System.Locale (defaultTimeLocale)

-- * Data types

{-| Profiling information is a sequence of time-stamped samples,
therefore the ideal data structure should have an efficient snoc
operation. Also, it should make it easy to extract an interval given
by a start and an end time. For a first approximation we can use a
simple map for this purpose, even if it gives us no constant-time
snoc. An interval can be extracted by splitting the map twice (a
logarithmic operation). Data.Sequence would give us nicer costs, but
it can only handle indices into the sequence efficiently, and creating
an auxiliary map from times to indices would obviously defeat the
purpose of using it. In the end, a dynamically growing array might be
the ultimate solution... -}

data Profile = Profile
    { profData :: Map Time ProfileSample
    , profNames :: IntMap CostCentreName
    , profNamesInv :: Trie CostCentreId
    , profJob :: String
    , profDate :: String
    }

instance Show Profile where
    show p = unlines $
             ["Job: " ++ profJob p
             ,"Date: " ++ profDate p
             ,"Name mappings:"] ++
             (map show . IM.assocs . profNames) p ++
             ["Measurements (" ++ (show . M.size . profData) p ++ "):"] ++
             (map show . M.assocs . profData) p

emptyProfile :: Profile
emptyProfile = Profile
               { profData = M.empty
               , profNames = IM.empty
               , profNamesInv = T.empty
               , profJob = ""
               , profDate = ""
               }

{-| Cost centres are identified by integers for simplicity (so we can
use IntMap) -}

type CostCentreId = Int

{-| While cost centre names reflect the call hierarchy, we are not
splitting them at this point. It can be done later if the application
needs it. -}

type CostCentreName = ByteString

{-| It's a good question how time should be represented. If we use
maps, we might want to use an IntMap for the profiling data
too. Assuming a resolution of 1 ms, we could keep track of time stamps
up to nearly 25 days on a 32-bit architecture, and there would be no
practical limitation with 64-bit integers. Using floats would also
lift the limit. For the time being we're going with the second
option. -}

type Time = Double

{-| A sampling point is simply a list of cost centres with the
associated cost. I don't see any need for a fancy data structure here,
since we normally process every value in this collection, and it's
usually not big either, only holding a few dozen entries at most. -}

type Cost = Int

type ProfileSample = [(CostCentreId,Cost)]

-- * Communication

{-| Since we want to possibly look at this information during the run,
we might need an action that returns the current state. -}

type ProfileReader = IO Profile

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

{-| In order to perform real-time profiling, we need to fire up the
program to analyse and create an accumulator in the background that we
can look at whenever we want using the reading action returned by the
starter function 'profile'. The 'ProcessHandle' is also returned in
order to allow the caller to control the execution of the slave
process as well as an IO action that stops the reading thread without
touching the slave process. Note that you can take a command line
given in a string and convert it into a 'CreateProcess' bundle using
'System.Process.shell'. -}

profile :: CreateProcess -> IO (ProfileReader, ProcessHandle, IO ())
profile prog = do
  let getCmd p = case cmdspec p of
                   ShellCommand cmd -> cmd
                   RawCommand prg args -> intercalate " " (prg:args)
  zt <- getZonedTime
  ref <- newIORef emptyProfile
               { profJob = getCmd prog
               -- The time format is deliberately different from the
               -- one currently used in heap profiles. Changing it
               -- doesn't hurt anyone, and it makes more sense this
               -- way, so there.
               , profDate = formatTime defaultTimeLocale "%F %H:%M:%S %Z" zt
               }
  (hdl,act) <- profileCallback prog $ \pkg -> do
    prof <- readIORef ref
    case pkg of
      SinkSample t smp   -> writeIORef ref $ prof
                            { profData = M.insert t smp (profData prof) }
      SinkId ccid ccname -> writeIORef ref $ prof
                            { profNames = IM.insert ccid ccname (profNames prof) }
      _                  -> return ()
  return (readIORef ref,hdl,act)

{-| We might not want to hold on to all the past output, just do some
stream processing. We can achieve this using a callback function
that's invoked whenever a new profile sample is available. It is also
necessary to send over the names that belong to the short cost centre
identifiers as well as the fact that no more data will come. These
three cases are united in the 'SinkInput' type. -}

type ProfileSink = SinkInput -> IO ()

data SinkInput = SinkSample Time ProfileSample
               | SinkId CostCentreId CostCentreName
               | SinkStop
                 deriving Show

{-| The 'profileCallback' function initiates an observation without
maintaining an internal data, passing profile samples to the callback
as they come. It returns the handle of the new process as well as the
thread stopper action. -}

profileCallback :: CreateProcess -> ProfileSink -> IO (ProcessHandle, IO ())
profileCallback prog sink = do
  dir <- getCurrentDirectory
  let hpPath = fromMaybe dir (cwd prog) ++
               '/' : (takeFileName . execPath . cmdspec) prog ++ ".hp"
      -- Yes, this is extremely naive, but it will do for the time being...
      execPath (ShellCommand cmd) = takeWhile (/=' ') cmd
      execPath (RawCommand path _) = path

  -- We have to delete the .hp file and wait for the process to create it.
  catch (removeFile hpPath) (const (return ()))
  (_,_,_,phdl) <- createProcess prog

  stopRequest <- newIORef False

  -- Question: do we want an alternative single-threaded interface?
  forkIO $ do
    maybeHpFile <- tryRepeatedly (openFile hpPath ReadMode) 50 10000
    let hpFile = fromJust maybeHpFile
        pass buf idmap smp = do
          let mlen = S.elemIndex '\n' buf
          stop <- readIORef stopRequest
          if mlen /= Nothing && not stop then do
              -- If there's a whole line in the buffer and we still
              -- have the green light, we'll parse it and notify the
              -- callback (sink) if necessary.
              let len = fromJust mlen
                  rest = S.drop (len+1) buf
              case parseHpLine (S.take len buf) of
                BeginSample _ -> pass rest idmap []
                EndSample t -> do
                  when (not (null smp)) $ sink (SinkSample t smp)
                  pass rest idmap []
                Cost ccname cost -> do
                  let (newid,ccid,idmap') = addCCId idmap ccname
                  when newid $ sink (SinkId ccid ccname)
                  pass rest idmap' ((ccid,cost):smp)
                _ -> pass rest idmap smp
            else do
              slaveCode <- getProcessExitCode phdl
              if slaveCode == Nothing && not stop then do
                  -- If there's no line known to be full while the
                  -- other process is still running, we keep trying to
                  -- fetch more data.
                  eof <- hIsEOF hpFile
                  if eof then do
                      threadDelay 100000
                      pass buf idmap smp
                    else do
                      newChars <- S.hGetNonBlocking hpFile 0x10000
                      pass (S.append buf newChars) idmap smp
                else do
                  -- At this point either the other process ended or
                  -- we were requested to stop reading further.
                  sink SinkStop
                  return ()
    -- Again, we should use a trie here instead of a Map String...
    when (maybeHpFile /= Nothing) $ pass S.empty T.empty []
  return (phdl,writeIORef stopRequest True)

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

profileLength :: Profile -> Time
profileLength = fst . M.findMax . profData

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
parseHpLine line = if S.null cost then head ([val | (key,val) <- results, key == cmd] ++ [Unknown])
                   else Cost ccname (read . S.unpack . S.tail $ cost)
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

-- Callback test (also: stopTest <- snd <$> test1)
test1 :: IO (ProcessHandle, IO ())
test1 = do
  dir <- getCurrentDirectory
  profileCallback (shell (dir++"/test/tester")) { cwd = Just (dir++"/test") } print

-- Accumulation test
test2 :: IO ()
test2 = do
  dir <- getCurrentDirectory
  (reader,_,_) <- profile (shell (dir++"/test/tester")) { cwd = Just (dir++"/test") }
  replicateM_ 4 $ do
    prof <- reader
    print prof
    threadDelay 1000000

-- Archive test
test3 :: IO Profile
test3 = do
  dir <- getCurrentDirectory
  readProfile $ dir ++ "/test/example.hp"
