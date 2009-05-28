{-| The purpose of the core library is to make profiling data easy to
access for others. It is hardly more than some glue code that hides
the actual profile format from the application. It also provides basic
data management for applications that don't want to deal with it
themselves -}

module Profiling.Heap.Read where

import Control.Applicative
import Control.Monad
import Control.Concurrent
import Data.IORef
import Data.List
import Data.Maybe
import qualified Data.IntMap as IM
import qualified Data.Map as M
import Data.Time.LocalTime
import Data.Time.Format
import System.Directory
import System.FilePath
import System.IO
import System.Locale
import System.Process

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

data Profile = Profile {
      profData :: M.Map Time ProfileSample,
      profNames :: IM.IntMap CostCentreName,
      -- This one should be a trie, e.g. from bytestring-trie,
      -- otherwise it takes ages to find the id... For the time being
      -- it is a simple map to avoid extra dependencies.
      profNamesInv :: ProfNamesInv,
      profJob :: String,
      profDate :: String
    }

type ProfNamesInv = M.Map CostCentreName CostCentreId

instance Show Profile where
    show p = unlines $ ["Job: " ++ profJob p,"Date: " ++ profDate p,"Name mappings:"] ++
             (map show . IM.assocs . profNames) p ++
             ["Measurements (" ++ (show . M.size . profData) p ++ "):"] ++
             (map show . M.assocs . profData) p

emptyProfile :: Profile
emptyProfile = Profile {
                 profData = M.empty,
                 profNames = IM.empty,
                 profNamesInv = M.empty,
                 profJob = "",
                 profDate = ""
               }

{-| Cost centres are identified by integers for simplicity (so we can
use IntMap) -}

type CostCentreId = Int

{-| While cost centre names reflect the call hierarchy, we are not
splitting them at this point. It can be done later if the application
needs it. -}

type CostCentreName = String

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
since we normally process every value in this collection, and it' s
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
            (stime',prof') <- accumProfile stime prof <$> hGetLine hdl
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
process. Note that you can take a command line given in a string and
convert it into a 'CreateProcess' bundle using
'System.Process.shell'. -}

profile :: CreateProcess -> IO (ProfileReader,ProcessHandle)
profile prog = do
  let getCmd p = case cmdspec p of
                   ShellCommand cmd -> cmd
                   RawCommand prg args -> intercalate " " (prg:args)
  zt <- getZonedTime
  ref <- newIORef emptyProfile {
                 profJob = getCmd prog,
                 profDate = formatTime defaultTimeLocale "%F %H:%M:%S %Z" zt
               }
  hdl <- profileCallback prog $ \pkg -> do
    prof <- readIORef ref
    case pkg of
      SinkSample t smp   -> writeIORef ref $ prof { profData = M.insert t smp (profData prof) }
      SinkId ccid ccname -> writeIORef ref $ prof { profNames = IM.insert ccid ccname (profNames prof) }
      _                  -> return ()
  return (readIORef ref,hdl)

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
as they come. -}

profileCallback :: CreateProcess -> ProfileSink -> IO ProcessHandle
profileCallback prog sink = do
  (_,_,_,phdl) <- createProcess prog
  -- Question: do we want an alternative single-threaded interface?
  forkIO $ do
    let -- We might actually need getCurrentDirectory instead of using ".".
        hpPath = maybe "." id (cwd prog) ++ '/' : (takeFileName . execPath . cmdspec) prog ++ ".hp"
        -- Yes, this is extremely naive, but it will do for the time being...
        execPath (ShellCommand cmd) = takeWhile (/=' ') cmd
        execPath (RawCommand path _) = path        
    maybeHpFile <- tryRepeatedly (openFile hpPath ReadMode) 50 10000
    let hpFile = fromJust maybeHpFile
        pass line idmap smp = do
          stop <- getProcessExitCode phdl
          if stop == Nothing then do
              eof <- hIsEOF hpFile
              if eof then do
                  threadDelay 1000
                  pass line idmap smp
                else do
                  -- Setting line buffering doesn't help me make this work...
                  --res <- parseHpLine <$> hGetLine hpFile
                  c <- hGetChar hpFile
                  if c == '\n' then do
                      case parseHpLine (reverse line) {- res -} of
                        BeginSample _      -> pass "" idmap []
                        EndSample t        -> do when (not (null smp)) $ sink (SinkSample t smp)
                                                 pass "" idmap []
                        Sample ccname cost -> do let (newid,ccid,idmap') = addCCId idmap ccname
                                                 when newid $ sink (SinkId ccid ccname)
                                                 pass "" idmap' ((ccid,cost):smp)
                        _ -> pass "" idmap smp
                    else pass (c:line) idmap smp
            else do sink SinkStop
                    return ()
    -- Again, we should use a trie here instead of a Map String...
    when (maybeHpFile /= Nothing) $ do                              
      -- Line buffering doesn't seem to help with input streams...
      --hSetBuffering hpFile LineBuffering
      pass "" M.empty []
  return phdl

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
                 | Sample CostCentreName Cost

parseHpLine :: String -> ParseResult
parseHpLine line = if null cost then head ([val | (key,val) <- results, key == cmd] ++ [Unknown])
                   else Sample ccname (read (tail cost))
    where (ccname,cost) = span (/='\t') line
          (cmd,_:param) = span (/=' ') line
          results = [("JOB",Job (read param)),
                     ("DATE",Date (read param)),
                     ("BEGIN_SAMPLE",BeginSample (read param)),
                     ("END_SAMPLE",EndSample (read param))]

accumProfile :: Maybe Time -> Profile -> String -> (Maybe Time,Profile)
accumProfile time prof line = case parseHpLine line of
  Job s              -> (Nothing,prof { profJob = s })
  Date s             -> (Nothing,prof { profDate = s })
  BeginSample t      -> (Just t,prof)
  EndSample _        -> (Nothing,prof)
  Sample ccname cost -> let (newid,ccid,pnsi') = addCCId (profNamesInv prof) ccname
                        in (time,
                            prof {
                              profData = M.insertWith (++) (fromJust time) [(ccid,cost)] (profData prof),
                              profNames = if newid then IM.insert ccid ccname (profNames prof) else profNames prof,
                              profNamesInv = pnsi'
                            })
  Unknown            -> (Nothing,prof)

addCCId :: ProfNamesInv -> CostCentreName -> (Bool, CostCentreId, ProfNamesInv)
addCCId idmap ccname = if ccid /= M.size idmap then (False,ccid,idmap)
                       else (True,ccid,M.insert ccname ccid idmap)
    where ccid = M.findWithDefault (M.size idmap) ccname idmap

-- Some tests --

-- For the time being, we assume that getCurrentDirectory returns the
-- dir of the cabal file, because we love emacs.

-- Callback test
test1 :: IO ProcessHandle
test1 = do
  dir <- getCurrentDirectory
  profileCallback (shell (dir++"/test/tester")) { cwd = Just (dir++"/test") } print

-- Accumulation test
test2 :: IO ()
test2 = do
  dir <- getCurrentDirectory
  (reader,_) <- profile (shell (dir++"/test/tester")) { cwd = Just (dir++"/test") }
  replicateM_ 4 $ do
    prof <- reader
    print prof
    threadDelay 1000000

-- Archive test
test3 :: IO Profile
test3 = do
  dir <- getCurrentDirectory
  readProfile $ dir ++ "/test/example.hp"
