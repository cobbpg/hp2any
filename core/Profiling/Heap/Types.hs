{-|

This module defines the commonly used data structures and basic types
of the heap profiling framework.

Profiling information is a sequence of time-stamped samples, therefore
the ideal data structure should have an efficient snoc operation.
Also, it should make it easy to extract an interval given by a start
and an end time.  On top of the raw data, we also want to access some
statistics as efficiently as possible.

We can separate two phases: looking at the profile during execution
and later.  In the first case we might not want statistics, just live
monitoring, while we probably want to analyse archived profiles more
deeply.  Therefore, it makes sense to define two separate data
structures for these two purposes, and give them a common interface
for extracting the necessary data.  The simple case is covered by the
'Profile' type defined here, while a more complex structure providing
fast off-line queries is defined in the "Profiling.Heap.Stats" module.

-}

module Profiling.Heap.Types
    ( CostCentreId
    , CostCentreName
    , Time
    , Cost
    , ProfileSample
    -- * Profile data structure
    , Profile(..)
    , emptyProfile
    -- * Query interface
    , ProfileQuery(..)
    -- * Streaming interface
    , ProfileSink
    , SinkInput(..)
    ) where

import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as S
import Data.Int
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Data.List
import Data.Map (Map)
import qualified Data.Map as T

type Trie v = Map ByteString v

{-| The 'ProfileQuery' class contains all kinds of reading operations.
The minimal definition consists of 'job', 'date', 'ccNames' and
'samples'.  All the statistics have default implementations, which are
mostly okay for a single query, but they are generally highly
inefficient. -}

class ProfileQuery p where
    -- | Job information (command line).
    job :: p -> String
    -- | Job start time.
    date :: p -> String
    -- | Cost centre id to name mapping.
    ccNames :: p -> IntMap CostCentreName
    -- | Find cost centre name by id.
    ccName :: p -> Int -> CostCentreName
    ccName p ccId = IM.findWithDefault S.empty ccId (ccNames p)

    -- | The measurements in a list ordered by time.
    samples :: p -> [(Time,ProfileSample)]
    -- | The samples between two given times.
    samplesIvl :: p -> Time -> Time -> [(Time,ProfileSample)]
    samplesIvl p t1 t2 = takeWhile ((<t2).fst) $ dropWhile ((<t1).fst) $ samples p

    -- | The time of the first sample.
    minTime :: p -> Time
    minTime p | null smp  = 0
              | otherwise = fst (head smp)
        where smp = samples p
    -- | The time of the last sample.
    maxTime :: p -> Time
    maxTime p | null smp  = 0
              | otherwise = fst (last smp)
        where smp = samples p

    -- | The highest individual cost at any time.
    maxCost :: p -> Cost
    maxCost p = maximum $ 0:[c | (_,s) <- samples p, (_,c) <- s]
    -- | The highest total cost at any time.
    maxCostTotal :: p -> Cost
    maxCostTotal p = maximum $ 0:[sum (map snd s) | (_,s) <- samples p]
    -- | The highest individual cost in the interval.
    maxCostIvl :: p -> Time -> Time -> Cost
    maxCostIvl p t1 t2 = maximum $ 0:[c | (_,s) <- samplesIvl p t1 t2, (_,c) <- s]
    -- | The highest total cost in the interval.
    maxCostTotalIvl :: p -> Time -> Time -> Cost
    maxCostTotalIvl p t1 t2 = maximum $ 0:[sum (map snd s) | (_,s) <- samplesIvl p t1 t2]

    -- | The total cost of each cost centre. Not a time integral;
    -- samples are simply summed.
    integral :: p -> ProfileSample
    integral = integral' . samples
    -- | The total cost of each cost centre in the interval.
    integralIvl :: p -> Time -> Time -> ProfileSample
    integralIvl p t1 t2 = integral' (samplesIvl p t1 t2)

integral' :: [(Time,ProfileSample)] -> ProfileSample
integral' = IM.assocs . foldl' accumSample IM.empty
    where accumSample acc = foldl' accumCost acc . snd
          accumCost acc (ccid,cost) = IM.alter (Just . maybe cost (+cost)) ccid acc

{-| A raw heap profile that's easy to grow further, therefore it is
used during loading. -}

data Profile = Profile
    { prSamples :: ![(Time,ProfileSample)] -- ^ Samples in decreasing time order (latest first).
    , prNames :: !(IntMap CostCentreName)  -- ^ A map from cost centre ids to names.
    , prNamesInv :: !(Trie CostCentreId)   -- ^ A map from cost centre names to ids.
    , prJob :: !String                     -- ^ Information about the job (command line).
    , prDate :: !String                    -- ^ Job start time and date.
    } deriving Eq

instance Show Profile where
    show p = unlines $
             ["Job: " ++ prJob p
             ,"Date: " ++ prDate p
             ,"Name mappings:"] ++
             (map show . IM.assocs . prNames) p ++
             ["Measurements:"] ++
             (map show . prSamples) p

instance ProfileQuery Profile where
    job = prJob
    date = prDate
    ccNames = prNames
    samples = reverse . prSamples

{-| An initial 'Profile' structure that can be used in
accumulations. -}

emptyProfile :: Profile
emptyProfile = Profile
               { prSamples = []
               , prNames = IM.empty
               , prNamesInv = T.empty
               , prJob = ""
               , prDate = ""
               }

{-| Cost centres are identified by integers for simplicity (so we can
use IntMap). -}

type CostCentreId = Int

{-| At this level cost centre names have no internal structure that we
would care about.  While in some cases they reflect the call
hierarchy, we are not splitting them at this point, because all kinds
of names can appear here. -}

type CostCentreName = ByteString

{-| Time is measured in seconds. -}

type Time = Double

{-| Costs are measured in bytes. -}

type Cost = Int64

{-| A sampling point is simply a list of cost centres with the
associated cost.  There is no need for a fancy data structure here,
since we normally process every value in this collection, and it's
usually not big either, only holding a few dozen entries at most. -}

type ProfileSample = [(CostCentreId,Cost)]

{-| We might not want to hold on to all the past output, just do some
stream processing.  We can achieve this using a callback function
that's invoked whenever a new profile sample is available.  The type
of this function can be 'ProfileSink'.  Besides the actual costs, it
is also necessary to send over the names that belong to the short cost
centre identifiers as well as the fact that no more data will come.
The 'SinkInput' type expresses these possibilities. -}

type ProfileSink = SinkInput -> IO ()

data SinkInput
    -- | A snapshot of costs at a given time.
    = SinkSample !Time !ProfileSample
    -- | The name behind a cost centre id used in the samples.
    | SinkId !CostCentreId !CostCentreName
    -- | Indication that no more data will come.
    | SinkStop
      deriving (Eq, Show)
