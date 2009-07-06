{-| This module defines the commonly used data structures of the heap
profiling framework.  The network protocol is defined in the
"Profiling.Heap.Network" module. -}

module Profiling.Heap.Types where

import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as S
import Data.List
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Data.Map (Map)
import qualified Data.Map as M
import Data.Trie (Trie)
import qualified Data.Trie as T

{-| Profiling information is a sequence of time-stamped samples,
therefore the ideal data structure should have an efficient snoc
operation.  Also, it should make it easy to extract an interval given
by a start and an end time.  For a first approximation we can use a
simple map for this purpose, even if it gives us no constant-time
snoc.  An interval can be extracted by splitting the map twice (a
logarithmic operation).  Data.Sequence would give us nicer costs, but
it can only handle indices into the sequence efficiently, and creating
an auxiliary map from times to indices would obviously defeat the
purpose of using it.  In the end, a dynamically growing array might be
the ultimate solution... -}

data Profile = Profile
    { profData :: !(Map Time ProfileSample)
    , profNames :: !(IntMap CostCentreName)
    , profNamesInv :: !(Trie CostCentreId)
    , profJob :: !String
    , profDate :: !String
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
splitting them at this point.  It can be done later if the application
needs it. -}

type CostCentreName = ByteString

{-| It's a good question how time should be represented.  If we use
maps, we might want to use an IntMap for the profiling data too.
Assuming a resolution of 1 ms, we could keep track of time stamps up
to nearly 25 days on a 32-bit architecture, and there would be no
practical limitation with 64-bit integers.  Using floats would also
lift the limit.  For the time being we're going with the second
option. -}

type Time = Double

{-| A sampling point is simply a list of cost centres with the
associated cost. I don't see any need for a fancy data structure here,
since we normally process every value in this collection, and it's
usually not big either, only holding a few dozen entries at most. -}

type Cost = Int

type ProfileSample = [(CostCentreId,Cost)]

{-| We might not want to hold on to all the past output, just do some
stream processing. We can achieve this using a callback function
that's invoked whenever a new profile sample is available. It is also
necessary to send over the names that belong to the short cost centre
identifiers as well as the fact that no more data will come. These
three cases are united in the 'SinkInput' type. -}

type ProfileSink = SinkInput -> IO ()

data SinkInput = SinkSample !Time !ProfileSample
               | SinkId !CostCentreId !CostCentreName
               | SinkStop
                 deriving (Eq, Show)
