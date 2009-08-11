{-| This module defines a heap profile data structure optimised for
querying various statistics, but not suitable for continuous
updating. -}

module Profiling.Heap.Stats
    ( ProfileWithStats
    , buildStats
    ) where

import Control.Arrow
import qualified Data.IntMap as IM
import Data.List
import Data.Map (Map)
import qualified Data.Map as M
import Profiling.Heap.Types

{-| A tree to accelerate range max and maxsum queries.  It takes O(n)
to construct and requires O(n) space (where n is the total number of
individual costs), and provides O(log(n)) query time.  Note: there are
more sophisticated algorithms allowing constant time access while
keeping the other characteristics as well. -}

data MaxQuery key val = Leaf (val,val) key
                      | Node ((val,val),(key,key)) (MaxQuery key val) (MaxQuery key val)

{-| A data structure providing profile statistics at a low cost.  It
accelerates interval extraction as well as determining maxima and
integrals over any subinterval: all of these operations take
logarithmic time to execute. -}

data ProfileWithStats = PWS
    { pmProfile :: Profile
    , pmData :: Map Time ProfileSample
    , pmIntegral :: Map Time ProfileSample
    , pmMaxQuery :: MaxQuery Time Cost
    }

instance ProfileQuery ProfileWithStats where
    job = job . pmProfile
    date = date . pmProfile
    ccNames = ccNames . pmProfile
    samples = M.assocs . pmData
    -- An interval can be extracted by splitting the map twice (which
    -- is a logarithmic operation).
    samplesIvl p t1 t2 = M.assocs . fst . M.split t2 . snd . M.split t1 $ pmData p

    -- Starting and ending time can be found in O(log n) steps.
    minTime p | M.null (pmData p) = 0
              | otherwise         = fst . M.findMin $ pmData p
    maxTime p | M.null (pmData p) = 0
              | otherwise         = fst . M.findMax $ pmData p

    -- Range maxima can be found in logarithmic time in general, but
    -- maxima for the full range are readily available.
    maxCost p = case pmMaxQuery p of
                  Leaf (x,_) _ -> x
                  Node ((x,_),_) _ _ -> x
    maxCostTotal p = case pmMaxQuery p of
                       Leaf (_,x) _ -> x
                       Node ((_,x),_) _ _ -> x
    maxCostIvl p t1 t2 = fst (maxIvl (pmMaxQuery p) t1 t2)
    maxCostTotalIvl p t1 t2 = snd (maxIvl (pmMaxQuery p) t1 t2)

    -- Range integrals can be found in logarithmic time.
    integral p | M.null (pmIntegral p) = []
               | otherwise             = snd . M.findMax $ pmIntegral p
    integralIvl p t1 t2 | M.null ivl = []
                        | otherwise  = IM.assocs $ diff smp2 smp1
        where ivl = fst . M.split t2 . snd . M.split t1 $ pmIntegral p
              smp1 = IM.fromDistinctAscList . snd $ M.findMin ivl
              smp2 = IM.fromDistinctAscList . snd $ M.findMax ivl
              diff s2 s1 = IM.unionWith (-) s2 s1

{-| Create extra data to speed up various queries. -}

buildStats :: Profile -> ProfileWithStats
buildStats p = PWS
               { pmProfile = p
               , pmData = M.fromDistinctAscList $ samples p
               , pmIntegral = M.fromDistinctAscList . buildIntegrals $ samples p
               , pmMaxQuery = buildMaxQuery $ samples p
               }

{-| Calculate all the partial integrals from zero to each sample. -}

buildIntegrals :: [(Time,ProfileSample)] -> [(Time,ProfileSample)]
buildIntegrals = map (fmap IM.assocs) . tail . scanl accumSample (undefined,IM.empty)
    where accumSample (_,acc) (t,smp) = (t,foldl' accumCost acc smp)
          accumCost acc (ccid,cost) = IM.alter (Just . maybe cost (+cost)) ccid acc

{-| Build range max-maxsum query tree. -}

buildMaxQuery :: [(Time,ProfileSample)] -> MaxQuery Time Cost
buildMaxQuery smps = head.head $ dropWhile ((>1).length) $
                   iterate mergeList (map smpMaxQuery smps)
    where smpMaxQuery (t,smp) = Leaf maxima t
              where maxima = (maximum &&& sum) (map snd smp)
          
          mergeList (t1:t2:ts) = node:mergeList ts
              where node = Node ( (max m1 m2,max ms1 ms2)
                                , (fst (getIvl t1),snd (getIvl t2))
                                ) t1 t2
                    (m1,ms1) = vmm t1
                    (m2,ms2) = vmm t2
                    vmm (Leaf x _) = x
                    vmm (Node (mm,_) _ _) = mm
          mergeList ts = ts

{-| Get the maxima for a given interval. -}

maxIvl :: MaxQuery Time Cost -> Time -> Time -> (Cost,Cost)
maxIvl (Leaf x _) _ _ = x
maxIvl (Node _ l r) t1 t2
    | t2 < t1r  = maxIvl l t1 t2
    | t1 > t2l  = maxIvl r t1 t2
    | otherwise = unionIval (maxIvl l t1 t2l) (maxIvl r t1r t2)
    where (_t1l,t2l) = getIvl l
          (t1r,_t2r) = getIvl r
          unionIval (ml,msl) (mr,msr) = (max ml mr,max msl msr)

{-| Get the interval covered by the query tree. -}

getIvl :: MaxQuery Time Cost -> (Time,Time)
getIvl (Leaf _ t) = (t,t)
getIvl (Node (_,ivl) _ _) = ivl
