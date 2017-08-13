{-| This module provides some half-ready solutions to visualise heap
profiles both during and after execution with the help of OpenGL.  All
the rendering functions will fill the viewport if the model view
matrix is the identity (they also change the matrix), assuming the
projection matrix is the following:

@
 matrixMode $= Projection
 loadIdentity
 translate $ Vector3 (-1) (-1) 0
 scale 2 2 1
@

In other words, these functions fill the unit square at the origin. -}

module Profiling.Heap.OpenGL
    ( colours
    , backgroundColour
    , otherColour
      -- * Processing raw samples (full profiles)
    , SamplePair(..)
    , prepareSamples
    , renderSamples
    , addSample
      -- * Processing optimised renders (profile streams)
    , GraphData
    , graphNames
    , emptyGraph
    , growGraph
    , renderGraph
    , GraphMode(..)
    , nextGraphMode
    ) where

import Control.Monad
import qualified Data.ByteString.Char8 as S
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Data.List
import Graphics.Rendering.OpenGL hiding (samples)
--import Graphics.Rendering.OpenGL.GL.DisplayLists
import Profiling.Heap.Types

{-| Two heap profile samples which contain the exact same cost centres
in the exact same order. -}

data SamplePair = SP
    { spTime1 :: !Time
    , spTime2 :: !Time
    , spData1 :: !ProfileSample
    , spData2 :: !ProfileSample
    } deriving Show

{-| An optimised graph rendering designed to be easily updated when a
new sample arrives. -}

data GraphData = GD
    { gdNames :: IntMap String                   -- ^ Cost centre id to name mapping.
    , gdSamples :: [SamplePair]                  -- ^ List of pairwise aligned samples.
    , gdLists :: [(Int,DisplayList,DisplayList)] -- ^ Display lists caching rendering in all modes.
    , gdMinTime :: Time                          -- ^ The time of the first sample.
    }

{-| The names of cost centres in a graph rendering. -}
graphNames :: GraphData -> IntMap String
graphNames = gdNames

{-| An empty rendering. -}

emptyGraph :: GraphData
emptyGraph = GD
             { gdNames = IM.singleton 0 "Other"
             , gdSamples = [SP 0 0 [] []]
             , gdLists = []
             , gdMinTime = 0
             }

{-| The possible ways of displaying heap profiles. -}

data GraphMode
    -- | Cost centres are stacked on top of each other without
    -- overlapping.
    = Accumulated
    -- | Each cost centre yields a separate line graph on the same
    -- scale.
    | Separate
      deriving Eq

{-| A cyclic successor function for graph modes. -}

nextGraphMode :: GraphMode -> GraphMode
nextGraphMode Accumulated = Separate
nextGraphMode Separate    = Accumulated

{-| A list of highly different colours, where the differences diminish
as we advance in the list.  The first element is black, and there is
no white. -}

colours :: [Color3 GLubyte]
colours = concatMap makeCol [0..]
    where comps = 0 : 255 : unfoldr cnext (256,127 :: Int)
          cnext (s,c) = Just (fromIntegral c,if s+fromIntegral c >= 255 then (s `div` 2,s `div` 4-1) else (s,s+c))
          makeCol n = if n == 1 then init res else res
              where res = [Color3 (comps !! rn) (comps !! gn) (comps !! bn) |
                           rn <- [0..n], gn <- [0..n], bn <- [0..n],
                           rn == n || gn == n || bn == n]

{-| The colour of the background (white).  It is not a member of
'colours'. -}

backgroundColour :: Color3 GLubyte
backgroundColour = Color3 255 255 255

{-| The colour used for unimportant cost centres (black).  It is the
first element of 'colours'. -}

otherColour :: Color3 GLubyte
otherColour = Color3 0 0 0

{-| The limit under which cost centres are filtered out (grouped under
the name \"Other\"). -}

costLimit :: Cost
costLimit = 256

{-| Create a list of sample pairs where each cost centre is paired up
with the consecutive one, so it is easier to render them.  Cost
centres with small costs (below 'costLimit') are lumped together under
identifier 0, reserved for \"Other\". -}

prepareSamples :: ProfileQuery p => p -> [SamplePair]
prepareSamples prof = foldl addSample [SP 0 0 [] []] (samples prof)

-- Must be called within "renderPrimitive Quads".
renderSampleAccumulated :: SamplePair -> IO ()
renderSampleAccumulated (SP t1 t2 smp1 smp2) = do
  let acc s1 s2 = scanl accCost (undefined,0,0) (zip s1 s2)
      accCost (_,c1,c2) ((ccid,c1'),(_,c2')) = (ccid,c1+c1',c2+c2')

  forM_ (zip <*> tail $ acc smp1 smp2) $ \((_,c1,c2),(ccid,c1',c2')) -> do
    color (colours !! ccid)
    vertex2 (realToFrac t1) (fromIntegral c1)
    vertex2 (realToFrac t2) (fromIntegral c2)
    vertex2 (realToFrac t2) (fromIntegral c2')
    vertex2 (realToFrac t1) (fromIntegral c1')

-- Must be called within "renderPrimitive Lines".
renderSampleSeparate :: SamplePair -> IO ()
renderSampleSeparate (SP t1 t2 smp1 smp2) = do
  forM_ (zip smp1 smp2) $ \((ccid,cost1),(_,cost2)) -> do
    color (colours !! ccid)
    vertex2 (realToFrac t1) (fromIntegral cost1)
    vertex2 (realToFrac t2) (fromIntegral cost2)

{-| Render a given list of prepared samples in the given mode.  The
third argument is the maximum time of the graph, which affects
horizontal scaling. -}

renderSamples :: GraphMode -> [SamplePair] -> Time -> IO ()
renderSamples Accumulated smps tmax = do
  let cmax = fromIntegral . maximum $ [sum (map snd smp) | SP _ _ _ smp <- smps]

  scale2 (1/realToFrac tmax) (1/cmax)

  renderPrimitive Quads $ forM_ smps renderSampleAccumulated

renderSamples Separate smps tmax = do
  let cmax = fromIntegral . maximum $ [cost | SP _ _ _ smp <- smps, (_,cost) <- smp]

  scale2 (1/realToFrac tmax) (1/cmax)

  renderPrimitive Lines $ forM_ smps renderSampleSeparate

{-| Integrating a new sample into the list of merged sample pairs we
have so far.  The input list should start with the latest sample, and
the new sample pair will be the head of the result. -}

addSample :: [SamplePair] -> (Time,ProfileSample) -> [SamplePair]
addSample smps (t,smp) = newSample : smps
    where newSample = mergeSamples (head smps) t (groupSmalls smp)
          mergeSamples (SP _ t1 _ smp1) t2 smp2 =
              SP { spTime1 = t1, spTime2 = t2, spData1 = smp1', spData2 = smp2' }
              where (smp1',smp2') = mergeSample smp1 (sort smp2)

          groupSmalls s = (0,sum . map snd $ sn) : map (\(ccid,cost) -> (ccid+1,cost)) sy
              where (sy,sn) = partition (\(_,c) -> c >= costLimit) s

          -- Merging key-value lists ordered by the key.  For each key that is
          -- present in only one of the lists we insert it with value 0 in the
          -- other list.
          mergeSample [] s = (map (\(ccid,_) -> (ccid,0)) s,s)
          mergeSample s [] = (s,map (\(ccid,_) -> (ccid,0)) s)
          mergeSample (s1@(cid1,cost1):ss1) (s2@(cid2,cost2):ss2) =
              if cid1 == cid2 then
                  let (smp1,smp2) = mergeSample ss1 ss2
                  in (s1:smp1,s2:smp2)
              else if cid1 > cid2 then
                       let (smp1,smp2) = mergeSample (s1:ss1) ss2
                       in if cost2 > 0 then ((cid2,0):smp1,s2:smp2)
                          else (smp1,smp2)
                   else let (smp1,smp2) = mergeSample ss1 (s2:ss2)
                        in if cost1 > 0 then (s1:smp1,(cid1,0):smp2)
                           else (smp1,smp2)

{-| Integrate a new sample in an extensible graph. -}

growGraph :: GraphData -> SinkInput -> IO GraphData
growGraph graph SinkStop = return graph
growGraph graph (SinkId ccid ccname) = return (modNames (IM.insert (ccid+1) (S.unpack ccname)) graph)
    where modNames f g = g { gdNames = f (gdNames g) }
growGraph graph (SinkSample t smp) = do
  let graph' = graph { gdSamples = addSample (gdSamples graph) (t,smp) }
      graph'' = if gdMinTime graph' <= 0 then graph' { gdMinTime = t } else graph'
      smps = gdSamples graph''

      -- The heart of the optimisation: compiling a tree of display lists as we go.
      dlUnion []             = return []
      dlUnion xs@((x,_,_):_) = if length prefix >= 20 then do
                                   dlAcc <- defineNewList Compile $ mapM_ clAcc prefix
                                   dlSep <- defineNewList Compile $ mapM_ clSep prefix
                                   dlUnion ((x+1,dlAcc,dlSep):rest)
                                 else return xs
          where (prefix,rest) = span (\(y,_,_) -> y == x) xs

      clAcc = \(_,dl,_) -> callList dl
      clSep = \(_,_,dl) -> callList dl

  dlAcc <- defineNewList Compile $ renderPrimitive Quads $ renderSampleAccumulated $ head smps
  dlSep <- defineNewList Compile $ renderPrimitive Lines $ renderSampleSeparate $ head smps

  dls' <- dlUnion ((0,dlAcc,dlSep):gdLists graph'')

  return (graph'' { gdLists = dls' })

{-| Render a stream in the given graph mode. -}

renderGraph :: GraphMode -> GraphData -> IO ()

renderGraph Accumulated graph = do
  let smps = gdSamples graph
      tmin = realToFrac $ gdMinTime graph
      tmax = realToFrac . spTime2 . head $ smps
      cmax = fromIntegral . maximum $ [sum (map snd smp) | SP _ _ _ smp <- take 50 smps]

  scale2 (1/(tmax-tmin)) (1/cmax)
  translate2 (-tmin) 0
  mapM_ (\(_,dl,_) -> callList dl) . gdLists $ graph

renderGraph Separate graph = do
  let smps = gdSamples graph
      tmin = realToFrac $ gdMinTime graph
      tmax = realToFrac . spTime2 . head $ smps
      cmax = fromIntegral . maximum $ [cost | SP _ _ _ smp <- take 50 smps, (_,cost) <- smp]

  scale2 (1/(tmax-tmin)) (1/cmax)
  translate2 (-tmin) 0

  mapM_ (\(_,_,dl) -> callList dl) . gdLists $ graph

-- Helper functions to make type disambiguation easier.

vertex2 :: GLfloat -> GLfloat -> IO ()
vertex2 x y = vertex $ Vertex2 x y

scale2 :: GLfloat -> GLfloat -> IO ()
scale2 x y = scale x y 1

translate2 :: GLfloat -> GLfloat -> IO ()
translate2 x y = translate $ Vector3 x y 0
