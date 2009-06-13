{-# OPTIONS_GHC -fno-warn-name-shadowing -fno-warn-missing-signatures #-}

{-

 glReadPixel is slow as hell, the alternative would be either to use
 the selection buffer (rumoured to be even slower...) or just
 calculate from the geometry. While the latter might be inconvenient
 for the simple purpose of finding out what the mouse cursor is
 covering at the moment, it can also be used to return the actual
 information from the data point in question (exact time and cost),
 so it should be used.

 This means that we'll have to take advantage of the data structures
 used by the simple interface in the core library while also needing
 the instant notification of the callback interface (so we don't have
 to poll). Another possibility is to add a function to be able to get
 a dirty bit, or even better, a list of new samples since the last
 reading, if we go for polling after all.

-}

import Control.Applicative
import Control.Concurrent.MVar
import Control.Monad
import qualified Data.ByteString.Char8 as S
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Data.IORef
import Data.List
import Data.Maybe
import Foreign.Marshal.Alloc
import Foreign.Storable
import Graphics.UI.GLFW
import Graphics.Rendering.OpenGL
import Graphics.Rendering.OpenGL.GL.DisplayLists
import Profiling.Heap.Read
import Profiling.Heap.Process
import System.Directory
import System.Environment
import System.Exit
import System.FilePath

data GraphData = GD
    { gdNames :: IntMap String
    , gdSamples :: [SamplePair]
    , gdLists :: [(Int, DisplayList, DisplayList)]
    }

emptyGraph = GD
             { gdNames = IM.singleton 0 "Other"
             , gdSamples = [SP 0 0 [] []]
             , gdLists = []
             }

mapGdNames   f g = g { gdNames   = f (gdNames   g) }
mapGdSamples f g = g { gdSamples = f (gdSamples g) }

data SamplePair = SP
    { spTime1 :: Time
    , spTime2 :: Time
    , spData1 :: ProfileSample
    , spData2 :: ProfileSample
    }

data UIState = UIS
    { uisGraphMode :: GraphMode
    , uisCcid :: CostCentreId
    }

mapUisGraphMode f u = u { uisGraphMode = f (uisGraphMode u) }

startUiState = UIS
               { uisGraphMode = GMAccumulated
               , uisCcid = -1
               }

data GraphMode = GMAccumulated | GMSeparate

-- Helper functions to make type disambiguation easier.
vertex2 :: GLfloat -> GLfloat -> IO ()
vertex2 x y = vertex $ Vertex2 x y

translate2 :: GLfloat -> GLfloat -> IO ()
translate2 x y = translate $ Vector3 x y 0

scale2 :: GLfloat -> GLfloat -> IO ()
scale2 x y = scale x y 1

color3 :: GLfloat -> GLfloat -> GLfloat -> IO ()
color3 r g b = color $ Color3 r g b

bkgColour :: Color3 GLubyte
bkgColour = Color3 255 255 255

main = do
  let fixName n = if takeFileName n == n then "./"++n else n

  -- Will switch to proper argument handling later...
  args <- getArgs
  (exec,dir,params) <- case args of
     [name] -> return (fixName name,Nothing,"")
     name:"-d":dir:rest -> do
       cdir <- Just <$> canonicalizePath dir
       if null rest
         then return (fixName name,cdir,"")
         else return (fixName name,cdir,concatMap (' ':) (tail rest))
     name:"--":params -> return (fixName name,Nothing,concatMap (' ':) params)
     _ -> do putStrLn "Usage: hp2any-graph <exec> [-d <working dir>] [-- parameters to pass]"
             exitFailure

  let procData = processToProfile exec dir params []

  initialize
  openWindow (Size 800 600) [DisplayRGBBits 8 8 8, DisplayAlphaBits 8] Window
  windowTitle $= "hp2any live graph"

  clearColor $= Color4 1 1 1 1
  blend $= Enabled
  blendFunc $= (SrcAlpha,OneMinusSrcAlpha)
  lineWidth $= 4

  -- Variables used for communication between callbacks.
  graphData <- newIORef emptyGraph
  uiState <- newIORef startUiState

  -- Window size needs to be monitored only to adjust the viewport.
  windowSizeCallback $= \size -> do
    viewport $= (Position 0 0,size)
    matrixMode $= Projection
    loadIdentity
    translate2 (-1) (-1)
    scale2 2 2
    matrixMode $= Modelview 0
    displayGraph uiState graphData

  -- If the mouse is moved, we find out which cost centre it is
  -- hovering over, and refresh the display if there is a change.
  mousePosCallback $= \pos -> do
    -- Note: maybe we want to use colour index mode to make colour
    -- picking easier, but it's not likely, since it can put an
    -- unpredictable limit on the number of colours we can use, and
    -- traversing a list of at most a few hundred elements a few times
    -- a second shouldn't cause much problem (besides, we can switch
    -- to a map later if we want to).
    readBuffer $= FrontBuffers
    ccid <- colourToCcid <$> readIORef graphData <*> hoverColour pos
    uis <- readIORef uiState
    when (ccid /= uisCcid uis) $ displayGraph uiState graphData

  -- Handling keyboard events.
  keyCallback $= \key keyState ->
      case (key,keyState) of
        (CharKey 'M',Press) -> do
          uis <- readIORef uiState
          let newGraphMode GMAccumulated = GMSeparate
              newGraphMode GMSeparate = GMAccumulated
          writeIORef uiState $ mapUisGraphMode newGraphMode uis
        _ -> return ()

  -- Starting up the slave process and getting its heap profile
  -- updates through a message box.
  profData <- newEmptyMVar
  (_,stop) <- profileCallback procData (putMVar profData)

  -- Tying the stop action to the window close event.
  windowCloseCallback $= stop

  -- Looping as long as the other process is running.
  let consume = do
        keepGoing <- accumGraph uiState graphData =<< (takeMVar profData)
        when keepGoing consume

  consume

  closeWindow

-- RGB values under the mouse cursor.
hoverColour (Position x y) = allocaBytes 4 $ \colData -> do    
  Size _ h <- get windowSize
  readPixels (Position x (h-y)) (Size 1 1) (PixelData RGBA UnsignedByte colData)
  r <- peekElemOff colData 0
  g <- peekElemOff colData 1
  b <- peekElemOff colData 2
  return (Color3 r g b)

-- RGB to cost centre id, -1 being the background colour.
colourToCcid graph col = fromMaybe 0 (elemIndex col colsUsed) - 1
    where colsUsed = take (1 + IM.size (gdNames graph)) (bkgColour:colours)

-- Consuming profiling input. If a new id comes, we just store it. If
-- a new sample comes, we pair it up with the last one in a way that
-- common cost centres are connected, and then update the graph.

-- Note that if the breakdown is by type, a name can appear more than
-- once in the list!
accumGraph uiState graphData profInput = do
  let addSample t smp smps = mergeSamples (head smps) t (groupSmalls smp) : smps

      mergeSamples (SP _ t1 _ smp1) t2 smp2 =
          SP { spTime1 = t1, spTime2 = t2, spData1 = smp1', spData2 = smp2' }
          where (smp1',smp2') = mergeSample smp1 (sort smp2)

      groupSmalls smp = (0,sum . map snd $ sn) : map (\(ccid,cost) -> (ccid+1,cost)) sy
          where (sy,sn) = partition (\(_,c) -> c >= 256) smp

  graph <- readIORef graphData

  case profInput of
    SinkSample t smp   -> do let graph' = mapGdSamples (addSample t smp) graph
                             writeIORef graphData graph'
                             displayGraph uiState graphData
                             return True
    SinkId ccid ccname -> do writeIORef graphData $ mapGdNames (IM.insert (ccid+1) (S.unpack ccname)) graph
                             return True
    SinkStop           -> return False

-- Merging key-value lists ordered by the key.  For each key that is
-- present in only one of the lists we insert it with value 0 in the
-- other list.
mergeSample [] smp = (map (\(ccid,_) -> (ccid,0)) smp,smp)
mergeSample smp [] = (smp,map (\(ccid,_) -> (ccid,0)) smp)
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

-- The colours of the cost centres.  The first element is reserved for
-- the cost centres that fall below a certain limit (referred to as
-- "Other"), so they are lumped together to prevent littering the
-- graph.
colours :: [Color3 GLubyte]
colours = concatMap makeCol [0..]
    where comps = 0 : 255 : Data.List.unfoldr cnext (256,127 :: Int)
          cnext (s,c) = Just (fromIntegral c,if s+fromIntegral c >= 255 then (s `div` 2,s `div` 4-1) else (s,s+c))
          makeCol n = if n == 1 then init res else res
              where res = [Color3 (comps !! rn) (comps !! gn) (comps !! bn) |
                           rn <- [0..n], gn <- [0..n], bn <- [0..n],
                           rn == n || gn == n || bn == n]

displayGraph uiState graphData = do
  uis <- readIORef uiState
  graph <- readIORef graphData

  -- The limits of the graph in both dimensions.
  let samples = gdSamples graph
      maxTime = realToFrac . spTime2 . head $ samples
      maxCost = case uisGraphMode uis of
                  GMAccumulated -> maxCostAcc
                  GMSeparate    -> maxCostSep
      maxCostSep = fromIntegral . maximum $ [cost | SP _ _ _ smp <- take 50 samples, (_,cost) <- smp]
      maxCostAcc = fromIntegral . maximum $ [sum (map snd smp) | SP _ _ _ smp <- take 50 samples]

  clear [ColorBuffer]
  loadIdentity
  scale2 (1/maxTime) (1/maxCost)

  -- Simultaneously build display lists for both modes.
  dlAcc <- defineNewList Compile $ renderPrimitive Quads $ do
    let acc smp1 smp2 = scanl accCost (undefined,0,0) (zip smp1 smp2)
        accCost (_,c1,c2) ((ccid,c1'),(_,c2')) = (ccid,c1+c1',c2+c2')
        SP t1 t2 smp1 smp2 = head samples
    forM_ (zip <*> tail $ acc smp1 smp2) $ \((_,cost1,cost2),(ccid,cost1',cost2')) -> do
      color (colours !! ccid)
      vertex2 (realToFrac t1) (fromIntegral cost1)
      vertex2 (realToFrac t2) (fromIntegral cost2)
      vertex2 (realToFrac t2) (fromIntegral cost2')
      vertex2 (realToFrac t1) (fromIntegral cost1')

  dlSep <- defineNewList Compile $ renderPrimitive Lines $ do
    let SP t1 t2 smp1 smp2 = head samples
    forM_ (zip smp1 smp2) $ \((ccid,cost1),(_,cost2)) -> do
      color (colours !! ccid)
      vertex2 (realToFrac t1) (fromIntegral cost1)
      vertex2 (realToFrac t2) (fromIntegral cost2)

  let dlUnion []             = return []
      dlUnion xs@((x,_,_):_) = if length prefix >= 20 then do
                                   dlAcc <- defineNewList Compile $ mapM_ clAcc prefix
                                   dlSep <- defineNewList Compile $ mapM_ clSep prefix
                                   dlUnion ((x+1,dlAcc,dlSep):rest)
                                 else return xs
          where (prefix,rest) = span (\(y,_,_) -> y == x) xs

      clAcc = \(_,dl,_) -> callList dl
      clSep = \(_,_,dl) -> callList dl

  dls' <- dlUnion ((0,dlAcc,dlSep):gdLists graph)

  writeIORef graphData (graph { gdLists = dls' })

  -- Time to render according to the current mode!
  case uisGraphMode uis of
    GMAccumulated -> mapM_ clAcc dls'
    GMSeparate    -> mapM_ clSep dls'

  --forM_ (IM.assocs ccnames) $ \(ccid,ccname) -> do
  --  color (colours !! ccid)
  --  translate2 0 (-16)
  --  renderString Fixed8x16 ccname

  flush

  -- The graph is rendered at this point, so we can find out which
  -- cost centre the mouse cursor is resting on.
  readBuffer $= BackBuffers
  ccid <- colourToCcid graph <$> (hoverColour =<< get mousePos)

  -- The value is only stored so we can easily check later whether it
  -- changed.
  writeIORef uiState $ uis { uisCcid = ccid }

  -- Displaying the text.  Note that the GLFW routine uses textured
  -- quads for this task.
  let magn = 1
  
  loadIdentity
  Size w h <- get windowSize
  scale2 (magn/fromIntegral w) (magn/fromIntegral h)
  translate2 0 (fromIntegral h/magn-16)
  
  color3 0 0 0
  renderString Fixed8x16 (fromMaybe "" (IM.lookup ccid (gdNames graph)))

  translate2 (fromIntegral w/magn-28*8) 0
  renderString Fixed8x16 "Press M to change graph mode"

  flush

  swapBuffers
