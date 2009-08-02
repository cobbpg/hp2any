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
--import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.Fix
--import qualified Data.ByteString.Char8 as S
import qualified Data.IntMap as IM
import Data.IORef
import Data.List
import Data.Maybe
import Foreign.Marshal.Alloc
import Foreign.Storable
import Graphics.UI.GLFW
import Graphics.Rendering.OpenGL hiding (Arg)
--import Graphics.Rendering.OpenGL.GL.DisplayLists
import Network
import Profiling.Heap.OpenGL
import Profiling.Heap.Read
import Profiling.Heap.Process
import Profiling.Heap.Types
import System.IO

import HandleArgs

data UIState = UIS
    { uisGraphMode :: GraphMode
    , uisCcid :: CostCentreId
    }

mapUisGraphMode f u = u { uisGraphMode = f (uisGraphMode u) }

startUiState = UIS
               { uisGraphMode = Accumulated
               , uisCcid = -1
               }

-- Helper functions to make type disambiguation easier.
--vertex2 :: GLfloat -> GLfloat -> IO ()
--vertex2 x y = vertex $ Vertex2 x y

translate2 :: GLfloat -> GLfloat -> IO ()
translate2 x y = translate $ Vector3 x y 0

scale2 :: GLfloat -> GLfloat -> IO ()
scale2 x y = scale x y 1

color3 :: GLfloat -> GLfloat -> GLfloat -> IO ()
color3 r g b = color $ Color3 r g b

main = withSocketsDo $ do
  profInfo <- graphArgs

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
          modifyIORef uiState (mapUisGraphMode nextGraphMode)
        _ -> return ()

  profData <- newEmptyMVar

  let procData =
          case profInfo of
            -- Connecting to the server and interpreting the profile stream
            -- messages it keeps sending while ignoring the rest.
            Left server -> Remote server
            -- Starting up the slave process and getting its heap profile
            -- updates through a message box.
            Right (exec,dir,params) -> Local (processToProfile exec dir params [])

  profileCallback procData (putMVar profData) >>= \cbres -> case cbres of
    Just (stop,_) -> do
      windowCloseCallback $= stop
      
      -- Looping as long as the other process is running.
      fix $ \consume -> do
        keepGoing <- accumGraph uiState graphData =<< takeMVar profData
        when keepGoing consume
    Nothing -> putStrLn "Error starting profile reader thread. Did you enable heap profiling?"

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
    where colsUsed = take (1 + IM.size (gdNames graph)) (backgroundColour:colours)

-- Consuming profiling input. If a new id comes, we just store it. If
-- a new sample comes, we pair it up with the last one in a way that
-- common cost centres are connected, and then update the graph.

-- Note that if the breakdown is by type, a name can appear more than
-- once in the list!
accumGraph uiState graphData profInput = do
  writeIORef graphData =<< flip growGraph profInput =<< readIORef graphData

  case profInput of
    SinkSample _ _ -> displayGraph uiState graphData
    _              -> return ()

  return (profInput /= SinkStop)

displayGraph uiState graphData = do
  uis <- readIORef uiState
  graph <- readIORef graphData

  clear [ColorBuffer]
  loadIdentity

  renderGraph (uisGraphMode uis) graph

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
