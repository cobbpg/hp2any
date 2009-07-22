{-# OPTIONS_GHC -Wall -fno-warn-missing-signatures #-}

import Control.Applicative
import Control.Concurrent
import Control.Monad
import Data.IORef
import Data.Maybe
import Data.List
import Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL.GL.DisplayLists
import Graphics.UI.Gtk as Gtk
import Graphics.UI.Gtk.Glade
import Graphics.UI.Gtk.OpenGL
import Profiling.Heap.OpenGL
import Profiling.Heap.Read as P
import Profiling.Heap.Types
import System.FilePath

import Paths_hp2any_manager (getDataFileName)

-- Why is this function not in gtk2hs?
refresh = do
  count <- eventsPending
  replicateM_ count mainIteration
  when (count > 0) refresh

loadWidget name = do
  fileName <- getDataFileName "src/manager.glade"
  -- It would be nice if I could create a partially applied getWidget
  -- here, but monomorphism gets in the way...
  fromMaybe (error ("Error loading widget " ++ name)) <$>
            xmlNewWithRootAndDomain fileName (Just name) Nothing

makeFileFilters fs = forM fs $ \(name,pat) -> do
  f <- fileFilterNew
  fileFilterSetName f name
  fileFilterAddPattern f pat
  return f

makeProfileChooserDialog = do
  dialog <- fileChooserDialogNew (Just "Load profile") Nothing FileChooserActionOpen
            [(stockOpen,ResponseOk),(stockCancel,ResponseCancel)]
  mapM_ (fileChooserAddFilter dialog) =<< makeFileFilters [("Heap profiles","*.hp"),("All files","*")]
  fileChooserSetSelectMultiple dialog True

  return dialog

makeProgressWindow = do
  progressXml <- loadWidget "progressWindow"
  let getWidget cast name = xmlGetWidget progressXml cast name

  progressWindow <- getWidget castToWindow "progressWindow"
  cancelButton <- getWidget castToButton "cancelProgressButton"
  progressBar <- getWidget castToProgressBar "progressBar"

  cancelActRef <- newIORef (return ())

  onClicked cancelButton $ do
    widgetDestroy progressWindow
    join (readIORef cancelActRef)

  return ( progressWindow
         , progressBarSetText progressBar
         , progressBarSetFraction progressBar
         , writeIORef cancelActRef
         )

loadHpFiles column hpFiles = do
  cancelled <- newIORef False

  (progWin,progString,progFrac,cancelHook) <- makeProgressWindow
  let numFiles = length hpFiles

  when (numFiles > 0) $ do
    widgetShowAll progWin
    
    forM_ (zip hpFiles [1..]) $ \(name,num) -> do
      isCancelled <- readIORef cancelled
      when (not isCancelled) $ do
        progString $ takeFileName name ++ " (" ++ show (num :: Int) ++ "/" ++ show numFiles ++ ")"
        refresh
        
        (queryProgress,stopLoading) <- readProfileAsync name
        
        let update = do
              progress <- queryProgress
              case progress of
                Right res -> return res
                Left frac -> do
                  progFrac frac
                  refresh
                  threadDelay 50000
                  update
        
        cancelHook $ do
          writeIORef cancelled True
          stopLoading

        graph <- makeProfileGraph =<< update
        isOkay <- not <$> readIORef cancelled
        when isOkay $ addPenultimate graph column

    widgetDestroy progWin

  return ()

addPenultimate child box = do
  lastChild <- last <$> containerGetChildren box
  boxPackStart box child PackGrow 0
  boxReorderChild box lastChild (-1)
  boxSetChildPacking box lastChild PackNatural 0 PackEnd
  widgetShowAll child

makeColumn = do
  column <- vBoxNew False 2
  closeButton <- buttonNewFromStock stockClose
  addButton <- buttonNewFromStock stockOpen
  boxPackStart column closeButton PackNatural 0
  boxPackEnd column addButton PackGrow 0

  -- Closing the column
  onClicked closeButton $ do
    parent <- fromJust <$> widgetGetParent column
    widgetDestroy column

    -- Needed to force refresh on OpenGL drawing areas
    widgetQueueDraw parent

  -- Adding a graph to the bottom of the column
  onClicked addButton $ do
    openDialog <- makeProfileChooserDialog
    widgetShow openDialog
    response <- dialogRun openDialog
    widgetHide openDialog

    when (response == ResponseOk) $ do
      loadHpFiles column =<< fileChooserGetFilenames openDialog

    widgetDestroy openDialog

  return column

makeProfileGraph prof = do
  graphXml <- loadWidget "graphWidget"
  let getWidget cast name = xmlGetWidget graphXml cast name

  graphWidget <- getWidget castToVBox "graphWidget"

  let getAncestors = do
        column <- castToVBox . fromJust <$> widgetGetParent graphWidget
        window <- castToHBox . fromJust <$> widgetGetParent column
        return (column,window)

  closeButton <- getWidget castToButton "closeButton"
  goLeftButton <- getWidget castToButton "goLeftButton"
  goRightButton <- getWidget castToButton "goRightButton"
  jobLabel <- getWidget castToLabel "jobLabel"

  glCanvas <- glDrawingAreaNew =<< glConfigNew [GLModeRGB,GLModeDouble]
  boxPackStart graphWidget glCanvas PackGrow 0
  widgetSetRedrawOnAllocate glCanvas True
  labelSetText jobLabel $ profJob prof ++ " @ " ++ profDate prof

  onClicked closeButton $ do
    (column,window) <- getAncestors

    -- Resizing the column open button if this was the last graph
    siblings <- containerGetChildren column
    when (length siblings <= 3) $ do
     boxSetChildPacking column (last siblings) PackGrow 0 PackEnd

    -- Removing the graph and refreshing the whole window (OpenGL needs it)
    widgetDestroy graphWidget
    widgetQueueDraw window

  onClicked goLeftButton $ do
    (column,window) <- getAncestors

    let addLeft (cl:c:cs) | c == column = do
                              containerRemove column graphWidget
                              addPenultimate graphWidget cl
                          | otherwise   = addLeft (c:cs)
        addLeft _ = return ()

    addLeft =<< map castToVBox . init <$> containerGetChildren window

  onClicked goRightButton $ do
    (column,window) <- getAncestors

    let addRight (c:cr:cs) | c == column = do
                               containerRemove column graphWidget
                               addPenultimate graphWidget cr
                           | otherwise   = addRight (cr:cs)
        addRight _ = return ()

    addRight =<< map castToVBox . init <$> containerGetChildren window

  graphRender <- newIORef undefined

  let smpls = prepareSamples prof
      tmax = realToFrac . maxTime $ prof
        
  -- Creation handler (called whenever the widget is removed and readded).
  onRealize glCanvas $ withGLDrawingArea glCanvas $ \_ -> do
    clearColor $= Color4 1 1 1 1
    
    -- Display lists have to be rebuilt every time. They can't be
    -- migrated between different canvases, which is annoying.
    accList <- defineNewList Compile (renderSamplesAccumulated smpls tmax)
    sepList <- defineNewList Compile (renderSamplesSeparate smpls tmax)
    
    let acc = preservingMatrix $ callList accList
        sep = do
          GL.lineWidth $= 4
          preservingMatrix $ callList sepList
    
    writeIORef graphRender (acc,sep)

  -- We need to communicate with ourselves on dedicated channels...
  canvasSize <- newIORef (Size 0 0)

  let repaint = withGLDrawingArea glCanvas $ \glwindow -> do
        clear [ColorBuffer]
        
        size <- readIORef canvasSize
        viewport $= (Position 0 0,size)
        matrixMode $= Projection
        loadIdentity
        translate2 (-1) (-1)
        scale2 2 2
        matrixMode $= Modelview 0
        loadIdentity
        
        (acc,_sep) <- readIORef graphRender
        acc

        glDrawableSwapBuffers glwindow

  onSizeAllocate glCanvas $ \(Rectangle _ _ w h) -> do
    writeIORef canvasSize (Size (fromIntegral w) (fromIntegral h))

  -- Repaint handler, called after every resize for instance.
  onExpose glCanvas $ \_ -> do
    repaint
    return True

  return graphWidget

main = do
  initGUI
  initGL

  mainWindow <- windowNew
  windowSetTitle mainWindow "Heap profile manager"
  onDestroy mainWindow mainQuit
  windowSetDefaultSize mainWindow 800 600
  mainColumns <- hBoxNew False 2
  containerAdd mainWindow mainColumns

  addColumnButton <- buttonNewWithLabel "+"
  boxPackEnd mainColumns addColumnButton PackNatural 0

  startColumn <- makeColumn
  boxPackStart mainColumns startColumn PackGrow 0

  onClicked addColumnButton $ do
    newColumn <- makeColumn
    boxPackStart mainColumns newColumn PackGrow 0
    boxReorderChild mainColumns addColumnButton (-1)
    widgetShowAll newColumn

  widgetShowAll mainWindow
  mainGUI

translate2 :: GLfloat -> GLfloat -> IO ()
translate2 x y = translate $ Vector3 x y 0

scale2 :: GLfloat -> GLfloat -> IO ()
scale2 x y = scale x y 1
