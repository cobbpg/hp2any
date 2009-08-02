{-# OPTIONS_GHC -Wall -fno-warn-missing-signatures -fno-warn-name-shadowing #-}

import Control.Applicative
import Control.Concurrent
import Control.Monad
import Control.Monad.Fix
import qualified Data.ByteString.Char8 as S
import Data.Array.MArray
import Data.IORef
import qualified Data.IntMap as IM
import Data.Maybe
import Data.List
import Data.Time.Clock
import Graphics.Rendering.Cairo as C
import Graphics.Rendering.OpenGL as GL hiding (get,set,samples)
import Graphics.Rendering.OpenGL.GL.DisplayLists
import Graphics.UI.Gtk as Gtk
import Graphics.UI.Gtk.Gdk.Events
import Graphics.UI.Gtk.Glade
import Graphics.UI.Gtk.OpenGL
import Profiling.Heap.OpenGL
import Profiling.Heap.Read
import Profiling.Heap.Types
import Profiling.Heap.Stats
import System.FilePath
import System.IO.Unsafe
import Text.Printf

import Paths_hp2any_manager (getDataFileName)

-- * OpenGL specific auxiliary functions

translate2 :: GLfloat -> GLfloat -> IO ()
translate2 x y = GL.translate $ Vector3 x y 0

scale2 :: GLfloat -> GLfloat -> IO ()
scale2 x y = GL.scale x y 1

-- * GTK specific auxiliary functions

{-| Flush the GTK event queue by running the necessary amount of main
loop iterations. -}

refresh :: IO ()
refresh = do
  count <- eventsPending
  replicateM_ count mainIteration
  when (count > 0) refresh

{-| Load the subtree of the UI description belonging to the widget of
the given name. -}

loadWidget :: String -> IO GladeXML
loadWidget name = do
  fileName <- getDataFileName "src/manager.glade"
  -- It would be nice if I could create a partially applied getWidget
  -- here, but monomorphism gets in the way...
  fromMaybe (error ("Error loading widget " ++ name)) <$>
            xmlNewWithRootAndDomain fileName (Just name) Nothing

{-| Insert the widget before the last child of the given box.  If
there's no last child, the insertion still happens. -}

addPenultimate :: (BoxClass box, WidgetClass widget) => widget -> box -> IO ()
addPenultimate child box = do
  children <- containerGetChildren box
  boxPackStart box child PackGrow 0

  unless (null children) $ do
    let lastChild = last children
    boxReorderChild box lastChild (-1)
    boxSetChildPacking box lastChild PackNatural 0 PackEnd

  widgetShowAll child

{-| Create a surface with the given string rendered on it with a font
that hopefully matches the rest of the interface.  When the surface is
not needed any more, it has to be explicitly disposed of by passing to
'surfaceFinish'. -}

renderString :: String -> IO Surface
renderString s = do
  -- Setting font
  ctx <- cairoCreateContext Nothing
  fontDesc <- contextGetFontDescription ctx
  fontDescriptionSetFamily fontDesc "Sans"
  contextSetFontDescription ctx fontDesc

  -- Setting resolution
  screenGetDefault >>= \s -> case s of
    Nothing -> return ()
    Just scr -> cairoContextSetResolution ctx =<< Gtk.get scr screenResolution

  -- Creating layout
  txt <- layoutText ctx s
  Rectangle _ _ w h <- snd <$> layoutGetPixelExtents txt

  -- Creating surface
  surface <- createImageSurface FormatARGB32 w h

  -- Rendering to surface
  renderWith surface $ showLayout txt

  return surface

renderSurface :: Surface -> GLWindow -> Int -> Int -> IO ()
renderSurface surface glWin x y = do
  buf <- pixbufFromImageSurface surface
  w <- imageSurfaceGetWidth surface
  h <- imageSurfaceGetHeight surface
  gc <- gcNew glWin
  drawPixbuf glWin gc buf 0 0 x y w h RgbDitherNone 0 0

-- * Widget constructors

{-| Create a filter to be used in a file picking dialog from a list of
textual description-wildcard pairs. -}

makeFileFilters :: [(String,String)] -> IO [FileFilter]
makeFileFilters fs = forM fs $ \(name,pat) -> do
  f <- fileFilterNew
  fileFilterSetName f name
  fileFilterAddPattern f pat
  return f

{-| Create a multi-selection file chooser dialog specific to .hp
files. -}

makeProfileChooserDialog :: IO FileChooserDialog
makeProfileChooserDialog = do
  dialog <- fileChooserDialogNew (Just "Load profile") Nothing FileChooserActionOpen
            [(stockOpen,ResponseOk),(stockCancel,ResponseCancel)]
  mapM_ (fileChooserAddFilter dialog) =<< makeFileFilters [("Heap profiles","*.hp"),("All files","*")]
  fileChooserSetSelectMultiple dialog True

  return dialog

{-| The type of the action that sets the progress percentage. -}

type SetProgress = Double -> IO ()

{-| Create a progress bar window along with three auxiliary functions,
which can set the text of the progress bar, the percentage of the
progress and the action to be taken when the cancel button is pressed.
The window is always closed by the cancel button, regardless of the
action specified. -}

makeProgressWindow :: IO (Window, String -> IO (), SetProgress, IO () -> IO ())
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
         , \n -> progressBarSetFraction progressBar n >> refresh
         , writeIORef cancelActRef
         )

{-| Create a new column to load graphs into. -}

makeColumn :: IO VBox
makeColumn = do
  column <- vBoxNew False 2
  closeButton <- buttonNewFromStock stockClose
  addButton <- buttonNewFromStock stockOpen
  boxPackStart column closeButton PackNatural 0
  boxPackEnd column addButton PackGrow 0

  -- Closing the column
  onClicked closeButton $ widgetDestroy column

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

{-| Create a scrollable and sortable list of colour-coded cost centres
to be shown next to the graph. -}

makeCostCentreList prof = do
  let costs = map sumCosts . groupBy fstEq . sort . concatMap snd $ samples prof
      sumCosts cs = (fst (head cs),sum (map snd cs))
      fstEq (x1,_) (x2,_) = x1 == x2

      modelData = zipWith modelSample (sort costs) (IM.assocs (ccNames prof))
      modelSample (ccid,cost) (_,ccname) = (ccid,ccname,cost)

      getName (ccid,ccname,_) = colSquare ++ " " ++ escapeMarkup (S.unpack ccname)
          where Color3 r g b = colours !! (ccid+1)
                colSquare = printf "<span background=\"#%02x%02x%02x\">    </span>" r g b
      getCost (_,_,cost) = cost

  mainBox <- hBoxNew False 0
  model <- listStoreNew modelData
  sortable <- treeModelSortNewWithModel model
  tree <- treeViewNewWithModel sortable
  widgetSetSizeRequest tree 0 0

  scrollPos <- fromJust <$> treeViewGetVAdjustment tree
  scrollBar <- vScrollbarNew scrollPos
  
  [nameColumn,costColumn] <- replicateM 2 treeViewColumnNew
  [nameRender,costRender] <- replicateM 2 cellRendererTextNew

  set nameRender [cellTextEllipsize := EllipsizeEnd]

  treeViewColumnPackStart nameColumn nameRender True
  treeViewColumnPackStart costColumn costRender True

  set nameColumn [ treeViewColumnTitle := "Name"
                 , treeViewColumnExpand := True
                 ]

  set costColumn [ treeViewColumnTitle := "Total cost"
                 , treeViewColumnSortColumnId := 1
                 ]

  treeSortableSetSortFunc sortable 1 $ \i1 i2 ->
    compare <$> (getCost <$> treeModelGetRow model i1)
            <*> (getCost <$> treeModelGetRow model i2)

  cellLayoutSetAttributes nameColumn nameRender model $ \ccdat -> [cellTextMarkup := Just (getName ccdat)]
  cellLayoutSetAttributes costColumn costRender model $ \ccdat -> [cellText := show (getCost ccdat)]

  mapM_ (treeViewAppendColumn tree) [nameColumn,costColumn]
  
  onScroll tree $ \Scroll { eventDirection = dir } -> do
    let mult = case dir of
                 ScrollUp -> -1
                 ScrollDown -> 1
                 _ -> 0
  
    step <- (mult*) <$> adjustmentGetStepIncrement scrollPos    
    valMax <- (-) <$> adjustmentGetUpper scrollPos <*> adjustmentGetPageSize scrollPos
    adjustmentSetValue scrollPos =<< (\val -> min (step+val) valMax) <$> adjustmentGetValue scrollPos
    adjustmentValueChanged scrollPos
    return True

  boxPackStart mainBox tree PackGrow 0
  boxPackStart mainBox scrollBar PackNatural 0

  -- Select the cost centre with the given colour
  rgb <- newIORef backgroundColour
  selection <- treeViewGetSelection tree

  let selectRgb rgbNew = do
        rgbOld <- readIORef rgb
        when (rgbNew /= rgbOld) $ do
          writeIORef rgb rgbNew
          if (rgbNew == backgroundColour) || (rgbNew == otherColour) then
              treeSelectionUnselectAll selection
            else do
              let Just idx = findIndex (==rgbNew) colours
              Just iter <- treeModelGetIterFromString model (show (idx-1))
              iter' <- treeModelSortConvertChildIterToIter sortable iter
              treeSelectionSelectIter selection iter'

  return (mainBox,selectRgb)

-- Fast hack: run this bugger only once in order to reduce the chance
-- of hanging...
glConfig = unsafePerformIO $ glConfigNew [GLModeRGB,GLModeDouble]

{-| Create an OpenGL graph widget including a toolbar.  The input is a
previously loaded heap profile and a function that can be used to
update a progress bar.  The latter is used during the phase where the
raw profile data is preprocessed for rendering, and it expects numbers
between 0 and 1. -}

makeProfileGraph :: Profile -> IO VBox
makeProfileGraph prof = do
  graphXml <- loadWidget "graphWidget"
  menuXml <- loadWidget "graphMenu"
  let getWidget cast name = xmlGetWidget graphXml cast name
      getMenuWidget cast name = xmlGetWidget menuXml cast name
      stats = buildStats prof

  graphWidget <- getWidget castToVBox "graphWidget"

  let getAncestors = do
        column <- castToVBox . fromJust <$> widgetGetParent graphWidget
        window <- castToHBox . fromJust <$> widgetGetParent column
        return (column,window)

  closeButton <- getWidget castToButton "closeButton"
  goLeftButton <- getWidget castToButton "goLeftButton"
  goRightButton <- getWidget castToButton "goRightButton"
  optionsButton <- getWidget castToButton "optionsButton"
  jobLabel <- getWidget castToLabel "jobLabel"
  graphPane <- getWidget castToHPaned "graphPane"

  graphMenu <- getMenuWidget castToMenu "graphMenu"

  labelSetText jobLabel $ job stats ++ " @ " ++ date stats

  -- It seems to randomly crap out at this point. :s
  -- But only if there is a text surface???
  -- gdk_gl_config_new_by_mode doesn't seem to return?
  -- glDrawableWait* calls in renderSurface don't help...
  --config <- glConfigNew [GLModeRGB,GLModeDouble]

  glCanvas <- glDrawingAreaNew glConfig
  (ccList,ccSelectRgb) <- makeCostCentreList stats

  panedPack1 graphPane glCanvas True True
  panedPack2 graphPane ccList True True

  widgetSetRedrawOnAllocate glCanvas True

  -- MonadFix helps us define a self-disconnecting signal. We don't
  -- know the size of the pane before it is exposed first, so we
  -- position the separator at that moment.
  mfix $ \sig -> onExpose graphPane $ const $ do
    columnWidth <- fst <$> (widgetGetSize =<< fst <$> getAncestors)
    panedSetPosition graphPane (columnWidth*2 `div` 3)
    signalDisconnect sig
    return False

  --textSurface <- renderString "Hello, cruel world!"

  -- Resizing the column open button if this was the last graph
  let removeFromColumn column = do
        siblings <- containerGetChildren column
        -- 3: column close button, new graph button, and the last graph
        -- that's just about to be destroyed
        when (length siblings <= 3) $ do
          boxSetChildPacking column (last siblings) PackGrow 0 PackEnd

        containerRemove column graphWidget
        
  onClicked closeButton $ do
    -- Removing the graph from the column
    --(column,_window) <- getAncestors
    --removeFromColumn column
    widgetDestroy graphWidget

    --surfaceFinish textSurface

  onClicked goLeftButton $ do
    (column,window) <- getAncestors

    let addLeft (cl:c:cs) | c == column = do
                              removeFromColumn column
                              addPenultimate graphWidget cl
                          | otherwise   = addLeft (c:cs)
        addLeft _ = return ()

    addLeft =<< map castToVBox . init <$> containerGetChildren window

  onClicked goRightButton $ do
    (column,window) <- getAncestors

    let addRight (c:cr:cs) | c == column = do
                               removeFromColumn column
                               addPenultimate graphWidget cr
                           | otherwise   = addRight (cr:cs)
        addRight _ = return ()

    addRight =<< map castToVBox . init <$> containerGetChildren window

  onClicked optionsButton $ menuPopup graphMenu Nothing

  graphRender <- newIORef []
  graphMode <- newIORef Accumulated

  let smps = prepareSamples stats
      tmax = realToFrac . maxTime $ stats

  -- Creation handler (called whenever the widget is removed and readded).
  onRealize glCanvas $ withGLDrawingArea glCanvas $ const $ do
    clearColor $= Color4 1 1 1 1
    
    -- Display lists have to be rebuilt every time. They can't be
    -- migrated between different canvases, which is annoying.
    [accList,sepList] <- forM [Accumulated,Separate] $ \mode ->
      defineNewList Compile (renderSamples mode smps tmax)
    
    let acc = preservingMatrix $ callList accList
        sep = do
          GL.lineWidth $= 4
          preservingMatrix $ callList sepList
    
    writeIORef graphRender [(Accumulated,acc),(Separate,sep)]

  -- We need to communicate with ourselves on dedicated channels...
  canvasSize <- newIORef (Size 0 0)

  let repaint = withGLDrawingArea glCanvas $ \glw -> do
        clear [ColorBuffer]
        
        size <- readIORef canvasSize
        viewport $= (Position 0 0,size)
        matrixMode $= Projection
        loadIdentity
        translate2 (-1) (-1)
        scale2 2 2
        matrixMode $= Modelview 0
        loadIdentity
        
        renders <- readIORef graphRender
        mode <- readIORef graphMode
        maybe (return ()) (preservingMatrix.snd) $ find ((==mode).fst) renders

        --renderSurface textSurface glw 10 10

        glDrawableSwapBuffers glw

  onSizeAllocate glCanvas $ \(Rectangle _ _ w h) -> do
    writeIORef canvasSize (Size (fromIntegral w) (fromIntegral h))

  -- Repaint handler, called after every resize for instance.
  onExpose glCanvas $ const $ repaint >> return True

  showCostCentres <- getMenuWidget castToCheckMenuItem "showCostCentres"
  viewMode <- getMenuWidget castToMenuItem "viewMode"

  let showHideCcList = do
        c <- get showCostCentres checkMenuItemActive
        (if c then widgetShowAll else widgetHideAll) ccList

      updateViewMode = do
        mode <- readIORef graphMode
        label <- castToLabel . fromJust <$> binGetChild viewMode
        labelSetText label $ "View mode: " ++ case mode of
          Accumulated -> "accumulated"
          Separate -> "separate"

  onActivateLeaf showCostCentres showHideCcList

  onActivateLeaf viewMode $ do
    modifyIORef graphMode nextGraphMode
    updateViewMode
    widgetQueueDraw glCanvas

  onButtonPress glCanvas $ \evt -> case evt of
    --Button { eventButton = RightButton, eventTime = t } -> do
    --  menuPopup graphMenu (Just (RightButton,t))
    --  return True
    --Button { eventButton = LeftButton, eventX = x, eventY = y } ->
    --  withGLDrawingArea glCanvas $ \glw -> do
    --    mpb <- pixbufGetFromDrawable glw (Rectangle (floor x) (floor y) 1 1)
    --    case mpb of
    --      Nothing -> return ()
    --      Just buf -> do
    --        dat <- pixbufGetPixels buf :: IO (PixbufData Int GLubyte)
    --        r <- readArray dat 0
    --        g <- readArray dat 1
    --        b <- readArray dat 2
    --        print (r,g,b)
    --    return True
    _ -> return False

  onMotionNotify glCanvas False $ \evt -> do
    let (x,y) = (floor (eventX evt),floor (eventY evt))

    withGLDrawingArea glCanvas $ \glw -> do
      mpb <- pixbufGetFromDrawable glw (Rectangle x y 1 1)
      case mpb of
        Nothing -> return ()
        Just buf -> do
          dat <- pixbufGetPixels buf :: IO (PixbufData Int GLubyte)
          r <- readArray dat 0
          g <- readArray dat 1
          b <- readArray dat 2
          ccSelectRgb $ Color3 r g b

    return True

  onExpose graphWidget $ const $ showHideCcList >> return False

  -- This should happen automatically, but apparently it doesn't...
  onSizeAllocate graphWidget $ const $ repaint >> widgetQueueDraw glCanvas

  updateViewMode
  return graphWidget

-- * Program logic

{-| Load the given list of files into the specified column.  The new
graphs will be created at the bottom. -}

loadHpFiles :: VBox -> [FilePath] -> IO ()
loadHpFiles column hpFiles = do
  let numFiles = length hpFiles

  when (numFiles > 0) $ do
    cancelled <- newIORef False
    let withCancelled act = readIORef cancelled >>= act
    (progWin,progString,progFrac,cancelHook) <- makeProgressWindow
    widgetShowAll progWin

    -- Load the files one by one
    forM_ (zip hpFiles [1..]) $ \(name,num) -> withCancelled $ \c -> unless c $ do
      progString $ takeFileName name ++ " (" ++ show (num :: Int) ++ "/" ++ show numFiles ++ ")"
      refresh
      
      -- Initiate loading
      (queryProgress,stopLoading) <- readProfileAsync name
      
      cancelHook $ do
        writeIORef cancelled True
        stopLoading

      tStart <- getCurrentTime

      -- Update the progress bar while loading
      profData <- fix $ \update -> do
        progress <- queryProgress
        case progress of
          Right res -> return res
          Left frac -> do
            progFrac (frac*0.5)
            threadDelay 50000
            update

      tEnd <- getCurrentTime

      -- Update a fake progress bar for the rest of the preparation,
      -- because the preparation of statistics cannot be truly
      -- monitored. We assume that it takes about the same time as
      -- loading, which is more or less realistic.  This doesn't work
      -- in ghci by default, but it does in the compiled executable
      -- (note that -threaded is not allowed).
      ptid <- forkIO $ forever $ do
        t <- getCurrentTime
        progFrac . realToFrac $ 0.5+min 0.5
                        (diffUTCTime t tEnd/(diffUTCTime tEnd tStart*2))
        threadDelay 50000

      -- Create the graph widget (more progress bar action)
      graph <- makeProfileGraph profData
      killThread ptid

      withCancelled $ \c -> if c
        then widgetDestroy graph
        else addPenultimate graph column

    widgetDestroy progWin

  return ()

-- * Entry point

main :: IO ()
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
