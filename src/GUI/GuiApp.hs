{-# LANGUAGE NoMonomorphismRestriction #-}
{- |
Module : GUI.GuiApp
Copyright : Copyright (C) 2014 Krzysztof Langner
License : BSD3

Maintainer : Krzysztof Langner <klangner@gmail.com>
Stability : alpha
Portability : portable

The application main window. 
-}
module GUI.GuiApp( runGuiApp ) where

import Control.Monad (when)
import Graphics.UI.Gtk
import GUI.AppWindow
--import Diagrams.Prelude
--import Diagrams.Backend.Cairo
--import Diagrams.Backend.Gtk
import Graphics.Rendering.Cairo


-- | Open GUI window with report data    
runGuiApp :: Maybe FilePath -> IO ()
runGuiApp src = do
    _ <- initGUI
    gui <- mainWindowNew
    connectActions gui
    _ <- diagramCanvas gui `onExpose` (\_ -> do scanProject src gui; return True)
    widgetShowAll (mainWindow gui)
    mainGUI


-- Connect all actions to the GUI elements
connectActions :: GUI -> IO ()
connectActions gui = do    
    _ <- onDestroy (mainWindow gui) mainQuit
    let menu = appMenu gui 
    let prjMenu = projectMenu menu
    _ <- on (exitMenu prjMenu) menuItemActivate mainQuit 
    _ <- on (openMenu prjMenu) menuItemActivate (openProject gui)
    let btns = actionButtons gui 
    _ <- openButton btns `onToolButtonClicked` openProject gui
    return ()
    
    
-- Scan project
scanProject :: Maybe FilePath -> GUI -> IO ()
scanProject (Just src) gui = drawDiagram src (diagramCanvas gui)
scanProject Nothing _ = return ()    


-- | Show open dialog to select project path and then start scanning project
openProject :: GUI -> IO ()
openProject gui = do
    putStrLn "open"
    dlg <- fileChooserDialogNew Nothing 
                                (Just (mainWindow gui)) 
                                FileChooserActionSelectFolder 
                                [("gtk-open"   , ResponseAccept)
                                ,("gtk-cancel" , ResponseCancel)]
    widgetShow dlg
    value <- dialogRun dlg
    when (value == ResponseAccept) $ do
          src <- fileChooserGetFilename dlg
          scanProject src gui 
    widgetHide dlg    
    return ()
    
    
-- Draw diagram    
drawDiagram :: FilePath -> DrawingArea -> IO ()
drawDiagram path canvas = do
    dw <- widgetGetDrawWindow canvas
    renderWithDrawable dw $ do 
        setSourceRGBA 1 1 1 1.0
        paint
        setSourceRGBA 0 1 0 1.0
        moveTo 100.0 100.0
        showText path

{-
-- Show status window during project scanning
scanningWindow :: GUI -> ((String -> IO ()) -> IO ()) -> IO ()
scanningWindow gui func = do
    -- Clear the status text
    labelSetText (swLabel gui) ""
    -- Start the operation
    childThread <- forkIO childTasks
    windowPresent (statusWin gui)
        where childTasks =
              do updateLabel "Starting thread..."
                 func updateLabel
                 -- After the child task finishes, enable OK
                 -- and disable Cancel
                 enableOK
                 
          enableOK = 
              do widgetSetSensitivity (swCancelBt gui) False
                 widgetSetSensitivity (swOKBt gui) True
                 onClicked (swOKBt gui) (widgetHide (statusWin gui))
                 return ()

          updateLabel text =
              labelSetText (swLabel gui) text
-}