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
import AST.Dependency
import GUI.AppWindow
import GUI.WaitDialog
import Graphics.Rendering.Cairo


-- | Open GUI window with report data    
runGuiApp :: Maybe FilePath -> IO ()
runGuiApp src = do
    _ <- initGUI
    gui <- mainWindowNew
    connectActions gui
    widgetShowAll (mainWindow gui)
    showProjectDependency src gui
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
    
    
-- | Show open dialog to select project path and then start scanning project
openProject :: GUI -> IO ()
openProject gui = do
    dlg <- fileChooserDialogNew Nothing 
                                (Just (mainWindow gui)) 
                                FileChooserActionSelectFolder 
                                [("gtk-open"   , ResponseAccept)
                                ,("gtk-cancel" , ResponseCancel)]
    widgetShow dlg
    value <- dialogRun dlg
    widgetHide dlg    
    when (value == ResponseAccept) $ do
          src <- fileChooserGetFilename dlg
          showProjectDependency src gui 
    
    
-- Scan project
showProjectDependency :: Maybe FilePath -> GUI -> IO ()
showProjectDependency (Just src) gui = runWaitDlg "Scanning project" (processDependencies gui src)
showProjectDependency Nothing _ = return ()    


-- | Process project
processDependencies :: GUI -> FilePath -> WaitDlg -> IO ()
processDependencies gui src (WaitDlg dlg msgLabel) = do
    updateLabel src
    _ <- packages src
    postGUIAsync $ dialogResponse dlg ResponseOk
    postGUIAsync $ drawDiagram src (diagramCanvas gui)
        where updateLabel text = postGUIAsync $ labelSetText msgLabel text 


-- Draw diagram    
drawDiagram :: FilePath -> DrawingArea -> IO ()
drawDiagram path canvas = do
    dw <- widgetGetDrawWindow canvas
    renderWithDrawable dw $ do 
        setSourceRGBA 1 1 1 1.0
        paint
        setSourceRGBA 0.2 0.2 0.2 1.0
        moveTo 100.0 100.0
        showText path

