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
import Utils.Graph
import AST.Dependency
import GUI.AppWindow
import GUI.WaitDialog
import Diagrams.Backend.Gtk
import Diagrams.Backend.Cairo
import Diagrams.Prelude
import Report.Diagram ( buildDiagram )


-- | Open GUI window with report data    
runGuiApp :: Maybe FilePath -> IO ()
runGuiApp src = do
    _ <- initGUI
    gui <- mainWindowNew
    connectActions gui
    widgetShowAll (mainWindow gui)
    showPackages src gui
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
    vr <- dialogRun dlg
    widgetHide dlg    
    when (vr == ResponseAccept) $ do
          src <- fileChooserGetFilename dlg
          showPackages src gui 
    
    
-- Switch application state to show project dependencies
showPackages :: Maybe FilePath -> GUI -> IO ()
showPackages (Just src) gui = runWaitDlg "Scanning project" (processDependencies gui src)
showPackages Nothing _ = return ()    


-- | Process project
processDependencies :: GUI -> FilePath -> WaitDlg -> IO ()
processDependencies gui src (WaitDlg dlg msgLabel) = do
    postGUIAsync $ labelSetText msgLabel src
    g <- packageGraph src
    let graph = simplifyNames g
    let canvas = diagramCanvas gui
    let d = buildDiagram graph
    _ <- onExpose canvas (exposeCanvas (drawDiagram d canvas))
    postGUIAsync $ drawDiagram d canvas
    postGUIAsync $ dialogResponse dlg ResponseOk
        where exposeCanvas fun _ = do _ <- fun; return True
    

-- Draw diagram    
drawDiagram :: Diagram Cairo R2 -> DrawingArea -> IO ()
drawDiagram fig canvas = defaultRender canvas fig
