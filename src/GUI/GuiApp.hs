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
import Data.Graph
import Data.Tree
import Data.List
import Graphics.UI.Gtk
import GUI.AppWindow
import GUI.WaitDialog
import Diagrams.Backend.Gtk
import Diagrams.Backend.Cairo
import Diagrams.Prelude
import Visualize.Diagram ( buildDiagram )
import Project.Types
import Project.Java


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
showPackages (Just src) gui = runWaitDlg "Scanning project" (processProject gui src)
showPackages Nothing _ = return ()    


-- | Process project
processProject :: GUI -> FilePath -> WaitDlg -> IO ()
processProject gui src (WaitDlg dlg msgLabel) = do
    postGUIAsync $ labelSetText msgLabel src
    prj <- scanJavaProject src
    let canvas = diagramCanvas gui 
    let d = buildDiagram (projectPackages prj)
    _ <- onExpose canvas (exposeCanvas (drawDiagram d canvas))
    postGUIAsync $ drawDiagram d canvas
    postGUIAsync $ dialogResponse dlg ResponseOk
    printMetrics (projectPackages prj)
        where exposeCanvas fun _ = do _ <- fun; return True
    

-- Draw diagram    
drawDiagram :: Diagram Cairo R2 -> DrawingArea -> IO ()
drawDiagram fig canvas = defaultRender canvas fig


-- Print some metrics to the console
printMetrics :: DependencyGraph -> IO ()
printMetrics (PackageGraph pkgs es) = do
    let loops = filter (not . null . subForest ) (scc graph)
    print "Strongly connected components: "
    let groups = map mkPkgGroup loops
    mapM_ (print . intercalate "->") groups
        where graph = buildG (0, length pkgs - 1) es
              mkPkgGroup node = nodeName node : concatMap mkPkgGroup (subForest node)
              nodeName node = pkgs !! rootLabel node
              
    
    