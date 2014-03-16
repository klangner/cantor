{- |
Module : GUI.MainFrame
Copyright : Copyright (C) 2014 Krzysztof Langner
License : BSD3

Maintainer : Krzysztof Langner <klangner@gmail.com>
Stability : alpha
Portability : portable

The application main window. 
-}
module GUI.MainFrame( UI
                , runGuiApp
                , switchToNoProject
                , switchToProject
                ) where

import Graphics.UI.Gtk


-- | UI data type
data UI = UI Window

-- | Open GUI window with report data    
runGuiApp :: FilePath -> IO ()
runGuiApp src = do
    _ <- initGUI
    window <- windowNew
    _ <- onDestroy window mainQuit
    windowSetDefaultSize window 800 600
    windowSetPosition window WinPosCenter
    if null src then switchToNoProject (UI window)
    else switchToProject (UI window) src
    widgetShowAll window
    mainGUI


-- | Switch to load project page 
switchToNoProject :: UI -> IO ()   
switchToNoProject (UI window) = do
    button <- buttonNew
    set window [ containerChild := button ]
    set button [ buttonLabel := "Load project" ]
    _ <- onClicked button (switchToProject  (UI window) "???")
    return ()

    
-- | Switch to project page 
switchToProject :: UI -> FilePath -> IO ()   
switchToProject (UI window) src = do
    button <- buttonNew
    set window [ containerChild := button ]
    set button [ buttonLabel := "Project path: " ++ src]
    _ <- onClicked button (switchToNoProject  (UI window))
    return ()
    