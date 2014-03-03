{- |
Module : MainFrame
Copyright : Copyright (C) 2014 Krzysztof Langner
License : BSD3

Maintainer : Krzysztof Langner <klangner@gmail.com>
Stability : alpha
Portability : portable

Create main window. 
-}
module MainFrame( displayReport
                , runGuiApp
                ) where

import Graphics.UI.Gtk
import Report


-- | UI state
data UI = UI Window

-- | Open GUI window with report data    
runGuiApp :: IO UI
runGuiApp = do
    _ <- initGUI
    window <- mainFrameNew
    _ <- onDestroy window mainQuit
    windowSetDefaultSize window 800 600
    windowSetPosition window WinPosCenter
    widgetShowAll window
    mainGUI
    return $ UI window

-- | Create main frame content
mainFrameNew :: IO Window
mainFrameNew = do
    window <- windowNew
    button <- buttonNew
    set window [ containerChild := button ]
    set button [ buttonLabel := "Hello World" ]
    _ <- onClicked button (putStrLn "Hello World")
    return window
    
-- | Display report
displayReport :: UI -> Report -> IO ()   
displayReport _ _ = return ()