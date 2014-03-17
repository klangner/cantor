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

import Graphics.UI.Gtk
import GUI.AppWindow


-- | Open GUI window with report data    
runGuiApp :: FilePath -> IO ()
runGuiApp _ = do
    _ <- initGUI
    gui <- mainWindowNew
    _ <- onDestroy (mainWindow gui) mainQuit
    widgetShowAll (mainWindow gui)
    mainGUI
