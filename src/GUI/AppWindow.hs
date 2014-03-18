{- |
Module : GUI.AppWindow
Copyright : Copyright (C) 2014 Krzysztof Langner
License : BSD3

Maintainer : Krzysztof Langner <klangner@gmail.com>
Stability : alpha
Portability : portable

The application main window. 
-}
module GUI.AppWindow( GUI(..)
                    , AppMenu(..)
                    , ProjectMenu(..)
                    , Buttons(..)
                    , mainWindowNew ) where

import Graphics.UI.Gtk


data GUI = GUI { mainWindow :: Window 
               , appMenu :: AppMenu
               , actionButtons :: Buttons
               , diagramCanvas :: DrawingArea}

data AppMenu = AppMenu { projectMenu :: ProjectMenu }
data ProjectMenu = ProjectMenu { openMenu :: MenuItem
                               , exitMenu :: MenuItem }
                       
data Buttons = Buttons { openButton :: ToolButton }

-- | Switch to project page 
mainWindowNew :: IO GUI
mainWindowNew = do
    window <- windowNew
    (layout, menu, buttons, canvas) <- mainLayout
    set window [ containerChild := layout ]
    windowSetDefaultSize window 800 600
    windowSetPosition window WinPosCenter
    return $ GUI window menu buttons canvas
    
-- Main layout    
mainLayout :: IO (VBox, AppMenu, Buttons, DrawingArea)    
mainLayout = do    
    layout <- vBoxNew False 0
    (menu, am) <- mainMenuNew
    (ab, btns) <- actionbarNew
    canvas <- drawingAreaNew
    set layout [ containerChild := menu
               , containerChild := ab
               , containerChild := canvas
               , boxChildPacking menu := PackNatural
               , boxChildPacking ab := PackNatural
               , boxChildPacking canvas := PackGrow]
    return (layout, am, btns, canvas)
    
-- Create main menu
mainMenuNew :: IO (MenuBar, AppMenu)
mainMenuNew = do
    menu <- menuBarNew
    (prjMenu, pm) <- projectMenuNew
    containerAdd menu prjMenu
    return (menu, AppMenu pm)    
    
    
projectMenuNew :: IO (MenuItem, ProjectMenu)
projectMenuNew = do
    -- Create items
    menu <- menuItemNewWithLabel "Project"
    subMenu <- menuNew
    mOpen <- menuItemNewWithLabel "Open"
    mExit <- menuItemNewWithLabel "Exit"
    sep <- separatorMenuItemNew
    -- Build menu
    containerAdd subMenu mOpen
    containerAdd subMenu sep
    containerAdd subMenu mExit
    set menu [ menuItemSubmenu := subMenu ]
    return (menu, ProjectMenu mOpen mExit)
    
actionbarNew :: IO (Toolbar, Buttons)
actionbarNew = do
    tb <- toolbarNew
    btnOpen <- toolButtonNewFromStock stockOpen
    toolbarInsert tb btnOpen (-1) 
    return (tb, Buttons btnOpen)    