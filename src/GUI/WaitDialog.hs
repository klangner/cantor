{- |
Module : GUI.WaitDialog
Copyright : Copyright (C) 2014 Krzysztof Langner
License : BSD3

Maintainer : Krzysztof Langner <klangner@gmail.com>
Stability : alpha
Portability : portable

The application main window. 
-}
module GUI.WaitDialog ( WaitDlg(..)
                      , runWaitDlg
                      , waitDialogNew ) where

import Control.Concurrent
import Control.Monad (when)
import Graphics.UI.Gtk

-- | Data with widgets from wait dialog
data WaitDlg = WaitDlg Dialog Label ProgressBar

    
-- | Create wait dialog box
waitDialogNew :: String -> IO WaitDlg
waitDialogNew title = do
    dlg <- dialogNew
    set dlg [windowTitle := title]
    _ <- dialogAddButton dlg "gtk-cancel" ResponseCancel
    layout <- vBoxNew False 10
    spin <- progressBarNew
    progressBarSetPulseStep spin 1.0
    msg <- labelNew Nothing
    set layout [ containerChild := msg
               , containerChild := spin]
    upBox <- dialogGetUpper dlg
    set upBox [ containerChild := layout ]
    widgetShowAll upBox
    return $ WaitDlg dlg msg spin
    
-- | run wait dlg and spawn process for given function    
runWaitDlg :: String -> (WaitDlg -> IO ()) -> IO ()
runWaitDlg title fun = do
    wd <- waitDialogNew title
    let WaitDlg dlg _ _ = wd
    childThread <- forkIO $ fun wd
    value <- dialogRun dlg
    widgetHide dlg
    when (value == ResponseCancel) $ killThread childThread
    