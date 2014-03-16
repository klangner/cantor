{- |
Module : Main
Copyright : Copyright (C) 2014 Krzysztof Langner
License : BSD3

Maintainer : Krzysztof Langner <klangner@gmail.com>
Stability : alpha
Portability : portable

Main GUI application for gathering and presenting information..
-}
module Main where

import System.Environment
import GUI.MainFrame


main::IO ()
main = do
    args <- getArgs
    let src = if null args then "" else head args
    runGuiApp src
