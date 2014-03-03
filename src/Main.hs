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
import System.Console.GetOpt
import System.Exit
import Utils.Folder
import Maven.Pom as Pom


data Flag = Maven             -- -m
          | Help               -- --help
          deriving (Eq,Ord,Enum,Show,Bounded)
        
        
main::IO ()
main = do
    argv <- getArgs
    case getOpt Permute flags argv of
        ([],[src],[]) -> 
            summaryAction src
     
        ([Maven],[_],[]) -> do 
            putStrLn "I'm sorry this command is not implemented yet."
            exitWith (ExitFailure 1) 
     
        (_,_,[]) -> do 
            putStrLn (usageInfo header flags)
            exitSuccess 
     
        (_,_,errs)      -> do
            putStrLn (concat errs ++ usageInfo header flags)
            exitWith (ExitFailure 1)
    
    where header = "Usage: mavex [-m] <project_path>"    
    

-- | Command line flags
flags :: [OptDescr Flag]
flags =
       [Option "m" ["maven"] (NoArg Maven)
            "Convert the project to maven build system."
       ,Option "" ["help"] (NoArg Help)
            "Print this help message"
       ]

       
-- | Print information about project
summaryAction :: FilePath -> IO ()
summaryAction src = do
    pom <- Pom.load (joinPaths src "pom.xml")
    _ <- putStrLn (if isValid pom then "Project: " ++ projectName pom else "Not maven project.")
    return ()