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
import Paths_cantor (version)
import Data.Version (showVersion)
import Project.Sources (findJavaClassPaths)
import Project.Model


data Flag = Version             -- -v
          | Help               -- --help
          deriving (Eq,Ord,Enum,Show,Bounded)
        
        
main::IO ()
main = do
    argv <- getArgs
    case getOpt Permute flags argv of
        ([], cmd:src:_, []) -> commandAction cmd src
        ([], [src], []) -> printPathsAction src
        ([Version], _, []) -> printVersion  
        ([Help], _, []) -> printUsageInfo
        (_, _, []) -> printUsageInfo
        (_, _, errs) -> errorAction errs
    
    

-- | Command line flags
flags :: [OptDescr Flag]
flags =
       [Option "v" ["version"] (NoArg Version)
            "Print version number."
       ,Option "h" ["help"] (NoArg Help)
            "Print this help message."
       ]


-- | Print application version number
printVersion :: IO ()
printVersion = 
    putStrLn $ "cantor version " ++ showVersion version
    
    
-- | This action prints errors
errorAction :: [String] -> IO ()
errorAction errs = do     
    putStrLn (concat errs)
    exitWith (ExitFailure 1)
    
       
-- | Print usage info
printUsageInfo :: IO ()
printUsageInfo = do
    putStrLn "Usage: cantor [command] <project_path>"
    putStrLn "  commands:"
    putStrLn "    <none> - Print source class paths."
    putStrLn (usageInfo "" flags)
    

-- | Print information about project
printProjectInfo :: Project -> IO ()
printProjectInfo prj = putStrLn (getProjectInfo prj) 
    

-- | Print information about class paths
printPathsAction :: FilePath -> IO ()
printPathsAction src = do
    paths <- findJavaClassPaths src
    putStrLn (if null paths then "Can't find any valid Java files in: " ++ src 
              else "Found Java class paths:")
    mapM_ putStrLn paths
    

-- | Execute command
commandAction :: String -> FilePath -> IO ()
commandAction _ _ = putStrLn "Commands not implemented yet."
                   