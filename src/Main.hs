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
import System.FilePath (normalise)
import Messages
import Project.Maven as Pom


data Flag = Version             -- -v
          | Help               -- --help
          deriving (Eq,Ord,Enum,Show,Bounded)
        
        
main::IO ()
main = do
    argv <- getArgs
    case getOpt Permute flags argv of
        ([], cmd:src:_, []) -> commandAction cmd src
        ([], [src], []) -> analyzeAction src
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
    putStrLn "mavex version 0.1"
    
    
-- | This action prints errors
errorAction :: [String] -> IO ()
errorAction errs = do     
    putStrLn (concat errs)
    exitWith (ExitFailure 1)
    
       
-- | Print usage info
printUsageInfo :: IO ()
printUsageInfo = do
    putStrLn "Usage: mavex [command] <project_path>"
    putStrLn "  commands:"
    putStrLn "    create - Create maven POM file."
    putStrLn (usageInfo "" flags)
    

-- | Print information found in POM file
printMavenInfo :: FilePath -> Pom -> IO ()
printMavenInfo src pom | isValid pom = putStrLn (getProjectInfo pom) 
                       | otherwise = putStrLn $ errorNoPom src      
    

-- | Print information about project
analyzeAction :: FilePath -> IO ()
analyzeAction src = do
    pom <- Pom.load pomPath
    printMavenInfo src pom
    where pomPath = normalise (src ++ "/pom.xml")
    

-- | Execute command
commandAction :: String -> FilePath -> IO ()
commandAction _ _ = putStrLn "Commands not implemented yet."
                   