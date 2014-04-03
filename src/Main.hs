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
import System.Directory (createDirectoryIfMissing)
import System.Console.GetOpt
import System.Exit
import Data.List
import Paths_cantor (version)
import Data.Version (showVersion)
import Project.Core
import Project.Java
import Metric.Basic (lineOfCode)
import Report.Builder (buildReport)


data Flag = Version -- -v
          | Help -- --help
          deriving (Eq,Ord,Enum,Show,Bounded)
        
        
main::IO ()
main = do
    argv <- getArgs
    case getOpt Permute flags argv of
        ([], [src], []) -> analyzeProject src
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
    putStrLn "Usage: cantor <project_path>"
    putStrLn (usageInfo "" flags)
    

-- | Analize Java project and create report
analyzeProject :: FilePath -> IO ()
analyzeProject src = do
    reportFolder <- createReportFolder
    putStrLn "Scanning project..." 
    prj <- scanJavaProject src
    putStrLn "Build metrics"
    packageMetrics (projectPackages prj)
    putStrLn "Counting lines of code" 
    locMetric src
    buildReport reportFolder prj


-- | Create folder for report data
createReportFolder :: IO FilePath
createReportFolder = do
    createDirectoryIfMissing False path
    return path
    where path = "./cantor-report"
    
-- Print metrics
packageMetrics :: DependencyGraph -> IO ()
packageMetrics deps = do
    let groups = scp deps
    let count = sum $ map length groups
    putStrLn $ "There are " ++ show count ++ " strongly connected packages."
    mapM_ (putStrLn . intercalate "\n") groups
    
locMetric :: FilePath -> IO ()
locMetric src = do
    putStrLn "Line of code metric"
    loc <- lineOfCode src
    let loc1 = filter (\(_, a) -> a > 0) loc
    mapM_ f loc1
        where f (lang, count) = putStrLn $ lang ++ ": " ++ fmt count
              fmt :: Int -> String
              fmt a = if a > 1000 then show (a `div` 1000) ++ "K" else show a    

