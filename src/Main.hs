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
-- import System.Directory (createDirectoryIfMissing)
import System.Console.GetOpt
import System.Exit
import Paths_cantor (version)
import Data.Version (showVersion)
import Cantor.Project (scanProject)
import Cantor.Expert (mkExpert)
import Cantor.Analysis.Language (countSourceFiles)


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
    let expert = mkExpert
    prj <- scanProject src
    putStrLn "Found source files:"
    let lc = countSourceFiles expert prj
    mapM_ (\(l, n) -> putStrLn (l ++ ": " ++ show n)) lc
    return ()

{-
-- | Create folder for report data
createReportFolder :: IO FilePath
createReportFolder = do
    createDirectoryIfMissing False path
    return path
    where path = "./cantor-report"
-}
