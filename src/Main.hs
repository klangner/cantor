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
import Cantor.Project (Project, scanProject)
import Cantor.KnowledgeDB (KnowledgeDB, conceptUrl, loadKDB)
import Cantor.Analysis.Language (countSourceFiles)


data Flag = Version -- -v
          | Help -- --help
          | Build -- --build
          | Lang -- --languages
          | Modules -- --modules
          deriving (Eq,Ord,Enum,Show,Bounded)
        
        
main::IO ()
main = do
    argv <- getArgs
    case getOpt Permute flags argv of
        ([Version], _, []) -> printVersion
        ([Help], _, []) -> printUsageInfo
        ([], [src], []) -> analyzeProject [Lang] src
        (xs, [src], []) -> analyzeProject xs src
        (_, _, []) -> printUsageInfo
        (_, _, errs) -> errorAction errs
    
    

-- | Command line flags
flags :: [OptDescr Flag]
flags =
       [ Option "b" ["build"] (NoArg Build)
            "Check what build system is used by project."
       , Option "h" ["help"] (NoArg Help)
            "Print this help message."
       , Option "l" ["languages"] (NoArg Lang)
            "Check what languages is this application written in."
       , Option "m" ["modules"] (NoArg Modules)
            "Find independed modules in the project."
       , Option "v" ["version"] (NoArg Version)
            "Print version number."
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
    

-- | Analize project and create report
-- What is analyzed depends on passed flags
analyzeProject :: [Flag] -> FilePath -> IO ()
analyzeProject xs src = do
    let db = loadKDB
    prj <- scanProject src
    mapM_ (\x -> (f x) db prj) xs
    return ()
    where f Build = analyzeBuildSystem
          f _ = analyzeLanguages



-- | Analize langauge used in project
analyzeLanguages :: KnowledgeDB -> Project -> IO ()
analyzeLanguages db prj = do
    putStrLn "Found source files:"
    let lc = countSourceFiles db prj
    mapM_ (\(l, n) -> putStrLn (l ++ ": " ++ show n)) lc
    let (lang, _) = foldl (\(l0, n0) (l, n) -> if(n > n0) then (l,n) else (l0,n0)) ("",0) lc
    putStrLn $ "This application is written in " ++ lang
    putStrLn $ "Check more about " ++ lang ++ " at:"
    putStrLn $ (conceptUrl db lang)
    return ()

-- | Analize build system used by project
analyzeBuildSystem :: KnowledgeDB -> Project -> IO ()
analyzeBuildSystem _ _ = do
    putStrLn $ "Build system used by project: " ++ "Unknown"
    return ()
