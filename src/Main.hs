{- |
Module : Main
Copyright : Copyright (C) 2014 Krzysztof Langner
License : BSD3

Maintainer : Krzysztof Langner <klangner@gmail.com>
Stability : alpha
Portability : portable

Main application for gathering and presenting information about software project.
-}
module Main where

import System.Environment
import System.Console.GetOpt
import System.Exit
import Paths_cantor (version)
import Data.Version (showVersion)
import Cantor.Project (Project, projectLanguages, projectBuildSystem, projectReqs, scanProject)
import Cantor.KnowledgeDB (KnowledgeDB, conceptUrl, loadKDB)
import Cantor.Analysis.Metrics (lineOfCode)
import Cantor.Report
import Cantor.Parser.BuildSystem (bsProjectName, bsType)


data Flag = Version -- -v
          | Help -- --help
          | Arch -- --build
          | Build -- --build
          | Lang -- --languages
          | Modules -- --modules
          | Req -- --requirements
          deriving (Eq,Ord,Enum,Show,Bounded)
        
        
main::IO ()
main = do
    argv <- getArgs
    case getOpt Permute flags argv of
        ([Version], _, []) -> printVersion
        ([Help], _, []) -> printUsageInfo
        ([], [src], []) -> createFullReport src
        (xs, [src], []) -> analyzeProject xs src
        (_, _, []) -> printUsageInfo
        (_, _, errs) -> errorAction errs
    
    

-- | Command line flags
flags :: [OptDescr Flag]
flags =
       [ Option "a" ["architecture"] (NoArg Arch)
            "Describe project architecture."
       , Option "b" ["build"] (NoArg Build)
            "Check what build system is used by project."
       , Option "h" ["help"] (NoArg Help)
            "Print this help message."
       , Option "l" ["languages"] (NoArg Lang)
            "Check what languages is this application written in."
       , Option "m" ["modules"] (NoArg Modules)
            "Find independed modules in the project."
       , Option "r" ["requirements"] (NoArg Req)
            "Project requirements. What you need to on your OS to run this program"
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


-- | Build full report for given project
-- What is analyzed depends on passed flags
createFullReport :: FilePath -> IO ()
createFullReport src = analyzeProject [Lang, Build, Req, Arch] src


-- | Analize project and create report
-- What is analyzed depends on passed flags
analyzeProject :: [Flag] -> FilePath -> IO ()
analyzeProject xs src = do
    let db = loadKDB
    prj <- scanProject db src
    let pn = (bsProjectName . projectBuildSystem) prj
    let r0 = mkReport ("Project: " ++ pn)
    rs <- mapM (\x -> (f x) db prj) xs
    let r1 = addChapters r0 rs
    putStrLn $ markdown r1
    return ()
    where f Build = reportBuildSystem
          f Req = reportRequirements
          f Arch = reportArchitecture
          f _ = reportLanguages



-- | Analize langauge used in project
reportLanguages :: KnowledgeDB -> Project -> IO Report
reportLanguages db prj = do
    let lc = projectLanguages prj
    let langInfo = map (\(l, n) -> l ++ ": " ++ show n ++ " files.") lc
    let (lang, _) = foldl (\(l0, n0) (l, n) -> if(n > n0) then (l,n) else (l0,n0)) ("",0) lc
    loc <- lineOfCode db prj
    return $ mkChapter "Programming languages" [ mkParagraph [mkText "This project consists of the following languages:"]
                                               , mkList langInfo
                                               , mkParagraph [ mkText "The main language is "
                                                             , mkStrong lang
                                                             , mkText (" (" ++ (conceptUrl db lang) ++ ")")]
                                               , mkParagraph [ mkText $ "There are " ++ show loc ++ " lines of code."]
                                               ]

-- | Analize build system used by project
reportBuildSystem :: KnowledgeDB -> Project -> IO Report
reportBuildSystem db prj = do
    let bs = projectBuildSystem prj
    let xs1 = "This project is build with " ++ bsType bs ++ " (" ++ (conceptUrl db (bsType bs)) ++ ")"
    return $ mkChapter "Build system" [mkParagraph [mkText xs1]]

-- | Analize requirements
reportRequirements :: KnowledgeDB -> Project -> IO Report
reportRequirements _ prj = do
    let xs1 = concat (projectReqs prj)
    return $ mkChapter "Requirements" [mkParagraph [mkText xs1]]

-- | Analize architecture
reportArchitecture :: KnowledgeDB -> Project -> IO Report
reportArchitecture _ _ = do
    return $ mkChapter "Architecture" [mkParagraph [mkText "Not implemented yet"]]
