{- |
Module : Report.Builder
Copyright : Copyright (C) 2014 Krzysztof Langner
License : BSD3

Maintainer : Krzysztof Langner <klangner@gmail.com>
Stability : alpha
Portability : portable

Main GUI application for gathering and presenting information..
-}
module Report.Builder (buildReport) where

import Report.Html
import Project.Core
import Metric.Basic

    
data Report = Report { reportProjectPath :: FilePath
                     , reportLoc :: [String]
                     , reportPkgCount :: Int
                     , reportSCP :: [[String]]}
                          
-- | Build projec report
buildReport :: FilePath -> Project -> IO ()
buildReport path prj = do
    let PackageGraph pkgs _ = projectPackages prj
    let title = guessProjectName (projectPath prj)
    loc <- locMetric (projectPath prj)
    let s = Report { reportProjectPath = projectPath prj
                    , reportLoc = loc
                    , reportPkgCount = length pkgs
                    , reportSCP = scp (projectPackages prj)}
    writeFile indexFile $ indexHtml title s
        where indexFile = path ++ "/index.html"
        
locMetric :: FilePath -> IO [String]
locMetric src = do
    loc <- lineOfCode src
    let loc1 = filter (\(_, a) -> a > 0) loc
    return $ map f loc1
        where f (lang, count) = lang ++ ": " ++ fmt count
              fmt :: Int -> String
              fmt a = if a > 1000 then show (a `div` 1000) ++ "K" else show a    
        
        

indexHtml :: String -> Report -> Html
indexHtml name s = html $ body $ 
    h1 name ++
    summary s ++
    dependencyReport (reportSCP s)
    
    
summary :: Report -> Html
summary s = h2 "Summary" ++
          table [] [ ["Project path:", reportProjectPath s]
                   , ["Lines of code:", ul (reportLoc s)]
                   , ["Package count:", show (reportPkgCount s)]
                   , ["Strongly connected packages:", show (length (concat (reportSCP s)))]]
                   
                   
dependencyReport:: [[String]] -> Html
dependencyReport groups = h2 "Strongly connected packages" ++
                          ol (map ul groups)
    
    