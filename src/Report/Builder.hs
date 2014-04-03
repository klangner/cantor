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

import Project.Core

type Html = String
    
-- | Build projec report
buildReport :: FilePath -> Project -> IO ()
buildReport path prj = do
    let title = guessProjectName (projectPath prj) 
    writeFile indexFile $ indexHtml title
        where indexFile = path ++ "/index.html"
        

indexHtml :: String -> Html
indexHtml name =
    "<html><body>" ++  
    "<h1>" ++ name ++ "</h1>" ++
    summary ++
    "</body></html>"
    
    
summary :: Html
summary = "<h2>Summary</h2>" ++
          "<table>" ++
          "</table>"              