{- |
Module : Project.Maven
Copyright : Copyright (C) 2014 Krzysztof Langner
License : BSD3

Maintainer : Krzysztof Langner <klangner@gmail.com>
Stability : alpha
Portability : portable

Get information about project from Maven POM file.
-}
module Project.Maven ( Pom
                     , getProjectInfo
                     , isValid
                     , load
                     , projectDesc
                     , projectName
                     , projectSrcPath
                     , projectTestPath
                     , projectVersion
                     )where

import Text.XML.HXT.Core
import Data.Tree.NTree.TypeDefs
import Text.XML.HXT.XPath.XPathEval
import System.Directory (doesFileExist)


data Pom = Pom { projectSrc :: FilePath 
               , _xmlTree ::XmlTree
               } 
         | Empty deriving (Show)

-- | Check if Pom is valid
isValid :: Pom -> Bool
isValid Empty = False
isValid _ = True

-- | Load POM data from file and store as XML DOM
load :: FilePath -> IO Pom 
load p = do 
    fileExists <- doesFileExist p
    xs <- if fileExists then runX (readDocument [] p) else return []
    return $ if not (null xs) then Pom p (head xs) else Empty
    
-- | Get project name from POM. 
projectName :: Pom -> String
projectName (Pom _ dom) = case getXPath "/project/name/text()" dom of
                         [NTree (XText a) _] -> a
                         _ -> ""
projectName _ = ""
    
-- | Get project description from POM. 
projectDesc :: Pom -> String
projectDesc (Pom _ dom) = case getXPath "/project/description/text()" dom of
                         [NTree (XText a) _] -> a
                         _ -> ""
projectDesc _ = ""

    
-- | Get project version. 
projectVersion :: Pom -> String
projectVersion (Pom _ dom) = case getXPath "/project/version/text()" dom of
                         [NTree (XText a) _] -> a
                         _ -> ""
projectVersion _ = ""

    
-- | Get project source path. 
projectSrcPath :: Pom -> String
projectSrcPath (Pom _ dom) = case getXPath "/project/build/sourceDirectory/text()" dom of
                         [NTree (XText a) _] -> a
                         _ -> "src/main/java"
projectSrcPath _ = ""


-- | Get project tests path. 
projectTestPath :: Pom -> String
projectTestPath (Pom _ dom) = case getXPath "/project/build/testSourceDirectory/text()" dom of
                         [NTree (XText a) _] -> a
                         _ -> "src/test/java"
projectTestPath _ = ""


-- | Get project info
getProjectInfo :: Pom -> String
getProjectInfo pom = "POM location: " ++ projectSrc pom ++ "\n" ++
                     "Project name: " ++ projectName pom ++ "\n" ++
                     "Sources: " ++ projectSrcPath pom ++ "\n" ++
                     "Tests: " ++ projectTestPath pom ++ "\n"
