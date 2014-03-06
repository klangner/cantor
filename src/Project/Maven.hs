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
                     , pomNew
                     , projectDesc
                     , projectName
                     , projectSrcPath
                     , projectTestPath
                     , projectVersion
                     , rename
                     , save
                     )where

import Text.XML.HXT.Core
import Data.Tree.NTree.TypeDefs
import Text.XML.HXT.XPath.XPathEval
import qualified Text.XML.HXT.Parser.XmlParsec as XP
import System.Directory (doesFileExist)
import System.FilePath (splitDirectories)


data Pom = Pom { projectSrc :: FilePath 
               , _xmlTree ::XmlTree
               } 
         | Empty deriving (Show)


-- | Check if Pom is valid
isValid :: Pom -> Bool
isValid Empty = False
isValid _ = True


-- | Create new POM file. Last part of the path is recognized as project name
pomNew :: String -> Pom
pomNew src = Pom src xmlTree
    where xmlTree = head $ XP.xread (pomXml name)
          name = last (splitDirectories src)
    

-- | Default empty POM XML
pomXml :: String -> String
pomXml name = "<project xmlns='http://maven.apache.org/POM/4.0.0' " 
              ++ "xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance' " 
              ++ "xsi:schemaLocation='http://maven.apache.org/POM/4.0.0 http://maven.apache.org/xsd/maven-4.0.0.xsd'>" 
              ++ "  <modelVersion>4.0.0</modelVersion>"
              ++ "  <groupId>com.klangner</groupId>"
              ++ "  <artifactId>ast</artifactId>"
              ++ "  <version>0.0.1-SNAPSHOT</version>"
              ++ "  <name>" ++ name ++ "</name>"
              ++ "</project>"


-- | Load POM data from file and store as XML DOM
load :: FilePath -> IO Pom 
load p = do 
    fileExists <- doesFileExist p
    xs <- if fileExists then runX (readDocument [] p) else return []
    return $ if not (null xs) then Pom p (head xs) else Empty


-- | Save POM
save :: Pom -> IO () 
save (Pom src dom) = do 
    _ <- runX $ root [] [constA dom] >>> writeDocument [withIndent yes] src
    return ()
save Empty = return ()     
    
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
                     
-- | Rename project
rename :: Pom -> String -> Pom
rename pom _ = pom

--rename' :: ArrowXml a => String -> a XmlTree XmlTree
--rename' name = processTopDownUntil $ changeText (const name) `when` isNameNode     
  --  where isNameNode = isElem >>> hasName "name"



