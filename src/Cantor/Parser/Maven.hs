{- |
Module : Cantor.Parser.Maven
Copyright : Copyright (C) 2014 Krzysztof Langner
License : BSD3

Maintainer : Krzysztof Langner <klangner@gmail.com>
Stability : alpha
Portability : portable

Parser for Maven POM file.
-}
module Cantor.Parser.Maven ( Maven
                           , groupId
                           , projectId
                           , projectName
                           , projectDescription
                           , parseFile ) where

import Text.XML.HXT.Core
import Data.Tree.NTree.TypeDefs
import Text.XML.HXT.XPath.XPathEval
import System.FilePath (normalise)
import System.Directory (doesFileExist)


data Maven = Maven { groupId :: String
                   , projectId :: String
                   , projectName :: String
                   , projectDescription :: String
                   } deriving (Show)


-- | Parser POM file.
parseFile :: FilePath -> IO (Maybe Maven)
parseFile src = do
    let pomPath = normalise (src ++ "/pom.xml")
    fileExists <- doesFileExist pomPath
    xs <- if fileExists then runX (readDocument [] pomPath) else return []
    return $ if not (null xs) then Just (readPomXml (head xs)) else Nothing


-- | Read metadata from pom XML
readPomXml :: XmlTree -> Maven
readPomXml xt = Maven (readGroup xt) (readArtifact xt) (readName xt) (readDesc xt)


-- | Get project group from POM. 
readGroup :: XmlTree -> String
readGroup = getNodeText "/project/groupId/text()"

-- | Get project artifact from POM. 
readArtifact :: XmlTree -> String
readArtifact = getNodeText "/project/artifactId/text()"

-- | Get project name from POM. 
readName :: XmlTree -> String
readName = getNodeText "/project/name/text()"

-- | Get project description from POM. 
readDesc :: XmlTree -> String
readDesc = getNodeText "/project/description/text()"

-- | Get text from given node 
getNodeText :: String -> XmlTree -> String
getNodeText xpath dom = case getXPath xpath dom of
                         [NTree (XText a) _] -> a
                         _ -> ""
