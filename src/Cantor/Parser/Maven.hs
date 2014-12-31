{- |
Module : Cantor.Parser.Maven
Copyright : Copyright (C) 2014 Krzysztof Langner
License : BSD3

Maintainer : Krzysztof Langner <klangner@gmail.com>
Stability : alpha
Portability : portable

Parser for Maven POM file.
-}
module Cantor.Parser.Maven ( parseFile ) where

import Text.XML.HXT.Core
import Data.Tree.NTree.TypeDefs
import Text.XML.HXT.XPath.XPathEval
import Cantor.Parser.BuildSystem(BuildSystem, mkBuildSystem)



-- | Parse POM file.
parseFile :: FilePath -> IO BuildSystem
parseFile fp = do
    xs <- runX (readDocument [] fp)
    let bs = bsFromPom fp (head xs)
    return bs


-- Read metadata from pom XML
bsFromPom :: FilePath -> XmlTree -> BuildSystem
bsFromPom fp xt = mkBuildSystem fp "Maven" (readName xt)


-- Get project group from POM.
--readGroup :: XmlTree -> String
--readGroup = getNodeText "/project/groupId/text()"

-- Get project artifact from POM.
--readArtifact :: XmlTree -> String
--readArtifact = getNodeText "/project/artifactId/text()"

-- Get project name from POM.
readName :: XmlTree -> String
readName = getNodeText "/project/name/text()"

-- Get project description from POM.
--readDesc :: XmlTree -> String
--readDesc = getNodeText "/project/description/text()"

-- Get text from given node
getNodeText :: String -> XmlTree -> String
getNodeText xpath dom = case getXPath xpath dom of
                         [NTree (XText a) _] -> a
                         _ -> ""
