{- |
Module : Maven.Pom
Copyright : Copyright (C) 2014 Krzysztof Langner
License : BSD3

Maintainer : Krzysztof Langner <klangner@gmail.com>
Stability : alpha
Portability : portable

Get information from Maven POM file.
-}
module Maven.Pom ( Pom
                 , isValid
                 , load
                 , projectDesc
                 , projectName
                 , projectVersion
                 )where

import Text.XML.HXT.Core
import Data.Tree.NTree.TypeDefs
import Text.XML.HXT.XPath.XPathEval
import System.Directory (doesFileExist)


data Pom = Pom XmlTree | Empty deriving (Show)

-- | Check if Pom is valid
isValid :: Pom -> Bool
isValid Empty = False
isValid _ = True

-- | Load POM data from file and store as XML DOM
load :: FilePath -> IO Pom 
load p = do 
    fileExists <- doesFileExist p
    xs <- if fileExists then runX (readDocument [] p) else return []
    return $ if not (null xs) then Pom (head xs) else Empty
    
-- | Get project name from POM. 
projectName :: Pom -> String
projectName (Pom dom) = case getXPath "/project/name/text()" dom of
                         [NTree (XText a) _] -> a
                         _ -> ""
projectName _ = ""
    
-- | Get project description from POM. 
projectDesc :: Pom -> String
projectDesc (Pom dom) = case getXPath "/project/description/text()" dom of
                         [NTree (XText a) _] -> a
                         _ -> ""
projectDesc _ = ""

    
-- | Get project version. 
projectVersion :: Pom -> String
projectVersion (Pom dom) = case getXPath "/project/version/text()" dom of
                         [NTree (XText a) _] -> a
                         _ -> ""
projectVersion _ = ""