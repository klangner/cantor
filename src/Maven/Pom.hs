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
                 , isValidPom
                 , loadPom
                 )where

import Text.XML.HXT.Core
import System.Directory (doesFileExist)


data Pom = Pom XmlTree | Empty deriving (Show)

-- | Check if Pom is valid
isValidPom :: Pom -> Bool
isValidPom Empty = False
isValidPom _ = True

-- | Load POM data from file and store as XML DOM
loadPom :: FilePath -> IO Pom 
loadPom p = do 
    fileExists <- doesFileExist p
    xs <- if fileExists then runX (readDocument [] p) else return []
    return $ if not (null xs) then Pom (head xs) else Empty