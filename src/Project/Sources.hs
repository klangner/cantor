{- |
Module : Project.Sources
Copyright : Copyright (C) 2014 Krzysztof Langner
License : BSD3

Maintainer : Krzysztof Langner <klangner@gmail.com>
Stability : alpha
Portability : portable

Get information about project from sources.
-}
module Project.Sources ( findJavaClassPaths )where

import System.FilePath (takeDirectory)
import Data.List (isSuffixOf)
import Data.Set (fromList, toList)
import Utils.Folder (isJavaFile, listFilesR)
import AST.Parser.JavaParser (parseFile)
import AST.Model (packageDir)


-- | Find all source root path locations. 
-- Source paths all computed from source files location and 
-- package declaration.
findJavaClassPaths :: FilePath -> IO [FilePath] 
findJavaClassPaths src = do 
    files <- listFilesR isJavaFile src
    paths <- mapM javaFileClassPath files
    let validPaths = concatMap f paths
    return $ removeDuplicates validPaths
        where f :: Maybe FilePath -> [FilePath] 
              f (Just a) = [a]
              f Nothing = []

removeDuplicates :: Ord a => [a] -> [a]
removeDuplicates = toList . fromList
    
    
-- | Find source root folder from AST    
javaFileClassPath :: FilePath -> IO (Maybe FilePath)
javaFileClassPath src = do
    result <- parseFile src
    case result of
        Right pkg -> do let path = takeDirectory src
                        let dir = '/' : packageDir pkg
                        return $ removeSuffix path dir
        Left _ -> return Nothing

    
removeSuffix :: String -> String -> Maybe String
removeSuffix xs sufix = 
    if sufix `isSuffixOf` xs then Just (take len xs)
    else Nothing
        where len = length xs - length sufix
