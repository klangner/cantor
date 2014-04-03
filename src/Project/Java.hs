{- |
Module : Project.Sources
Copyright : Copyright (C) 2014 Krzysztof Langner
License : BSD3

Maintainer : Krzysztof Langner <klangner@gmail.com>
Stability : alpha
Portability : portable

Get information about project from sources.
-}
module Project.Java ( findJavaClassPaths
                    , scanJavaProject )where

import System.FilePath (takeDirectory)
import Data.List (isSuffixOf)
import Data.Set (fromList, toList)
import Utils.Folder (isJavaFile, listFilesR)
import Project.Core
import Utils.List (unique)
import AST.JavaParser (parseFile, parseProject)
import AST.Model (ImportDecl(..), packageDir, packageName, packageImports)


type Dependencies = [(String, String)]
type Package = String

-- | Scan Java project
scanJavaProject :: FilePath -> IO Project
scanJavaProject src = do
    (pkg, depends) <- packageGraph src 
    return $ Project src (buildPackageGraph pkg depends)


-- | Find all source root path locations. 
-- Source paths all computed from source files location and 
-- package declaration.
findJavaClassPaths :: FilePath -> IO [FilePath] 
findJavaClassPaths src = do 
    files <- listFilesR isJavaFile src
    xs <- mapM javaFileClassPath files
    let validPaths = concatMap f xs
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

-- | Create packages graph        
packageGraph :: FilePath -> IO ([Package], Dependencies) 
packageGraph src = do 
    pkgs <- parseProject src
    let names = unique $ map packageName pkgs
    let depends = unique $ concatMap f pkgs
    let depends2 = removeExternalDepends names depends 
    return (names, depends2)
        where f p = map (\(ImportDecl x _) -> (packageName p, x)) (packageImports p)


-- | Remove from graph external dependencies
removeExternalDepends :: [Package] -> Dependencies -> Dependencies
removeExternalDepends vs = filter (\ (_, y) -> y `elem` vs)
        
