{- |
Module : AST.Model
Copyright : Copyright (C) 2014 Krzysztof Langner
License : BSD3

Maintainer : Krzysztof Langner <klangner@gmail.com>
Stability : alpha
Portability : portable

AST model 
-}
module AST.Model ( Package(..)
                 , packageFromList
                 , packageDirs ) where


data Package = Package { packageName :: String 
                       , subPackages ::  [Package]
                       , packageClasses :: [Class] 
                       , packageImports :: [String]}
                       
data Class = Class Name [Function]
data Function = Function Name
         
type Name = String         


-- | Convert list of names into package structure
packageFromList :: [String] -> Package
packageFromList [] = packageNew ""
packageFromList [x] = packageNew x
packageFromList (x:xs) = Package x [packageFromList xs] [] []

-- Create new package with given name
packageNew :: String -> Package
packageNew name = Package name [] [] []


-- | Get package path from
packageDirs :: Package -> [FilePath]
packageDirs (Package a [] _ _) = [a]
packageDirs (Package a xs _ _) = map ((a++"/") ++ ) sub
    where sub = concatMap packageDirs xs 
