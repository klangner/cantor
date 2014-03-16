{- |
Module : AST.Model
Copyright : Copyright (C) 2014 Krzysztof Langner
License : BSD3

Maintainer : Krzysztof Langner <klangner@gmail.com>
Stability : alpha
Portability : portable

AST model 
-}
module AST.Model ( Class(..)
                 , Function(..)
                 , ImportDecl (..)
                 , Package(..)
                 , packageDir 
                 , packageNew ) where

type Name = String         

-- | Package
data Package = Package { packageName :: String 
                       , packageImports :: [ImportDecl]
                       , packageClasses :: [Class] } 
         
-- | Import declaration with package name and class name         
data ImportDecl = ImportDecl Name Name deriving (Eq, Show)

data Class = Class Name [Function] deriving (Eq, Show)

data Function = Function Name deriving (Eq, Show)


-- | Create new package with given name
packageNew :: String -> Package
packageNew name = Package name [] []


-- | Get package path from
packageDir :: Package -> FilePath
packageDir (Package a _ _) = map f a
    where f x | x == '.' = '/'
              | otherwise = x    
