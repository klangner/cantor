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
                 , Package(..)
                 , packageDir 
                 , packageNew ) where


data Function = Function Name

data Class = Class Name [Function]

data Package = Package { packageName :: String 
                       , packageImports :: [String]
                       , packageClasses :: [Class] }
                       
         
type Name = String         


-- | Create new package with given name
packageNew :: String -> Package
packageNew name = Package name [] []


-- | Get package path from
packageDir :: Package -> FilePath
packageDir (Package a _ _) = map f a
    where f x | x == '.' = '/'
              | otherwise = x    
