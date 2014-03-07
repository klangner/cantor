{- |
Module : AST.Model
Copyright : Copyright (C) 2014 Krzysztof Langner
License : BSD3

Maintainer : Krzysztof Langner <klangner@gmail.com>
Stability : alpha
Portability : portable

AST model 
-}
module AST.Model where


data Package = Package { packageName :: String 
                       , subPackages ::  [Package]
                       , packageClasses :: [Class] }
                       
data Class = Class Name [Function]
data Function = Function Name
         
type Name = String         


-- | Convert list of names into package structure
packageFromList :: [String] -> Package
packageFromList [] = Package "" [] []
packageFromList [x] = Package x [] []
packageFromList (x:xs) = Package x [packageFromList xs] []

