{- |
Module : AST.Model
Copyright : Copyright (C) 2014 Krzysztof Langner
License : BSD3

Maintainer : Krzysztof Langner <klangner@gmail.com>
Stability : alpha
Portability : portable

AST model for java project
-}
module AST.Model ( Class
                 , Function
                 , ImportDecl
                 , Package
                 , addImports
                 , importPkgPath
                 , importClass
                 , mkImportDecl
                 , mkPackage
                 , packageDir
                 , packageClasses
                 , packageImports
                 , packageName ) where

import Utils.List (splitByLast)


type PackagePath = String
type Name = String

-- | AST model of Java language
-- data AST = AST [Package]

-- | Package
data Package = Package { packageName :: String
                       , packageImports :: [ImportDecl]
                       , packageClasses :: [Class] }
         
-- | Import declaration with package name and class name         
data ImportDecl = ImportDecl { importPkgPath :: PackagePath
                             , importClass :: Name } deriving (Eq, Show)

data Class = Class Name [Function] deriving (Eq, Show)

data Function = Function Name deriving (Eq, Show)


-- | Create new package with given name
mkPackage :: String -> Package
mkPackage name = Package name [] []

-- | Add import declarations to the package
addImports :: Package -> [ImportDecl] -> Package
addImports (Package name is cs) xs = Package name (is++xs) cs

-- | Get package path from
packageDir :: Package -> FilePath
packageDir (Package a _ _) = map f a
    where f x | x == '.' = '/'
              | otherwise = x    


-- | Create import decalration
mkImportDecl :: PackagePath -> ImportDecl
mkImportDecl path = ImportDecl pkg cls
    where (pkg, cls) = splitByLast "." path
