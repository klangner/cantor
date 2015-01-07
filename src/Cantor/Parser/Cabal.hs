{-# LANGUAGE OverloadedStrings #-}
{- |
Module : Cantor.Parser.Cabal
Copyright : Copyright (C) 2015 Krzysztof Langner
License : BSD3

Maintainer : Krzysztof Langner <klangner@gmail.com>
Stability : alpha
Portability : portable

Parser for Cabal project file (*.cabal)
-}
module Cantor.Parser.Cabal ( parseFile ) where


import Distribution.PackageDescription.Parse(readPackageDescription)
import Distribution.Verbosity(silent)
import Distribution.PackageDescription(packageDescription, package)
import Distribution.Package(PackageName(..), pkgName)
import Cantor.Parser.BuildSystem(BuildSystem, mkBuildSystem)


-- | Parse Cabal file
parseFile :: FilePath -> IO BuildSystem
parseFile fp = do
    genDesc <- readPackageDescription silent fp
    let pkgDesc = packageDescription genDesc
    let (PackageName name) = (pkgName . package) pkgDesc
    return $ mkBuildSystem fp "Cabal" name
