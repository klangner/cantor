{- |
Module : Cantor.Parser.BuildSystem
Copyright : Copyright (C) 2014 Krzysztof Langner
License : BSD3

Maintainer : Krzysztof Langner <klangner@gmail.com>
Stability : alpha
Portability : portable

Data types used by different parsers
-}
module Cantor.Parser.BuildSystem ( BuildSystem
                                 , bsFilePath
                                 , bsProjectName
                                 , bsSDK
                                 , bsType
                                 , mkBuildSystem ) where


-- | Build system (Maven, Cabal etc.)
data BuildSystem = BS { bsFilePath :: String            -- Path to file with build information (eg pom.xml)
                      , bsType :: String                -- Name of the build system (eg. Maven)
                      , bsProjectName :: String         -- Project name taken from build file
                      , bsSDK :: String }               -- SDK needed to compile project (eg. Java 1.7)

-- | Create new BuildSystem
mkBuildSystem :: FilePath           -- Path to build readFile (eq. pom.xml)
              -> String             -- Build system name (eq. Maven)
              -> String             -- Project name
              -> BuildSystem
mkBuildSystem fp t n = BS fp t n ""

