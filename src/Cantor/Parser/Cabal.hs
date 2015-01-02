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


import Data.List as List
import Prelude hiding (break, readFile)
import Data.Text.IO (readFile)
import Data.Text as T
import Cantor.Parser.BuildSystem(BuildSystem, mkBuildSystem)


-- | Parse Cabal file
parseFile :: FilePath -> IO BuildSystem
parseFile fp = do
    contents <- readFile fp
    let cabal = parseCabal contents
    return $ mkBuildSystem fp "Cabal" (T.unpack (getKeyValue "name" cabal))

-- | Parse cabal file format. Return list with (keyword, data)
-- ATM only take project name
parseCabal :: Text -> [(Text, Text)]
parseCabal contents = List.foldl f [] ls
    where ls = List.map parseLine (T.lines contents)
          f m (Just key, value) = (key, value) : m
          f m _ = m

-- Parse line and return keyword (if any) and rest of the line
parseLine :: Text -> (Maybe Text, Text)
parseLine line = if T.length key < T.length line then (Just (T.toLower key), strip (T.tail value))
                 else (Nothing, line)
    where (key, value) = T.break (== ':') line

-- Get key value from cabal structure
-- If not found return then empty string
getKeyValue :: Text -> [(Text, Text)] -> Text
getKeyValue key cabal = case List.find (\(k,_) -> k == key) cabal of
                            Just (_,v) -> v
                            _ -> ""