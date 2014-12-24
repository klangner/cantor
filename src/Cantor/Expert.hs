{- |
Copyright : Copyright (C) 2014 Krzysztof Langner
License : BSD3

Maintainer : Krzysztof Langner <klangner@gmail.com>
Stability : alpha
Portability : portable

Expert system with information and rules about computer systems.
-}
module Cantor.Expert ( Expert
                     , mkExpert
                     , langFromExt ) where

import qualified Data.Map as Map
import Data.Maybe (fromMaybe)


data Expert = Expert { extensions :: Map.Map String [String] }   -- file extension -> Programming language

-- | Create expert system
--   Currently this expert system contains hardcoded data.
--   In the future it will be probably refactored to read data from external files (datastore)
mkExpert :: Expert
mkExpert = Expert languageExtensions

-- | Get language name based on given extension
--   Since some extension can be used by different files (like *.h)
--   This function will return list of possible languages or empty if extension is not registered
langFromExt :: Expert -> String -> [String]
langFromExt ex ext = fromMaybe [] e
    where e = Map.lookup ext (extensions ex)


-- Hardcoded language extensions
languageExtensions :: Map.Map String [String]
languageExtensions = Map.fromList [ (".c", ["C"])
                                  , (".cpp", ["C++"])
                                  , (".cs", ["C#"])
                                  , (".cljs", ["Clojure"])
                                  , (".css", ["CSS"])
                                  , (".go", ["Go"])
                                  , (".hs", ["Haskell"])
                                  , (".html", ["HTML"])
                                  , (".java", ["Java"])
                                  , (".js", ["JavaScript"])
                                  , (".m", ["Objective-C"])
                                  , (".pm", ["Perl"])
                                  , (".php", ["PHP"])
                                  , (".pro", ["Prolog"])
                                  , (".py", ["Python"])
                                  , (".r", ["R"])
                                  , (".rb", ["Ruby"])
                                  , (".rs", ["Rust"])
                                  , (".scala", ["Scala"])
                                  , (".sql", ["SQL"])
                                  , (".vb", ["Visual Basic"])]