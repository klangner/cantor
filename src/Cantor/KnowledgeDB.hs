{- |
Copyright : Copyright (C) 2014 Krzysztof Langner
License : BSD3

Maintainer : Krzysztof Langner <klangner@gmail.com>
Stability : alpha
Portability : portable

Expert system with information and rules about computer systems.
-}
module Cantor.KnowledgeDB ( KnowledgeDB
                          , conceptUrl
                          , loadKDB
                          , langFromExt ) where

import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Char


data KnowledgeDB = KDB { extensions :: Map.Map String [String]  -- file extension -> Programming language
                       , concepts :: Map.Map String String }    -- concept URLs

-- | Create expert system
--   Currently this expert system contains hardcoded data.
--   In the future it will be probably refactored to read data from external files (datastore)
loadKDB :: KnowledgeDB
loadKDB = KDB languageExtDB conceptUrlDB

-- | Get language name based on given extension
--   Since some extension can be used by different files (like *.h)
--   This function will return list of possible languages or empty if extension is not registered
langFromExt :: KnowledgeDB -> String -> [String]
langFromExt db ext = fromMaybe [] e
    where e = Map.lookup ext (extensions db)


-- | Get URL describing concept
conceptUrl :: KnowledgeDB -> String -> String
conceptUrl db c = fromMaybe "" e
    where e = Map.lookup (map toLower c) (concepts db)


-- ---------------------------------------------------------------------------------------------------------------------
-- The structures below eventually will be moved to the external text files
-- ---------------------------------------------------------------------------------------------------------------------

-- Language by extensions
languageExtDB :: Map.Map String [String]
languageExtDB = Map.fromList [ (".c", ["C"])
                             , (".cpp", ["C++"])
                             , (".cs", ["C#"])
                             , (".clj", ["Clojure"])
                             , (".css", ["CSS"])
                             , (".fs", ["F#"])
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


-- URLs with additional information about concepts
conceptUrlDB :: Map.Map String String
conceptUrlDB = Map.fromList [
                            -- Programming languages
                              ("c", "http://en.wikipedia.org/wiki/C_(programming_language)")
                            , ("c++", "http://en.wikipedia.org/wiki/C%2B%2B")
                            , ("c#", "http://en.wikipedia.org/wiki/C_Sharp_(programming_language)")
                            , ("clojure", "http://clojure.org/")
                            , ("css", "http://en.wikipedia.org/wiki/Cascading_Style_Sheets")
                            , ("f#", "http://fsharp.org/")
                            , ("go", "https://golang.org/")
                            , ("haskell", "https://www.haskell.org/")
                            , ("html", "http://en.wikipedia.org/wiki/HTML")
                            , ("java", "https://java.com")
                            , ("javascript", "http://en.wikipedia.org/wiki/JavaScript")
                            , ("objective-c", "http://en.wikipedia.org/wiki/Objective-C")
                            , ("perl", "https://www.perl.org/")
                            , ("php", "https://www.php.net/")
                            , ("prolog", "http://en.wikipedia.org/wiki/Prolog")
                            , ("python", "https://www.python.org/")
                            , ("r", "http://www.r-project.org/")
                            , ("ruby", "https://www.ruby-lang.org")
                            , ("rust", "http://www.rust-lang.org/")
                            , ("scala", "http://www.scala-lang.org/")
                            , ("sql", "http://en.wikipedia.org/wiki/SQL")
                            , ("visual basic", "http://en.wikipedia.org/wiki/Visual_Basic")
                            -- Build systems
                            ]