{- |
Module : Report
Copyright : Copyright (C) 2014 Krzysztof Langner
License : BSD3

Maintainer : Krzysztof Langner <klangner@gmail.com>
Stability : alpha
Portability : portable

Helper function for creating report from project analysis.
-}
module Report ( Report (..)
              , printReport
              ) where


data Report = Report String [Report]
             | KeyValue String String
             | TextBlock String
             
             
printReport :: Report -> IO ()
printReport (Report title xs) = do
    putStrLn title
    _ <- mapM printReport xs 
    return ()
printReport (KeyValue k v) = putStrLn $ k ++ ": " ++ v
printReport (TextBlock a) = putStrLn a

