{- |
Module : Report.Html
Copyright : Copyright (C) 2014 Krzysztof Langner
License : BSD3

Maintainer : Krzysztof Langner <klangner@gmail.com>
Stability : alpha
Portability : portable

Create html documents
-}
module Report.Html where


type Html = String


-- | Build html node          
node :: String -> String -> Html          
node name text = "<" ++ name ++ ">" ++ text ++ "</" ++ name ++ ">"


-- | Print html node
html :: String -> Html
html = node "html" 
          
-- | Print body node
body :: String -> Html
body = node "body" 
          
-- | Print h1-h6 node
h1 :: String -> Html
h1 = node "h1" 
          
h2 :: String -> Html
h2 = node "h2" 
          
table :: [String]       -- Headers
      -> [[String]]       -- Cells
      -> Html
table hs cs = node "table" $ headers ++ cells
    where headers = tr $ concatMap th hs
          cells = concatMap (tr . concatMap td) cs

tr :: String -> Html
tr a = node "tr" $ th a

th :: String -> Html
th = node "th"

td :: String -> Html
td = node "td"

ul :: [String] -> Html
ul xs = node "ul" $ concatMap li xs

ol :: [String] -> Html
ol xs = node "ol" $ concatMap li xs

li :: String -> Html
li = node "li"