{- |
Copyright : Copyright (C) 2014 Krzysztof Langner
License : BSD3

Maintainer : Krzysztof Langner <klangner@gmail.com>
Stability : alpha
Portability : portable

Report builder.
Report is  Tree like structure which consists of: chapters, test list etc.
-}
module Cantor.Report ( Report
                     , addChapters
                     , markdown
                     , mkChapter
                     , mkEmphasis
                     , mkList
                     , mkParagraph
                     , mkReport
                     , mkStrong
                     , mkText ) where

type Title = String

data Report = Main Title [Report]
            | Chapter Title [Report]
            | Paragraph [Inline]
            | List [String]

data Inline = Text String
            | Strong String
            | Emphasis String


-- | Create empty report with given title
mkReport :: String -> Report
mkReport ts = Main ts []

-- | Create chapter
mkChapter :: String -> [Report] -> Report
mkChapter t xs = Chapter t xs

-- | Create list
mkList :: [String] -> Report
mkList xs = List xs

-- | Create paragraph
mkParagraph :: [Inline] -> Report
mkParagraph xs = Paragraph xs

-- | Create text inline element
mkText :: String -> Inline
mkText = Text

-- | Create strong  inline element
mkStrong :: String -> Inline
mkStrong = Strong

-- | Create emphasis inline element
mkEmphasis :: String -> Inline
mkEmphasis = Emphasis

-- | Add chapter to the report
addChapters :: Report -> [Report] -> Report
addChapters (Main t rs0) rs = Main t (rs0 ++ rs)
addChapters r1 rs2 = Main "" (r1:rs2)


-- | Convert to markdown
markdown :: Report -> String
markdown (Main t rs) = "#" ++ t ++ "\n" ++ concatMap markdown rs
markdown (Chapter t rs) = "\n##" ++ t ++ "\n" ++ concatMap markdown rs
markdown (Paragraph rs) = concatMap mdInline rs ++ "\n"
markdown (List rs) = concatMap (\x -> "*" ++ x ++ "\n") rs

-- render inline elements
mdInline :: Inline -> String
mdInline (Text xs) = xs
mdInline (Strong xs) = "**" ++ xs ++ "**"
mdInline (Emphasis xs) = "*" ++ xs ++ "*"