{- |
Module : AST.JavaParser
Copyright : Copyright (C) 2014 Krzysztof Langner
License : BSD3

Maintainer : Krzysztof Langner <klangner@gmail.com>
Stability : alpha
Portability : portable

Parse Java sources into AST 
-}
module AST.JavaParser ( parseFile 
                      ) where

import AST.Model
import Text.ParserCombinators.Parsec


-- | Parse java source file
parseFile :: FilePath -> IO Package
parseFile fp = do
    input <- readFile fp
    case parse package "" input of
        Left _ -> return $ Package "" [] []
        Right val -> return $ packageFromList val

    
-- | Parse package declaration    
package :: Parser [String]
package = do
    _ <- string "package"
    skipMany1 space
    x <- dotSep
    spaces
    _ <- char ';'
    return x      
    
-- | Parse dot separated names    
dotSep :: Parser [String]    
dotSep = many1 alphaNum `sepBy` char '.'