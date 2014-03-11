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
import Control.Monad (void)


-- | Parse java source file
parseFile :: FilePath -> IO Package
parseFile fp = do
    result <- parseFromFile package fp
    case result of
        Left _ -> return $ Package "" [] [] []
        Right val -> return $ packageFromList val

    
-- | Parse package declaration    
package :: Parser [String]
package = do
    skipComments
    _ <- string "package"
    skipMany1 space
    x <- dotSep
    spaces
    _ <- char ';'
    return x      
    
-- | Parse dot separated names    
dotSep :: Parser [String]    
dotSep = many1 alphaNum `sepBy` char '.'


-- | skip comments and spaces
skipComments :: Parser ()
skipComments = skipMany $ void comment <|> skipMany1 space

-- | comment
comment :: Parser String
comment =    try multiLineComment
         <|> singleLineComment
         
-- | Parse comment /* */
multiLineComment :: Parser String
multiLineComment = do
    _ <- string "/*"
    manyTill anyChar (try (string "*/"))


-- | Parse comment //
singleLineComment :: Parser String
singleLineComment = do
    _ <- string "//"
    many (noneOf "\n")
    