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
    result <- parseFromFile compilationUnit fp
    case result of
        Left _ -> return $ packageNew ""
        Right val -> return val

    
-- | Parse java compilation unit
compilationUnit :: Parser Package
compilationUnit = do
    skipComments
    val <- package
    --skipComments  
    --val <- package   
    return $ packageNew val

    
-- | Parse package declaration    
package :: Parser String
package = packageDecl "package"


-- | Parse import declaration    
-- importDecl :: Parser String
-- importDecl = packageDecl "import"

    
-- | Parse package declaration
-- keyword com.abc.ef
packageDecl :: String -> Parser String    
packageDecl keyword = do
    _ <- string keyword
    skipMany1 space
    name <- pkgName
    spaces
    _ <- char ';'
    return name    
        where pkgName = many1 (alphaNum <|> char '.')


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
    