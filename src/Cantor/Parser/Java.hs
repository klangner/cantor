{- |
Module : Cantor.Parser.Java
Copyright : Copyright (C) 2014 Krzysztof Langner
License : BSD3

Maintainer : Krzysztof Langner <klangner@gmail.com>
Stability : alpha
Portability : portable

Parser for Java sources.

To parse single file use: parseFile
To parse all files in given directory use: parseProject
-}
module Cantor.Parser.Java ( parseFile ) where

import Cantor.Parser.AST
import Text.ParserCombinators.Parsec
import Control.Monad (void)
import Cantor.Utils.Folder (listFilesR)


-- | Parse java source file
parseFile :: FilePath -> IO (Either ParseError Package)
parseFile = parseFromFile compilationUnit

-- | Parse java compilation unit
compilationUnit :: Parser Package
compilationUnit = do
    skipWhitespaces
    val <- package
    skipWhitespaces  
    imps <- importDecls
    return $ addImports (mkPackage val) imps

    
-- | Parse package declaration    
package :: Parser String
package = do
    skipWhitespaces
    option "" (packageDecl "package")


-- | Parse import declaration    
importDecls :: Parser [ImportDecl]
importDecls = many $ do 
    x <- packageDecl "import"
    skipWhitespaces
    return $ mkImportDecl x

    
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
        where pkgName = many1 (alphaNum <|> char '.' <|> char '*')


-- | skip comments and spaces
skipWhitespaces :: Parser ()
skipWhitespaces = skipMany $ void comment <|> skipMany1 (oneOf " \n\r\t") 

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
    
    
    
