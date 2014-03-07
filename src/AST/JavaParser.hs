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


-- | Parse java source file
parseFile :: FilePath -> IO Package
parseFile fp = return $ Package fp [] []