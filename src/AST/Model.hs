{- |
Module : AST.Model
Copyright : Copyright (C) 2014 Krzysztof Langner
License : BSD3

Maintainer : Krzysztof Langner <klangner@gmail.com>
Stability : alpha
Portability : portable

AST model 
-}
module AST.Model where

data Package = Package Name [Package] [Class]
data Class = Class Name [Function]
data Function = Function Name
         
type Name = String         