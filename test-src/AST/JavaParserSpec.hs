module AST.JavaParserSpec (spec) where

import AST.Model
import AST.JavaParser
import Test.Hspec


spec :: Spec
spec = 
  describe "Parse: fixtures/java/com/abc/Parser.java" $ do
  
    it "check root package" $ do
        pkg <- parseFile "fixtures/java/com/abc/Parser.java"
        packageName pkg `shouldBe` "com"
