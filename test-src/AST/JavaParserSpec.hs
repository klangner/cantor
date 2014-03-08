module AST.JavaParserSpec (spec) where

import AST.Model
import AST.JavaParser
import Test.Hspec


spec :: Spec
spec = do
  -- Easy case
  describe "Parse: fixtures/java/com/abc/Parser.java" $ do
  
    it "check root package" $ do
        pkg <- parseFile "fixtures/java/com/abc/Parser.java"
        packageName pkg `shouldBe` "com"
  
    it "second level package is abc" $ do
        pkg <- parseFile "fixtures/java/com/abc/Parser.java"
        let xs = subPackages pkg
        length xs `shouldBe` 1
        packageName (head xs) `shouldBe` "abc"

  -- Source with comments
  describe "Parse: fixtures/java/com/abc/ef/Parser2.java" $ do
  
    it "check root package" $ do
        pkg <- parseFile "fixtures/java/com/abc/ef/Parser2.java"
        packageName pkg `shouldBe` "com"
  
    it "second level package is abc" $ do
        pkg <- parseFile "fixtures/java/com/abc/ef/Parser2.java"
        let xs = subPackages (head (subPackages pkg))
        packageName (head xs) `shouldBe` "ef"
        