module AST.JavaParserSpec (spec) where

import AST.Model
import AST.JavaParser
import Test.Hspec


spec :: Spec
spec =
  describe "should return package declaration" $ do
  
    it "for simply case" $ do
        pkg <- parseFile "fixtures/java/src1/com/abc/Parser.java"
        packageName pkg `shouldBe` "com"
        let xs = subPackages pkg
        length xs `shouldBe` 1
        packageName (head xs) `shouldBe` "abc"

    it "for declaration with comments" $ do
        pkg <- parseFile "fixtures/java/src1/com/abc/ef/Parser2.java"
        packageName pkg `shouldBe` "com"
        let xs = subPackages (head (subPackages pkg))
        packageName (head xs) `shouldBe` "ef"
        