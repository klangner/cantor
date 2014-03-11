module AST.JavaParserSpec (spec) where

import AST.Model
import AST.JavaParser
import Test.Hspec


spec :: Spec
spec = do
  describe "should return package declaration" $ do
  
    it "for simply case" $ do
        pkg <- parseFile "fixtures/java/src1/com/abc/Parser.java"
        packageName pkg `shouldBe` "com.abc"

    it "for declaration with comments" $ do
        pkg <- parseFile "fixtures/java/src1/com/abc/ef/Parser2.java"
        packageName pkg `shouldBe` "com.abc.ef"

    it "for file in the root package" $ do
        pkg <- parseFile "fixtures/java/src2/Root.java"
        packageName pkg `shouldBe` ""
{-  
  describe "should parse import declarations" $ 
  
    it "for simply case" $ do
        pkg <- parseFile "fixtures/java/src1/com/abc/Parser.java"
        let imports = packageImports pkg
        length imports `shouldBe` 1
        head imports `shouldBe` "com.abc.model.Package"
-}