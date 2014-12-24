module Cantor.Language.Java.ProjectSpec (spec) where

import Cantor.Language.Java.Project
import Test.Hspec
import System.Directory


spec :: Spec
spec = 
  describe "Source path" $ do
  
    it "fixtures/java/src1" $ do
        cd <- getCurrentDirectory
        paths <- findJavaClassPaths "fixtures/java/src1"
        head paths `shouldBe` cd ++ "/fixtures/java/src1"
        
    it "fixtures/java/multi1" $ do
        cd <- getCurrentDirectory
        let src = cd ++ "/fixtures/java/multi1"
        paths <- findJavaClassPaths src 
        elem (src ++ "/src1") paths `shouldBe` True
        elem (src ++ "/src2") paths `shouldBe` True
        
