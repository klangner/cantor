module Metric.BasicSpec (spec) where

import Metric.Basic
import Test.Hspec



spec :: Spec
spec = 
  describe "calculate LOC metric" $ 
  
    it "for filtered files in all subfolders" $ do
        loc <- lineOfCode "fixtures/java/src1"
        let java = filter (\(e, _) -> e == ".java") loc 
        head java `shouldBe` (".java", 24)
{-
  describe "counting loops" $ do
  
    it "no loops for empty list" $ do
        let loops = findLoops ([] :: [(Int, Int)])
        length loops `shouldBe` 0
  
    it "single loop in 2 elements list" $ do
        let loops = findLoops [(1,2), (2,1)]
        length loops `shouldBe` 1
        let first = head loops 
        1 `elem` first `shouldBe` True
        2 `elem` first `shouldBe` True
  
    it "loop with 3 elements" $ do
        let loops = findLoops [(1,2), (2,4), (2,3), (4, 3), (3, 2)]
        let first = head loops
        length first `shouldBe` 3
        
  
    it "connected loops" $ do
        let loops = findLoops [(1,2), (3,4), (2,3), (3,1), (4,3)]
        length loops `shouldBe` 2
-}