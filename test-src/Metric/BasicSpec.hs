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
