module Metric.BasicSpec (spec) where

import Metric.Basic
import Test.Hspec



spec :: Spec
spec = 
  describe "Line of code" $ 
  
    it "fixtures/java/src1" $ do
        loc <- lineOfCode "fixtures/java/src1"
        let java = filter (\(e, _) -> e == ".java") loc 
        head java `shouldBe` (".java", 24)
