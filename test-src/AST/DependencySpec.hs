module AST.DependencySpec (spec) where

import AST.Dependency
import Utils.Graph
import Test.Hspec


spec :: Spec
spec = 
  describe "should return package dependency graph" $
  
    it "for simply case" $ do
        Graph vs es <- packageGraph "fixtures/java/dependency1"
        length vs `shouldBe` 3
        length es `shouldBe` 5
