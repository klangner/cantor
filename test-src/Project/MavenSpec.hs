module Project.MavenSpec (spec) where

import Project.Model
import Project.Maven
import Test.Hspec
--import Test.QuickCheck


spec :: Spec
spec = 
  describe "Read information from POM" $ do
  
    it "returns name from pom file" $ do
        prj <- loadProject "fixtures/maven/minimal"
        let name = getProjectName prj
        getProjectName prj `shouldBe` "Minimal project"
        
    it "returns Nothing" $ do
        prj <- loadProject "fixtures/maven/invalid"
        getProjectName prj `shouldBe` "error"
        
        
getProjectName :: Maybe Project -> String
getProjectName (Just (Project src md)) = name md
getProjectName _ = "error"         