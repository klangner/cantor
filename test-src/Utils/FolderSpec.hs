module Utils.FolderSpec (spec) where

import Utils.Folder
import Test.Hspec


spec :: Spec
spec = 
  describe "Count files" $ 
    it "fixtures/utils/folder contains 3 files" $ do
        files <- listFiles "fixtures/utils/folder"
        length files `shouldBe` 3
