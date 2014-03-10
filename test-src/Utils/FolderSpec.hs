module Utils.FolderSpec (spec) where

import Utils.Folder
import Test.Hspec


spec :: Spec
spec = do

  describe "List files" $ 
    it "fixtures/utils/folder contains 3 files" $ do
        files <- listFiles "fixtures/utils/folder"
        length files `shouldBe` 3

  describe "List recursively" $ do 
    it "fixtures/utils/folder" $ do
        files <- listFilesR (const True) "fixtures/utils/folder"
        length files `shouldBe` 3
    it "fixtures/utils/abc" $ do
        files <- listFilesR (const True) "fixtures/utils/abc"
        length files `shouldBe` 5
        
        