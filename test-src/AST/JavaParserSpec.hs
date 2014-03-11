module AST.JavaParserSpec (spec) where

import AST.Model
import AST.JavaParser
import Text.Parsec.Error
import Test.Hspec


spec :: Spec
spec = do
  describe "should return package declaration" $ do
  
    it "for simply case" $ do
        result <- parseFile "fixtures/java/src1/com/abc/Parser.java"
        case result of
            Right pkg -> packageName pkg `shouldBe` "com.abc"
            Left err -> parseError err
        
    it "for declaration with comments" $ do
        result <- parseFile "fixtures/java/src1/com/abc/ef/Parser2.java"
        case result of
            Right pkg -> packageName pkg `shouldBe` "com.abc.ef"
            Left err -> parseError err
        
    it "for file in the root package" $ do
        result <- parseFile "fixtures/java/src2/Root.java"
        case result of
            Right pkg -> packageName pkg `shouldBe` ""
            Left err -> parseError err
        

  describe "should parse import declarations" $  do
  
    it "for single import" $ do
        result <- parseFile "fixtures/java/src1/com/abc/Parser.java"
        case result of
            Right pkg -> do
                let imports = packageImports pkg
                length imports `shouldBe` 1
                head imports `shouldBe` "com.abc.model.Package"
            Left err -> parseError err
  
    it "for multiple imports in root package" $ do
        result <- parseFile "fixtures/java/src2/Root.java"
        case result of
            Right pkg -> do
                let imports = packageImports pkg
                length imports `shouldBe` 2
                "abc" `elem` imports `shouldBe` True
                "com.foo.*" `elem` imports `shouldBe` True
            Left err -> parseError err
  
    it "for imports with comments" $ do
        result <- parseFile "fixtures/java/src2/net/foo/Solaris.java"
        case result of
            Right pkg -> do
                let imports = packageImports pkg
                length imports `shouldBe` 3
                "com.abc" `elem` imports `shouldBe` True
                "net.foo.model.*" `elem` imports `shouldBe` True
                "java.lang.*" `elem` imports `shouldBe` True
            Left err -> parseError err

        
-- | Handle parser errors        
parseError :: ParseError -> IO ()
parseError err = do            
    let msgs = map messageString (errorMessages err)
    mapM_ putStrLn msgs
    "Parser error " ++ head msgs `shouldBe` ""

    
                                