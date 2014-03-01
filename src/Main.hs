module Main where

import System.Environment
import Utils.Folder
import Maven.Pom


main::IO ()
main = do
    (path:_) <- getArgs
    putStrLn $ "Project path: " ++ path
    _ <- analyzePom (joinPaths path "pom.xml")
    return ()
    
    
-- | Load POM file and get info about project    
analyzePom :: FilePath -> IO Bool
analyzePom f = do 
    pom <- loadPom f
    putStrLn $ "Load pom file: " ++ show(pom)
    return False