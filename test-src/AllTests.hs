module AllTests (tests) where

import qualified Distribution.TestSuite as C
import qualified Distribution.TestSuite.HUnit as H
import Utils.FolderTest (testCases)
import Maven.PomTest (testCases)


tests :: IO [C.Test]
tests = return $ map (uncurry H.test) $  Utils.FolderTest.testCases
                                      ++ Maven.PomTest.testCases
                                      