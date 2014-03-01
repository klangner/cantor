module AllTests (tests) where

import qualified Distribution.TestSuite as C
import qualified Distribution.TestSuite.HUnit as H
import Utils.FolderTest (testCases)


tests :: IO [C.Test]
tests = return $ map (uncurry H.test) $  Mavex.IndexTest.testCases
                                      