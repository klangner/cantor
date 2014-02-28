module AllTests (tests) where

import qualified Distribution.TestSuite as C
import qualified Distribution.TestSuite.HUnit as H
import Abel.IndexTest (testCases)


tests :: IO [C.Test]
tests = return $ map (uncurry H.test) $  Abel.IndexTest.testCases
                                      