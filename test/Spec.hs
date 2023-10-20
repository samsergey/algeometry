module Main where

import Test.Tasty
import qualified Test.Base

-----------------------------------------------------------

testSuite :: TestTree
testSuite = testGroup "Algeometry tests"
  [
    Test.Base.testSuite
  ]

main :: IO ()
main = defaultMain testSuite 
