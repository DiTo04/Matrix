import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)
import Utility
 
main = defaultMain unitTests

unitTests = testGroup "Unit tests for Utility Module" [
  testGetElement,
  testGetElementOrZero,
  testGetElementOrEmpty
  ]


list = [1,2,3] :: [Int]

testGetElement = testCase "getElement" $ do 
  assertEqual [] (Just 1) (getElement 0 list)
  assertEqual [] (Just 2) (getElement 1 list)
  assertEqual [] (Just 3) (getElement 2 list)

testGetElementOrZero = testCase "getElementorZero" $ do 
  assertEqual "Does not get 0 when out of range" 0 (getElementOrZero 70 list)
  assertEqual "Can not find nr in range" 1 (getElementOrZero 0 list)

testGetElementOrEmpty = testCase "getElementOrEmpty" $ do
  assertEqual "Does not return empty when outside range" [] (getElementOrEmpty 3 [list, list])
  assertEqual "Does return empty when in range..." list (getElementOrEmpty 0 [list, list])
