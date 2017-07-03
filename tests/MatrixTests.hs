import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)
import Numeric.Matrix
import Utility

main = defaultMain unitTests

unitTests = testGroup "Unit tests" [
  testGetElement,
  testGetElementOrZero,
  testGetElementOrEmpty,
  creatingMatrix,
  selectElementTest,
  atTest,
  --rowTest,
  --colTest,
  --dimensionsTest,
  --numOfRowsAndColsTest,
  toListTest
  -- fromListTest,
  --unitTest,
  --zeroTest,
  -- diagTest
  -- emptyTest,
  -- minusTest,
  -- plusTest,
  -- timesTest,
  -- invTest,
  -- detTest,
  --transposeTest,
  --mapTest,
  --sumTest
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

mat = matrix (2,2) (\ (i,j) -> (i-1)*2 + j) 
expectedMat = Matrix (2,2) [[1,2],[3,4]]

creatingMatrix = testCase "Creates a matrix" $ assertEqual [] expectedMat mat

selectElementTest = testCase "Selecting Elements" $ 
  assertEqual [] [1,3] (select (\ (i,j) -> j == 1) mat )

atTest = testCase "Testing the at function" $ 
  assertEqual [] 2 (mat `at` (1,2))

toListTest = testCase "toList Test" $
  assertEqual [] [[1,2],[3,4]] (toList mat)

