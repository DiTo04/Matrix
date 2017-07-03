import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)
import Numeric.Matrix
import Utility
import Prelude hiding (map, sum)
import qualified Prelude as P
main = defaultMain unitTests

unitTests = testGroup "Unit tests" [
  testGetElement,
  testGetElementOrZero,
  testGetElementOrEmpty,
  creatingMatrix,
  selectElementTest,
  atTest,
  rowTest,
  colTest,
  dimensionsTest,
  numOfRowsAndColsTest,
  toListTest,
  fromListTest,
  unitTest,
  zeroTest,
  diagTest,
  emptyTest,
  minusTest,
  plusTest,
  timesTest,
 -- invTest,
 -- detTest,
  transposeTest,
  mapTest,
  sumTest
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

rowTest = testCase "rowTest" $ do
  assertEqual "getExistingRow" [1,2] (row 1 mat)
  assertEqual "get empty" [] $ (row 10 mat)

colTest = testCase "colTest" $ do
  assertEqual "getExistingCol" [1,3] (col 1 mat)
  assertEqual "get empty" [] $ (col 10 mat)

dimensionsTest = testCase "Dimensions" $
  assertEqual "" (2,2) (dimensions mat)

numOfRowsAndColsTest = testCase "nrOfRowsAndCols" $ do
  assertEqual "" 1 (numRows mat2)
  assertEqual "" 2 (numCols mat2)
  where
    mat2 = matrix (1,2) (\(i,j) -> (1 ::Int))

toListTest = testCase "toList Test" $
  assertEqual [] [[1,2],[3,4]] (toList mat)

fromListTest = testCase "fromListTest" $ 
  assertEqual "" mat (fromList [[1,2], [3,4]])

unitTest = testCase "unit matrix" $ 
  assertEqual "" unitMatrix (unit 2)
  where unitMatrix = fromList [[1,0],[0,1]] :: Matrix Int

zeroTest = testCase "zero Matrix" $ 
  assertEqual "" zeroMatrix (zero 2)
  where zeroMatrix = fromList [[0,0],[0,0]] :: Matrix Int

diagTest = testCase "diagonal" $ 
  assertEqual "" diagMatrix (diag [1,2,3])
  where diagMatrix = fromList [[1,0,0],[0,2,0],[0,0,3]] :: Matrix Int

emptyTest = testCase "empty" $
  assertEqual "" ((Matrix (0,0) []) :: Matrix Int) (empty)

minusTest = testCase "minus" $ assertEqual "" mat (a-b)
  where a = fromList [[2,3],[4,5]] :: Matrix Int
        b = fromList [[1,1],[1,1]] :: Matrix Int

plusTest = testCase "plus" $ assertEqual "" mat (a+b)
  where a = fromList [[0,1],[2,3]] :: Matrix Int
        b = fromList [[1,1],[1,1]] :: Matrix Int
  
timesTest = testCase "times" $ assertEqual "" mat (mat * (unit 2))

transposeTest = testCase "transposeTest" $ 
  assertEqual "" tMatrix (transpose mat)
  where tMatrix = fromList [[1,3],[2,4]] :: Matrix Int

mapTest = testCase "mapTest" $ 
  assertEqual "" doubleMatrix (map (*2) mat)
  where doubleMatrix = fromList [[2,4],[6,8]] :: Matrix Int

sumTest = testCase "sumTest" $ 
  assertEqual "" (10 :: Int) (sum mat)
