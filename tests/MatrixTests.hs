import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)
import Numeric.Matrix
import Prelude hiding (map, sum)
import qualified Prelude as P
main = defaultMain unitTests

unitTests = testGroup "Unit tests for Matrix Module" [
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
  detTest,
  evilDetTest,
  transposeTest,
  mapTest,
  sumTest
  ]

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

detTest = testCase "determinent" $ assertEqual "" 1 (det $ (unit 2 :: Matrix Int))

evilDetTest = testCase  "Evil determinent evaluation" $ 
  assertEqual "" (1*5*9 + 2*6*7 + (-3)*4*8 - 1*6*8 - 2*4*9 - (-3)*5*7) $  det $ (Matrix (3,3) [[1,2,-3],[4,5,6],[7,8,9]] :: Matrix Int)

transposeTest = testCase "transposeTest" $ 
  assertEqual "" tMatrix (transpose mat)
  where tMatrix = fromList [[1,3],[2,4]] :: Matrix Int

mapTest = testCase "mapTest" $ 
  assertEqual "" doubleMatrix (map (*2) mat)
  where doubleMatrix = fromList [[2,4],[6,8]] :: Matrix Int

sumTest = testCase "sumTest" $ 
  assertEqual "" (10 :: Int) (sum mat)
