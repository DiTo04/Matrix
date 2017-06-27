module Numeric.Matrix (
  Matrix,
  MatrixElement (..),
  (<|>),
  (<->),
  scale,

  {-
  isUnit,
  isZero,
  isDiagonal,
  isEmpty,
  isSquare,

  toDoubleMatrix,
  toComplexMatrix,
  toRationalMatrix -}
) where

import Prelude hiding ( map)
import qualified Prelude as P
import Utility

data Matrix e = Matrix (Int, Int) [[e]]

instance (MatrixElement e, Show e) => Show (Matrix e) where
  show = unlines . P.map showRow . toList
    where showRow = unwords . P.map show

instance (MatrixElement e) => Num (Matrix e) where
  (+) = plus
  (-) = minus
  (*) = times

  abs = map abs
  signum = matrix (1,1) . const . signum . det
  fromInteger = matrix (1,1) . const . fromInteger

instance (MatrixElement e) => Eq (Matrix e) where
  m == n
    | dimensions m == dimensions n = toList m == toList n
    | otherwise = False

(<|>) :: MatrixElement e => Matrix e -> Matrix e -> Matrix e
mtx1 <|> mtx2  = matrix (max n1 n2, m1+m2) (\(i,j) 
    ->  if j < m1 
    then (if i < n1 then mtx1 `at` (i,j) else 0)
    else (if i < n2 then mtx2 `at` (i,j-m1) else 0))
        where 
          n1 = numRows mtx1
          n2 = numRows mtx2
          m1 = numCols mtx1
          m2 = numCols mtx2 

(<->) :: MatrixElement e => Matrix e -> Matrix e -> Matrix e
mtx1 <-> mtx2  = matrix (n1 + n2, max m1 m2) (\(i,j) 
    ->  if i < n1 
    then (if j < m1 then mtx1 `at` (i,j) else 0)
    else (if j < m2 then mtx2 `at` (i-n1,j) else 0))
        where 
          n1 = numRows mtx1
          n2 = numRows mtx2
          m1 = numCols mtx1
          m2 = numCols mtx2 

scale :: MatrixElement e => e -> Matrix e -> Matrix e
scale i = map (i *)


class (Num e, Eq e) => MatrixElement e where
  matrix :: (Int, Int) -> ((Int, Int) -> e) -> Matrix e
  select :: ((Int, Int) -> Bool) -> Matrix e -> [e]
  at :: Matrix e -> (Int, Int) -> e
  row :: Int -> Matrix e -> [e]
  col :: Int -> Matrix e -> [e]
  dimensions :: Matrix e -> (Int, Int)
  numRows :: Matrix e -> Int
  numCols :: Matrix e -> Int
  fromList :: [[e]] -> Matrix e -- TODO
  toList :: Matrix e -> [[e]] -- TODO
  unit :: Int -> Matrix e
  zero :: Int -> Matrix e
  diag :: [e] -> Matrix e
  empty :: Matrix e
  minus :: Matrix e -> Matrix e -> Matrix e -- TODO
  plus :: Matrix e -> Matrix e -> Matrix e -- TODO
  times :: Matrix e -> Matrix e -> Matrix e -- TODO
  inv :: Matrix e -> Maybe (Matrix e) -- TODO
  det :: Matrix e -> e -- TODO
  transpose :: Matrix e -> Matrix e
  map :: MatrixElement f => (e -> f) -> Matrix e -> Matrix f
  foldMap :: Monoid m => (e->m) -> Matrix e -> m
  sum :: Matrix e -> e

-- Implementation.
  unit n = fromList [[if i == j then 1 else 0 | i <- [1..n]] | j <- [1..n]]
  zero n = matrix (n,n) (const 0)
  empty = fromList []
  diag xs = matrix (n,n) (\ (i, j) -> if (i == j) then getElementOrZero (i-1) xs else 0)
    where n = length xs

  select p m = [ m `at` (i,j) | i <- [1..numRows m]
                            , j <- [1..numCols m]
                            , p (i,j)]
  
  at mat (i, j) = ((getElementOrZero j) . (getElementOrEmpty i) . toList) mat
  
  row i = (getElementOrEmpty (i-1)) . toList
  col i = row i . transpose

  numRows = fst . dimensions
  numCols = snd . dimensions

  dimensions m = case toList m of [] -> (0,0)
                              (x:xs) -> (length xs + 1, length x)

  transpose mat = matrix (m,n) (\(i,j) -> mat `at` (j,i))
    where (n,m) = dimensions mat

  map f mat = matrix (dimensions mat) (\x -> f (mat `at` x))
  
  foldMap f mat = mconcat [f (mat `at` (i,j)) | i <- (1..n), j <- (1..m)]
    where n = numRows mat
          m = numCols mat

  sum = (\(Sum s) -> s) . foldMap Sum

main :: IO ()
main = do
  let 
    n = getElementOrZero 2 [1,2,3]
    m = getElementOrZero 4 [1,2,3]
  print n
  print m
