module Numeric.Matrix (
  Matrix(..),
  MatrixElement(..),
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

import Prelude hiding (foldMap, map)
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
  matrix (n, m) f = Matrix (n,m) [[f (i,j) | j <- [1..m]] | i <- [1..n]]

  select :: ((Int, Int) -> Bool) -> Matrix e -> [e]
  select p m = [ m `at` (i,j) | i <- [1..numRows m]
                            , j <- [1..numCols m]
                            , p (i,j)]

  at :: Matrix e -> (Int, Int) -> e
  at mat (i, j) = ((getElementOrZero (j-1)) . (getElementOrEmpty (i-1)) . toList) mat
  
  row :: Int -> Matrix e -> [e]
  row i = (getElementOrEmpty (i-1)) . toList
  
  col :: Int -> Matrix e -> [e]
  col i = row i . transpose
  
  dimensions :: Matrix e -> (Int, Int)
  dimensions m = case toList m of 
    [] -> (0,0)
    (x:xs) -> (length xs + 1, length x)


  numRows :: Matrix e -> Int
  numRows = fst . dimensions

  numCols :: Matrix e -> Int
  numCols = snd . dimensions

  fromList :: [[e]] -> Matrix e -- TODO
  fromList list = Matrix (n,m) list
    where n = length list
          m = length $ getElementOrEmpty 1 list

  toList :: Matrix e -> [[e]] -- TODO
  toList (Matrix (n,m) l) = l

  unit :: Int -> Matrix e
  unit n = fromList [[if i == j then 1 else 0 | i <- [1..n]] | j <- [1..n]]
  
  zero :: Int -> Matrix e
  zero n = matrix (n,n) (const 0)
  
  diag :: [e] -> Matrix e
  diag xs = matrix (n,n) (\ (i, j) -> if (i == j) then getElementOrZero (i-1) xs else 0)
    where n = length xs

  empty :: Matrix e
  empty = fromList []
  
  minus :: Matrix e -> Matrix e -> Matrix e -- TODO
  minus = undefined

  plus :: Matrix e -> Matrix e -> Matrix e -- TODO
  plus = undefined

  times :: Matrix e -> Matrix e -> Matrix e -- TODO
  times = undefined

  inv :: Matrix e -> Maybe (Matrix e) -- TODO
  inv = undefined

  det :: Matrix e -> e -- TODO
  det = undefined
  
  transpose :: Matrix e -> Matrix e
  transpose mat = matrix (m,n) (\(i,j) -> mat `at` (j,i))
    where (n,m) = dimensions mat
  
  map :: MatrixElement f => (e -> f) -> Matrix e -> Matrix f
  map f mat = matrix (dimensions mat) (\x -> f (mat `at` x))
  
  foldMap :: Monoid m => (e->m) -> Matrix e -> m
  foldMap f mat = mconcat [f (mat `at` (i,j)) | i <- [1..n], j <- [1..m]]
    where n = numRows mat
          m = numCols mat

  sum :: Matrix e -> e
  sum = (\(Sum s) -> s) . foldMap Sum


instance MatrixElement Int where 
