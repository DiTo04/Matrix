module Utility (Sum (..), 
                getElement,
                getElementOrZero,
                getElementOrEmpty
                ) where

getElement :: Int -> [a] -> Maybe a
getElement _ [] = Nothing
getElement 0 (x:_) = Just x
getElement n xs
  | n < 0 = Nothing
  | otherwise = getElement (n-1) (tail xs)

getElementOrZero :: Num a => Int -> [a] -> a
getElementOrZero n = nothingToZero . (getElement n)

getElementOrEmpty :: Num a => Int -> [[a]] -> [a]
getElementOrEmpty n = nothingToEmpty . (getElement n)

nothingToZero :: Num a => Maybe a -> a
nothingToZero (Just a) = a
nothingToZero (Nothing) = 0

nothingToEmpty :: Num a => Maybe [a] -> [a]
nothingToEmpty (Just a) = a
nothingToEmpty (Nothing) = [] 

newtype Sum n = Sum n deriving (Show, Read)

instance Num n => Monoid (Sum n) where
    mempty = Sum 0
    mappend (Sum x) (Sum y) = Sum (x+y)
