module Utility (Sum (..), 
                getElement,
                getElementOrZero,
                getElementOrEmpty,
                chunksOf
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

-- | Standard build function, specialized to building lists.
--
--   Usually build is given the rank-2 type
--
--   > build :: (forall b. (a -> b -> b) -> b -> b) -> [a]
--
--   but since we only use it when @(b ~ [a])@, we give it the more
--   restricted type signature in order to avoid needing a
--   non-Haskell2010 extension.
--
--   Note that the 0.1.4.3 release of this package did away with a
--   custom @build@ implementation in favor of importing one from
--   "GHC.Exts", which was (reportedly) faster for some applications.
--   However, in the interest of simplicity and complete Haskell2010
--   compliance as @split@ is being included in the Haskel Platform,
--   version 0.2.1.0 has gone back to defining @build@ manually.  This
--   is in line with @split@'s design philosophy of having efficiency
--   as a non-goal.
build :: ((a -> [a] -> [a]) -> [a] -> [a]) -> [a]
build g = g (:) []

-- | @'chunksOf' n@ splits a list into length-n pieces.  The last
--   piece will be shorter if @n@ does not evenly divide the length of
--   the list.  If @n <= 0@, @'chunksOf' n l@ returns an infinite list
--   of empty lists.  For example:
--
--   Note that @'chunksOf' n []@ is @[]@, not @[[]]@.  This is
--   intentional, and is consistent with a recursive definition of
--   'chunksOf'; it satisfies the property that
--
--   @chunksOf n xs ++ chunksOf n ys == chunksOf n (xs ++ ys)@
--
--   whenever @n@ evenly divides the length of @xs@.
chunksOf :: Int -> [e] -> [[e]]
chunksOf i ls = map (take i) (build (splitter ls)) where
  splitter :: [e] -> ([e] -> a -> a) -> a -> a
  splitter [] _ n = n
  splitter l c n  = l `c` splitter (drop i l) c n
