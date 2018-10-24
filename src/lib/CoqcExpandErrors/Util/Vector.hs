module CoqcExpandErrors.Util.Vector (
  -- * Re-exports
  module Data.Vector,
  -- * Manipulate vector endpoints
  ensureHead, ensureLast,
  -- * Binary search
  binarySearch, binarySearchBy
) where

import Prelude hiding (null, head, last, length)

import Data.Vector

--------------------------------------------------------------------------------

-- |@ensureHead h v@ behaves like @cons h v@ if the first element of @v@ is
-- not @h@, and returns @v@ otherwise.
ensureHead :: Eq a => a -> Vector a -> Vector a
ensureHead h v | null v || head v /= h = cons h v
               | otherwise             = v 

-- |@ensureHead l v@ behaves like @snoc v l@ if the last element of @v@ is
-- not @l@, and returns @v@ otherwise.
ensureLast :: Eq a => a -> Vector a -> Vector a
ensureLast l v | null v || last v /= l = snoc v l
               | otherwise             = v

-- |Find the nearest preceding element with respect to the given ordering, or
-- 'Nothing' if there isn't one.
binarySearchBy :: (a -> a -> Ordering) -> Vector a -> a -> Maybe Int
binarySearchBy (<=>) v goal = go 0 (length v) where
  go l h | l < h     = let m = (l + h) `quot` 2
                       in case goal <=> (v ! m) of
                            LT -> go l m
                            EQ -> Just m
                            GT -> go (m+1) h
         | l > 0     = Just $ l - 1
         | otherwise = Nothing

-- |Find the nearest preceding element, or 'Nothing' if there isn't one.
binarySearch :: Ord a => Vector a -> a -> Maybe Int
binarySearch = binarySearchBy compare
