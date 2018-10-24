module CoqcExpandErrors.Util.Num (
  (∸), (∸~)
) where

import Control.Lens

--------------------------------------------------------------------------------

-- |\"Monus": subtraction that stops at 0.  Only well-behaved for nonnegative
-- types.
--
-- >>> 5 ∸ 3 :: Word
-- 2
-- >>> 3 ∸ 5 :: Word
-- 0
-- >>> 2 ∸ 2 :: Word
-- 0
(∸) :: (Num a, Ord a) => a -> a -> a
m ∸ n | m > n     = m - n
      | otherwise = 0
infixl 6 ∸

-- |Apply monus @('∸')@ to the target(s) of a numerically valued 'Lens',
-- 'Setter' or 'Traversal'.
(∸~) :: (Num a, Ord a) => ASetter s t a a -> a -> s -> t
l ∸~ n = over l (∸ n)
{-# INLINE (∸~) #-}
infixr 4 ∸~
