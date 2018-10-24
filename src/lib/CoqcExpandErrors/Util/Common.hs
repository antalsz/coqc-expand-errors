module CoqcExpandErrors.Util.Common (
  -- * Triples
  curry3, uncurry3,
  -- * Flipped functions
  forAccumL
) where

import Data.Traversable

--------------------------------------------------------------------------------

-- |Like 'curry', but for triples.
curry3 :: ((a,b,c) -> d) -> a -> b -> c -> d
curry3 f a b c = f (a,b,c)

-- |Like 'uncurry', but for triples.
uncurry3 :: (a -> b -> c -> d) -> (a,b,c) -> d
uncurry3 f (a,b,c) = f a b c

-- |Like 'mapAccumL', but takes the argument last for prettier application to
-- lambdas.
forAccumL :: Traversable t => a -> t b -> (a -> b -> (a,c)) -> (a, t c)
forAccumL s0 t f = mapAccumL f s0 t
