{-# LANGUAGE FlexibleContexts, OverloadedStrings,
             LambdaCase, MultiWayIf, RecordWildCards #-}

module CoqcExpandErrors.Pipes.Report (
  reportingBeforeNext,
  reportBeforeNext,
  announcing
) where

import Control.Lens

import Data.Foldable
import Control.Monad.State

import Pipes

--------------------------------------------------------------------------------

-- |@announcing prod@ yields 'Just' the results of @prod@ until it finishes,
-- then yields a single 'Nothing' and terminates.
announcing :: Monad m => Producer a m () -> Producer (Maybe a) m ()
announcing prod = for prod (yield . Just) *> yield Nothing

-- |@reportBeforeNext parse display@ forwards all its inputs along.  If it sees
-- one matching the @parse@ function, it will store that, and then yield
-- (report) the result of @display@ing that match before the next match or at
-- the very end.
--
-- The inputs are expected to be a sequences of 'Just's followed by a single
-- 'Nothing', as produced by 'announcing'.
reportBeforeNext :: (Monad m, Foldable f)
                 => (a -> Maybe b)
                 -> (b -> m (f a))
                 -> Pipe (Maybe a) a m ()
reportBeforeNext parse display = evalStateT ?? Nothing $
  let report = traverse_ (traverse_ (lift . yield) <=< lift . lift . display) =<< get
      loop   = lift await >>= \case
       Just line -> do
         for_ (parse line) $ \loc -> do
           report
           put $ Just loc
         lift $ yield line
         loop
       Nothing ->
         report
  in loop

-- |@reportBeforeNext parse display producer consumer@ forwards @producer@ to
-- @consumer@, annotating reports as per 'reportBeforeNext' in between.
--
-- The results of @producer@ do not need to be wrapped in any way, unlike for
-- 'reportBeforeNext'.
reportingBeforeNext :: (Monad m, Foldable f)
                    => (a -> Maybe b)
                    -> (b -> m (f a))
                    -> Producer a m ()
                    -> Consumer a m ()
                    -> Effect m ()
reportingBeforeNext parse display producer consumer =
  announcing producer >-> reportBeforeNext parse display >-> consumer
