{-# LANGUAGE LambdaCase #-}

module CoqcExpandErrors.Util.Pipes (
  -- * Re-exports
  module Pipes,
  module Pipes.Prelude.Text,
  -- * Output to handles
  toHandleLn, toHandleLn'
) where

import Control.Monad

import Data.Text (Text)
import qualified Data.Text.IO as T

import Pipes
import Pipes.Prelude.Text hiding (toHandleLn)
import qualified Pipes.Prelude.Text as P

import Foreign.C.Error
import GHC.IO.Exception
import Control.Exception

import System.IO

--------------------------------------------------------------------------------

-- |Write separate lines of 'Text' to a 'Handle' using 'T.hPutStrLn',
-- terminating without error on a broken output pipe.
-- 
-- Copied from 'stdoutLn', but generalized to any handle.
toHandleLn :: MonadIO m => Handle -> Consumer Text m ()
toHandleLn h = go where
  go = await >>= liftIO . try . T.hPutStrLn h >>= \case
         Left (IOError { ioe_type  = ResourceVanished
                       , ioe_errno = Just ioe })
           | Errno ioe == ePIPE -> pure ()
         Left  e                -> liftIO $ throwIO e
         Right ()               -> go
{-# INLINABLE toHandleLn #-}

-- |Write separate lines of 'Text' to a 'Handle' using 'T.hPutStrLn'.  This does
-- not handle a broken output pipe, but has a polymorphic return value.
--
-- This is a re-export of 'P.toHandleLn', but with naming that corresponds to
-- 'P.stdoutLn'/'P.stdoutLn''.
toHandleLn' :: MonadIO m => Handle -> Consumer Text m r
toHandleLn' = P.toHandleLn
{-# INLINE toHandleLn' #-}
