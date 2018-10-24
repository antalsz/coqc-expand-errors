{-# LANGUAGE TypeFamilies #-}

module CoqcExpandErrors.Util.Megaparsec (
  -- * Re-exports
  module Text.Megaparsec,
  module Text.Megaparsec.Char,
  -- * Combining tokens with a single space
  (<+>), (<+), (+>),
  -- * Combining tokens with multiple spaces
  (<++>), (<++), (++>),
  -- * Quotes
  quoted,
  -- * Parsing 'Text'
  TextParser, manyT,
  -- * Run 'Parsec'
  parseStartMaybe
) where

import Data.Void
import Control.Monad

import Data.Text (Text)
import qualified Data.Text as T

import Text.Megaparsec
import Text.Megaparsec.Char

--------------------------------------------------------------------------------

-- |A simple parser that processes 'Text'.
type TextParser = Parsec Void Text

-- |Like @('<*>')@, but separates the two parsed tokens with a single space.
(<+>) :: (MonadParsec e s m, Token s ~ Char) => m (a -> b) -> m a -> m b
-- |Like @('<*')@, but separates the two parsed tokens with a single space.
(<+)  :: (MonadParsec e s m, Token s ~ Char) => m a        -> m b -> m a
-- |Like @('*>')@, but separates the two parsed tokens with a single space.
(+>)  :: (MonadParsec e s m, Token s ~ Char) => m a        -> m b -> m b
p <+> q = p <* char ' ' <*> q
p <+  q = p <* char ' ' <*  q
p  +> q = p *> char ' ' *>  q
infixl 4 <+>, <+, +>

-- |Like @('<*>')@, but separates the two parsed tokens with any number of space characters.
(<++>) :: (MonadParsec e s m, Token s ~ Char) => m (a -> b) -> m a -> m b
-- |Like @('<*')@, but separates the two parsed tokens with any number of space characters.
(<++)  :: (MonadParsec e s m, Token s ~ Char) => m a        -> m b -> m a
-- |Like @('*>')@, but separates the two parsed tokens with any number of space characters.
(++>)  :: (MonadParsec e s m, Token s ~ Char) => m a        -> m b -> m b
p <++> q = p <* space1 <*> q
p <++  q = p <* space1 <*  q
p  ++> q = p *> space1 *>  q
infixl 4 <++>, <++, ++>

-- |Parse something surrounded by double quotes (@"@).  Does not do any parsing
-- of the interior.
quoted :: (MonadParsec e s m, Token s ~ Char) => m a -> m a
quoted p = char '"' *> p <* char '"'

-- |Like 'many', but produces a 'Text'.
manyT :: MonadPlus f => f Char -> f Text
manyT = fmap T.pack . many

-- |Like 'parseMaybe', but only checks the start of the string for a match.
--
-- >>> parseStartMaybe (string "hello") "hello, world!"
-- Just "hello"
-- >>> parseMaybe      (string "hello") "hello, world!"
-- Nothing
parseStartMaybe :: Parsec e s a -> s -> Maybe a
parseStartMaybe p s = either (const Nothing) Just $ parse p "" s
