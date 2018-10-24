{-# LANGUAGE TypeFamilies, TypeApplications, OverloadedStrings #-}

module CoqcExpandErrors.Parse.CoqcErrors (
  -- * Parse error locations
  locationP,
  -- * Subsidiary parsers
  fileP, linesP, charsP,
  rangeP, singletonRangeP,
  word
) where

import Control.Monad

import Data.Text (Text)

import CoqcExpandErrors.Util.Megaparsec
import qualified Text.Megaparsec.Char.Lexer as L

import CoqcExpandErrors.Types

--------------------------------------------------------------------------------

word :: TextParser Word
word = do
  n <- L.decimal
  if n <= toInteger @Word maxBound
  then pure $ fromInteger n
  else fail "word out of range"

rangeP :: TextParser a -> TextParser (Range a)
rangeP elt = Range <$> elt <* char '-' <*> elt

singletonRangeP :: TextParser a -> TextParser (Range a)
singletonRangeP elt = join Range <$> elt

fileP :: TextParser Text
fileP = string "File" +> quoted (manyT $ anySingleBut '"')

linesP :: TextParser (Range Line)
linesP =   string "lines" +> rangeP line
       <|> string "line"  +> singletonRangeP line
  where line = Line <$> word

charsP :: TextParser (Range Word)
charsP = string "characters" +> rangeP word

locationP :: TextParser Location
locationP = Location <$> fileP <&> linesP <&> charsP <* char ':'
  where p <&> q = p <* char ',' <+> q
        infixl 4 <&>
