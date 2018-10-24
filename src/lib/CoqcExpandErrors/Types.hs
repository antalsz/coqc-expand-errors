{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving, DeriveFunctor #-}

module CoqcExpandErrors.Types (
  -- * Line numbers
  Line(..),
  unLine, fromIndex, toIndex,
  -- * Ranges of things (line numbers, characters, …)
  Range(..), start, end,
  expand, inRange, shift, shiftDn,
  -- * File locations, as reported from @coqc@
  Location(..), file, lines, characters,
) where

import Prelude hiding (lines)

import Control.Lens

import CoqcExpandErrors.Util.Num

import Data.Text (Text)

--------------------------------------------------------------------------------

newtype Line = Line Word
             deriving (Eq, Ord, Enum, Bounded, Show, Read)

unLine :: Line -> Word
unLine (Line l) = l

fromIndex :: Int -> Line
fromIndex = Line . fromIntegral . (+ 1)

toIndex :: Line -> Int
toIndex = subtract 1 . fromIntegral . unLine

data Range a = Range { _start, _end :: !a }
             deriving (Eq, Ord, Show, Read, Functor)
makeLenses ''Range

expand :: Enum a => Range a -> [a]
expand r = [r^.start .. r^.end]

inRange :: Ord a => a -> Range a -> Bool
inRange x r = r^.start <= x && x <= r^.end

shift :: Num a => Range a -> a -> Range a
shift r δ = r <&> (+ δ)

shiftDn :: (Ord a, Num a) => Range a -> a -> Range a
shiftDn r δ = r <&> (∸ δ)

data Location = Location { _file       :: !Text
                         , _lines      :: !(Range Line)
                         , _characters :: !(Range Word) }
              deriving (Eq, Ord, Show, Read)
makeLenses ''Location
