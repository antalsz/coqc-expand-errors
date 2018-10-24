{-# LANGUAGE LambdaCase #-}

module CoqcExpandErrors.Text.Divvy (
  -- * Divvy up 'Text'
  Divvied(..),
  divvy, divvyFrom,
  -- * Mark text divisions
  highlightDivvy, highlightDivvy',
  -- * Split 'Text' on ranges
  splitRange, splitRangeFrom,
) where

import Control.Lens

import CoqcExpandErrors.Util.Common

import Data.Text (Text)
import qualified Data.Text    as T

import CoqcExpandErrors.Types

--------------------------------------------------------------------------------

-- |@splitRangeFrom base range text@ is like @'splitRange' range text@, except
-- it functions as though there were a phantom @base@ characters before the
-- @text@.  It splits @text@ into the characters before the range, within the
-- range, and after the range, except the indices are all shifted by @base@.
-- (The range is treated as half-open.)  For instance, given
--
-- > ABCDEFGHIJKLMNOPQRSTUVWXYZ
-- >      [--range--)
--
-- We have
--
-- >>> let range = Range 5 15
-- >>> let text  = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
-- >>> traverse_ print [splitRangeFrom i range text | i <- [0..15]]
-- ("ABCDE","FGHIJKLMNO","PQRSTUVWXYZ")
-- ("ABCD","EFGHIJKLMN","OPQRSTUVWXYZ")
-- ("ABC","DEFGHIJKLM","NOPQRSTUVWXYZ")
-- ("AB","CDEFGHIJKL","MNOPQRSTUVWXYZ")
-- ("A","BCDEFGHIJK","LMNOPQRSTUVWXYZ")
-- ("","ABCDEFGHIJ","KLMNOPQRSTUVWXYZ")
-- ("","ABCDEFGHI","JKLMNOPQRSTUVWXYZ")
-- ("","ABCDEFGH","IJKLMNOPQRSTUVWXYZ")
-- ("","ABCDEFG","HIJKLMNOPQRSTUVWXYZ")
-- ("","ABCDEF","GHIJKLMNOPQRSTUVWXYZ")
-- ("","ABCDE","FGHIJKLMNOPQRSTUVWXYZ")
-- ("","ABCD","EFGHIJKLMNOPQRSTUVWXYZ")
-- ("","ABC","DEFGHIJKLMNOPQRSTUVWXYZ")
-- ("","AB","CDEFGHIJKLMNOPQRSTUVWXYZ")
-- ("","A","BCDEFGHIJKLMNOPQRSTUVWXYZ")
-- ("","","ABCDEFGHIJKLMNOPQRSTUVWXYZ")'
--
-- See also 'splitRange' and 'divvyFrom'.
splitRangeFrom :: Word -> Range Word -> Text -> (Text, Text, Text)
splitRangeFrom base r text =
  let r' = r `shiftDn` base
      i  = fromIntegral
      (pre, midPost) = T.splitAt (i (r'^.start))               text
      (mid, post)    = T.splitAt (i (r'^.end) - i (r'^.start)) midPost
  in (pre, mid, post)

-- |Splits the given 'Text' into the characters before the range, within the
-- range, and after the range.  (The range is treated as half-open.)  For
-- instance, given
--
-- > ABCDEFGHIJKLMNOPQRSTUVWXYZ
-- >      [--range--)
--
-- We have
--
-- >>> let range = Range 5 15
-- >>> let text  = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
-- >>> splitRange range text
-- ("ABCDE","FGHIJKLMNO","PQRSTUVWXYZ")
--
-- See also 'splitRangeFrom' and 'divvy'.
splitRange :: Range Word -> Text -> (Text, Text, Text)
splitRange = splitRangeFrom 0
{-# INLINE splitRange #-}

-- |@Divvied@ is a 'Text' that's been divided by a (half-open) range.  There are
-- six possible ways for this to happen:
--
-- 'DivvyBefore':
--
-- >           Before
-- >             |
-- > [-----------V------------]
-- > ABCDEFGHIJKLMNOPQRSTUVWXYZ
-- >                               [--range--)
--
-- 'DivvyStart':
--
-- >         Before       Within
-- >           |            |
-- > [---------V---------][-V-]
-- > ABCDEFGHIJKLMNOPQRSTUVWXYZ
-- >                      [--range--)
--
-- 'DivvyWithin':
--
-- >              Within
-- >                |
-- >    [-----------V------------]
-- >    ABCDEFGHIJKLMNOPQRSTUVWXYZ
-- > [-------------range-------------)
--
-- 'DivvyEnd':
--
-- >      Within       After
-- >        |            | 
-- >      [-V-][---------V---------]
-- >      ABCDEFGHIJKLMNOPQRSTUVWXYZ
-- > [--range--)    
--
-- 'DivvyAfter':
--
-- >                          After
-- >                            |
-- >                [-----------V------------]
-- >                ABCDEFGHIJKLMNOPQRSTUVWXYZ
-- > [--range--)    
--
-- 'DivvyWhole':
--
-- > Before  Within    After
-- >   |       |         |
-- > [-V-][----V---][----V----]
-- > ABCDEFGHIJKLMNOPQRSTUVWXYZ
-- >      [--range--)
--
--
-- Or, in other words:
--
-- > DivvyBefore before
-- > DivvyStart  before within
-- > DivvyWithin        within
-- > DivvyEnd           within end
-- > DivvyAfter                end
-- > DivvyWhole  before within end
data Divvied = DivvyBefore    Text
             | DivvyStart     Text Text
             | DivvyWithin         Text
             | DivvyEnd            Text Text
             | DivvyAfter               Text
             | DivvyWhole     Text Text Text
             deriving (Eq, Ord, Show, Read)

-- |Divides the given 'Text' into the characters before the range, within the
-- range, and after the range, with the adjustment that all indices are shifted
-- by the given base value.  As it returns a 'Divvied', the possible splits are
-- all differentiated.  (The range is treated as half-open.)
--
-- See also 'Divvied', 'divvy', and 'splitRangeFrom'.
divvyFrom :: Word -> Range Word -> Text -> Divvied
divvyFrom base r text =
  let cap  = base + fromIntegral (T.length text) - 1
      here = Range base cap
  in case ((r^.start) `inRange` here, (r^.end) `inRange` here) of
       (True,  True)         -> uncurry3 DivvyWhole  $ splitRangeFrom base r                           text
       (True,  False)        -> uncurry  DivvyStart  $ T.splitAt      (fromIntegral $ r^.start - base) text
       (False, True)         -> uncurry  DivvyEnd    $ T.splitAt      (fromIntegral $ r^.end   - base) text
       (False, False)
         | cap    < r^.start ->          DivvyBefore                                                   text
         | r^.end < base     ->          DivvyAfter                                                    text
         | otherwise         ->          DivvyWithin                                                   text

-- |Divides the given 'Text' into the characters before the range, within the
-- range, and after the range; as it returns a 'Divvied', the possible splits
-- are all differentiated.  (The range is treated as half-open.)
--
-- See also 'Divvied', 'divvyFrom', and 'splitRange'.
divvy :: Range Word -> Text -> Divvied
divvy = divvyFrom 0
{-# INLINE divvy #-}

-- |@highlightDivvy open close divvy@ surrounds the text within the 'Divvied'
-- string @divvy@ with @open@ and @close@.  If @divvy@ doesn't contain a
-- starting point, then @open@ is left out; if it doesn't contain an ending
-- point, then @close@ is left out.
highlightDivvy :: Text -> Text -> Divvied -> Text
highlightDivvy open close = \case
  DivvyBefore pre          -> pre
  DivvyStart  pre mid      -> pre <> open <> mid
  DivvyWithin     mid      ->                mid
  DivvyEnd        mid post ->                mid <> close <> post
  DivvyAfter          post ->                                post
  DivvyWhole  pre mid post -> pre <> open <> mid <> close <> post

-- |@highlightDivvy' open close divvy@ surrounds the text within the 'Divvied'
-- string @divvy@ with @open@ and @close@.  If @divvy@ contains any portion of
-- interior text, said portion is surrounded on both sides by @open@ and
-- @close@.
highlightDivvy' :: Text -> Text -> Divvied -> Text
highlightDivvy' open close = \case
  DivvyBefore pre          -> pre
  DivvyStart  pre mid      -> pre <> open <> mid <> close
  DivvyWithin     mid      ->        open <> mid <> close
  DivvyEnd        mid post ->        open <> mid <> close <> post
  DivvyAfter          post ->                                post
  DivvyWhole  pre mid post -> pre <> open <> mid <> close <> post
