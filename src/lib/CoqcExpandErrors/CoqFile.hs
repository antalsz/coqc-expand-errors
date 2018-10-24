{-# LANGUAGE FlexibleContexts, LambdaCase, MultiWayIf, RecordWildCards #-}

module CoqcExpandErrors.CoqFile (
  -- * Locate errors inside Coq files
  Marker(..),
  locate, locate1,
  -- * Mark errors inside Coq files
  markRangeInEnclosingDefinition,
  -- * Manipulating Coq files directly
  CoqFile(..),
  parseCoqFile,
  -- ** Finding definitions
  findEnclosingDefinition, findEnclosingDefinition'
) where

import Prelude hiding (lines)

import Control.Lens

import CoqcExpandErrors.Util.Common

import Data.Maybe
import Data.Bitraversable
import Control.Monad.State

import CoqcExpandErrors.Util.Vector (Vector, (!), (!?))
import qualified CoqcExpandErrors.Util.Vector as V

import Data.Map (Map)

import Data.Text (Text)
import qualified Data.Text    as T
import qualified Data.Text.IO as T
import CoqcExpandErrors.Text.Divvy

import CoqcExpandErrors.Util.Megaparsec
import CoqcExpandErrors.Parse.CoqDefinitions

import CoqcExpandErrors.Types

--------------------------------------------------------------------------------

-- |A loosely-parsed Coq file, containing…
data CoqFile = CoqFile { lineText        :: !(Vector Text)
                         -- ^The text of every line
                       , definitionLines :: !(Vector Line)
                         -- ^The (1-based) indices of lines that start definitions
                       , definitionNames :: !(Vector Text)
                         -- ^The names of the corresponding definitions (has the
                         -- same length as 'definitionLines')
                       }
             deriving (Eq, Ord, Show, Read)

-- |Given a file name, parse the Coq file.
parseCoqFile :: MonadIO m => Text -> m CoqFile
parseCoqFile file = liftIO $ do
  lineText <- V.fromList . T.lines <$> T.readFile (T.unpack file)
  let (definitionLines, definitionNames)
        = V.unzip
        . V.mapMaybe (bitraverse (pure . fromIndex) $ parseStartMaybe definitionStartP)
        $ V.indexed lineText
  pure CoqFile{..}

-- |Given a line number, find the definition in the Coq file that encloses it:
-- its name and the span of lines it encompasses.
findEnclosingDefinition' :: CoqFile -> Line -> (Maybe Text, Range Line)
findEnclosingDefinition' CoqFile{..} line =
  let (name, before, afterIx, afterDef) = case V.binarySearch definitionLines line of
        Just i  -> (Just $ definitionNames ! i, definitionLines ! i, i+1, fromIndex $ V.length lineText)
        Nothing -> (Nothing,                    Line 1,              0,   Line 2)
      after = pred . fromMaybe afterDef $ definitionLines !? afterIx
  in (name, Range before after)

-- |Given a line number, find the definition in the Coq file that encloses it:
-- its name and the span of lines it encompasses.  Drop all trailing blank or
-- comment lines.
--
-- TODO: Currently, cannot skip multi-line comments.
findEnclosingDefinition :: CoqFile -> Line -> (Maybe Text, Range Line)
findEnclosingDefinition cf@CoqFile{..} line =
  let def = findEnclosingDefinition' cf line
      trim i | i > def^._2.start
             , i > line
             , let mtext = lineText !? toIndex i
             , maybe False (isJust . parseMaybe coqSkipP) mtext = trim $ pred i
             | otherwise                                        = i
  in def & _2.end %~ trim

-- |How to display a definition
data Marker = Marker { markerOnLine         :: !(Range Line -> Line -> Text -> Text)
                       -- ^Render a line which was picked out for highlighting;
                       -- takes the whole definition's line range, the current
                       -- line number, and its text.  For example, can prepend a
                       -- bold line number.
                     , markerOtherLine      :: !(Range Line -> Line -> Text -> Text)
                       -- ^Render a line which was /not/ picked out for
                       -- highlighting; takes the whole definition's line range,
                       -- the current line number, and its text.  For example,
                       -- can prepend a non-bold line number.
                     , markerPreChars       :: !Text
                       -- ^What to put prior to the highlit character range.
                       -- For instance, can be "begin yellow background
                       -- highlighting".
                     , markerPostChars      :: !Text
                       -- ^What to put after the highlit character range.  For
                       -- instance, can be "end yellow background highlighting".
                     , markerPrePostPerLine :: !Bool
                       -- ^Whether to use 'markerPrechars' and 'markerPostChars'
                       -- on every line ('True') or not ('False'); the
                       -- difference between 'highlightDivvy'' and
                       -- 'highlightDivvy'.
                     }

-- |Given a 'Marker', a 'CoqFile', a closed range of lines, and a half-open
-- range of characters starting at the first selected line, pull out the
-- the name and text of the definition and highlight the text.
markRangeInEnclosingDefinition :: Marker -> CoqFile -> Range Line -> Range Word -> (Maybe Text, [Text])
markRangeInEnclosingDefinition Marker{..} cf lines chars =
  let (name, def) = findEnclosingDefinition cf $ lines^.start
      ixedLines   = mapMaybe sequence [(i, lineText cf !? toIndex i) | i <- expand def]
      highlight   = (if markerPrePostPerLine then highlightDivvy' else highlightDivvy)
                      markerPreChars markerPostChars
      (_, hlines) = forAccumL 0 ixedLines $ \c (line, text) ->
                      if | line `inRange` lines ->
                           ( c + fromIntegral (T.length text)
                           , markerOnLine def line . highlight $ divvyFrom c chars text )
                         | line >= lines^.start ->
                           ( c + fromIntegral (T.length text)
                           , markerOtherLine def line . highlight $ divvyFrom c chars text )
                         | otherwise ->
                           ( c
                           , markerOtherLine def line text )
  in (name, hlines)

-- |Given a 'Marker' and a 'Location' from a @coqc@ error message, load the file
-- in the 'Location', find the definition that encloses the error, and return
-- the name and the higlighted text of the definition.
--
-- See also 'locate'.
locate1 :: MonadIO m => Marker -> Location -> m (Maybe Text, [Text])
locate1 marker loc = do
  cf <- parseCoqFile (loc^.file)
  pure $ markRangeInEnclosingDefinition marker cf (loc^.lines) (loc^.characters)

-- |Given a 'Marker' and a 'Location' from a @coqc@ error message, load and
-- cache the file in the 'Location', find the definition that encloses the
-- error, and return the name and the higlighted text of the definition.
--
-- This is like 'locate1', but uses a 'Map' in a state monad so that every Coq
-- file is only loaded once.
locate :: (MonadState (Map Text CoqFile) m, MonadIO m) => Marker -> Location -> m (Maybe Text, [Text])
locate marker loc = do
  cf <- use (at $ loc^.file) >>= \case
          Nothing -> (at (loc^.file) <?=) =<< parseCoqFile (loc^.file)
          Just cf -> pure cf
  pure $ markRangeInEnclosingDefinition marker cf (loc^.lines) (loc^.characters)
