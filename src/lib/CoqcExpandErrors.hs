{-# LANGUAGE TemplateHaskell,
             TypeFamilies, FlexibleContexts,
             GeneralizedNewtypeDeriving, DeriveFunctor,
             TypeApplications, OverloadedStrings,
             LambdaCase, MultiWayIf, RecordWildCards #-}

module CoqcExpandErrors where

import Prelude hiding (lines)

import Control.Lens

import Data.Void
import Data.Maybe
import Data.Foldable
import Data.Traversable hiding (for)
import Data.Bitraversable
import Control.Monad
import Control.Monad.State

import Data.Vector (Vector, (!), (!?))
import qualified Data.Vector as V

import Data.Map (Map)

import Data.Text (Text)
import qualified Data.Text    as T
import qualified Data.Text.IO as T
import Text.Printf

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Pipes
import qualified Pipes.Prelude.Text as P

import Foreign.C.Error
import GHC.IO.Exception
import Control.Exception hiding (try)
import qualified Control.Exception as Exn

import System.IO
import System.Process
import System.IO.Error
import System.Environment
import System.Exit

--------------------------------------------------------------------------------
-- Parsing utilities

type TextParser = Parsec Void Text

(<+>) :: (MonadParsec e s m, Token s ~ Char) => m (a -> b) -> m a -> m b
(<+)  :: (MonadParsec e s m, Token s ~ Char) => m a        -> m b -> m a
(+>)  :: (MonadParsec e s m, Token s ~ Char) => m a        -> m b -> m b
p <+> q = p <* char ' ' <*> q
p <+  q = p <* char ' ' <*  q
p  +> q = p *> char ' ' *>  q
infixl 4 <+>, <+, +>

(<++>) :: (MonadParsec e s m, Token s ~ Char) => m (a -> b) -> m a -> m b
(<++)  :: (MonadParsec e s m, Token s ~ Char) => m a        -> m b -> m a
(++>)  :: (MonadParsec e s m, Token s ~ Char) => m a        -> m b -> m b
p <++> q = p <* space1 <*> q
p <++  q = p <* space1 <*  q
p  ++> q = p *> space1 *>  q
infixl 4 <++>, <++, ++>

quoted :: (MonadParsec e s m, Token s ~ Char) => m a -> m a
quoted p = char '"' *> p <* char '"'

manyT :: MonadPlus f => f Char -> f Text
manyT = fmap T.pack . many

parseStartMaybe :: Parsec e s a -> s -> Maybe a
parseStartMaybe p s = either (const Nothing) Just $ parse p "" s

--------------------------------------------------------------------------------
-- Vectors

ensureHead :: Eq a => a -> Vector a -> Vector a
ensureHead h v | null v || V.head v /= h = V.cons h v
               | otherwise               = v 

ensureLast :: Eq a => a -> Vector a -> Vector a
ensureLast l v | null v || V.last v /= l = V.snoc v l
               | otherwise               = v

-- Find the nearest preceding element, or 'Nothing' if there isn't one
binarySearchBy :: (a -> a -> Ordering) -> Vector a -> a -> Maybe Int
binarySearchBy (<=>) v goal = go 0 (length v) where
  go l h | l < h     = let m = (l + h) `quot` 2
                       in case goal <=> (v ! m) of
                            LT -> go l m
                            EQ -> Just m
                            GT -> go (m+1) h
         | l > 0     = Just $ l - 1
         | otherwise = Nothing

binarySearch :: Ord a => Vector a -> a -> Maybe Int
binarySearch = binarySearchBy compare

--------------------------------------------------------------------------------
-- Words

(∸) :: (Num a, Ord a) => a -> a -> a
m ∸ n | m > n     = m - n
      | otherwise = 0
infixl 6 ∸

(∸~) :: (Num a, Ord a) => ASetter s t a a -> a -> s -> t
l ∸~ n = over l (∸ n)
{-# INLINE (∸~) #-}
infixr 4 ∸~

--------------------------------------------------------------------------------
-- Streaming

-- Copied from 'Pipes.Prelude.Text.stdoutLn', but generalized to any handle
toHandleLn :: MonadIO m => Handle -> Consumer Text m ()
toHandleLn h = go where
  go = await >>= liftIO . Exn.try . T.hPutStrLn h >>= \case
         Left (IOError { ioe_type  = ResourceVanished
                       , ioe_errno = Just ioe })
           | Errno ioe == ePIPE -> pure ()
         Left  e                -> liftIO $ throwIO e
         Right ()               -> go
{-# INLINABLE toHandleLn #-}

--------------------------------------------------------------------------------
-- General

curry3 :: ((a,b,c) -> d) -> a -> b -> c -> d
curry3 f a b c = f (a,b,c)

uncurry3 :: (a -> b -> c -> d) -> (a,b,c) -> d
uncurry3 f (a,b,c) = f a b c

forAccumL :: Traversable t => a -> t b -> (a -> b -> (a,c)) -> (a, t c)
forAccumL s0 t f = mapAccumL f s0 t

--------------------------------------------------------------------------------
-- Data types

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

--------------------------------------------------------------------------------
-- Parsing 

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

--------------------------------------------------------------------------------
-- Coq file lookup

coqCommentP :: TextParser ()
coqCommentP =  string "(*"
            *> skipMany (   coqCommentP
                        <|> void (anySingleBut '*')
                        <|> try (char '*' *> void (anySingleBut ')')))
            <* string "*)"

coqSkipP :: TextParser ()
coqSkipP = skipMany $ space1 <|> coqCommentP

coqStringP :: TextParser Text
coqStringP = char '"' *> manyT (anySingleBut '"' <|> (char '"' *> char '"')) <* char '"'

coqNatP :: TextParser Integer
coqNatP = L.decimal

localityP :: TextParser Text
localityP = string "Local" <|> string "Global"

programP :: TextParser Text
programP = string "Program"

withMetaVernacP :: TextParser Text -> TextParser Text
withMetaVernacP without = go where
  go =   string "Time"                    ++> go
     <|> string "Redirect" ++> coqStringP ++> go
     <|> string "Timeout"  ++> coqNatP    ++> go
     <|> string "Fail"                    ++> go
     <|> without

simpleVernacP :: TextParser Text
simpleVernacP =   oneof "Ltac"
              <|> oneof "Fixpoint CoFixpoint"
              <|> oneof "Let" <* opt' (oneof "Fixpoint CoFixpoint")
              <|> oneof "Theorem Lemma Fact Remark Corollary Proposition Property"
              <|> oneof "Hypothesis Variable  Axiom  Parameter  Conjecture"
              <|> oneof "Hypotheses Variables Axioms Parameters Conjectures"
              <|> oneof "Definition Example SubClass"
              <|> opt (oneof "Cumulative NonCumulative") *> opt (string "Private")
                    *> oneof "Inductive CoInductive Variant Record Structure Class"
              <|> oneof "Instance"
  where oneof  = asum . map string . T.words
        opt  p = optional $ p <* space1
        opt' p = optional $ space1 *> p

vernacP :: TextParser Text
vernacP = withMetaVernacP simpleVernacP

definitionStartP :: TextParser Text
definitionStartP =   space
                 *>  opt localityP
                 *>  opt programP
                 *>  vernacP
                 ++> manyT (alphaNumChar <|> char '_' <|> char '\'')
  where opt p = optional $ p <* space1

data CoqFile = CoqFile { lineText        :: !(Vector Text)
                       , definitionLines :: !(Vector Line)
                       , definitionNames :: !(Vector Text) }
             deriving (Eq, Ord, Show, Read)

parseCoqFile :: MonadIO m => Text -> m CoqFile
parseCoqFile file = liftIO $ do
  lineText <- V.fromList . T.lines <$> T.readFile (T.unpack file)
  let (definitionLines, definitionNames)
        = V.unzip
        . V.mapMaybe (bitraverse (pure . fromIndex) $ parseStartMaybe definitionStartP)
        $ V.indexed lineText
  pure CoqFile{..}

findEnclosingDefinition' :: CoqFile -> Line -> (Maybe Text, Range Line)
findEnclosingDefinition' CoqFile{..} line =
  let (name, before, afterIx, afterDef) = case binarySearch definitionLines line of
        Just i  -> (Just $ definitionNames ! i, definitionLines ! i, i+1, fromIndex $ V.length lineText)
        Nothing -> (Nothing,                    Line 1,              0,   Line 2)
      after = pred . fromMaybe afterDef $ definitionLines !? afterIx
  in (name, Range before after)

-- Drop blank lines
findEnclosingDefinition :: CoqFile -> Line -> (Maybe Text, Range Line)
findEnclosingDefinition cf@CoqFile{..} line =
  let def = findEnclosingDefinition' cf line
      trim i | i > def^._2.start
             , i > line
             , let mtext = lineText !? toIndex i
             , maybe False (isJust . parseMaybe coqSkipP) mtext = trim $ pred i
             | otherwise                                        = i
  in def & _2.end %~ trim

textOnLines :: CoqFile -> Range Line -> Text
textOnLines CoqFile{..} = T.unlines . mapMaybe ((lineText !?) . toIndex) . expand

splitRangeFrom :: Word -> Range Word -> Text -> (Text, Text, Text)
splitRangeFrom base r text =
  let r' = r `shiftDn` base
      i  = fromIntegral
      (pre, midPost) = T.splitAt (i (r'^.start))               text
      (mid, post)    = T.splitAt (i (r'^.end) - i (r'^.start)) midPost
  in (pre, mid, post)

data Divvied = DivvyBefore    Text
             | DivvyStart     Text Text
             | DivvyWithin         Text
             | DivvyEnd            Text Text
             | DivvyAfter               Text
             | DivvyWhole     Text Text Text
             deriving (Eq, Ord, Show, Read)

highlightDivvy :: Text -> Text -> Divvied -> Text
highlightDivvy open close = \case
  DivvyBefore pre          -> pre
  DivvyStart  pre mid      -> pre <> open <> mid
  DivvyWithin     mid      ->                mid
  DivvyEnd        mid post ->                mid <> close <> post
  DivvyAfter          post ->                                post
  DivvyWhole  pre mid post -> pre <> open <> mid <> close <> post

highlightDivvy' :: Text -> Text -> Divvied -> Text
highlightDivvy' open close = \case
  DivvyBefore pre          -> pre
  DivvyStart  pre mid      -> pre <> open <> mid <> close
  DivvyWithin     mid      ->        open <> mid <> close
  DivvyEnd        mid post ->        open <> mid <> close <> post
  DivvyAfter          post ->                                post
  DivvyWhole  pre mid post -> pre <> open <> mid <> close <> post

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

data Marker = Marker { markerOnLine         :: !(Range Line -> Line -> Text -> Text)
                     , markerOtherLine      :: !(Range Line -> Line -> Text -> Text)
                     , markerPreChars       :: !Text
                     , markerPostChars      :: !Text
                     , markerPrePostPerLine :: !Bool }

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

locate1 :: MonadIO m => Marker -> Location -> m (Maybe Text, [Text])
locate1 marker loc = do
  cf <- parseCoqFile (loc^.file)
  pure $ markRangeInEnclosingDefinition marker cf (loc^.lines) (loc^.characters)

locate :: (MonadState (Map Text CoqFile) m, MonadIO m) => Marker -> Location -> m (Maybe Text, [Text])
locate marker loc = do
  cf <- use (at $ loc^.file) >>= \case
          Nothing -> (at (loc^.file) <?=) =<< parseCoqFile (loc^.file)
          Just cf -> pure cf
  pure $ markRangeInEnclosingDefinition marker cf (loc^.lines) (loc^.characters)

--------------------------------------------------------------------------------
-- Streaming

announcing :: Monad m => Producer a m () -> Producer (Maybe a) m ()
announcing prod = for prod (yield . Just) *> yield Nothing

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

reportingBeforeNext :: (Monad m, Foldable f)
                    => (a -> Maybe b)
                    -> (b -> m (f a))
                    -> Producer a m ()
                    -> Consumer a m ()
                    -> Effect m ()
reportingBeforeNext parse display producer consumer =
  announcing producer >-> reportBeforeNext parse display >-> consumer

--------------------------------------------------------------------------------
-- CLI

-- Bold green line markers, yellow background highlights
colorOutput :: Marker
colorOutput = Marker { markerOnLine         = line "\ESC[32;1m" "\ESC[39;22m" '>'
                     , markerOtherLine      = line ""           ""            ' '
                     , markerPreChars       = "\ESC[43m"
                     , markerPostChars      = "\ESC[49m"
                     , markerPrePostPerLine = True }
  where
    line hstart hend c def l t = hstart <> lineTag c def l <> hend <> " " <> t
    lineTag c def (Line i) = T.pack $ printf "%c %*d:" c (maxLength def) i
    
    maxLength :: Range Line -> Int
    maxLength def = round (logBase (10 :: Double) . fromIntegral . unLine $ def^.end) + 1

visualize :: (Maybe Text, [Text]) -> [Text]
visualize (mname, lines) = lines ++ ["", inDefinition, ""] where
  inDefinition = bold $ "Error found in " <> case mname of
                          Just name -> "the definition \"" <> red name <> "\""
                          Nothing   -> red "an unknown definition"
  bold s = "\ESC[1m"  <> s <> "\ESC[22m"
  red  s = "\ESC[31m" <> s <> "\ESC[39m"

annotateCoqErrors :: MonadIO m => Handle -> Handle -> m ()
annotateCoqErrors source sink =
  flip evalStateT mempty . runEffect
    $ reportingBeforeNext (parseMaybe locationP)
                          (fmap visualize . locate colorOutput)
                          (P.fromHandleLn source)
                          (toHandleLn sink)

showProcessCommand :: CreateProcess -> String
showProcessCommand = cmdspec <&> \case
  ShellCommand shell    -> shell
  RawCommand   cmd args -> showCommandForUser cmd args

withCoqErrs :: FilePath -> [String] -> (Handle -> IO a) -> IO (ExitCode, a)
withCoqErrs coq args action =
  let coqProcess = (proc coq $ ["-color", "on"] ++ args)
                     { std_err       = CreatePipe
                     , close_fds     = True
                     , delegate_ctlc = True }
  in withCreateProcess coqProcess $ \_inMH _outMH errMH procH ->
       case errMH of
         Just errH -> do
           result <- action errH
           ec     <- waitForProcess procH
           pure (ec, result)
         Nothing ->
           ioError $ mkIOError
             doesNotExistErrorType
             ("stderr of Coq process `" ++ showProcessCommand coqProcess ++ "`")
             Nothing
             Nothing

withCoqErrs_ :: FilePath -> [String] -> (Handle -> IO ()) -> IO ExitCode
withCoqErrs_ coq args action = fst <$> withCoqErrs coq args action

help :: IO ()
help = do
  name <- getProgName
  putStrLn $ "Usage: " ++ name ++ " COQC [ARGS...]"
  putStrLn ""
  putStrLn "Runs `coqc` and prints out colorized Coq error location information"
  putStrLn ""
  putStrLn "COQC should be a `coqc` executable.  You'll usually want to run this as"
  putStrLn ""
  putStrLn $ "    " ++ name ++ " coqc file.v ..."
  putStrLn ""
  putStrLn "which will function as a drop-in replacement for your system `coqc`."

main :: IO ()
main = getArgs >>= \case
  []       -> help
  coq:args -> exitWith =<< withCoqErrs_ coq args (annotateCoqErrors ?? stderr)
