{-# LANGUAGE TypeFamilies, OverloadedStrings #-}

module CoqcExpandErrors.Parse.CoqDefinitions (
  -- * Parse the start of a definition, including and returning its name
  definitionStartP,
  -- * Subsidiary definition fragments
  vernacP, simpleVernacP, withMetaVernacP,
  localityP, programP,
  -- * Coq lexemes
  coqCommentP, coqSkipP, coqStringP, coqNatP
) where

import Data.Foldable
import Control.Monad

import Data.Text (Text)
import qualified Data.Text as T

import CoqcExpandErrors.Util.Megaparsec
import qualified Text.Megaparsec.Char.Lexer as L

--------------------------------------------------------------------------------

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
