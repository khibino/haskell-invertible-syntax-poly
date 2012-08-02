{-# LANGUAGE Rank2Types #-}

-- |
-- Module      : Text.Syntax.Poly.Type
-- Copyright   : 2012 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- Type synonyms to represent syntax which has forall type value.

module Text.Syntax.Poly.Type (
  -- * Syntax type
  SyntaxT,
  -- * Type for runParser \/ runPrinter from syntax
  RunAsParser, RunAsPrinter,
  RunAsParserM, RunAsPrinterM,
  -- * Error string type
  ErrorString, errorString
  ) where

import Text.Syntax.Poly.Class (Syntax)

-- | Syntax type synonym includes contexts. Rank2Types extension is needed.
type SyntaxT tok a = forall delta . Syntax tok delta => delta a

type RunAsParser     tok tks a e = SyntaxT tok a -> tks -> Either e a
type RunAsParserM  m tok tks a e = SyntaxT tok a -> tks -> m (Either e a)
type RunAsPrinter    tok tks a e = SyntaxT tok a -> a   -> Either e tks
type RunAsPrinterM m tok tks a e = SyntaxT tok a -> a   -> m (Either e tks)

-- | String type which is Show instance not to show but just return string
newtype ErrorString = ErrorString String

errorString :: String -> ErrorString
errorString =  ErrorString

instance Show ErrorString where
  show (ErrorString s) = s
