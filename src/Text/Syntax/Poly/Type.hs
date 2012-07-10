{-# LANGUAGE Rank2Types #-}

----------------------------------------------------------------------------
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
----------------------------------------------------------------------------

module Text.Syntax.Poly.Type (
  -- * Syntax type
  SyntaxT,
  -- * Type for runParser \/ runPrinter from syntax
  RunParser, RunPrinter,
  RunParserM, RunPrinterM,
  -- * Handy error type
  ErrorString, errorString
  ) where

import Text.Syntax.Poly.Class (Syntax)

type SyntaxT tok tks a = forall delta . Syntax tok tks delta => delta a

-- type StreamSyntaxT tks a = forall delta . StreamSyntax tks delta => delta a

type RunParser     tok tks a e = SyntaxT tok tks a -> tks -> Either e a
type RunPrinter    tok tks a e = SyntaxT tok tks a -> a   -> Either e tks
type RunParserM  m tok tks a e = SyntaxT tok tks a -> tks -> m (Either e a)
type RunPrinterM m tok tks a e = SyntaxT tok tks a -> a   -> m (Either e tks)

newtype ErrorString = ErrorString String

errorString :: String -> ErrorString
errorString =  ErrorString

instance Show ErrorString where
  show (ErrorString s) = s
