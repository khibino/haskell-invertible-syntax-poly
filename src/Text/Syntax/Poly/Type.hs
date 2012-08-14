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
-- This module contains handy Rank2 type synonyms which has 'Syntax' contexts.
module Text.Syntax.Poly.Type (
  -- * Syntax type
  SyntaxT,
  -- * Type to run syntax as Parser \/ Printer.
  RunAsParser, RunAsParserM,
  RunAsPrinter, RunAsPrinterM,
  -- * Error string type
  ErrorString, errorString
  ) where

import Text.Syntax.Poly.Class (Syntax)

-- | Type synonym for 'Syntax' includes contexts. Rank2Types extension is needed.
type SyntaxT tok a = forall delta . Syntax tok delta => delta a

-- | Type to run syntax as parser
type RunAsParser     tok tks a e = SyntaxT tok a -> tks -> Either e a
-- | Same as 'RunAsParser' other than with computation @m@
type RunAsParserM  m tok tks a e = SyntaxT tok a -> tks -> m (Either e a)

-- | Type to run syntax as printer
type RunAsPrinter    tok tks a e = SyntaxT tok a -> a   -> Either e tks
-- | Same as 'RunAsPrinter' other than with computation @m@
type RunAsPrinterM m tok tks a e = SyntaxT tok a -> a   -> m (Either e tks)

-- | String type which is 'Show' instance not to show but just return String
newtype ErrorString = ErrorString String

-- | Construct 'ErrorString'
errorString :: String -> ErrorString
errorString =  ErrorString

instance Show ErrorString where
  show (ErrorString s) = s
