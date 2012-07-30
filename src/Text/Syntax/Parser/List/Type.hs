----------------------------------------------------------------------------
-- |
-- Module      : Text.Syntax.Parser.List.Type
-- Copyright   : 2012 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module includes a naive parser implementation for invertible-syntax-poly.
-- The same as Text.Syntax.Parser.List other than result Either type.
----------------------------------------------------------------------------

module Text.Syntax.Parser.List.Type (

  -- * Type to store error state
  ErrorStack,

  -- * Types to run invertible syntax definitions as parsers
  RunAsParser, RunAsStringParser,

  module Text.Syntax.Poly.Type
  ) where

import Text.Syntax.Poly.Type (ErrorString, errorString)
import qualified Text.Syntax.Poly.Type as T

-- | Type to store error state
type ErrorStack = [ErrorString]

-- | Type to run invertible syntax definitions as parsers
type RunAsParser tok a e = T.RunParser tok [tok] a e

-- | Case of @RunAsParser@ when token is @Char@ type
type RunAsStringParser a e = RunAsParser Char a e
