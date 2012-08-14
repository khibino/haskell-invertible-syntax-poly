-- |
-- Module      : Text.Syntax.Parser.List.Type
-- Copyright   : 2012 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module includes type synonyms for naive parsers.
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
type RunAsParser tok a e = T.RunAsParser tok [tok] a e

-- | Case of @RunAsParser@ when token is @Char@ type
type RunAsStringParser a e = RunAsParser Char a e
