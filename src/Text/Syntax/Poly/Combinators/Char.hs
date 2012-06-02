{-# LANGUAGE FlexibleContexts #-}

----------------------------------------------------------------------------
-- |
-- Module      : Text.Syntax.Poly.Combinators.Char
-- Copyright   : 2010-11 University of Marburg, 2012 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module contains `Char` combinators for invertible-syntax-poly classes.
----------------------------------------------------------------------------

module Text.Syntax.Poly.Combinators.Char (
  -- * Lexemes
  comma,
  dot,
  -- * Whitespace
  skipSpace,
  sepSpace,
  optSpace
  ) where

import Control.Isomorphism.Partial.Prim ((<$>), ignore)
import Text.Syntax.Poly.Class (StreamSyntax(string))
import Text.Syntax.Poly.Combinators (many, (<*))

-- | Syntax comma (,).
comma :: StreamSyntax String delta => delta ()
comma = string ","

-- | Syntax dot (.).
dot :: StreamSyntax String delta => delta ()
dot = string "."


-- | `skipSpace` marks a position where whitespace is allowed to
-- occur. It accepts arbitrary space while parsing, and produces
-- no space while printing. 

skipSpace  ::  StreamSyntax String delta => delta ()
skipSpace  =   ignore []    <$>  many (string " ")
 
-- | `optSpace` marks a position where whitespace is desired to occur.
-- It accepts arbitrary space while parsing, and produces a 
-- single space character while printing.

optSpace  ::  StreamSyntax String delta => delta ()
optSpace  =   ignore [()]  <$>  many (string " ")

-- | `sepSpace` marks a position where whitespace is required to
-- occur. It requires one or more space characters while parsing, 
-- and produces a single space character while printing.
   
sepSpace  ::  StreamSyntax String delta => delta ()
sepSpace  =   string " " <* skipSpace
