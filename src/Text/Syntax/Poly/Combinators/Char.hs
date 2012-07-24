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
import Text.Syntax.Poly.Class (Syntax)
import Text.Syntax.Poly.Combinators (many, (<*), this)

-- | Syntax comma (,).
comma :: Syntax Char delta => delta ()
comma =  this ','

-- | Syntax dot (.).
dot :: Syntax Char delta => delta ()
dot =  this '.'


-- | `skipSpace` marks a position where whitespace is allowed to
-- occur. It accepts arbitrary space while parsing, and produces
-- no space while printing. 

skipSpace  ::  Syntax Char delta => delta ()
skipSpace  =   ignore []    <$>  many (this ' ')
 
-- | `optSpace` marks a position where whitespace is desired to occur.
-- It accepts arbitrary space while parsing, and produces a 
-- single space character while printing.

optSpace  ::  Syntax Char delta => delta ()
optSpace  =   ignore [()]  <$>  many (this ' ')

-- | `sepSpace` marks a position where whitespace is required to
-- occur. It requires one or more space characters while parsing, 
-- and produces a single space character while printing.

sepSpace  ::  Syntax Char delta => delta ()
sepSpace  =   this ' ' <* skipSpace
