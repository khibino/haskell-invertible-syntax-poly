{-# LANGUAGE FlexibleContexts #-}

-- |
-- Module      : Text.Syntax.Poly.Combinators.Char
-- Copyright   : 2010-11 University of Marburg, 2012 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module contains 'Char' type combinators for 'Syntax'.
module Text.Syntax.Poly.Combinators.Char (
  -- * Lexemes
  char,
  comma,
  dot,
  -- * Whitespace
  space,
  skipSpace,
  sepSpace,
  optSpace
  ) where

import Data.Char (isSpace)
import Control.Isomorphism.Partial.Prim ((<$>), ignore, subset)
import Text.Syntax.Poly.Class (Syntax, syntax, token)
import Text.Syntax.Poly.Combinators
  (many, this, (<*), skipMany, skipSome)

-- | Syntax of passed 'Char'.
char :: Syntax Char delta => Char -> delta Char
char c = syntax c <* this c

-- | Syntax comma (,).
comma :: Syntax Char delta => delta ()
comma =  this ','

-- | Syntax dot (.).
dot :: Syntax Char delta => delta ()
dot =  this '.'

-- | Syntax space.
space :: Syntax Char delta => delta Char
space = subset isSpace <$> token

-- | 'skipSpace' marks a position where whitespace is allowed to occur.
-- It accepts arbitrary space while parsing, and produces
-- no space while printing. 
skipSpace  ::  Syntax Char delta => delta ()
skipSpace  =   skipMany space

-- | 'optSpace' marks a position where whitespace is desired to occur.
-- It accepts arbitrary space while parsing, and produces a 
-- single space character while printing.
optSpace  ::  Syntax Char delta => delta ()
optSpace  =   ignore [' ']  <$>  many space

-- | 'sepSpace' marks a position where whitespace is required to occur.
-- It requires one or more space characters while parsing,
-- and produces a single space character while printing.
sepSpace  ::  Syntax Char delta => delta ()
sepSpace  =   ignore ' ' <$> skipSome space
