{-# LANGUAGE FlexibleContexts #-}

----------------------------------------------------------------------------
-- |
-- Module      : Text.Syntax.Poly.Combinators
-- Copyright   : 2010-11 University of Marburg, 2012 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module contains combinators for invertible-syntax-poly classes.
----------------------------------------------------------------------------

module Text.Syntax.Poly.Combinators (
  -- * Lexemes
  this,
  list,
  -- * Repetition
  none,
  many,
  some,
  sepBy, sepBy1,
  chainl1,
  count,
  -- * Sequencing
  (*>),
  (<*),
  between,
  -- * Alternation
  (<+>),
  optional, bool,
  (<$?>), (<?$>),
  -- * Printing
  format
  ) where

import Prelude (String, Char, Eq, Enum(toEnum)
                -- , foldr
               )

import Control.Category ((.))
import Control.Isomorphism.Partial.Constructors
  (nothing, just, nil, cons, left, right)
import Control.Isomorphism.Partial.Derived (foldl)
import Control.Isomorphism.Partial.Prim
  (Iso, (<$>), inverse, element, unit, commute, ignore)
  
import Control.Isomorphism.Partial.Ext.Prim (mayAppend, mayPrepend, inc)

import Text.Syntax.Poly.Class
  (ProductFunctor((<*>)), TryAlternative((<|>)),
   AbstractSyntax(syntax), StreamSyntax(string), Syntax(token))

import Data.Either (Either)
import Data.Maybe (Maybe(Just))
import Data.Bool (Bool(True, False))

-- | `none` parses\/prints empty tokens stream consume\/produces a empty list.
none :: AbstractSyntax delta => delta [alpha]
none =  nil <$> syntax ()

-- | The `many` combinator is used to repeat syntax.
-- @many p@ repeats the passed syntax @p@
-- zero or more than zero times.
many :: AbstractSyntax delta => delta alpha -> delta [alpha]
many p = some p <|> none

-- | The `some` combinator is used to repeat syntax. 
-- @some p@ repeats the passed syntax @p@
-- more than zero times.
some :: AbstractSyntax delta => delta alpha -> delta [alpha]
some p = cons <$> p <*> many p
             
infixl 4 <+>

-- | The '<+>' combinator choose one of two syntax.
(<+>) :: AbstractSyntax delta => delta alpha -> delta beta -> delta (Either alpha beta)
p <+> q = (left <$> p) <|> (right <$> q) 

-- |  this  parses\/prints a fixed token
this :: (Syntax tok tks delta, Eq tok) => tok -> delta ()
this t = inverse (element t) <$> token

-- | `list` parses\/prints a fixed token list and consumes\/produces a unit value.
list :: (Syntax tok tks delta, Eq tok) => [tok] -> delta ()
list []      =    syntax ()
list (c:cs)  =    inverse (element ((), ()))
             <$>  this c
             <*>  list cs
-- list cs = foldr
--           (\ c -> (inverse (element ((), ())) <$>) . (this c <*>))
--           (syntax ())
--           cs

-- | This variant of `<*>` ignores its left result.
-- In contrast to its counterpart derived from the `Applicative` class, the ignored
-- parts have type `delta ()` rather than `delta beta` because otherwise information relevant
-- for pretty-printing would be lost. 

(*>) :: AbstractSyntax delta => delta () -> delta alpha -> delta alpha
p *> q = inverse unit . commute <$> p <*> q

-- | This variant of `<*>` ignores its right result.
-- In contrast to its counterpart derived from the `Applicative` class, the ignored
-- parts have type `delta ()` rather than `delta beta` because otherwise information relevant
-- for pretty-printing would be lost. 

(<*) :: AbstractSyntax delta => delta alpha -> delta () -> delta alpha
p <* q = inverse unit <$> p <*> q

infixl 7 *>, <*

-- | The `between` function combines `*>` and `<*` in the obvious way.
between :: AbstractSyntax delta => delta () -> delta () -> delta alpha -> delta alpha
between p q r = p *> r <* q

-- | The `chainl1` combinator is used to parse a
-- left-associative chain of infix operators. 
chainl1 :: AbstractSyntax delta =>
           delta alpha -> delta beta -> Iso (alpha, (beta, alpha)) alpha -> delta alpha
chainl1 arg op f 
  = foldl f <$> arg <*> many (op <*> arg)

-- | The `count` combinator counts fixed syntax.
count :: (Eq beta, Enum beta, AbstractSyntax delta) => delta () -> delta beta
count p = inc <$> p *> count p <|> syntax (toEnum 0)

-- | The `optional` combinator may parse \/ print passed syntax.
optional :: AbstractSyntax delta => delta alpha -> delta (Maybe alpha)
optional x = just <$> x <|> nothing <$> syntax ()

-- | The `bool` combinator parse \/ print passed syntax or not.
bool :: AbstractSyntax delta => delta () -> delta Bool
bool x = x *> syntax True <|> syntax False

-- | The `sepBy` combinator separates syntax into delimited list.
-- @sepBy p d@ is @p@ list syntax delimited by @d@ syntax.
sepBy :: AbstractSyntax delta => delta alpha -> delta () -> delta [alpha]
sepBy x sep 
  =    x `sepBy1` sep
  <|>  none

-- | The `sepBy1` combinator separates syntax into delimited non-empty list.
-- @sepBy p d@ is @p@ list syntax delimited by @d@ syntax.
sepBy1 :: AbstractSyntax delta => delta alpha -> delta () -> delta [alpha]
sepBy1 x sep = cons <$> x <*> many (sep *> x)

-- | May append not to repeat prefix syntax.
(<$?>) :: AbstractSyntax delta => Iso (a, b) a -> delta (a, Maybe b) -> delta a
cf <$?> pair = mayAppend  cf <$> pair

-- | May prepend not to repeat suffix syntax.
(<?$>) :: AbstractSyntax delta => Iso (a, b) b -> delta (Maybe a, b) -> delta b
cf <?$> pair = mayPrepend cf <$> pair

infix 5 <$?>, <?$>

-- | The `format` combinator just print passed tokens
-- or may parse passed tokens.
-- This is useful in cases when just formatting with indents.
format :: StreamSyntax tks delta => tks -> delta ()
format tks = ignore (Just ()) <$> optional (string tks)
