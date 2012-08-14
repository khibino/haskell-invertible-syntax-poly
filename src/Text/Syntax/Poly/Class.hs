{-# LANGUAGE MultiParamTypeClasses #-}

-- |
-- Module      : Text.Syntax.Poly.Class
-- Copyright   : 2012 Kei Hibino, 2010-11 University of Marburg
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module contains type classes for invertible syntax.
module Text.Syntax.Poly.Class (
  ProductFunctor((<*>)),
  IsoAlternative((<||>), empty),
  TryAlternative((<|>), try),
  AbstractSyntax(syntax, syntaxError),
  Syntax(token)
  ) where

import Control.Isomorphism.Partial (IsoFunctor)

-- | Apply 'IsoFunctor' to another argument with uncurried style.
class ProductFunctor f where
  (<*>) :: f alpha -> f beta -> f (alpha, beta)

infixr 6 <*>

-- | Monoid class for 'IsoFunctor'
class IsoAlternative f where
  -- | like MonadPlus (mplus) or Alternative ((\<|\>))
  (<||>) :: f alpha -> f alpha -> f alpha
  empty  :: f alpha

-- | Support try for combinators which semantics is like Parsec
class IsoAlternative f => TryAlternative f where
  {- | This method should be implemented for combinators
       which semantics is not full-backtracking like parsec.
       ex. @try = Text.Parsec.try@ -}
  try   :: f alpha -> f alpha
  try   =  id
  {- | This method should be implemented for combinators
       which semantics is not full-backtracking like parsec.
       ex. @p <|> q = try p <||> q@ -}
  (<|>) :: f alpha -> f alpha -> f alpha
  (<|>) = (<||>)

infixr 3 <|>, <||>

-- | Syntax abstraction.
class (IsoFunctor delta, ProductFunctor delta,
       IsoAlternative delta, TryAlternative delta)
      => AbstractSyntax delta  where
  -- | Lift a value.
  syntax :: Eq alpha => alpha -> delta alpha
  syntaxError :: String -> delta alpha
  syntaxError =  const empty

-- | Syntax abstraction with token type @tok@.
class AbstractSyntax delta => Syntax tok delta where
  -- | Get a token from stream.
  token :: delta tok
