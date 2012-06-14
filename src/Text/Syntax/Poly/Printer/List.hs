{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

----------------------------------------------------------------------------
-- |
-- Module      : Text.Syntax.Poly.Printer.List
-- Copyright   : 2012 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module includes a naive printer implementation for invertible-syntax-poly.
----------------------------------------------------------------------------
module Text.Syntax.Poly.Printer.List (
  -- * Syntax instance Printer type
  Printer, runPrinter,
  -- * Print action
  printM
  ) where

import Control.Isomorphism.Partial (IsoFunctor ((<$>)), unapply)
import Control.Monad (liftM2, mplus)

import Text.Syntax.Poly.Class
  (ProductFunctor ((<*>)),
   IsoAlternative ((<||>), empty), TryAlternative,
   AbstractSyntax (syntax), StreamSyntax (string), Syntax (token))
import Text.Syntax.Poly.Combinators (list)


newtype Printer tok alpha =
  Printer { runPrinter :: alpha -> Maybe [tok] }

-- | Expect print side effect.
printM :: Monad m => Printer tok alpha -> alpha -> m [tok]
printM p x = maybe (fail "print error") return $ runPrinter p x

instance IsoFunctor (Printer tok) where
  iso <$> Printer p 
    = Printer (\b -> unapply iso b >>= p)

instance ProductFunctor (Printer tok) where
  Printer p <*> Printer q 
    = Printer (\(x, y) -> liftM2 (++) (p x) (q y))

instance IsoAlternative (Printer tok) where
  Printer p <||> Printer q 
    = Printer (\s -> mplus (p s) (q s))
  empty = Printer (\_ -> Nothing)

instance TryAlternative (Printer tok)

instance AbstractSyntax (Printer tok) where
  syntax x = Printer (\y ->  if x == y 
                             then Just []
                             else Nothing)

instance Eq tok => StreamSyntax [tok] (Printer tok) where
  string = list

instance Eq tok => Syntax tok [tok] (Printer tok) where
  token  = Printer (\t -> Just [t])
