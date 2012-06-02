{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

----------------------------------------------------------------------------
-- |
-- Module      : Text.Syntax.Poly.Instances
-- Copyright   : 2012 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module contains instances of invertible-syntax-poly classes.
----------------------------------------------------------------------------

module Text.Syntax.Poly.Instances () where

import Control.Monad (MonadPlus (mzero, mplus))

import Control.Isomorphism.Partial (IsoFunctor((<$>)), apply)
import Text.Syntax.Poly.Class
  (ProductFunctor((<*>)),
   IsoAlternative((<||>), empty), TryAlternative,
   AbstractSyntax(syntax))

-- | Instances on MonadPlus contexts 
-- which are prerequisites for syntax definitions

instance MonadPlus m => IsoFunctor m where
  iso <$> mp = do a <- mp
                  maybe mzero return $ apply iso a

instance Monad m => ProductFunctor m where
  ma <*> mb = do a <- ma
                 b <- mb
                 return (a, b)

instance MonadPlus m => IsoAlternative m where
  (<||>) = mplus
  empty  = mzero

instance (MonadPlus m, TryAlternative m) => AbstractSyntax m where
  syntax = return
