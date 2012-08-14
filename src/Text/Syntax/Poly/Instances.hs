{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      : Text.Syntax.Poly.Instances
-- Copyright   : 2012 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module contains basic instance definitions for classes defined in "Text.Syntax.Poly.Class".
module Text.Syntax.Poly.Instances () where

import Control.Monad (MonadPlus (mzero, mplus))

import Control.Isomorphism.Partial (IsoFunctor)
import Text.Syntax.Poly.Class
  (ProductFunctor((<*>)),
   IsoAlternative((<||>), empty), TryAlternative,
   AbstractSyntax(syntax, syntaxError))

-- | 'ProductFunctor' instance on 'Monad' context
-- which is a prerequisite for 'Syntax' definitions.
instance Monad m => ProductFunctor m where
  ma <*> mb = do a <- ma
                 b <- mb
                 return (a, b)

-- | 'IsoAlternative' instance on 'MonadPlus' context
-- which is a prerequisite for 'Syntax' definitions.
instance MonadPlus m => IsoAlternative m where
  (<||>) = mplus
  empty  = mzero

-- | 'AbstractSyntax' instance on 'MonadPlus' context
-- which is a prerequisite for 'Syntax' definitions.
instance (IsoFunctor m, MonadPlus m, TryAlternative m) => AbstractSyntax m where
  syntax = return
  syntaxError = fail
