{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      : Text.Syntax.Parser.Instances
-- Copyright   : 2012 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module contains basic parsers instances for 'Syntax'.
module Text.Syntax.Parser.Instances () where

import Control.Isomorphism.Partial (IsoFunctor((<$>)))
import Control.Monad (MonadPlus (mzero))

import Control.Isomorphism.Partial.Ext.Prim (apply')
import Text.Syntax.Poly.Instances ()

instance MonadPlus m => IsoFunctor m where
  iso <$> mp = do a <- mp
                  maybe mzero return $ apply' iso a
