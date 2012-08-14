-- |
-- Module      : Control.Isomorphism.Partial.Ext.Constructors
-- Copyright   : Kei Hibino 2012
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module contains @Iso@ definitions which are inverted definitions
-- defined in @Control.Isomorphism.Partial.Constructors@.

module Control.Isomorphism.Partial.Ext.Constructors (
  cons', nil',
  just', nothing',
  left', right'
  ) where

import Control.Isomorphism.Partial
  (Iso, inverse,
   cons, nil,
   just, nothing, left, right)

-- | 'inverse' of 'nil'
nil' :: Iso [alpha] ()
nil'  = inverse nil

-- | 'inverse' of 'cons'
cons' :: Iso [alpha] (alpha, [alpha])
cons' =  inverse cons

-- | 'inverse' of 'just'
just' :: Iso (Maybe alpha) alpha
just' =  inverse just

-- | 'inverse' of 'nothing'
nothing' :: Iso (Maybe a) ()
nothing' =  inverse nothing

-- | 'inverse' of 'left'
left'  :: Iso (Either alpha b) alpha
left'  =  inverse left

-- | 'inverse' of 'right'
right' :: Iso (Either a alpha) alpha
right' =  inverse right
