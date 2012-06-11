
------------------------------------------------------------
-- |
-- Module      : Control.Isomorphism.Partial.Ext.Constructors
-- Copyright   : (c) Kei Hibino 2012
-- Licese      : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module contains @Iso@ definitions which are inverted
-- definitions defined in
-- @Control.Isomorphism.Partial.Constructors@.
------------------------------------------------------------

module Control.Isomorphism.Partial.Ext.Constructors (
  cons', nil',
  just', nothing',
  left', right'
  ) where

import Control.Isomorphism.Partial
  (Iso, inverse,
   cons, nil,
   just, nothing, left, right)

nil' :: Iso [alpha] ()
nil'  = inverse nil

cons' :: Iso [alpha] (alpha, [alpha])
cons' =  inverse cons

just' :: Iso (Maybe alpha) alpha
just' =  inverse just

nothing' :: Iso (Maybe a) ()
nothing' =  inverse nothing

left'  :: Iso (Either alpha b) alpha
left'  =  inverse left

right' :: Iso (Either a alpha) alpha
right' =  inverse right
