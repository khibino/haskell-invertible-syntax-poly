
------------------------------------------------------------
-- |
-- Module      : Control.Isomorphism.Partial.Ext.Prim
-- Copyright   : (c) Kei Hibino 2012
-- Licese      : BSD3
-- 
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module contains utility functions for 
-- Control.Isomorphism.Partial.
------------------------------------------------------------

module Control.Isomorphism.Partial.Ext.Prim (
  iso,
  mayAppend',  (||?), mayAppend,
  mayPrepend', (?||), mayPrepend,
  ) where

import Prelude hiding (id)
import Control.Category (id)
import Control.Isomorphism.Partial.Prim (apply, unapply)
import Control.Isomorphism.Partial.Unsafe (Iso (Iso))

-- | Define a isomorphism from two pure functions.
iso :: (a -> b) -> (b -> a) -> Iso a b
iso f g = Iso (Just . f) (Just . g)

-- | May construct or destruct with postfix term.
mayAppend' :: Iso (a, b) c -> Iso a c -> Iso (a, Maybe b) c
mayAppend' iso1 iso2 = Iso f g  where
  f (x, Just y)  = apply iso1 (x, y)
  f (x, Nothing) = apply iso2 x
  g x = maybe
        (maybe
         Nothing
         (\p -> Just (p, Nothing))
         (unapply iso2 x))
        (\(p, q) -> Just (p, Just q))
        (unapply iso1 x)
        
-- | Operator version of 'mayAppend''.
(||?) :: Iso (a, b) c -> Iso a c -> Iso (a, Maybe b) c
(||?) =  mayAppend'

-- | Restricted version of 'mayAppend''
mayAppend :: Iso (a, b) a -> Iso (a, Maybe b) a
mayAppend =  (||? id)

-- | May construct or destruct with prefix term.
mayPrepend' :: Iso (a, b) c -> Iso b c -> Iso (Maybe a, b) c
mayPrepend' iso1 iso2 = Iso f g  where
  f (Just x, y)  = apply iso1 (x, y)
  f (Nothing, y) = apply iso2 y
  g y = maybe
        (maybe
         Nothing
         (\p -> Just (Nothing, p))
         (unapply iso2 y))
        (\(p, q) -> Just (Just p, q))
        (unapply iso1 y)

-- | Operator version of 'mayPrepend''.
(?||) :: Iso (a, b) c -> Iso b c -> Iso (Maybe a, b) c
(?||) =  mayPrepend'

-- | Restricted version of 'mayPrepend''.
mayPrepend :: Iso (a, b) b -> Iso (Maybe a, b) b
mayPrepend =  (?|| id)

infixr 6 ||?
infixl 6 ?||
