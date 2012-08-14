-- |
-- Module      : Control.Isomorphism.Partial.Ext.Data
-- Copyright   : Kei Hibino 2012
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module contains isomorphism definitions of basic
-- data types for Control.Isomorphism.Partial.

module Control.Isomorphism.Partial.Ext.Data (
  succ, singleton,
  readShow, negate, not, reverse,
  digitInt, chrOrd, oct, hex,
  signumAbs, digitsFloat, floatTripleDigits, floatTriple
  ) where

import Prelude hiding (id, succ, pred, negate, not, reverse, (.))
import qualified Prelude as P
import Control.Category ((.))

import Data.Maybe (listToMaybe)
import Data.Char (intToDigit, digitToInt, ord, chr)
import Data.List (foldl')
import Numeric (readOct, showOct, readHex, showHex, floatToDigits)

import Control.Isomorphism.Partial.Unsafe (Iso (Iso))
import Control.Isomorphism.Partial.Ext.Prim (iso)

-- | Church number succ isomorphism
succ :: Enum a => Iso a a
succ =  Iso f g  where
  f = Just . P.succ
  g n | fromEnum n <= 0 = Nothing
      | otherwise       = Just . P.pred $ n

-- | A value and singleton list
singleton :: Iso a [a]
singleton = Iso f g where
  f = Just . (:[])
  g [x] = Just x
  g _   = Nothing


-- | Isomorphism from read and show
readShow :: (Read a, Show a) => Iso String a
readShow =  iso read show

-- | Negate number
negate :: Num a => Iso a a
negate =  iso P.negate P.negate

-- | Negate boolean
not :: Iso Bool Bool
not =  iso P.not P.not

-- | List reverse
reverse :: Iso [a] [a]
reverse =  iso P.reverse P.reverse

-- | digit char and integer
digitInt :: Iso Char Int
digitInt =  iso digitToInt intToDigit

-- | char and charactor code
chrOrd :: Iso Char Int
chrOrd =  iso ord chr

-- | Read and show octal
oct :: (Integral a, Show a) => Iso String a
oct =  Iso f g where
  f =  fmap fst . listToMaybe . readOct
  g =  Just . (`showOct` "")

-- | Read and show hexadecimal
hex :: (Integral a, Show a) => Iso String a
hex =  Iso f g where
  f =  fmap fst . listToMaybe . readHex
  g =  Just . (`showHex` "")

-- | Isomorphism between 'signum', 'abs' pair and number
signumAbs :: (Num a, Eq a) => Iso (a, a) a
signumAbs =  iso f g  where
  f (x, y) = x * y
  g 0      = (1, 0)
  g v      = (signum v, abs v)

-- | 'floatToDigits' and that's inverse.
digitsFloat :: RealFloat a => Iso ([Int], Int) a
digitsFloat =  iso f g where
  f (ds, e) | e' >= 0   = dv * 10 ^ e'
            | otherwise = dv / 10 ^ (- e')
    where dv = foldl' (\v d -> v * 10 + fromIntegral d) 0 ds
          e' = e - length ds
  g = floatToDigits 10

-- | Float Triple is (int part, (fraction part, exponent)), and Digits is result of 'floatToDigits'
floatTripleDigits :: Iso (String, (String, Int)) ([Int], Int)
floatTripleDigits =  iso p q  where
  p (i, (f, e)) = (map digitToInt (i ++ f), length i + e)
  q (ds, e)     = ("", (map intToDigit ds, e))

-- | Isomorphism between Float Triple and floating number
floatTriple :: RealFloat a => Iso (String, (String, Int)) a
floatTriple =  digitsFloat . floatTripleDigits
