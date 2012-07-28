-- |
-- Module      : Control.Isomorphism.Partial.Ext
-- Copyright   : 2012 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown


module Control.Isomorphism.Partial.Ext (
  module Exports
  ) where

-- Original definitions
import Control.Isomorphism.Partial as Exports

-- Operator to compose Iso
import Control.Category as Exports ((.))

-- Additional definitions
import Control.Isomorphism.Partial.Ext.Prim         as Exports
import Control.Isomorphism.Partial.Ext.Constructors as Exports
import Control.Isomorphism.Partial.Ext.Data         as Exports
