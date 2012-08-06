
-- |
-- Module      : Text.Syntax.Poly
-- Copyright   : 2012 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- Integrated namespace for invertible syntax
module Text.Syntax.Poly (
  module Text.Syntax.Poly.Class,
  module Text.Syntax.Poly.Type,
  -- module Text.Syntax.Poly.Instances,
  module Text.Syntax.Poly.Combinators,
  module Text.Syntax.Poly.Combinators.Char,
  ) where

import Text.Syntax.Poly.Class
import Text.Syntax.Poly.Type
-- import Text.Syntax.Poly.Instances
import Text.Syntax.Poly.Combinators
import Text.Syntax.Poly.Combinators.Char

{-# ANN module "ignore import/export shortcut" #-}
