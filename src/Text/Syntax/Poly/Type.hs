{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}

----------------------------------------------------------------------------
-- |
-- Module      : Text.Syntax.Poly.Type
-- Copyright   : 2012 Kei Hibino, 2010-11 University of Marburg
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- Simple type synonym for `Syntax` type-class.
----------------------------------------------------------------------------

module Text.Syntax.Poly.Type (
  SyntaxT, ListSyntaxT) where

import Text.Syntax.Poly.Class (Syntax)

type SyntaxT tok a = Syntax tok tks delta => delta a
type ListSyntaxT tok a = Syntax tok [tok] delta => delta a
