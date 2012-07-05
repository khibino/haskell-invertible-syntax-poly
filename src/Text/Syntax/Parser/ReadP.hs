{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}

----------------------------------------------------------------------------
-- |
-- Module      : Text.Syntax.Poly.Parser.ReadP
-- Copyright   : 2012 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module includes invertible-syntax-poly instance implementation for ReadP.
-- Token may be polimorphic.
----------------------------------------------------------------------------
module Text.Syntax.Parser.ReadP () where

import Text.Syntax.Parser.Instances ()
import Text.Syntax.Poly.Class
  (TryAlternative, StreamSyntax(string), Syntax(token))
import Text.Syntax.Poly.Combinators (list)

import Text.ParserCombinators.ReadP (ReadP, get)

instance TryAlternative ReadP

instance StreamSyntax String ReadP where
  string = list

instance Syntax Char String ReadP where
  token = get
