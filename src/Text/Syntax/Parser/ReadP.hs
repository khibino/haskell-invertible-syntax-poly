{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}

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

module Text.Syntax.Parser.ReadP (runPolyParser) where

import Data.List (find)

import Text.Syntax.Parser.Instances ()
import Text.Syntax.Poly.Class
  (TryAlternative, Syntax(token))
import Text.Syntax.Poly.Combinators (list)
import Text.Syntax.Poly.Type (RunParser, ErrorString, errorString)

import Text.ParserCombinators.ReadP (ReadP, get, readP_to_S)

instance TryAlternative ReadP

instance Syntax Char ReadP where
  token = get

runPolyParser :: RunParser Char String a ErrorString
runPolyParser parser s =
  case find ((== []) . snd) $ readP_to_S parser s of
    Just (a, _) -> Right a
    Nothing     -> Left $ errorString "parse error"
