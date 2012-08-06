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
-- This module includes 'Syntax' instance implementation for ReadP.
module Text.Syntax.Parser.ReadP (runAsParser) where

import Data.List (find)

import Text.Syntax.Parser.Instances ()
import Text.Syntax.Poly.Class
  (TryAlternative, Syntax(token))
import Text.Syntax.Poly.Type (RunAsParser, ErrorString, errorString)

import Text.ParserCombinators.ReadP (ReadP, get, readP_to_S)

instance TryAlternative ReadP

instance Syntax Char ReadP where
  token = get

runAsParser :: RunAsParser Char String a ErrorString
runAsParser parser s =
  case find ((== []) . snd) $ readP_to_S parser s of
    Just (a, _) -> Right a
    Nothing     -> Left $ errorString "parse error"
