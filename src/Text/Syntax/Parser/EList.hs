{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

----------------------------------------------------------------------------
-- |
-- Module      : Text.Syntax.Parser.List
-- Copyright   : 2012 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module includes a naive parser implementation for invertible-syntax-poly.
----------------------------------------------------------------------------
module Text.Syntax.Parser.List (
  -- * Syntax instance Parser type
  Parser, runParser,
  -- * Poly- morphic wrapper of runParser
  runPolyParser
  ) where

import Control.Monad (MonadPlus(mzero, mplus))

import Text.Syntax.Poly.Instances ()
import Text.Syntax.Poly.Class
  (TryAlternative, StreamSyntax (string), Syntax (token))
import Text.Syntax.Poly.Combinators (list)
import Text.Syntax.Poly.Type (RunParserT)

newtype Parser tok alpha =
  Parser { runParser :: [tok] -> Maybe (alpha, [tok]) }

instance Monad (Parser tok) where
  return a = Parser $ \s -> Just (a, s)
  Parser p >>= fb = Parser (\s -> do (a, s') <- p s
                                     runParser (fb a) s')

instance MonadPlus (Parser tok) where
  mzero = Parser $ const Nothing
  Parser p1 `mplus` Parser p2 =
    Parser (\s -> p1 s `mplus` p2 s)

instance TryAlternative (Parser tok)

instance Eq tok => StreamSyntax [tok] (Parser tok) where
  string = list

instance Eq tok => Syntax tok [tok] (Parser tok) where
  token = Parser (\s -> case s of
                     t:ts -> Just (t, ts)
                     []   -> Nothing)

runPolyParser :: Eq tok => RunParserT tok [tok] a String
runPolyParser parser = maybe (Left "parse error") Right . runParser parser
