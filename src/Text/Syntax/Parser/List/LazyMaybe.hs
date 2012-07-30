{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

----------------------------------------------------------------------------
-- |
-- Module      : Text.Syntax.Parser.List.LazyMaybe
-- Copyright   : 2012 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module includes a naive parser implementation for invertible-syntax-poly.
----------------------------------------------------------------------------
module Text.Syntax.Parser.List.LazyMaybe (
  -- * Syntax instance Parser type
  Parser, runParser,
  -- * Poly- morphic wrapper of runParser
  runAsParser
  ) where

import Control.Monad (MonadPlus(mzero, mplus))

import Text.Syntax.Parser.Instances ()
import Text.Syntax.Poly.Class
  (TryAlternative, Syntax (token))
import Text.Syntax.Parser.List.Type (RunAsParser, ErrorString, errorString)

newtype Parser tok alpha =
  Parser { runParser :: [tok] -> Maybe (alpha, [tok]) }

instance Monad (Parser tok) where
  return a = Parser $ \s -> Just (a, s)
  Parser p >>= fb = Parser (\s -> do (a, s') <- p s
                                     runParser (fb a) s')

instance MonadPlus (Parser tok) where
  mzero = Parser $ const Nothing
  Parser p1 `mplus` p2' =
    Parser (\s -> p1 s `mplus` runParser p2' s)

instance TryAlternative (Parser tok)

instance Eq tok => Syntax tok (Parser tok) where
  token = Parser (\s -> case s of
                     t:ts -> Just (t, ts)
                     []   -> Nothing)

runAsParser :: Eq tok => RunAsParser tok a ErrorString
runAsParser parser s = case runParser parser s of
  Just (a, [])    -> Right a
  Just (_, (_:_)) -> Left . errorString $ "Not the end of token stream."
  Nothing         -> Left . errorString $ "parse error"

