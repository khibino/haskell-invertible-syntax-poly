{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE BangPatterns #-}

----------------------------------------------------------------------------
-- |
-- Module      : Text.Syntax.Parser.List.Strict
-- Copyright   : 2012 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module includes a strict parser implementation for invertible-syntax-poly.
----------------------------------------------------------------------------
module Text.Syntax.Parser.List.Strict (
  -- * Syntax instance Parser type
  Parser, runParser, Result(..), ErrorStack,
  -- * Poly- morphic wrapper of runParser
  runPolyParser
  ) where

import Control.Monad (MonadPlus(mzero, mplus))

import Text.Syntax.Parser.Instances ()
import Text.Syntax.Poly.Class
  (TryAlternative, StreamSyntax (string), Syntax (token, fail))
import Text.Syntax.Poly.Combinators (list)
import Text.Syntax.Poly.Type (RunParser, ErrorString, errorString)

type ErrorStack = [ErrorString]

data Result a tok = Good !a ![tok] | Bad !ErrorStack

newtype Parser tok alpha =
  Parser { runParser :: [tok] -> ErrorStack -> Result alpha tok }

instance Monad (Parser tok) where
  return !a = Parser $ \s _ -> Good a s
  Parser !p >>= fb = Parser (\s e -> case p s e of
                                Good a s'   -> case runParser (fb a) s' e of
                                  !rv -> rv
                                Bad e'      -> Bad $ e' ++ e)

instance MonadPlus (Parser tok) where
  mzero = Parser $ const Bad
  Parser p1 `mplus` p2' =
    Parser (\s e -> case p1 s e of
               (Bad e')        -> case runParser p2' s e' of
                 !rv -> rv
               good@(Good _ _) -> good)

instance TryAlternative (Parser tok)

instance Eq tok => StreamSyntax [tok] (Parser tok) where
  string = list

instance Eq tok => Syntax tok [tok] (Parser tok) where
  token = Parser (\s e -> case s of
                     t:ts -> Good t ts
                     []   -> Bad $ errorString "eof" : e)
  fail msg = Parser (\_ e -> Bad $ errorString msg : e)

runPolyParser :: Eq tok => RunParser tok [tok] a ErrorStack
runPolyParser parser s = case runParser parser s [] of
  Good x []    -> Right x
  Good _ (_:_) -> Left  [errorString "Not the end of token stream."] 
  Bad  err     -> Left  err
