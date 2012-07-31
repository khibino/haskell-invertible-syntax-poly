{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

----------------------------------------------------------------------------
-- |
-- Module      : Text.Syntax.Parser.List.Lazy
-- Copyright   : 2012 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module includes a naive parser implementation for invertible-syntax-poly.
-- The same as Text.Syntax.Parser.List other than result Either type.
----------------------------------------------------------------------------
module Text.Syntax.Parser.List.Lazy (
  -- * Syntax instance Parser type
  Parser, runParser, ErrorStack,
  -- * Poly- morphic wrapper of runParser
  runAsParser
  ) where

import Control.Monad (MonadPlus(mzero, mplus))

import Text.Syntax.Parser.Instances ()
import Text.Syntax.Poly.Instances ()
import Text.Syntax.Poly.Class
  (TryAlternative, Syntax (..))
import Text.Syntax.Parser.List.Type (RunAsParser, ErrorStack, errorString)

newtype Parser tok alpha =
  Parser { runParser
           :: [tok] -> ErrorStack -> Either ErrorStack (alpha, [tok]) }

instance Monad (Parser tok) where
  return a = Parser $ \s _ -> Right (a, s)
  Parser p >>= fb = Parser (\s e -> do (a, s') <- p s e
                                       runParser (fb a) s' e)
  fail msg = Parser (\_ e -> Left $ errorString msg : e)

instance MonadPlus (Parser tok) where
  mzero = Parser $ const Left
  Parser p1 `mplus` p2' =
    Parser (\s e -> case p1 s e of
               Left e' -> runParser p2' s (e' ++ e)
               r1      -> r1)

instance TryAlternative (Parser tok)

instance Eq tok => Syntax tok (Parser tok) where
  token = Parser (\s e -> case s of
                     t:ts -> Right (t, ts)
                     []   -> Left $ errorString "The end of token stream." : e)

runAsParser :: Eq tok => RunAsParser tok a ErrorStack
runAsParser parser s =
  do (a, s') <- runParser parser s []
     if s' == []
       then Right a
       else Left [errorString "Not the end of token stream."]
