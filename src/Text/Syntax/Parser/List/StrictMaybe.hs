{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE BangPatterns #-}

-- |
-- Module      : Text.Syntax.Parser.List.StrictMaybe
-- Copyright   : 2012 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module includes a strict parser implementation for "Text.Syntax.Poly". Result does not have error informations.
module Text.Syntax.Parser.List.StrictMaybe (
  -- * Syntax instance Parser type
  Parser, runParser, Result(..),
  -- * Poly- morphic wrapper of runParser
  runAsParser
  ) where

import Control.Monad (MonadPlus(mzero, mplus))

import Text.Syntax.Parser.Instances ()
import Text.Syntax.Poly.Class
  (TryAlternative, Syntax (token))
import Text.Syntax.Parser.List.Type (RunAsParser, ErrorString, errorString)

-- | Result type of 'Parser'
data Result a tok = Good !a ![tok] | Bad

-- | Naive 'Parser' type. Parse @[tok]@ into @alpha@.
newtype Parser tok alpha =
  Parser {
    -- | Function to run parser
    runParser :: [tok] -> Result alpha tok
    }

instance Monad (Parser tok) where
  return !a = Parser $ \s -> Good a s
  Parser p >>= fb = Parser (\s -> case p s of
                               Good !a s'   -> case runParser (fb a) s' of
                                 good@(Good !_ _) -> good
                                 Bad              -> Bad
                               Bad                -> Bad)
  fail = const mzero

instance MonadPlus (Parser tok) where
  mzero = Parser $ const Bad
  Parser p1 `mplus` Parser p2 =
    Parser (\s -> case p1 s of
               Bad             -> case p2 s of
                 good@(Good !_ _) -> good
                 Bad              -> Bad
               good@(Good !_ _)   -> good)

instance TryAlternative (Parser tok)

instance Eq tok => Syntax tok (Parser tok) where
  token = Parser (\s -> case s of
                     t:ts -> Good t ts
                     []   -> Bad)

-- | Run 'Syntax' as @'Parser' tok@.
runAsParser :: Eq tok => RunAsParser tok a ErrorString
runAsParser parser s = case runParser parser s of
  Good x []    -> Right x
  Good _ (_:_) -> Left  $ errorString "Not the end of token stream."
  Bad          -> Left  $ errorString "StrictList: parse error."
