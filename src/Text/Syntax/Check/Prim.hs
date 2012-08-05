{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}

-- |
-- Module      : Text.Syntax.Check.Prim
-- Copyright   : 2012 Kei Hibino, 2010-11 University of Marburg
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module contains isomorphism check function.

module Text.Syntax.Check.Prim (
  printParseIso0, printParseIso,
  printParseIso0Default, printParseIsoDefault
  ) where

import Text.Syntax.Poly.Type (SyntaxT, RunAsParser, RunAsPrinter)

import Text.Syntax.Printer.List (runAsPrinter)
import Text.Syntax.Parser.List.LazyMaybe  (runAsParser)

-- | Run print and parse series, then check the equality between input and output.
printParseIso0 :: (Eq a, Show e0, Show e1) =>
                  RunAsPrinter tok tks a e0 -> RunAsParser tok tks a e1 ->
                  SyntaxT tok a -> a -> Either String a
printParseIso0 runPrint runParse syntax tree0 =
  do tks   <- either (Left . show) Right $ runPrint syntax tree0
     tree1 <- either (Left . show) Right $ runParse syntax tks
     if tree0 == tree1
       then Right tree0
       else Left  "not isomorphism"

-- | Run parse, print and parse series, then check the quality between first and second AST.
printParseIso :: (Eq a, Show e0, Show e1) =>
                 RunAsPrinter tok tks a e0 -> RunAsParser tok tks a e1
                 -> SyntaxT tok a -> tks -> Either String a
printParseIso runPrint runParse syntax tks0 =
  do tree0 <- either (Left . show) Right $ runParse syntax tks0
     printParseIso0 runPrint runParse syntax tree0

-- | Run print and parse series with naive implementations, then check the equality between input and output.
printParseIso0Default :: (Eq tok, Eq a) => SyntaxT tok a -> a -> Either String a
printParseIso0Default =  printParseIso0 runAsPrinter runAsParser

-- | Run parse, print and parse series with naive implementations, then check the equality between first and second AST.
printParseIsoDefault :: (Eq tok, Eq a) => SyntaxT tok a -> [tok] -> Either String a
printParseIsoDefault =  printParseIso runAsPrinter runAsParser
