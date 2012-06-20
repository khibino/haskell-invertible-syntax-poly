{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}

----------------------------------------------------------------------------
-- |
-- Module      : Text.Syntax.Poly.Check
-- Copyright   : 2012 Kei Hibino, 2010-11 University of Marburg
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module contains isomorphism check function.
----------------------------------------------------------------------------

module Text.Syntax.Poly.Check (
  printParseIso', printParseIso,
  printParseIsoDefault', printParseIsoDefault
  ) where

import Text.Syntax.Poly.Type (SyntaxT, RunParserT, RunPrinterT)

import Text.Syntax.Printer.List (runPolyPrinter)
import Text.Syntax.Parser.List  (runPolyParser)

printParseIso' :: Eq a =>
                  RunPrinterT tok tks a -> RunParserT tok tks a -> SyntaxT tok tks a ->
                  a -> Either String a
printParseIso' runPrint runParse syntax tree0 =
  do tks        <- maybe (Left "print error") Right (runPrint syntax tree0)
     (tree1, _) <- maybe (Left "parse error") Right (runParse syntax tks)
     if tree0 == tree1
       then Right tree0
       else Left  "not isomorphism"

printParseIso :: Eq a =>
                 RunPrinterT tok tks a -> RunParserT tok tks a -> SyntaxT tok tks a ->
                 tks -> Either String a
printParseIso runPrint runParse syntax tks0 =
  do (tree0, _) <- maybe (Left "parse error 0") Right (runParse syntax tks0)
     printParseIso' runPrint runParse syntax tree0

printParseIsoDefault' :: (Eq tok, Eq a) => SyntaxT tok [tok] a -> a -> Either String a
printParseIsoDefault' =  printParseIso' runPolyPrinter runPolyParser

printParseIsoDefault :: (Eq tok, Eq a) => SyntaxT tok [tok] a -> [tok] -> Either String a
printParseIsoDefault =  printParseIso runPolyPrinter runPolyParser
