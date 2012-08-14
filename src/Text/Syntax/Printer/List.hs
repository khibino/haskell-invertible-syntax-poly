{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

-- |
-- Module      : Text.Syntax.Printer.List
-- Copyright   : 2012 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module includes a naive printer implementation for 'Syntax'.
module Text.Syntax.Printer.List (
  -- * Syntax instance Printer type
  Printer, runPrinter,
  -- * Print action
  printM,
  -- * Poly-morphic wrapper of runPrinter
  RunAsPrinter, RunAsStringPrinter, runAsPrinter
  ) where

import Control.Isomorphism.Partial (IsoFunctor ((<$>)), unapply)
import Control.Monad (liftM2, mplus)

import Text.Syntax.Poly.Class
  (ProductFunctor ((<*>)),
   IsoAlternative ((<||>), empty), TryAlternative,
   AbstractSyntax (syntax), Syntax (token))
import Text.Syntax.Poly.Type (ErrorString, errorString)
import qualified Text.Syntax.Poly.Type as T

-- | Naive 'Printer' type. Print @alpha@ into @[tok]@.
newtype Printer tok alpha =
  Printer {
    -- | Function to run printer
    runPrinter :: alpha -> Maybe [tok]
    }

-- | Expect print side effect.
printM :: Monad m => Printer tok alpha -> alpha -> m [tok]
printM p x = maybe (fail "print error") return $ runPrinter p x

-- | 'IsoFunctor' instance for 'Printer'. Unapplying 'Iso' and print.
instance IsoFunctor (Printer tok) where
  iso <$> Printer p
    = Printer (\b -> unapply iso b >>= p)

-- | 'ProductFunctor' instance for 'Printer'. Just print sequential.
instance ProductFunctor (Printer tok) where
  Printer p <*> Printer q
    = Printer (\(x, y) -> liftM2 (++) (p x) (q y))

-- | 'IsoAlternative' instance for 'Printer'. Print first or second.
instance IsoAlternative (Printer tok) where
  Printer p <||> Printer q
    = Printer (\s -> mplus (p s) (q s))
  empty = Printer (\_ -> Nothing)

-- | 'TryAlternative' instance for 'Printer'. Along with default definition.
instance TryAlternative (Printer tok)

-- | 'AbstractSyntax' instance for 'Printer'. Match parsed result and success.
instance AbstractSyntax (Printer tok) where
  syntax x = Printer (\y ->  if x == y
                             then Just []
                             else Nothing)

-- | 'Syntax' instance for 'Printer'. Print token into singleton.
instance Eq tok => Syntax tok (Printer tok) where
  token  = Printer (\t -> Just [t])

-- | Specialized 'RunAsPrinter' type into list.
type RunAsPrinter tok a e = T.RunAsPrinter tok [tok] a e

-- | Specialized 'RunAsPrinter' type into 'String'.
type RunAsStringPrinter a e = RunAsPrinter Char a e

-- | Run 'Syntax' type as 'Printer'.
runAsPrinter :: Eq tok => RunAsPrinter tok a ErrorString
runAsPrinter printer = maybe (Left . errorString $ "print error") Right
                       . runPrinter printer

