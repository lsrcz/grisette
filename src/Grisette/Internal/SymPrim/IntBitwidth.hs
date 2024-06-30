-- |
-- Module      :   Grisette.Internal.SymPrim.IntBitwidth
-- Copyright   :   (c) Sirui Lu 2021-2023
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Internal.SymPrim.IntBitwidth (intBitwidthQ) where

import Data.Bits (FiniteBits (finiteBitSize))
import Language.Haskell.TH (TyLit (NumTyLit), Type (LitT), TypeQ)

-- | Get the bitwidth of 'Int'.
intBitwidthQ :: TypeQ
intBitwidthQ =
  return $ LitT (NumTyLit $ toInteger $ finiteBitSize (undefined :: Int))
