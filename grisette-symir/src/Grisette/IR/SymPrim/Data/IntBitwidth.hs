module Grisette.IR.SymPrim.Data.IntBitwidth where

import Data.Bits (FiniteBits (finiteBitSize))
import Language.Haskell.TH

intBitwidthQ :: TypeQ
intBitwidthQ = return $ LitT (NumTyLit $ toInteger $ finiteBitSize (undefined :: Int))
