module Grisette.Core.Data.Class.TestValues
  ( conBool,
    symTrue,
    symFalse,
    ssymBool,
    isymBool,
    ssymbolBool,
    isymbolBool,
  )
where

import Grisette.Core.Data.Class.Solvable (Solvable (con, isym, ssym))
import Grisette.Core.Data.Symbol
  ( Identifier,
    Symbol (IndexedSymbol, SimpleSymbol),
  )
import Grisette.SymPrim.Prim.Term
  ( TypedSymbol (TypedSymbol),
  )
import Grisette.SymPrim.SymBool (SymBool)

conBool :: Bool -> SymBool
conBool = con

symTrue :: SymBool
symTrue = conBool True

symFalse :: SymBool
symFalse = conBool False

ssymBool :: Identifier -> SymBool
ssymBool = ssym

isymBool :: Identifier -> Int -> SymBool
isymBool = isym

ssymbolBool :: Identifier -> TypedSymbol Bool
ssymbolBool = TypedSymbol . SimpleSymbol

isymbolBool :: Identifier -> Int -> TypedSymbol Bool
isymbolBool i idx = TypedSymbol $ IndexedSymbol i idx
