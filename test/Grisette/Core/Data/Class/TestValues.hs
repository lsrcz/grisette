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

import Grisette
  ( Solvable (isym),
    TypedSymbol (IndexedSymbol, SimpleSymbol),
  )
import Grisette.Core.Data.Class.Solvable (Solvable (con, ssym))
import Grisette.IR.SymPrim.Data.SymPrim (SymBool)

conBool :: Bool -> SymBool
conBool = con

symTrue :: SymBool
symTrue = conBool True

symFalse :: SymBool
symFalse = conBool False

ssymBool :: String -> SymBool
ssymBool = ssym

isymBool :: String -> Int -> SymBool
isymBool = isym

ssymbolBool :: String -> TypedSymbol Bool
ssymbolBool = SimpleSymbol

isymbolBool :: String -> Int -> TypedSymbol Bool
isymbolBool = IndexedSymbol
