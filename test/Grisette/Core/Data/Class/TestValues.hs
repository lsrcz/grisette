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
  ( Identifier,
    Solvable (con, isym, ssym),
    SymBool,
    Symbol (IndexedSymbol, SimpleSymbol),
    TypedSymbol (TypedSymbol),
  )

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
