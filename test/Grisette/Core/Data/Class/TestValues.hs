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

import qualified Data.Text as T
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

ssymBool :: T.Text -> SymBool
ssymBool = ssym

isymBool :: T.Text -> Int -> SymBool
isymBool = isym

ssymbolBool :: T.Text -> TypedSymbol Bool
ssymbolBool = SimpleSymbol

isymbolBool :: T.Text -> Int -> TypedSymbol Bool
isymbolBool = IndexedSymbol
