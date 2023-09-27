module Grisette.IR.SymPrim.Data.Prim.Model
  ( SymbolSet (..),
  )
where

import qualified Data.HashSet as S
import Grisette.IR.SymPrim.Data.Prim.InternedTerm.Term

newtype SymbolSet = SymbolSet {unSymbolSet :: S.HashSet SomeTypedSymbol}

instance Monoid SymbolSet

instance Semigroup SymbolSet
