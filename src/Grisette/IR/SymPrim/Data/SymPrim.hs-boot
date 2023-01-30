{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Grisette.IR.SymPrim.Data.SymPrim
  ( SymBool (..),
    SymInteger (..),
    SymRep (..),
  )
where

import Control.DeepSeq
import Data.Hashable
import GHC.Generics
import {-# SOURCE #-} Grisette.Core.Data.Class.Bool
import Grisette.Core.Data.Class.Evaluate
import Grisette.Core.Data.Class.ExtractSymbolics
import Grisette.Core.Data.Class.Solvable
import Grisette.IR.SymPrim.Data.Prim.InternedTerm.Term
import Language.Haskell.TH.Syntax

newtype SymBool = SymBool {underlyingBoolTerm :: Term Bool}

instance Solvable Bool SymBool

instance Eq SymBool

instance Lift SymBool

instance NFData SymBool

instance Show SymBool

instance Hashable SymBool

instance EvaluateSym SymBool

instance ExtractSymbolics SymBool

newtype SymInteger = SymInteger {underlyingIntegerTerm :: Term Integer}

instance Solvable Integer SymInteger

instance Eq SymInteger

instance Lift SymInteger

instance NFData SymInteger

instance Show SymInteger

instance Hashable SymInteger

instance EvaluateSym SymInteger

instance ExtractSymbolics SymInteger

class ConRep sym where
  type ConType sym

class SupportedPrim con => SymRep con where
  type SymType con
  underlyingTerm :: SymType con -> Term con
  wrapTerm :: Term con -> SymType con

{-
newtype Sym a = Sym {underlyingTerm :: Term a}

type SymBool = Sym Bool

instance NFData (Sym a)

instance Lift (Sym a)

instance (SupportedPrim a) => Solvable a (Sym a)

instance (SupportedPrim a) => Eq (Sym a)

instance (SupportedPrim a) => Hashable (Sym a)

instance (SupportedPrim a) => Show (Sym a)

instance (SupportedPrim a) => EvaluateSym (Sym a)

instance (SupportedPrim a) => ExtractSymbolics (Sym a)

-}
