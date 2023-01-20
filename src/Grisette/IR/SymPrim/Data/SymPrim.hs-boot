{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Grisette.IR.SymPrim.Data.SymPrim (Sym (..), SymBool) where

import Control.DeepSeq
import Data.Hashable
import GHC.Generics
import {-# SOURCE #-} Grisette.Core.Data.Class.Bool
import Grisette.Core.Data.Class.Evaluate
import Grisette.Core.Data.Class.ExtractSymbolics
import Grisette.Core.Data.Class.Solvable
import Grisette.IR.SymPrim.Data.Prim.InternedTerm.Term
import Language.Haskell.TH.Syntax

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
