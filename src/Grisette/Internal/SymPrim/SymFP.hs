{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Grisette.Internal.SymPrim.SymFP
  ( SymFP (SymFP),
    SymFP16,
    SymFP32,
    SymFP64,
  )
where

import Control.DeepSeq (NFData)
import Data.Hashable (Hashable (hashWithSalt))
import Data.String (IsString (fromString))
import GHC.Generics (Generic)
import Grisette.Internal.Core.Data.Class.Function (Apply (FunType, apply))
import Grisette.Internal.Core.Data.Class.Solvable
  ( Solvable (con, conView, ssym, sym),
  )
import Grisette.Internal.SymPrim.AllSyms (AllSyms (allSymsS), SomeSym (SomeSym))
import Grisette.Internal.SymPrim.FP (FP, ValidFP)
import Grisette.Internal.SymPrim.Prim.Internal.Term
  ( ConRep (ConType),
    LinkedRep (underlyingTerm, wrapTerm),
    SymRep (SymType),
    Term (ConTerm),
    conTerm,
    pformat,
    symTerm,
  )
import Language.Haskell.TH.Syntax (Lift)

-- | Symbolic IEEE 754 floating-point number with @eb@ exponent bits and @sb@
-- significand bits.
newtype SymFP eb sb = SymFP {underlyingFPTerm :: Term (FP eb sb)}
  deriving (Lift, NFData, Generic)

-- | Symbolic IEEE 754 half-precision floating-point number.
type SymFP16 = SymFP 5 11

-- | Symbolic IEEE 754 single-precision floating-point number.
type SymFP32 = SymFP 8 24

-- | Symbolic IEEE 754 double-precision floating-point number.
type SymFP64 = SymFP 11 53

instance ConRep (SymFP eb sb) where
  type ConType (SymFP eb sb) = FP eb sb

instance (ValidFP eb sb) => SymRep (FP eb sb) where
  type SymType (FP eb sb) = SymFP eb sb

instance (ValidFP eb sb) => LinkedRep (FP eb sb) (SymFP eb sb) where
  underlyingTerm (SymFP a) = a
  wrapTerm = SymFP

instance (ValidFP eb sb) => Apply (SymFP eb sb) where
  type FunType (SymFP eb sb) = SymFP eb sb
  apply = id

instance (ValidFP eb sb) => Eq (SymFP eb sb) where
  SymFP a == SymFP b = a == b

instance (ValidFP eb sb) => Hashable (SymFP eb sb) where
  hashWithSalt s (SymFP a) = hashWithSalt s a

instance (ValidFP eb sb) => IsString (SymFP eb sb) where
  fromString = ssym . fromString

instance (ValidFP eb sb) => Solvable (FP eb sb) (SymFP eb sb) where
  con = SymFP . conTerm
  sym = SymFP . symTerm
  conView (SymFP (ConTerm _ t)) = Just t
  conView _ = Nothing

instance (ValidFP eb sb) => Show (SymFP eb sb) where
  show (SymFP a) = pformat a

instance (ValidFP eb sb) => AllSyms (SymFP eb sb) where
  allSymsS v = (SomeSym v :)
