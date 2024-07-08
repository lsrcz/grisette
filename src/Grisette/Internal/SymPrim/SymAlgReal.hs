{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Grisette.Internal.SymPrim.SymAlgReal (SymAlgReal (SymAlgReal)) where

import Control.DeepSeq (NFData)
import Data.Hashable (Hashable (hashWithSalt))
import Data.String (IsString (fromString))
import GHC.Generics (Generic)
import Grisette.Internal.Core.Data.Class.Function (Apply (FunType, apply))
import Grisette.Internal.Core.Data.Class.Solvable
  ( Solvable (con, conView, ssym, sym),
  )
import Grisette.Internal.SymPrim.AlgReal (AlgReal)
import Grisette.Internal.SymPrim.AllSyms (AllSyms (allSymsS), SomeSym (SomeSym))
import Grisette.Internal.SymPrim.Prim.Internal.Term
  ( PEvalNumTerm
      ( pevalAbsNumTerm,
        pevalMulNumTerm,
        pevalNegNumTerm,
        pevalSignumNumTerm
      ),
    pevalSubNumTerm,
  )
import Grisette.Internal.SymPrim.Prim.Term
  ( ConRep (ConType),
    LinkedRep (underlyingTerm, wrapTerm),
    PEvalNumTerm (pevalAddNumTerm),
    SymRep (SymType),
    Term (ConTerm),
    conTerm,
    pformat,
    symTerm,
  )
import Language.Haskell.TH.Syntax (Lift)

-- | Symbolic representation of algebraic real numbers.
newtype SymAlgReal = SymAlgReal {underlyingAlgRealTerm :: Term AlgReal}
  deriving (Lift, Generic)
  deriving anyclass (NFData)

instance ConRep SymAlgReal where
  type ConType SymAlgReal = AlgReal

instance SymRep AlgReal where
  type SymType AlgReal = SymAlgReal

instance LinkedRep AlgReal SymAlgReal where
  underlyingTerm = underlyingAlgRealTerm
  wrapTerm = SymAlgReal

instance Apply SymAlgReal where
  type FunType SymAlgReal = SymAlgReal
  apply = id

instance Eq SymAlgReal where
  SymAlgReal a == SymAlgReal b = a == b

instance Hashable SymAlgReal where
  hashWithSalt s (SymAlgReal a) = hashWithSalt s a

instance IsString SymAlgReal where
  fromString = ssym . fromString

instance Solvable AlgReal SymAlgReal where
  con = SymAlgReal . conTerm
  sym = SymAlgReal . symTerm
  conView (SymAlgReal (ConTerm _ t)) = Just t
  conView _ = Nothing

instance Show SymAlgReal where
  show (SymAlgReal t) = pformat t

instance AllSyms SymAlgReal where
  allSymsS v = (SomeSym v :)

instance Num SymAlgReal where
  (SymAlgReal l) + (SymAlgReal r) = SymAlgReal $ pevalAddNumTerm l r
  (SymAlgReal l) - (SymAlgReal r) = SymAlgReal $ pevalSubNumTerm l r
  (SymAlgReal l) * (SymAlgReal r) = SymAlgReal $ pevalMulNumTerm l r
  negate (SymAlgReal v) = SymAlgReal $ pevalNegNumTerm v
  abs (SymAlgReal v) = SymAlgReal $ pevalAbsNumTerm v
  signum (SymAlgReal v) = SymAlgReal $ pevalSignumNumTerm v
  fromInteger = con . fromInteger
