{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Pizza.Backend.SBV.Data.SMT.Config
  ( PizzaSMTConfig (..),
    sbvConfig,
    TermTy,
  )
where

import Data.Kind
import qualified Data.SBV as SBV
import GHC.TypeNats
import Pizza.IR.SymPrim.Data.BV
import Pizza.IR.SymPrim.Data.Prim.InternedTerm.Term
import Pizza.IR.SymPrim.Data.TabularFunc

type Aux :: Bool -> Nat -> Type
type family Aux o n where
  Aux 'True n = SBV.SInteger
  Aux 'False n = SBV.SInt n

type IsZero :: Nat -> Bool
type family IsZero n where
  IsZero 0 = 'True
  IsZero _ = 'False

type TermTy :: Nat -> Type -> Type
type family TermTy bitWidth b where
  TermTy _ Bool = SBV.SBool
  TermTy n Integer = Aux (IsZero n) n
  TermTy n (IntN x) = SBV.SBV (SBV.IntN x)
  TermTy n (WordN x) = SBV.SBV (SBV.WordN x)
  TermTy n (a =-> b) = TermTy n a -> TermTy n b
  TermTy n (a --> b) = TermTy n a -> TermTy n b
  TermTy _ v = v

data PizzaSMTConfig (integerBitWidth :: Nat) where
  UnboundedReasoning :: SBV.SMTConfig -> PizzaSMTConfig 0
  BoundedReasoning ::
    (KnownNat integerBitWidth, IsZero integerBitWidth ~ 'False, SBV.BVIsNonZero integerBitWidth) =>
    SBV.SMTConfig ->
    PizzaSMTConfig integerBitWidth

sbvConfig :: forall integerBitWidth. PizzaSMTConfig integerBitWidth -> SBV.SMTConfig
sbvConfig (UnboundedReasoning config) = config
sbvConfig (BoundedReasoning config) = config
