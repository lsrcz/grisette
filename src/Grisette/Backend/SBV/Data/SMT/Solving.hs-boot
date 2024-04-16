{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      :   Grisette.Backend.SBV.Data.SMT.Solving
-- Copyright   :   (c) Sirui Lu 2021-2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Backend.SBV.Data.SMT.Solving
  ( ApproximationConfig (..),
    ExtraConfig (..),
    GrisetteSMTConfig (..),
    TermTy,
  )
where

import Data.Kind (Type)
import qualified Data.SBV as SBV
import GHC.TypeNats (KnownNat, Nat)
import Grisette.Core.Data.BV (IntN, WordN)
import Grisette.IR.SymPrim.Data.GeneralFun (type (-->))
import Grisette.IR.SymPrim.Data.TabularFun (type (=->))

type Aux :: Bool -> Nat -> Type
type family Aux o n where
  Aux 'True _ = SBV.SInteger
  Aux 'False n = SBV.SInt n

type IsZero :: Nat -> Bool
type family IsZero n where
  IsZero 0 = 'True
  IsZero _ = 'False

type TermTy :: Nat -> Type -> Type
type family TermTy bitWidth b where
  TermTy _ Bool = SBV.SBool
  TermTy n Integer = Aux (IsZero n) n
  TermTy _ (IntN x) = SBV.SBV (SBV.IntN x)
  TermTy _ (WordN x) = SBV.SBV (SBV.WordN x)
  TermTy n (a =-> b) = TermTy n a -> TermTy n b
  TermTy n (a --> b) = TermTy n a -> TermTy n b
  TermTy _ v = v

data ApproximationConfig (n :: Nat) where
  NoApprox :: ApproximationConfig 0
  Approx :: (KnownNat n, IsZero n ~ 'False, SBV.BVIsNonZero n) => p n -> ApproximationConfig n

data ExtraConfig (i :: Nat) = ExtraConfig
  { timeout :: Maybe Int,
    integerApprox :: ApproximationConfig i
  }

data GrisetteSMTConfig (i :: Nat) = GrisetteSMTConfig {sbvConfig :: SBV.SMTConfig, extraConfig :: ExtraConfig i}
