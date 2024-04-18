{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Grisette.IR.SymPrim.Data.Prim.Internal.IsZero
  ( IsZero,
    KnownIsZero (..),
    IsZeroCases (..),
  )
where

import qualified Data.SBV as SBV
import GHC.TypeNats (KnownNat, Nat)

type family IsZero (a :: Nat) :: Bool where
  IsZero 0 = 'True
  IsZero _ = 'False

data IsZeroCases (a :: Nat) where
  IsZeroEvidence :: (IsZero a ~ 'True) => IsZeroCases a
  NonZeroEvidence :: (IsZero a ~ 'False, SBV.BVIsNonZero a) => IsZeroCases a

instance Show (IsZeroCases a) where
  show IsZeroEvidence = "IsZeroEvidence"
  show NonZeroEvidence = "NonZeroEvidence"

class (KnownNat a) => KnownIsZero (a :: Nat) where
  isZero :: proxy a -> IsZeroCases a

instance KnownIsZero 0 where
  isZero _ = IsZeroEvidence

instance
  {-# OVERLAPPABLE #-}
  (KnownNat a, IsZero a ~ 'False, SBV.BVIsNonZero a) =>
  KnownIsZero a
  where
  isZero _ = NonZeroEvidence
