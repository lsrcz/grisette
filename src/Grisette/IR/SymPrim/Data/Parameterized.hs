{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Grisette.IR.SymPrim.Data.Parameterized
  ( NatRepr (..),
    withKnownNat,
    LeqProof (..),
    unsafeAxiom,
    unsafeLeqProof,
    unsafeMkNatRepr,
    unsafeWithNonZeroKnownNat,
    leqAddPos,
    knownAdd,
    KnownProof (..),
    leqTrans,
  )
where

import Data.Typeable
import GHC.Natural
import GHC.TypeNats
import Unsafe.Coerce

newtype NatRepr (n :: Nat) = NatRepr Natural

data KnownProof (n :: Nat) where
  KnownProof :: KnownNat n => KnownProof n

reprIsKnown :: forall n. NatRepr n -> KnownProof n
reprIsKnown (NatRepr nVal) =
  case someNatVal nVal of
    SomeNat (Proxy :: Proxy n') ->
      case unsafeAxiom :: n :~: n' of
        Refl -> KnownProof

withKnownNat :: forall n r. NatRepr n -> (KnownNat n => r) -> r
withKnownNat (NatRepr nVal) v =
  case someNatVal nVal of
    SomeNat (Proxy :: Proxy n') ->
      case unsafeAxiom :: n :~: n' of
        Refl -> v

data LeqProof (m :: Nat) (n :: Nat) where
  LeqProof :: m <= n => LeqProof m n

-- | Assert a proof of equality between two types.
-- This is unsafe if used improperly, so use this with caution!
unsafeAxiom :: forall a b. a :~: b
unsafeAxiom = unsafeCoerce (Refl @a)
{-# NOINLINE unsafeAxiom #-} -- Note [Mark unsafe axioms as NOINLINE]

unsafeLeqProof :: forall m n. LeqProof m n
unsafeLeqProof = unsafeCoerce (LeqProof @0 @0)
{-# NOINLINE unsafeLeqProof #-} -- Note [Mark unsafe axioms as NOINLINE]

unsafeMkNatRepr :: Int -> NatRepr w
unsafeMkNatRepr x = NatRepr (fromInteger $ toInteger x)

unsafeWithNonZeroKnownNat :: forall w r. Int -> ((KnownNat w, 1 <= w) => r) -> r
unsafeWithNonZeroKnownNat i r
  | i <= 0 = error "Not an nonzero natural number"
  | otherwise = withKnownNat @w (unsafeMkNatRepr i) $ unsafeBVIsNonZero r
  where
    unsafeBVIsNonZero :: ((1 <= w) => r) -> r
    unsafeBVIsNonZero r1 = case unsafeAxiom :: w :~: 1 of
      Refl -> r1

leqAddPos :: (1 <= m, 1 <= n) => p m -> q n -> LeqProof 1 (m + n)
leqAddPos _ _ = unsafeLeqProof

knownAdd :: forall p m q n r. (KnownNat m, KnownNat n) => p m -> q n -> KnownProof (m + n)
knownAdd _ _ = reprIsKnown @(m + n) (NatRepr (natVal (Proxy @m) + natVal (Proxy @n)))

leqTrans :: LeqProof a b -> LeqProof b c -> LeqProof a c
leqTrans _ _ = unsafeLeqProof
