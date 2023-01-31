{-
Part of the code in this file comes from the parameterized-utils package:

Copyright (c) 2013-2022 Galois Inc.
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions
are met:

  * Redistributions of source code must retain the above copyright
    notice, this list of conditions and the following disclaimer.

  * Redistributions in binary form must reproduce the above copyright
    notice, this list of conditions and the following disclaimer in
    the documentation and/or other materials provided with the
    distribution.

  * Neither the name of Galois, Inc. nor the names of its contributors
    may be used to endorse or promote products derived from this
    software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER
OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Grisette.IR.SymPrim.Data.Parameterized
  ( NatRepr (..),
    natRepr,
    addNat,
    subNat,
    divNat,
    reprIsKnown,
    withKnownNat,
    LeqProof (..),
    unsafeAxiom,
    unsafeLeqProof,
    unsafeMkNatRepr,
    unsafeWithKnownNat,
    -- unsafeWithNonZeroKnownNat,
    unsafeKnownNat,
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

natRepr :: forall n. KnownNat n => NatRepr n
natRepr = NatRepr (natVal (Proxy @n))

addNat :: NatRepr m -> NatRepr n -> NatRepr (m + n)
addNat (NatRepr m) (NatRepr n) = NatRepr (m + n)

subNat :: n <= m => NatRepr m -> NatRepr n -> NatRepr (m - n)
subNat (NatRepr m) (NatRepr n) = NatRepr (m - n)

divNat :: 1 <= n => NatRepr m -> NatRepr n -> NatRepr (Div m n)
divNat (NatRepr m) (NatRepr n) = NatRepr (m `div` n)

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

unsafeLeqProof :: forall m n. LeqProof m n
unsafeLeqProof = unsafeCoerce (LeqProof @0 @0)

unsafeMkNatRepr :: Natural -> NatRepr w
unsafeMkNatRepr x = NatRepr (fromInteger $ toInteger x)

unsafeKnownNat :: Natural -> KnownProof n
unsafeKnownNat nVal = reprIsKnown (unsafeMkNatRepr nVal)

unsafeWithKnownNat :: forall p w r. p w -> Natural -> (KnownNat w => r) -> r
unsafeWithKnownNat _ i = withKnownNat @w (unsafeMkNatRepr i)

leqAddPos :: (1 <= m, 1 <= n) => p m -> q n -> LeqProof 1 (m + n)
leqAddPos _ _ = unsafeLeqProof

knownAdd :: forall p m q n r. (KnownNat m, KnownNat n) => p m -> q n -> KnownProof (m + n)
knownAdd _ _ = reprIsKnown @(m + n) (NatRepr (natVal (Proxy @m) + natVal (Proxy @n)))

leqTrans :: LeqProof a b -> LeqProof b c -> LeqProof a c
leqTrans _ _ = unsafeLeqProof
