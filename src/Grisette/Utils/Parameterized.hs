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

-- |
-- Module      :   Grisette.Utils.Parameterized
-- Copyright   :   (c) Sirui Lu 2021-2023
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Utils.Parameterized
  ( -- * Unsafe axiom
    unsafeAxiom,

    -- * Unparameterized type
    SomeNatRepr (..),
    SomePositiveNatRepr (..),

    -- * Runtime representation of type-level natural numbers
    NatRepr,
    withKnownNat,
    natValue,
    mkNatRepr,
    mkPositiveNatRepr,
    natRepr,
    decNat,
    predNat,
    incNat,
    addNat,
    subNat,
    divNat,
    halfNat,

    -- * Proof of KnownNat
    KnownProof (..),
    hasRepr,
    withKnownProof,
    unsafeKnownProof,
    knownAdd,

    -- * Proof of (<=) for type-level natural numbers
    LeqProof (..),
    withLeqProof,
    unsafeLeqProof,
    testLeq,
    leqRefl,
    leqSucc,
    leqTrans,
    leqZero,
    leqAdd2,
    leqAdd,
    leqAddPos,
  )
where

import Data.Typeable (Proxy (Proxy), type (:~:) (Refl))
import GHC.TypeNats
  ( Div,
    KnownNat,
    Nat,
    SomeNat (SomeNat),
    natVal,
    someNatVal,
    type (+),
    type (-),
    type (<=),
  )
import Numeric.Natural (Natural)
import Unsafe.Coerce (unsafeCoerce)

-- | Assert a proof of equality between two types.
-- This is unsafe if used improperly, so use this with caution!
unsafeAxiom :: forall a b. a :~: b
unsafeAxiom = unsafeCoerce (Refl @a)

withKnownNat :: forall n r. NatRepr n -> ((KnownNat n) => r) -> r
withKnownNat (NatRepr nVal) v =
  case someNatVal nVal of
    SomeNat (Proxy :: Proxy n') ->
      case unsafeAxiom :: n :~: n' of
        Refl -> v

-- | A runtime representation of type-level natural numbers.
-- This can be used for performing dynamic checks on type-level natural numbers.
newtype NatRepr (n :: Nat) = NatRepr Natural

-- | The underlying runtime natural number value of a type-level natural number.
natValue :: NatRepr n -> Natural
natValue (NatRepr n) = n

data SomeNatReprHelper where
  SomeNatReprHelper :: NatRepr n -> SomeNatReprHelper

data SomeNatRepr where
  SomeNatRepr :: (KnownNat n) => NatRepr n -> SomeNatRepr

-- | Turn a @Natural@ into the corresponding @NatRepr@ with the KnownNat
-- constraint.
mkNatRepr :: Natural -> SomeNatRepr
mkNatRepr n = case SomeNatReprHelper (NatRepr n) of
  SomeNatReprHelper natRepr -> withKnownNat natRepr $ SomeNatRepr natRepr

data SomePositiveNatRepr where
  SomePositiveNatRepr ::
    (KnownNat n, 1 <= n) => NatRepr n -> SomePositiveNatRepr

-- | Turn a @NatRepr@ into the corresponding @NatRepr@ with the KnownNat
-- constraint and asserts that its greater than 0.
mkPositiveNatRepr :: Natural -> SomePositiveNatRepr
mkPositiveNatRepr 0 = error "mkPositiveNatRepr: 0 is not a positive number"
mkPositiveNatRepr n = case mkNatRepr n of
  SomeNatRepr (natRepr :: NatRepr n) -> case unsafeLeqProof @1 @n of
    LeqProof -> SomePositiveNatRepr natRepr

-- | Construct a runtime representation of a type-level natural number when its
-- runtime value is known.
natRepr :: forall n. (KnownNat n) => NatRepr n
natRepr = NatRepr (natVal (Proxy @n))

-- | Decrement a 'NatRepr' by 1.
decNat :: (1 <= n) => NatRepr n -> NatRepr (n - 1)
decNat (NatRepr n) = NatRepr (n - 1)

-- | Predecessor of a 'NatRepr'
predNat :: NatRepr (n + 1) -> NatRepr n
predNat (NatRepr n) = NatRepr (n - 1)

-- | Increment a 'NatRepr' by 1.
incNat :: NatRepr n -> NatRepr (n + 1)
incNat (NatRepr n) = NatRepr (n + 1)

-- | Addition of two 'NatRepr's.
addNat :: NatRepr m -> NatRepr n -> NatRepr (m + n)
addNat (NatRepr m) (NatRepr n) = NatRepr (m + n)

-- | Subtraction of two 'NatRepr's.
subNat :: (n <= m) => NatRepr m -> NatRepr n -> NatRepr (m - n)
subNat (NatRepr m) (NatRepr n) = NatRepr (m - n)

-- | Division of two 'NatRepr's.
divNat :: (1 <= n) => NatRepr m -> NatRepr n -> NatRepr (Div m n)
divNat (NatRepr m) (NatRepr n) = NatRepr (m `div` n)

-- | Half of a 'NatRepr'.
halfNat :: NatRepr (n + n) -> NatRepr n
halfNat (NatRepr n) = NatRepr (n `div` 2)

-- | @'KnownProof n'@ is a type whose values are only inhabited when @n@ has
-- a known runtime value.
data KnownProof (n :: Nat) where
  KnownProof :: (KnownNat n) => KnownProof n

-- | Introduces the 'KnownNat' constraint when it's proven.
withKnownProof :: KnownProof n -> ((KnownNat n) => r) -> r
withKnownProof p r = case p of KnownProof -> r

-- | Construct a 'KnownProof' given the runtime value.
--
-- __Note:__ This function is unsafe, as it does not check that the runtime
-- representation is consistent with the type-level representation.
-- You should ensure the consistency yourself or the program can crash or
-- generate incorrect results.
unsafeKnownProof :: Natural -> KnownProof n
unsafeKnownProof nVal = hasRepr (NatRepr nVal)

-- | Construct a 'KnownProof' given the runtime representation.
hasRepr :: forall n. NatRepr n -> KnownProof n
hasRepr (NatRepr nVal) =
  case someNatVal nVal of
    SomeNat (Proxy :: Proxy n') ->
      case unsafeAxiom :: n :~: n' of
        Refl -> KnownProof

-- | Adding two type-level natural numbers with known runtime values gives a
-- type-level natural number with a known runtime value.
knownAdd :: forall m n. KnownProof m -> KnownProof n -> KnownProof (m + n)
knownAdd KnownProof KnownProof = hasRepr @(m + n) (NatRepr (natVal (Proxy @m) + natVal (Proxy @n)))

-- | @'LeqProof m n'@ is a type whose values are only inhabited when @m <= n@.
data LeqProof (m :: Nat) (n :: Nat) where
  LeqProof :: (m <= n) => LeqProof m n

-- | Introduces the @m <= n@ constraint when it's proven.
withLeqProof :: LeqProof m n -> ((m <= n) => r) -> r
withLeqProof p r = case p of LeqProof -> r

-- | Construct a 'LeqProof'.
--
-- __Note:__ This function is unsafe, as it does not check that the left-hand
-- side is less than or equal to the right-hand side.
-- You should ensure the consistency yourself or the program can crash or
-- generate incorrect results.
unsafeLeqProof :: forall m n. LeqProof m n
unsafeLeqProof = unsafeCoerce (LeqProof @0 @0)

-- | Checks if a 'NatRepr' is less than or equal to another 'NatRepr'.
testLeq :: NatRepr m -> NatRepr n -> Maybe (LeqProof m n)
testLeq (NatRepr m) (NatRepr n) =
  case compare m n of
    LT -> Nothing
    EQ -> Just unsafeLeqProof
    GT -> Just unsafeLeqProof

-- | Apply reflexivity to 'LeqProof'.
leqRefl :: f n -> LeqProof n n
leqRefl _ = LeqProof

-- | A natural number is less than or equal to its successor.
leqSucc :: f n -> LeqProof n (n + 1)
leqSucc _ = unsafeLeqProof

-- | Apply transitivity to 'LeqProof'.
leqTrans :: LeqProof a b -> LeqProof b c -> LeqProof a c
leqTrans _ _ = unsafeLeqProof

-- | Zero is less than or equal to any natural number.
leqZero :: LeqProof 0 n
leqZero = unsafeLeqProof

-- | Add both sides of two inequalities.
leqAdd2 :: LeqProof xl xh -> LeqProof yl yh -> LeqProof (xl + yl) (xh + yh)
leqAdd2 _ _ = unsafeLeqProof

-- | Produce proof that adding a value to the larger element in an 'LeqProof'
-- is larger.
leqAdd :: LeqProof m n -> f o -> LeqProof m (n + o)
leqAdd _ _ = unsafeLeqProof

-- | Adding two positive natural numbers is positive.
leqAddPos :: (1 <= m, 1 <= n) => p m -> q n -> LeqProof 1 (m + n)
leqAddPos _ _ = unsafeLeqProof
