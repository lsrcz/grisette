-- Disable this warning because we are re-exporting things.
{-# OPTIONS_GHC -Wno-missing-import-lists #-}

-- |
-- Module      :   Grisette.Utils
-- Copyright   :   (c) Sirui Lu 2021-2023
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Utils
  ( -- * Utilities for type-level natural numbers.

    -- ** Unsafe axiom
    unsafeAxiom,

    -- ** Runtime representation of type-level natural numbers
    NatRepr,
    natValue,
    unsafeMkNatRepr,
    natRepr,
    decNat,
    predNat,
    incNat,
    addNat,
    subNat,
    divNat,
    halfNat,

    -- ** Proof of KnownNat
    KnownProof (..),
    hasRepr,
    withKnownProof,
    unsafeKnownProof,
    knownAdd,

    -- ** Proof of (<=) for type-level natural numbers
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

import Grisette.Utils.Parameterized
  ( KnownProof (..),
    LeqProof (..),
    NatRepr,
    addNat,
    decNat,
    divNat,
    halfNat,
    hasRepr,
    incNat,
    knownAdd,
    leqAdd,
    leqAdd2,
    leqAddPos,
    leqRefl,
    leqSucc,
    leqTrans,
    leqZero,
    natRepr,
    natValue,
    predNat,
    subNat,
    testLeq,
    unsafeAxiom,
    unsafeKnownProof,
    unsafeLeqProof,
    unsafeMkNatRepr,
    withKnownProof,
    withLeqProof,
  )
