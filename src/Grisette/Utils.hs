-- Disable this warning because we are re-exporting things.
{-# OPTIONS_GHC -Wno-missing-import-lists #-}

-- This file contains code from the parameterized-utils package:
--
-- Copyright (c) 2013-2022 Galois Inc.
-- All rights reserved.
--
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions
-- are met:
--
--   * Redistributions of source code must retain the above copyright
--     notice, this list of conditions and the following disclaimer.
--
--   * Redistributions in binary form must reproduce the above copyright
--     notice, this list of conditions and the following disclaimer in
--     the documentation and/or other materials provided with the
--     distribution.
--
--   * Neither the name of Galois, Inc. nor the names of its contributors
--     may be used to endorse or promote products derived from this
--     software without specific prior written permission.
--
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
-- IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
-- TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
-- PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER
-- OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
-- EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
-- PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
-- PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
-- LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
-- NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
-- SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

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

import Grisette.Internal.Utils.Parameterized
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
    withKnownProof,
    withLeqProof,
  )
