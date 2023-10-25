{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      :   Grisette.Core.Data.Class.ITEOp
-- Copyright   :   (c) Sirui Lu 2021-2023
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Core.Data.Class.ITEOp
  ( ITEOp (..),
  )
where

import GHC.TypeNats (KnownNat, type (<=))
import Grisette.IR.SymPrim.Data.Prim.InternedTerm.Term
  ( LinkedRep,
    SupportedPrim,
  )
import Grisette.IR.SymPrim.Data.Prim.PartialEval.Bool (pevalITETerm)
import Grisette.IR.SymPrim.Data.SymPrim
  ( SomeSymIntN,
    SomeSymWordN,
    SymBool (SymBool),
    SymIntN (SymIntN),
    SymInteger (SymInteger),
    SymWordN (SymWordN),
    binSomeSymIntNR1,
    binSomeSymWordNR1,
    type (-~>) (SymGeneralFun),
    type (=~>) (SymTabularFun),
  )

-- $setup
-- >>> import Grisette.Core
-- >>> import Grisette.IR.SymPrim
-- >>> :set -XDataKinds
-- >>> :set -XBinaryLiterals
-- >>> :set -XFlexibleContexts
-- >>> :set -XFlexibleInstances
-- >>> :set -XFunctionalDependencies

-- | ITE operator for solvable (see "Grisette.Core#solvable")s, including symbolic boolean, integer, etc.
--
-- >>> let a = "a" :: SymBool
-- >>> let b = "b" :: SymBool
-- >>> let c = "c" :: SymBool
-- >>> ites a b c
-- (ite a b c)
class ITEOp v where
  ites :: SymBool -> v -> v -> v

-- ITEOp instances
#define ITEOP_SIMPLE(type) \
instance ITEOp type where \
  ites (SymBool c) (type t) (type f) = type $ pevalITETerm c t f; \
  {-# INLINE ites #-}

#define ITEOP_BV(type) \
instance (KnownNat n, 1 <= n) => ITEOp (type n) where \
  ites (SymBool c) (type t) (type f) = type $ pevalITETerm c t f; \
  {-# INLINE ites #-}

#define ITEOP_BV_SOME(symtype, bf) \
instance ITEOp symtype where \
  ites c = bf (ites c) "ites"; \
  {-# INLINE ites #-}

#define ITEOP_FUN(op, cons) \
instance (SupportedPrim ca, SupportedPrim cb, LinkedRep ca sa, LinkedRep cb sb) => ITEOp (sa op sb) where \
  ites (SymBool c) (cons t) (cons f) = cons $ pevalITETerm c t f; \
  {-# INLINE ites #-}

#if 1
ITEOP_SIMPLE(SymBool)
ITEOP_SIMPLE(SymInteger)
ITEOP_BV(SymIntN)
ITEOP_BV(SymWordN)
ITEOP_BV_SOME(SomeSymIntN, binSomeSymIntNR1)
ITEOP_BV_SOME(SomeSymWordN, binSomeSymWordNR1)
ITEOP_FUN(=~>, SymTabularFun)
ITEOP_FUN(-~>, SymGeneralFun)
#endif
