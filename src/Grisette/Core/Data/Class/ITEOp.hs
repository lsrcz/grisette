{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
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
import Grisette.IR.SymPrim.Data.GeneralFun (type (-->))
import Grisette.IR.SymPrim.Data.Prim.Term
  ( LinkedRep,
    SupportedPrim (pevalITETerm),
  )
import Grisette.IR.SymPrim.Data.SymPrim
  ( SymBool (SymBool),
    SymIntN (SymIntN),
    SymInteger (SymInteger),
    SymWordN (SymWordN),
    type (-~>) (SymGeneralFun),
    type (=~>) (SymTabularFun),
  )
import Grisette.IR.SymPrim.Data.TabularFun (type (=->))

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
-- >>> symIte a b c
-- (ite a b c)
class ITEOp v where
  symIte :: SymBool -> v -> v -> v

-- ITEOp instances
#define ITEOP_SIMPLE(type) \
instance ITEOp type where \
  symIte (SymBool c) (type t) (type f) = type $ pevalITETerm c t f; \
  {-# INLINE symIte #-}

#define ITEOP_BV(type) \
instance (KnownNat n, 1 <= n) => ITEOp (type n) where \
  symIte (SymBool c) (type t) (type f) = type $ pevalITETerm c t f; \
  {-# INLINE symIte #-}

#define ITEOP_FUN(cop, op, cons) \
instance (SupportedPrim (cop ca cb), LinkedRep ca sa, LinkedRep cb sb) => ITEOp (op sa sb) where \
  symIte (SymBool c) (cons t) (cons f) = cons $ pevalITETerm c t f; \
  {-# INLINE symIte #-}

#if 1
ITEOP_SIMPLE(SymBool)
ITEOP_SIMPLE(SymInteger)
ITEOP_BV(SymIntN)
ITEOP_BV(SymWordN)
ITEOP_FUN((=->), (=~>), SymTabularFun)
ITEOP_FUN((-->), (-~>), SymGeneralFun)
#endif
