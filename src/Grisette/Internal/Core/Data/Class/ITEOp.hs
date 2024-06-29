{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      :   Grisette.Internal.Core.Data.Class.ITEOp
-- Copyright   :   (c) Sirui Lu 2021-2023
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Internal.Core.Data.Class.ITEOp
  ( ITEOp (..),
  )
where

import GHC.TypeNats (KnownNat, type (<=))
import Grisette.Internal.SymPrim.FP (ValidFP)
import Grisette.Internal.SymPrim.GeneralFun (type (-->))
import Grisette.Internal.SymPrim.Prim.Term
  ( LinkedRep,
    SupportedPrim (pevalITETerm),
  )
import Grisette.Internal.SymPrim.SymBV
  ( SymIntN (SymIntN),
    SymWordN (SymWordN),
  )
import Grisette.Internal.SymPrim.SymBool (SymBool (SymBool))
import Grisette.Internal.SymPrim.SymFP
  ( SymFP (SymFP),
    SymFPRoundingMode (SymFPRoundingMode),
  )
import Grisette.Internal.SymPrim.SymGeneralFun (type (-~>) (SymGeneralFun))
import Grisette.Internal.SymPrim.SymInteger (SymInteger (SymInteger))
import Grisette.Internal.SymPrim.SymTabularFun (type (=~>) (SymTabularFun))
import Grisette.Internal.SymPrim.TabularFun (type (=->))

-- $setup
-- >>> import Grisette.Core
-- >>> import Grisette.SymPrim
-- >>> :set -XDataKinds
-- >>> :set -XBinaryLiterals
-- >>> :set -XFlexibleContexts
-- >>> :set -XFlexibleInstances
-- >>> :set -XFunctionalDependencies

-- | ITE operator for solvable (see "Grisette.Core#solvable")s, including
-- symbolic boolean, integer, etc.
--
-- >>> let a = "a" :: SymBool
-- >>> let b = "b" :: SymBool
-- >>> let c = "c" :: SymBool
-- >>> symIte a b c
-- (ite a b c)
class ITEOp v where
  -- | Symbolic if-then-else.
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
ITEOP_SIMPLE(SymFPRoundingMode)
ITEOP_BV(SymIntN)
ITEOP_BV(SymWordN)
ITEOP_FUN((=->), (=~>), SymTabularFun)
ITEOP_FUN((-->), (-~>), SymGeneralFun)
#endif

instance (ValidFP eb sb) => ITEOp (SymFP eb sb) where
  symIte (SymBool c) (SymFP t) (SymFP f) = SymFP $ pevalITETerm c t f
  {-# INLINE symIte #-}
