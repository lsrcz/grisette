{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      :   Grisette.Internal.Unified.Class.UnifiedRep
-- Copyright   :   (c) Sirui Lu 2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Internal.Unified.Class.UnifiedRep
  ( UnifiedConRep (..),
    UnifiedSymRep (..),
  )
where

import GHC.TypeLits (KnownNat, type (<=))
import Grisette.Internal.SymPrim.AlgReal (AlgReal)
import Grisette.Internal.SymPrim.BV (IntN, WordN)
import Grisette.Internal.SymPrim.FP (FP, ValidFP)
import Grisette.Internal.SymPrim.SymAlgReal (SymAlgReal)
import Grisette.Internal.SymPrim.SymBV (SymIntN, SymWordN)
import Grisette.Internal.SymPrim.SymBool (SymBool)
import Grisette.Internal.SymPrim.SymFP (SymFP)
import Grisette.Internal.SymPrim.SymInteger (SymInteger)

-- | A class that gives the concrete type of a unified primitive type.
class UnifiedConRep a where
  type ConType a

-- | A class that gives the symbolic type of a unified primitive type.
class UnifiedSymRep a where
  type SymType a

instance UnifiedConRep Bool where
  type ConType Bool = Bool

instance UnifiedSymRep Bool where
  type SymType Bool = SymBool

instance UnifiedConRep SymBool where
  type ConType SymBool = Bool

instance UnifiedSymRep SymBool where
  type SymType SymBool = SymBool

instance UnifiedConRep Integer where
  type ConType Integer = Integer

instance UnifiedSymRep Integer where
  type SymType Integer = SymInteger

instance UnifiedConRep SymInteger where
  type ConType SymInteger = Integer

instance UnifiedSymRep SymInteger where
  type SymType SymInteger = SymInteger

instance UnifiedConRep AlgReal where
  type ConType AlgReal = AlgReal

instance UnifiedSymRep AlgReal where
  type SymType AlgReal = SymAlgReal

instance UnifiedConRep SymAlgReal where
  type ConType SymAlgReal = AlgReal

instance UnifiedSymRep SymAlgReal where
  type SymType SymAlgReal = SymAlgReal

instance (KnownNat n, 1 <= n) => UnifiedConRep (IntN n) where
  type ConType (IntN n) = IntN n

instance (KnownNat n, 1 <= n) => UnifiedSymRep (IntN n) where
  type SymType (IntN n) = SymIntN n

instance (KnownNat n, 1 <= n) => UnifiedConRep (SymIntN n) where
  type ConType (SymIntN n) = IntN n

instance (KnownNat n, 1 <= n) => UnifiedSymRep (SymIntN n) where
  type SymType (SymIntN n) = SymIntN n

instance (KnownNat n, 1 <= n) => UnifiedConRep (WordN n) where
  type ConType (WordN n) = WordN n

instance (KnownNat n, 1 <= n) => UnifiedSymRep (WordN n) where
  type SymType (WordN n) = SymWordN n

instance (KnownNat n, 1 <= n) => UnifiedConRep (SymWordN n) where
  type ConType (SymWordN n) = WordN n

instance (KnownNat n, 1 <= n) => UnifiedSymRep (SymWordN n) where
  type SymType (SymWordN n) = SymWordN n

instance (ValidFP eb sb) => UnifiedConRep (FP eb sb) where
  type ConType (FP eb sb) = FP eb sb

instance (ValidFP eb sb) => UnifiedSymRep (FP eb sb) where
  type SymType (FP eb sb) = SymFP eb sb

instance (ValidFP eb sb) => UnifiedConRep (SymFP eb sb) where
  type ConType (SymFP eb sb) = FP eb sb

instance (ValidFP eb sb) => UnifiedSymRep (SymFP eb sb) where
  type SymType (SymFP eb sb) = SymFP eb sb
