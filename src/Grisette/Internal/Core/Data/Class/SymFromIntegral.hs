{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      :   Grisette.Internal.Core.Data.Class.SymFromIntegral
-- Copyright   :   (c) Sirui Lu 2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Internal.Core.Data.Class.SymFromIntegral
  ( SymFromIntegral (..),
  )
where

import GHC.TypeLits (KnownNat, type (<=))
import Grisette.Internal.Core.Data.Class.AsKey (AsKey (AsKey))
import Grisette.Internal.SymPrim.FP (ValidFP)
import Grisette.Internal.SymPrim.Prim.Internal.Term
  ( PEvalFromIntegralTerm (pevalFromIntegralTerm),
  )
import Grisette.Internal.SymPrim.SymAlgReal (SymAlgReal (SymAlgReal))
import Grisette.Internal.SymPrim.SymBV (SymIntN (SymIntN), SymWordN (SymWordN))
import Grisette.Internal.SymPrim.SymFP (SymFP (SymFP))
import Grisette.Internal.SymPrim.SymInteger (SymInteger (SymInteger))

-- | Conversion from a symbolic integral type.
class SymFromIntegral from to where
  symFromIntegral :: from -> to

instance SymFromIntegral SymInteger SymInteger where
  symFromIntegral = id
  {-# INLINE symFromIntegral #-}

instance SymFromIntegral SymInteger SymAlgReal where
  symFromIntegral (SymInteger x) = SymAlgReal $ pevalFromIntegralTerm x
  {-# INLINE symFromIntegral #-}

instance (KnownNat n, 1 <= n) => SymFromIntegral SymInteger (SymWordN n) where
  symFromIntegral (SymInteger x) = SymWordN $ pevalFromIntegralTerm x
  {-# INLINE symFromIntegral #-}

instance (KnownNat n, 1 <= n) => SymFromIntegral SymInteger (SymIntN n) where
  symFromIntegral (SymInteger x) = SymIntN $ pevalFromIntegralTerm x
  {-# INLINE symFromIntegral #-}

instance (ValidFP eb sb) => SymFromIntegral SymInteger (SymFP eb sb) where
  symFromIntegral (SymInteger x) = SymFP $ pevalFromIntegralTerm x
  {-# INLINE symFromIntegral #-}

instance (KnownNat n, 1 <= n) => SymFromIntegral (SymWordN n) SymInteger where
  symFromIntegral (SymWordN x) = SymInteger $ pevalFromIntegralTerm x
  {-# INLINE symFromIntegral #-}

instance
  (KnownNat n, KnownNat m, 1 <= n, 1 <= m) =>
  SymFromIntegral (SymWordN n) (SymWordN m)
  where
  symFromIntegral (SymWordN x) = SymWordN $ pevalFromIntegralTerm x
  {-# INLINE symFromIntegral #-}

instance (KnownNat n, 1 <= n) => SymFromIntegral (SymWordN n) SymAlgReal where
  symFromIntegral (SymWordN x) = SymAlgReal $ pevalFromIntegralTerm x
  {-# INLINE symFromIntegral #-}

instance
  (KnownNat n, KnownNat m, 1 <= n, 1 <= m) =>
  SymFromIntegral (SymWordN n) (SymIntN m)
  where
  symFromIntegral (SymWordN x) = SymIntN $ pevalFromIntegralTerm x
  {-# INLINE symFromIntegral #-}

instance
  (KnownNat n, 1 <= n, ValidFP eb sb) =>
  SymFromIntegral (SymWordN n) (SymFP eb sb)
  where
  symFromIntegral (SymWordN x) = SymFP $ pevalFromIntegralTerm x
  {-# INLINE symFromIntegral #-}

instance (KnownNat n, 1 <= n) => SymFromIntegral (SymIntN n) SymInteger where
  symFromIntegral (SymIntN x) = SymInteger $ pevalFromIntegralTerm x
  {-# INLINE symFromIntegral #-}

instance
  (KnownNat n, KnownNat m, 1 <= n, 1 <= m) =>
  SymFromIntegral (SymIntN n) (SymWordN m)
  where
  symFromIntegral (SymIntN x) = SymWordN $ pevalFromIntegralTerm x
  {-# INLINE symFromIntegral #-}

instance (KnownNat n, 1 <= n) => SymFromIntegral (SymIntN n) SymAlgReal where
  symFromIntegral (SymIntN x) = SymAlgReal $ pevalFromIntegralTerm x
  {-# INLINE symFromIntegral #-}

instance
  (KnownNat n, KnownNat m, 1 <= n, 1 <= m) =>
  SymFromIntegral (SymIntN n) (SymIntN m)
  where
  symFromIntegral (SymIntN x) = SymIntN $ pevalFromIntegralTerm x
  {-# INLINE symFromIntegral #-}

instance
  (KnownNat n, 1 <= n, ValidFP eb sb) =>
  SymFromIntegral (SymIntN n) (SymFP eb sb)
  where
  symFromIntegral (SymIntN x) = SymFP $ pevalFromIntegralTerm x
  {-# INLINE symFromIntegral #-}

instance
  {-# INCOHERENT #-}
  (SymFromIntegral a b) =>
  SymFromIntegral (AsKey a) (AsKey b)
  where
  symFromIntegral (AsKey x) = AsKey $ symFromIntegral x
  {-# INLINE symFromIntegral #-}

instance
  {-# INCOHERENT #-}
  (SymFromIntegral a b) =>
  SymFromIntegral (AsKey a) b
  where
  symFromIntegral (AsKey x) = symFromIntegral x
  {-# INLINE symFromIntegral #-}

instance
  {-# INCOHERENT #-}
  (SymFromIntegral a b) =>
  SymFromIntegral a (AsKey b)
  where
  symFromIntegral x = AsKey $ symFromIntegral x
  {-# INLINE symFromIntegral #-}
