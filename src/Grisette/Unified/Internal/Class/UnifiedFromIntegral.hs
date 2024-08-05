{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      :   Grisette.Unified.Internal.Class.UnifiedFromIntegral
-- Copyright   :   (c) Sirui Lu 2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Unified.Internal.Class.UnifiedFromIntegral
  ( UnifiedFromIntegral (..),
    symFromIntegral,
  )
where

import Data.Type.Bool (If)
import Data.Typeable (Typeable)
import GHC.TypeNats (KnownNat, type (<=))
import Grisette.Internal.Core.Data.Class.SymFromIntegral (SymFromIntegral)
import qualified Grisette.Internal.Core.Data.Class.SymFromIntegral as SymFromIntegral
import Grisette.Internal.SymPrim.AlgReal (AlgReal)
import Grisette.Internal.SymPrim.BV (IntN, WordN)
import Grisette.Internal.SymPrim.FP (FP, ValidFP)
import Grisette.Internal.SymPrim.SymAlgReal (SymAlgReal)
import Grisette.Internal.SymPrim.SymBV (SymIntN, SymWordN)
import Grisette.Internal.SymPrim.SymFP (SymFP)
import Grisette.Internal.SymPrim.SymInteger (SymInteger)
import Grisette.Unified.Internal.EvalModeTag (EvalModeTag (Con, Sym), IsConMode)
import Grisette.Unified.Internal.Util (withMode)

-- | Unified `Grisette.Internal.Core.Data.Class.SymFromIntegral.symFromIntegral`
-- operation.
--
-- This function isn't able to infer the mode, so you need to provide the mode
-- explicitly. For example:
--
-- > symFromIntegral @mode a
symFromIntegral ::
  forall mode a b. (Typeable mode, UnifiedFromIntegral mode a b) => a -> b
symFromIntegral a =
  withMode @mode
    (withBaseFromIntegral @mode @a @b $ fromIntegral a)
    (withBaseFromIntegral @mode @a @b $ SymFromIntegral.symFromIntegral a)

-- | A class that provides unified conversion from integral types.
--
-- We use this type class to help resolve the constraints for `SymFromIntegral`.
class UnifiedFromIntegral (mode :: EvalModeTag) a b where
  withBaseFromIntegral ::
    ((If (IsConMode mode) (Integral a, Num b) (SymFromIntegral a b)) => r) -> r

instance
  {-# INCOHERENT #-}
  ( Typeable mode,
    (If (IsConMode mode) (Integral a, Num b) (SymFromIntegral a b))
  ) =>
  UnifiedFromIntegral mode a b
  where
  withBaseFromIntegral r = r

instance UnifiedFromIntegral 'Con Integer AlgReal where
  withBaseFromIntegral r = r

instance UnifiedFromIntegral 'Con Integer Integer where
  withBaseFromIntegral r = r

instance (KnownNat n, 1 <= n) => UnifiedFromIntegral 'Con Integer (IntN n) where
  withBaseFromIntegral r = r

instance
  (KnownNat n, 1 <= n) =>
  UnifiedFromIntegral 'Con Integer (WordN n)
  where
  withBaseFromIntegral r = r

instance (ValidFP eb sb) => UnifiedFromIntegral 'Con Integer (FP eb sb) where
  withBaseFromIntegral r = r

instance
  (KnownNat n', 1 <= n') =>
  UnifiedFromIntegral 'Con (IntN n') AlgReal
  where
  withBaseFromIntegral r = r

instance
  (KnownNat n', 1 <= n') =>
  UnifiedFromIntegral 'Con (IntN n') Integer
  where
  withBaseFromIntegral r = r

instance
  (KnownNat n', 1 <= n', KnownNat n, 1 <= n) =>
  UnifiedFromIntegral 'Con (IntN n') (IntN n)
  where
  withBaseFromIntegral r = r

instance
  (KnownNat n', 1 <= n', KnownNat n, 1 <= n) =>
  UnifiedFromIntegral 'Con (IntN n') (WordN n)
  where
  withBaseFromIntegral r = r

instance
  (KnownNat n', 1 <= n', ValidFP eb sb) =>
  UnifiedFromIntegral 'Con (IntN n') (FP eb sb)
  where
  withBaseFromIntegral r = r

instance
  (KnownNat n', 1 <= n') =>
  UnifiedFromIntegral 'Con (WordN n') AlgReal
  where
  withBaseFromIntegral r = r

instance
  (KnownNat n', 1 <= n') =>
  UnifiedFromIntegral 'Con (WordN n') Integer
  where
  withBaseFromIntegral r = r

instance
  (KnownNat n', 1 <= n', KnownNat n, 1 <= n) =>
  UnifiedFromIntegral 'Con (WordN n') (IntN n)
  where
  withBaseFromIntegral r = r

instance
  (KnownNat n', 1 <= n', KnownNat n, 1 <= n) =>
  UnifiedFromIntegral 'Con (WordN n') (WordN n)
  where
  withBaseFromIntegral r = r

instance
  (KnownNat n', 1 <= n', ValidFP eb sb) =>
  UnifiedFromIntegral 'Con (WordN n') (FP eb sb)
  where
  withBaseFromIntegral r = r

instance UnifiedFromIntegral 'Sym SymInteger SymAlgReal where
  withBaseFromIntegral r = r

instance UnifiedFromIntegral 'Sym SymInteger SymInteger where
  withBaseFromIntegral r = r

instance (KnownNat n, 1 <= n) => UnifiedFromIntegral 'Sym SymInteger (SymIntN n) where
  withBaseFromIntegral r = r

instance
  (KnownNat n, 1 <= n) =>
  UnifiedFromIntegral 'Sym SymInteger (SymWordN n)
  where
  withBaseFromIntegral r = r

instance (ValidFP eb sb) => UnifiedFromIntegral 'Sym SymInteger (SymFP eb sb) where
  withBaseFromIntegral r = r

instance
  (KnownNat n', 1 <= n') =>
  UnifiedFromIntegral 'Sym (SymIntN n') SymAlgReal
  where
  withBaseFromIntegral r = r

instance
  (KnownNat n', 1 <= n') =>
  UnifiedFromIntegral 'Sym (SymIntN n') SymInteger
  where
  withBaseFromIntegral r = r

instance
  (KnownNat n', 1 <= n', KnownNat n, 1 <= n) =>
  UnifiedFromIntegral 'Sym (SymIntN n') (SymIntN n)
  where
  withBaseFromIntegral r = r

instance
  (KnownNat n', 1 <= n', KnownNat n, 1 <= n) =>
  UnifiedFromIntegral 'Sym (SymIntN n') (SymWordN n)
  where
  withBaseFromIntegral r = r

instance
  (KnownNat n', 1 <= n', ValidFP eb sb) =>
  UnifiedFromIntegral 'Sym (SymIntN n') (SymFP eb sb)
  where
  withBaseFromIntegral r = r

instance
  (KnownNat n', 1 <= n') =>
  UnifiedFromIntegral 'Sym (SymWordN n') SymAlgReal
  where
  withBaseFromIntegral r = r

instance
  (KnownNat n', 1 <= n') =>
  UnifiedFromIntegral 'Sym (SymWordN n') SymInteger
  where
  withBaseFromIntegral r = r

instance
  (KnownNat n', 1 <= n', KnownNat n, 1 <= n) =>
  UnifiedFromIntegral 'Sym (SymWordN n') (SymIntN n)
  where
  withBaseFromIntegral r = r

instance
  (KnownNat n', 1 <= n', KnownNat n, 1 <= n) =>
  UnifiedFromIntegral 'Sym (SymWordN n') (SymWordN n)
  where
  withBaseFromIntegral r = r

instance
  (KnownNat n', 1 <= n', ValidFP eb sb) =>
  UnifiedFromIntegral 'Sym (SymWordN n') (SymFP eb sb)
  where
  withBaseFromIntegral r = r
