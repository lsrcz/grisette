{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

-- |
-- Module      :   Grisette.Internal.Internal.Impl.Unified.UnifiedBV
-- Copyright   :   (c) Sirui Lu 2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Internal.Internal.Impl.Unified.UnifiedBV () where

import Control.Exception (ArithException)
import Control.Monad.Except (MonadError)
import GHC.TypeLits (KnownNat, type (<=))
import Grisette.Internal.Core.Data.Class.BitVector (SizedBV)
import Grisette.Internal.Internal.Decl.Unified.UnifiedBV
  ( AllUnifiedBV,
    GetSomeIntN,
    GetSomeWordN,
    SafeUnifiedBV,
    SafeUnifiedBVImpl,
    SafeUnifiedSomeBV,
    SafeUnifiedSomeBVImpl,
    SomeBVPair,
    UnifiedBV,
    UnifiedBVImpl (GetIntN, GetWordN),
  )
import Grisette.Internal.SymPrim.BV (IntN, WordN)
import Grisette.Internal.SymPrim.SomeBV
  ( SomeBV,
    SomeBVException,
  )
import Grisette.Internal.SymPrim.SymBV (SymIntN, SymWordN)
import Grisette.Internal.Unified.Class.UnifiedSafeDiv (UnifiedSafeDiv)
import Grisette.Internal.Unified.Class.UnifiedSafeLinearArith
  ( UnifiedSafeLinearArith,
  )
import Grisette.Internal.Unified.Class.UnifiedSafeSymRotate
  ( UnifiedSafeSymRotate,
  )
import Grisette.Internal.Unified.Class.UnifiedSafeSymShift (UnifiedSafeSymShift)
import Grisette.Internal.Unified.Class.UnifiedSimpleMergeable
  ( UnifiedBranching,
  )
import Grisette.Internal.Unified.EvalModeTag (EvalModeTag (C, S))

instance
  (KnownNat n, 1 <= n) =>
  UnifiedBVImpl 'C WordN IntN n (WordN n) (IntN n)
  where
  type GetWordN 'C = WordN
  type GetIntN 'C = IntN

instance
  (KnownNat n, 1 <= n) =>
  UnifiedBVImpl 'S SymWordN SymIntN n (SymWordN n) (SymIntN n)
  where
  type GetWordN 'S = SymWordN
  type GetIntN 'S = SymIntN

instance
  ( UnifiedBVImpl
      mode
      (GetWordN mode)
      (GetIntN mode)
      n
      (GetWordN mode n)
      (GetIntN mode n)
  ) =>
  UnifiedBV mode n

instance
  ( UnifiedSafeDiv mode ArithException word m,
    UnifiedSafeLinearArith mode ArithException word m,
    UnifiedSafeSymShift mode ArithException word m,
    UnifiedSafeSymRotate mode ArithException word m,
    UnifiedSafeDiv mode ArithException int m,
    UnifiedSafeLinearArith mode ArithException int m,
    UnifiedSafeSymShift mode ArithException int m,
    UnifiedSafeSymRotate mode ArithException int m,
    UnifiedBVImpl mode wordn intn n word int
  ) =>
  SafeUnifiedBVImpl mode wordn intn n word int m

instance
  ( SafeUnifiedBVImpl
      mode
      (GetWordN mode)
      (GetIntN mode)
      n
      (GetWordN mode n)
      (GetIntN mode n)
      m
  ) =>
  SafeUnifiedBV mode n m

instance
  ( SomeBVPair mode word int,
    UnifiedSafeDiv mode (Either SomeBVException ArithException) word m,
    UnifiedSafeLinearArith mode (Either SomeBVException ArithException) word m,
    UnifiedSafeSymRotate mode (Either SomeBVException ArithException) word m,
    UnifiedSafeSymShift mode (Either SomeBVException ArithException) word m,
    UnifiedSafeDiv mode (Either SomeBVException ArithException) int m,
    UnifiedSafeLinearArith mode (Either SomeBVException ArithException) int m,
    UnifiedSafeSymRotate mode (Either SomeBVException ArithException) int m,
    UnifiedSafeSymShift mode (Either SomeBVException ArithException) int m
  ) =>
  SafeUnifiedSomeBVImpl (mode :: EvalModeTag) word int m

instance
  ( SafeUnifiedSomeBVImpl
      mode
      (SomeBV (GetWordN mode))
      (SomeBV (GetIntN mode))
      m
  ) =>
  SafeUnifiedSomeBV mode m

instance
  ( forall n m.
    ( UnifiedBranching mode m,
      MonadError ArithException m,
      KnownNat n,
      1 <= n
    ) =>
    SafeUnifiedBV mode n m,
    forall m.
    ( UnifiedBranching mode m,
      MonadError (Either SomeBVException ArithException) m
    ) =>
    SafeUnifiedSomeBV mode m,
    forall n. (KnownNat n, 1 <= n) => UnifiedBV mode n,
    SomeBVPair mode (GetSomeWordN mode) (GetSomeIntN mode),
    SizedBV (GetWordN mode),
    SizedBV (GetIntN mode)
  ) =>
  AllUnifiedBV mode
