{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

-- |
-- Module      :   Grisette.Unified.Internal.UnifiedBV
-- Copyright   :   (c) Sirui Lu 2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Unified.Internal.UnifiedBV
  ( GetWordN,
    GetIntN,
    UnifiedBVImpl,
    UnifiedBV,
    GetSomeWordN,
    GetSomeIntN,
    SafeUnifiedBV,
    SafeUnifiedSomeBV,
    AllUnifiedBV,
  )
where

import Control.Exception (ArithException)
import Control.Monad.Except (MonadError)
import Data.Bits (Bits, FiniteBits)
import Data.Kind (Constraint, Type)
import GHC.TypeNats (KnownNat, Nat, type (<=))
import Grisette.Internal.Core.Data.Class.BitCast (BitCast)
import Grisette.Internal.Core.Data.Class.BitVector (BV, SizedBV)
import Grisette.Internal.Core.Data.Class.SignConversion (SignConversion)
import Grisette.Internal.Core.Data.Class.SymRotate (SymRotate)
import Grisette.Internal.Core.Data.Class.SymShift (SymShift)
import Grisette.Internal.SymPrim.BV
  ( BitwidthMismatch,
    IntN,
    WordN,
  )
import Grisette.Internal.SymPrim.SomeBV
  ( SomeBV,
    SomeIntN,
    SomeSymIntN,
    SomeSymWordN,
    SomeWordN,
  )
import Grisette.Internal.SymPrim.SymBV (SymIntN, SymWordN)
import Grisette.Unified.Internal.BaseConstraint
  ( BasicGrisetteType,
    ConSymConversion,
  )
import Grisette.Unified.Internal.Class.UnifiedFiniteBits
  ( UnifiedFiniteBits,
  )
import Grisette.Unified.Internal.Class.UnifiedFromIntegral (UnifiedFromIntegral)
import Grisette.Unified.Internal.Class.UnifiedITEOp (UnifiedITEOp)
import Grisette.Unified.Internal.Class.UnifiedSafeDiv (UnifiedSafeDiv)
import Grisette.Unified.Internal.Class.UnifiedSafeLinearArith
  ( UnifiedSafeLinearArith,
  )
import Grisette.Unified.Internal.Class.UnifiedSafeSymRotate
  ( UnifiedSafeSymRotate,
  )
import Grisette.Unified.Internal.Class.UnifiedSafeSymShift (UnifiedSafeSymShift)
import Grisette.Unified.Internal.Class.UnifiedSimpleMergeable
  ( UnifiedBranching,
    UnifiedSimpleMergeable,
  )
import Grisette.Unified.Internal.Class.UnifiedSymEq (UnifiedSymEq)
import Grisette.Unified.Internal.Class.UnifiedSymOrd (UnifiedSymOrd)
import Grisette.Unified.Internal.EvalModeTag (EvalModeTag (Con, Sym))
import Grisette.Unified.Internal.UnifiedAlgReal (GetAlgReal)
import Grisette.Unified.Internal.UnifiedInteger (GetInteger)

type BVConstraint mode word int =
  ( BasicGrisetteType word,
    BasicGrisetteType int,
    Num word,
    Num int,
    Bits word,
    Bits int,
    FiniteBits word,
    FiniteBits int,
    SymShift word,
    SymShift int,
    SymRotate word,
    SymRotate int,
    UnifiedSymEq mode word,
    UnifiedSymEq mode int,
    UnifiedSymOrd mode word,
    UnifiedSymOrd mode int,
    UnifiedITEOp mode word,
    UnifiedITEOp mode int,
    SignConversion word int,
    UnifiedFiniteBits mode word,
    UnifiedFiniteBits mode int
  ) ::
    Constraint

type SomeBVPair mode word int =
  ( BVConstraint mode word int,
    BV word,
    BV int,
    ConSymConversion SomeWordN SomeSymWordN word,
    ConSymConversion SomeIntN SomeSymIntN int
  ) ::
    Constraint

-- | Implementation for 'UnifiedBV'.
class
  ( BVConstraint mode (GetWordN mode n) (GetIntN mode n),
    ConSymConversion (WordN n) (SymWordN n) (GetWordN mode n),
    UnifiedSimpleMergeable mode (GetWordN mode n),
    ConSymConversion (IntN n) (SymIntN n) (GetIntN mode n),
    UnifiedSimpleMergeable mode (GetIntN mode n),
    wordn ~ GetWordN mode,
    intn ~ GetIntN mode,
    word ~ wordn n,
    int ~ intn n,
    BitCast word int,
    BitCast int word,
    UnifiedFromIntegral mode (GetInteger mode) word,
    UnifiedFromIntegral mode (GetInteger mode) int,
    UnifiedFromIntegral mode word (GetInteger mode),
    UnifiedFromIntegral mode word (GetAlgReal mode),
    UnifiedFromIntegral mode int (GetInteger mode),
    UnifiedFromIntegral mode int (GetAlgReal mode)
  ) =>
  UnifiedBVImpl (mode :: EvalModeTag) wordn intn n word int
    | wordn -> intn,
      intn -> wordn,
      wordn n -> word,
      word -> wordn n,
      intn n -> int,
      int -> intn n
  where
  -- | Get a unified unsigned size-tagged bit vector type. Resolves to 'WordN'
  -- in 'Con' mode, and 'SymWordN' in 'Sym' mode.
  type GetWordN mode = (w :: Nat -> Type) | w -> mode

  -- | Get a unified signed size-tagged bit vector type. Resolves to 'IntN'
  -- in 'Con' mode, and 'SymIntN' in 'Sym' mode.
  type GetIntN mode = (i :: Nat -> Type) | i -> mode

instance
  (KnownNat n, 1 <= n) =>
  UnifiedBVImpl 'Con WordN IntN n (WordN n) (IntN n)
  where
  type GetWordN 'Con = WordN
  type GetIntN 'Con = IntN

instance
  (KnownNat n, 1 <= n) =>
  UnifiedBVImpl 'Sym SymWordN SymIntN n (SymWordN n) (SymIntN n)
  where
  type GetWordN 'Sym = SymWordN
  type GetIntN 'Sym = SymIntN

-- | Get a unified unsigned dynamic bit width bit vector type. Resolves to
-- 'SomeWordN' in 'Con' mode, and 'SomeSymWordN' in 'Sym' mode.
type family GetSomeWordN mode = sw | sw -> mode where
  GetSomeWordN mode = SomeBV (GetWordN mode)

-- | Get a unified signed dynamic bit width bit vector type. Resolves to
-- 'SomeIntN' in 'Con' mode, and 'SomeSymIntN' in 'Sym' mode.
type family GetSomeIntN mode = sw | sw -> mode where
  GetSomeIntN mode = SomeBV (GetIntN mode)

-- | This class is needed as constraint in user code prior to GHC 9.2.1.
-- See the notes in 'Grisette.Unified.Internal.EvalMode.EvalMode'.
class
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
  ( UnifiedBVImpl
      mode
      (GetWordN mode)
      (GetIntN mode)
      n
      (GetWordN mode n)
      (GetIntN mode n)
  ) =>
  UnifiedBV mode n

class
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
  SafeUnifiedBVImpl (mode :: EvalModeTag) wordn intn n word int m

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

-- | This class is needed as constraint in user code prior to GHC 9.2.1.
-- See the notes in 'Grisette.Unified.Internal.EvalMode.EvalMode'.
class
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

class
  ( SomeBVPair mode word int,
    UnifiedSafeDiv mode (Either BitwidthMismatch ArithException) word m,
    UnifiedSafeLinearArith mode (Either BitwidthMismatch ArithException) word m,
    UnifiedSafeSymRotate mode (Either BitwidthMismatch ArithException) word m,
    UnifiedSafeSymShift mode (Either BitwidthMismatch ArithException) word m,
    UnifiedSafeDiv mode (Either BitwidthMismatch ArithException) int m,
    UnifiedSafeLinearArith mode (Either BitwidthMismatch ArithException) int m,
    UnifiedSafeSymRotate mode (Either BitwidthMismatch ArithException) int m,
    UnifiedSafeSymShift mode (Either BitwidthMismatch ArithException) int m
  ) =>
  SafeUnifiedSomeBVImpl (mode :: EvalModeTag) word int m

instance
  ( SomeBVPair mode word int,
    UnifiedSafeDiv mode (Either BitwidthMismatch ArithException) word m,
    UnifiedSafeLinearArith mode (Either BitwidthMismatch ArithException) word m,
    UnifiedSafeSymRotate mode (Either BitwidthMismatch ArithException) word m,
    UnifiedSafeSymShift mode (Either BitwidthMismatch ArithException) word m,
    UnifiedSafeDiv mode (Either BitwidthMismatch ArithException) int m,
    UnifiedSafeLinearArith mode (Either BitwidthMismatch ArithException) int m,
    UnifiedSafeSymRotate mode (Either BitwidthMismatch ArithException) int m,
    UnifiedSafeSymShift mode (Either BitwidthMismatch ArithException) int m
  ) =>
  SafeUnifiedSomeBVImpl (mode :: EvalModeTag) word int m

-- | This class is needed as constraint in user code prior to GHC 9.2.1.
-- See the notes in 'Grisette.Unified.Internal.EvalMode.EvalMode'.
class
  ( SafeUnifiedSomeBVImpl
      mode
      (SomeBV (GetWordN mode))
      (SomeBV (GetIntN mode))
      m
  ) =>
  SafeUnifiedSomeBV mode m

instance
  ( SafeUnifiedSomeBVImpl
      mode
      (SomeBV (GetWordN mode))
      (SomeBV (GetIntN mode))
      m
  ) =>
  SafeUnifiedSomeBV mode m

-- | Evaluation mode with unified bit vector types.
class
  ( forall n m.
    ( UnifiedBranching mode m,
      MonadError ArithException m,
      KnownNat n,
      1 <= n
    ) =>
    SafeUnifiedBV mode n m,
    forall m.
    ( UnifiedBranching mode m,
      MonadError (Either BitwidthMismatch ArithException) m
    ) =>
    SafeUnifiedSomeBV mode m,
    forall n. (KnownNat n, 1 <= n) => UnifiedBV mode n,
    SomeBVPair mode (GetSomeWordN mode) (GetSomeIntN mode),
    SizedBV (GetWordN mode),
    SizedBV (GetIntN mode)
  ) =>
  AllUnifiedBV mode

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
      MonadError (Either BitwidthMismatch ArithException) m
    ) =>
    SafeUnifiedSomeBV mode m,
    forall n. (KnownNat n, 1 <= n) => UnifiedBV mode n,
    SomeBVPair mode (GetSomeWordN mode) (GetSomeIntN mode),
    SizedBV (GetWordN mode),
    SizedBV (GetIntN mode)
  ) =>
  AllUnifiedBV mode
