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
import Grisette.Internal.Core.Data.Class.SafeDiv (DivOr)
import Grisette.Internal.Core.Data.Class.SignConversion (SignConversion)
import Grisette.Internal.Core.Data.Class.SymRotate (SymRotate)
import Grisette.Internal.Core.Data.Class.SymShift (SymShift)
import Grisette.Internal.SymPrim.BV
  ( IntN,
    WordN,
  )
import Grisette.Internal.SymPrim.SomeBV
  ( SomeBV,
    SomeBVException,
    SomeIntN,
    SomeSymIntN,
    SomeSymWordN,
    SomeWordN,
  )
import Grisette.Internal.SymPrim.SymBV (SymIntN, SymWordN)
import Grisette.Unified.Internal.BaseConstraint (ConSymConversion)
import Grisette.Unified.Internal.Class.UnifiedFiniteBits
  ( UnifiedFiniteBits,
  )
import Grisette.Unified.Internal.Class.UnifiedFromIntegral (UnifiedFromIntegral)
import Grisette.Unified.Internal.Class.UnifiedRep
  ( UnifiedConRep (ConType),
    UnifiedSymRep (SymType),
  )
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
  )
import Grisette.Unified.Internal.EvalModeTag (EvalModeTag (C, S))
import Grisette.Unified.Internal.UnifiedAlgReal (GetAlgReal)
import Grisette.Unified.Internal.UnifiedInteger (GetInteger)
import Grisette.Unified.Internal.UnifiedPrim (UnifiedBasicPrim, UnifiedPrim)

type BVConstraint mode word int =
  ( Num word,
    Num int,
    Bits word,
    Bits int,
    FiniteBits word,
    FiniteBits int,
    SymShift word,
    SymShift int,
    SymRotate word,
    SymRotate int,
    SignConversion word int,
    UnifiedFiniteBits mode word,
    UnifiedFiniteBits mode int
  ) ::
    Constraint

type SomeBVPair mode word int =
  ( UnifiedPrim mode word,
    UnifiedPrim mode int,
    BVConstraint mode word int,
    BV word,
    BV int,
    DivOr word,
    DivOr int,
    ConSymConversion SomeWordN SomeSymWordN word,
    ConSymConversion SomeIntN SomeSymIntN int
  ) ::
    Constraint

-- | Implementation for 'UnifiedBV'.
class
  ( UnifiedConRep word,
    UnifiedSymRep int,
    ConType word ~ WordN n,
    SymType word ~ SymWordN n,
    ConType int ~ IntN n,
    SymType int ~ SymIntN n,
    UnifiedBasicPrim mode word,
    UnifiedBasicPrim mode int,
    BVConstraint mode word int,
    wordn ~ GetWordN mode,
    intn ~ GetIntN mode,
    word ~ wordn n,
    int ~ intn n,
    BitCast word int,
    BitCast int word,
    DivOr word,
    DivOr int,
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
      MonadError (Either SomeBVException ArithException) m
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
      MonadError (Either SomeBVException ArithException) m
    ) =>
    SafeUnifiedSomeBV mode m,
    forall n. (KnownNat n, 1 <= n) => UnifiedBV mode n,
    SomeBVPair mode (GetSomeWordN mode) (GetSomeIntN mode),
    SizedBV (GetWordN mode),
    SizedBV (GetIntN mode)
  ) =>
  AllUnifiedBV mode
