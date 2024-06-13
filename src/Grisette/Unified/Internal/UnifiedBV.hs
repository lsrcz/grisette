{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Grisette.Unified.Internal.UnifiedBV
  ( UnifiedBVImpl (..),
    UnifiedBV,
    AllUnifiedBV,
    GetSomeWordN,
    GetSomeIntN,
    SafeUnifiedBVImpl,
    SafeUnifiedBV,
    SafeAllUnifiedBV,
  )
where

import Control.Exception (ArithException)
import Control.Monad.Except (ExceptT)
import Data.Bits (Bits, FiniteBits)
import Data.Kind (Constraint, Type)
import GHC.TypeNats (KnownNat, Nat, type (<=))
import Grisette.Internal.Core.Control.Monad.Union (MonadUnion)
import Grisette.Internal.Core.Data.Class.BitVector (BV, SizedBV)
import Grisette.Internal.Core.Data.Class.SafeDivision (SafeDivision)
import Grisette.Internal.Core.Data.Class.SafeLinearArith (SafeLinearArith)
import Grisette.Internal.Core.Data.Class.SafeSymRotate (SafeSymRotate)
import Grisette.Internal.Core.Data.Class.SafeSymShift (SafeSymShift)
import Grisette.Internal.Core.Data.Class.SignConversion (SignConversion)
import Grisette.Internal.Core.Data.Class.SymRotate (SymRotate)
import Grisette.Internal.Core.Data.Class.SymShift (SymShift)
import Grisette.Internal.Core.Data.Class.TryMerge (MonadTryMerge)
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
import Grisette.Unified.Internal.Class.UnifiedITEOp (UnifiedITEOp)
import Grisette.Unified.Internal.Class.UnifiedSEq (UnifiedSEq)
import Grisette.Unified.Internal.Class.UnifiedSOrd (UnifiedSOrd)
import Grisette.Unified.Internal.Class.UnifiedSimpleMergeable
  ( UnifiedSimpleMergeable,
  )
import Grisette.Unified.Internal.EvaluationMode (EvaluationMode (Con, Sym))

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
    UnifiedSEq mode word,
    UnifiedSEq mode int,
    UnifiedSOrd mode word,
    UnifiedSOrd mode int,
    UnifiedITEOp mode word,
    UnifiedITEOp mode int,
    SignConversion word int
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

class
  ( BVConstraint mode (GetWordN mode n) (GetIntN mode n),
    ConSymConversion (WordN n) (SymWordN n) (GetWordN mode n),
    UnifiedSimpleMergeable mode (GetWordN mode n),
    ConSymConversion (IntN n) (SymIntN n) (GetIntN mode n),
    UnifiedSimpleMergeable mode (GetIntN mode n),
    SizedBV wordn,
    SizedBV intn,
    wordn ~ GetWordN mode,
    intn ~ GetIntN mode,
    word ~ wordn n,
    int ~ intn n
  ) =>
  UnifiedBVImpl (mode :: EvaluationMode) wordn intn n word int
    | wordn n -> word intn int,
      word -> wordn intn n int,
      intn n -> int wordn word,
      int -> intn wordn n word
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
-- See the notes in 'Grisette.Unified.Internal.IsMode.IsMode'.
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
  ( forall n. (KnownNat n, 1 <= n) => UnifiedBV mode n,
    SomeBVPair mode (GetSomeWordN mode) (GetSomeIntN mode)
  ) =>
  AllUnifiedBV mode

instance
  ( forall n. (KnownNat n, 1 <= n) => UnifiedBV mode n,
    SomeBVPair mode (GetSomeWordN mode) (GetSomeIntN mode)
  ) =>
  AllUnifiedBV mode

type SafeSomeBVPair mode word int m =
  ( SomeBVPair mode word int,
    SafeDivision
      (Either BitwidthMismatch ArithException)
      word
      (ExceptT (Either BitwidthMismatch ArithException) m),
    SafeLinearArith
      (Either BitwidthMismatch ArithException)
      word
      (ExceptT (Either BitwidthMismatch ArithException) m),
    SafeSymShift
      (Either BitwidthMismatch ArithException)
      word
      (ExceptT (Either BitwidthMismatch ArithException) m),
    SafeSymRotate
      (Either BitwidthMismatch ArithException)
      word
      (ExceptT (Either BitwidthMismatch ArithException) m),
    SafeDivision
      (Either BitwidthMismatch ArithException)
      int
      (ExceptT (Either BitwidthMismatch ArithException) m),
    SafeLinearArith
      (Either BitwidthMismatch ArithException)
      int
      (ExceptT (Either BitwidthMismatch ArithException) m),
    SafeSymShift
      (Either BitwidthMismatch ArithException)
      int
      (ExceptT (Either BitwidthMismatch ArithException) m),
    SafeSymRotate
      (Either BitwidthMismatch ArithException)
      int
      (ExceptT (Either BitwidthMismatch ArithException) m)
  ) ::
    Constraint

class
  ( UnifiedBVImpl mode wordn intn n word int,
    SafeDivision ArithException (wordn n) (ExceptT ArithException m),
    SafeLinearArith ArithException (wordn n) (ExceptT ArithException m),
    SafeSymShift ArithException (wordn n) (ExceptT ArithException m),
    SafeSymRotate ArithException (wordn n) (ExceptT ArithException m),
    SafeDivision ArithException (intn n) (ExceptT ArithException m),
    SafeLinearArith ArithException (intn n) (ExceptT ArithException m),
    SafeSymShift ArithException (intn n) (ExceptT ArithException m),
    SafeSymRotate ArithException (intn n) (ExceptT ArithException m),
    SafeSomeBVPair mode (SomeBV wordn) (SomeBV intn) m
  ) =>
  SafeUnifiedBVImpl
    (mode :: EvaluationMode)
    wordn
    intn
    n
    word
    int
    (m :: Type -> Type)

instance
  (MonadTryMerge m, KnownNat n, 1 <= n) =>
  SafeUnifiedBVImpl 'Con WordN IntN n (WordN n) (IntN n) m

instance
  (MonadUnion m, KnownNat n, 1 <= n) =>
  SafeUnifiedBVImpl 'Sym SymWordN SymIntN n (SymWordN n) (SymIntN n) m

-- | This class is needed as constraint in user code prior to GHC 9.2.1.
-- See the notes in 'Grisette.Unified.Internal.IsMode.IsMode'.
class
  ( UnifiedBV mode n,
    SafeUnifiedBVImpl
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
  ( UnifiedBV mode n,
    SafeUnifiedBVImpl
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
  ( forall n. (KnownNat n, 1 <= n) => SafeUnifiedBV mode n m,
    SomeBVPair mode (GetSomeWordN mode) (GetSomeIntN mode)
  ) =>
  SafeAllUnifiedBV mode m

instance
  ( forall n. (KnownNat n, 1 <= n) => SafeUnifiedBV mode n m,
    SomeBVPair mode (GetSomeWordN mode) (GetSomeIntN mode)
  ) =>
  SafeAllUnifiedBV mode m
