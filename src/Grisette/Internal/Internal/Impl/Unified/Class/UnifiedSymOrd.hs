{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Eta reduce" #-}

-- |
-- Module      :   Grisette.Internal.Internal.Impl.Unified.Class.UnifiedSymOrd
-- Copyright   :   (c) Sirui Lu 2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Internal.Internal.Impl.Unified.Class.UnifiedSymOrd
  ( (.<=),
    (.<),
    (.>=),
    (.>),
    symMax,
    symMin,
    mrgMax,
    mrgMin,
  )
where

import Control.Monad.Except (ExceptT)
import Control.Monad.Identity (Identity, IdentityT)
import Control.Monad.Trans.Maybe (MaybeT)
import qualified Control.Monad.Writer.Lazy as WriterLazy
import qualified Control.Monad.Writer.Strict as WriterStrict
import qualified Data.ByteString as B
import Data.Functor.Classes (Ord1, Ord2)
import Data.Functor.Sum (Sum)
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Ratio (Ratio)
import qualified Data.Text as T
import Data.Type.Bool (If)
import Data.Word (Word16, Word32, Word64, Word8)
import Grisette.Internal.Core.Control.Exception
  ( AssertionError,
    VerificationConditions,
  )
import Grisette.Internal.Core.Control.Monad.Union (Union)
import Grisette.Internal.Core.Data.Class.Mergeable (Mergeable)
import Grisette.Internal.Core.Data.Class.TryMerge (tryMerge)
import Grisette.Internal.Internal.Decl.Core.Data.Class.SymOrd
  ( SymOrd,
    SymOrd1,
    SymOrd2,
  )
import qualified Grisette.Internal.Internal.Decl.Core.Data.Class.SymOrd as SymOrd
import Grisette.Internal.Internal.Decl.Unified.Class.UnifiedITEOp
  ( UnifiedITEOp (withBaseITEOp),
  )
import Grisette.Internal.Internal.Decl.Unified.Class.UnifiedSimpleMergeable
  ( UnifiedBranching (withBaseBranching),
  )
import Grisette.Internal.Internal.Decl.Unified.Class.UnifiedSymOrd
  ( UnifiedSymOrd (withBaseSymOrd),
    UnifiedSymOrd1 (withBaseSymOrd1),
    UnifiedSymOrd2 (withBaseSymOrd2),
  )
import Grisette.Internal.SymPrim.BV (IntN, WordN)
import Grisette.Internal.SymPrim.FP (FP, FPRoundingMode)
import Grisette.Internal.TH.Derivation.Common
  ( DeriveConfig (bitSizePositions, fpBitSizePositions),
  )
import Grisette.Internal.TH.Derivation.Derive (derive, deriveWith)
import Grisette.Internal.Unified.EvalModeTag
  ( EvalModeTag (S),
    IsConMode,
  )
import Grisette.Internal.Unified.UnifiedBool (GetBool)
import Grisette.Internal.Unified.Util (DecideEvalMode, withMode)

-- | Unified `(Grisette.Internal.Core.Data.Class.SymOrd..<=)`.
--
-- Note that you may sometimes need to write type annotations for the result
-- when the mode isn't clear:
--
-- > a .<= b :: GetBool mode
--
-- One example when it isn't clear is when this is used in unified
-- `Grisette.Internal.Unified.Class.UnifiedBranching.mrgIf`.
(.<=) ::
  forall mode a. (DecideEvalMode mode, UnifiedSymOrd mode a) => a -> a -> GetBool mode
(.<=) a b =
  withMode @mode
    (withBaseSymOrd @mode @a $ a <= b)
    (withBaseSymOrd @mode @a $ a SymOrd..<= b)
{-# INLINE (.<=) #-}

-- | Unified `(Grisette.Internal.Core.Data.Class.SymOrd..<)`.
(.<) ::
  forall mode a. (DecideEvalMode mode, UnifiedSymOrd mode a) => a -> a -> GetBool mode
(.<) a b =
  withMode @mode
    (withBaseSymOrd @mode @a $ a < b)
    (withBaseSymOrd @mode @a $ a SymOrd..< b)
{-# INLINE (.<) #-}

-- | Unified `(Grisette.Internal.Core.Data.Class.SymOrd..>=)`.
(.>=) ::
  forall mode a. (DecideEvalMode mode, UnifiedSymOrd mode a) => a -> a -> GetBool mode
(.>=) a b =
  withMode @mode
    (withBaseSymOrd @mode @a $ a >= b)
    (withBaseSymOrd @mode @a $ a SymOrd..>= b)
{-# INLINE (.>=) #-}

-- | Unified `(Grisette.Internal.Core.Data.Class.SymOrd..>)`.
(.>) ::
  forall mode a. (DecideEvalMode mode, UnifiedSymOrd mode a) => a -> a -> GetBool mode
(.>) a b =
  withMode @mode
    (withBaseSymOrd @mode @a $ a > b)
    (withBaseSymOrd @mode @a $ a SymOrd..> b)
{-# INLINE (.>) #-}

-- | Unified `Grisette.Internal.Core.Data.Class.SymOrd.symMax`.
symMax ::
  forall mode a.
  (UnifiedSymOrd mode a, UnifiedITEOp mode a, DecideEvalMode mode) =>
  a ->
  a ->
  a
symMax x y =
  withMode @mode
    (withBaseSymOrd @mode @a $ max x y)
    ( withBaseSymOrd @mode @a $
        withBaseITEOp @mode @a
          SymOrd.symMax
          x
          y
    )
{-# INLINE symMax #-}

-- | Unified `Grisette.Internal.Core.Data.Class.SymOrd.symMin`.
symMin ::
  forall mode a.
  (UnifiedSymOrd mode a, UnifiedITEOp mode a, DecideEvalMode mode) =>
  a ->
  a ->
  a
symMin x y =
  withMode @mode
    (withBaseSymOrd @mode @a $ min x y)
    ( withBaseSymOrd @mode @a $
        withBaseITEOp @mode @a
          SymOrd.symMin
          x
          y
    )
{-# INLINE symMin #-}

-- | Unified `Grisette.Internal.Core.Data.Class.SymOrd.mrgMax`.
mrgMax ::
  forall mode a m.
  ( UnifiedSymOrd mode a,
    UnifiedBranching mode m,
    DecideEvalMode mode,
    Applicative m,
    Mergeable a
  ) =>
  a ->
  a ->
  m a
mrgMax x y =
  withMode @mode
    ( withBaseSymOrd @mode @a $
        withBaseBranching @mode @m $
          tryMerge $
            pure $
              max x y
    )
    ( withBaseSymOrd @mode @a $
        withBaseBranching @mode @m $
          SymOrd.mrgMax x y
    )
{-# INLINE mrgMax #-}

-- | Unified `Grisette.Internal.Core.Data.Class.SymOrd.mrgMin`.
mrgMin ::
  forall mode a m.
  ( UnifiedSymOrd mode a,
    UnifiedBranching mode m,
    DecideEvalMode mode,
    Applicative m,
    Mergeable a
  ) =>
  a ->
  a ->
  m a
mrgMin x y =
  withMode @mode
    ( withBaseSymOrd @mode @a $
        withBaseBranching @mode @m $
          tryMerge $
            pure $
              min x y
    )
    ( withBaseSymOrd @mode @a $
        withBaseBranching @mode @m $
          SymOrd.mrgMin x y
    )
{-# INLINE mrgMin #-}

instance
  {-# INCOHERENT #-}
  (DecideEvalMode mode, If (IsConMode mode) (Ord a) (SymOrd a)) =>
  UnifiedSymOrd mode a
  where
  withBaseSymOrd r = r
  {-# INLINE withBaseSymOrd #-}

instance
  {-# INCOHERENT #-}
  ( DecideEvalMode mode,
    If (IsConMode mode) (Ord1 f) (SymOrd1 f),
    forall a. (UnifiedSymOrd mode a) => UnifiedSymOrd mode (f a)
  ) =>
  UnifiedSymOrd1 mode f
  where
  withBaseSymOrd1 r = r
  {-# INLINE withBaseSymOrd1 #-}

instance
  {-# INCOHERENT #-}
  ( DecideEvalMode mode,
    If (IsConMode mode) (Ord2 f) (SymOrd2 f),
    forall a. (UnifiedSymOrd mode a) => UnifiedSymOrd1 mode (f a)
  ) =>
  UnifiedSymOrd2 mode f
  where
  withBaseSymOrd2 r = r
  {-# INLINE withBaseSymOrd2 #-}

instance (UnifiedSymOrd 'S v) => UnifiedSymOrd 'S (Union v) where
  withBaseSymOrd r = withBaseSymOrd @'S @v r
  {-# INLINE withBaseSymOrd #-}

derive
  [ ''Either,
    ''(,)
  ]
  [''UnifiedSymOrd, ''UnifiedSymOrd1, ''UnifiedSymOrd2]

derive
  [ ''[],
    ''Maybe,
    ''Identity,
    ''ExceptT,
    ''MaybeT,
    ''WriterLazy.WriterT,
    ''WriterStrict.WriterT
  ]
  [''UnifiedSymOrd, ''UnifiedSymOrd1]

derive
  [ ''Bool,
    ''Integer,
    ''Char,
    ''Int,
    ''Int8,
    ''Int16,
    ''Int32,
    ''Int64,
    ''Word,
    ''Word8,
    ''Word16,
    ''Word32,
    ''Word64,
    ''Float,
    ''Double,
    ''B.ByteString,
    ''T.Text,
    ''FPRoundingMode,
    ''(),
    ''(,,,,),
    ''(,,,,,),
    ''(,,,,,,),
    ''(,,,,,,,),
    ''(,,,,,,,,),
    ''(,,,,,,,,,),
    ''(,,,,,,,,,,),
    ''(,,,,,,,,,,,),
    ''(,,,,,,,,,,,,),
    ''(,,,,,,,,,,,,,),
    ''(,,,,,,,,,,,,,,),
    ''AssertionError,
    ''VerificationConditions
  ]
  [''UnifiedSymOrd]

#if MIN_VERSION_base(4,16,0)
derive
  [ ''(,,),
    ''(,,,)
  ]
  [''UnifiedSymOrd, ''UnifiedSymOrd1, ''UnifiedSymOrd2]
#else
derive
  [ ''(,,),
    ''(,,,)
  ]
  [''UnifiedSymOrd]
#endif

deriveWith
  (mempty {bitSizePositions = [0]})
  [''WordN, ''IntN]
  [''UnifiedSymOrd]

deriveWith
  (mempty {fpBitSizePositions = [(0, 1)]})
  [''FP]
  [''UnifiedSymOrd]

instance
  (DecideEvalMode mode, UnifiedSymOrd mode a, Integral a) =>
  UnifiedSymOrd mode (Ratio a)
  where
  withBaseSymOrd r =
    withMode @mode (withBaseSymOrd @mode @a r) (withBaseSymOrd @mode @a r)
  {-# INLINE withBaseSymOrd #-}

-- Sum
instance
  ( DecideEvalMode mode,
    UnifiedSymOrd1 mode f,
    UnifiedSymOrd1 mode g,
    UnifiedSymOrd mode a
  ) =>
  UnifiedSymOrd mode (Sum f g a)
  where
  withBaseSymOrd r =
    withMode @mode
      ( withBaseSymOrd1 @mode @f $
          withBaseSymOrd1 @mode @g $
            withBaseSymOrd @mode @a r
      )
      ( withBaseSymOrd1 @mode @f $
          withBaseSymOrd1 @mode @g $
            withBaseSymOrd @mode @a r
      )
  {-# INLINE withBaseSymOrd #-}

instance
  (DecideEvalMode mode, UnifiedSymOrd1 mode f, UnifiedSymOrd1 mode g) =>
  UnifiedSymOrd1 mode (Sum f g)
  where
  withBaseSymOrd1 r =
    withMode @mode
      (withBaseSymOrd1 @mode @f $ withBaseSymOrd1 @mode @g r)
      (withBaseSymOrd1 @mode @f $ withBaseSymOrd1 @mode @g r)
  {-# INLINE withBaseSymOrd1 #-}

-- IdentityT
instance
  (DecideEvalMode mode, UnifiedSymOrd1 mode m, UnifiedSymOrd mode a) =>
  UnifiedSymOrd mode (IdentityT m a)
  where
  withBaseSymOrd r =
    withMode @mode
      (withBaseSymOrd1 @mode @m $ withBaseSymOrd @mode @a r)
      (withBaseSymOrd1 @mode @m $ withBaseSymOrd @mode @a r)
  {-# INLINE withBaseSymOrd #-}

instance
  (DecideEvalMode mode, UnifiedSymOrd1 mode m) =>
  UnifiedSymOrd1 mode (IdentityT m)
  where
  withBaseSymOrd1 r =
    withMode @mode (withBaseSymOrd1 @mode @m r) (withBaseSymOrd1 @mode @m r)
  {-# INLINE withBaseSymOrd1 #-}
