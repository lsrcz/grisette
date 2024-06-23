{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Grisette.Unified.Internal.Class.UnifiedSEq
  ( UnifiedSEq (..),
    UnifiedSEq1 (..),
    UnifiedSEq2 (..),
    (.==),
    (./=),
    seq1,
    seq2,
  )
where

import Control.Monad.Except (ExceptT)
import Control.Monad.Identity (Identity, IdentityT)
import Control.Monad.Trans.Maybe (MaybeT)
import qualified Control.Monad.Writer.Lazy as WriterLazy
import qualified Control.Monad.Writer.Strict as WriterStrict
import qualified Data.ByteString as B
import Data.Functor.Classes (Eq1, Eq2, eq1, eq2)
import Data.Functor.Sum (Sum)
import Data.Int (Int16, Int32, Int64, Int8)
import qualified Data.Text as T
import Data.Type.Bool (If)
import Data.Typeable (Typeable)
import Data.Word (Word16, Word32, Word64, Word8)
import Grisette.Internal.Core.Control.Exception
  ( AssertionError,
    VerificationConditions,
  )
import Grisette.Internal.Core.Data.Class.SEq (SEq, SEq1, SEq2)
import qualified Grisette.Internal.Core.Data.Class.SEq
import Grisette.Internal.SymPrim.BV (IntN, WordN)
import Grisette.Internal.SymPrim.FP (FP, FPRoundingMode, ValidFP)
import Grisette.Internal.TH.DeriveUnifiedInterface
  ( deriveFunctorArgUnifiedInterfaces,
    deriveUnifiedInterface1s,
  )
import Grisette.Unified.Internal.EvaluationMode
  ( IsConMode,
  )
import Grisette.Unified.Internal.UnifiedBool (UnifiedBool (GetBool))
import Grisette.Unified.Internal.Util (withMode)

(.==) ::
  forall mode a. (Typeable mode, UnifiedSEq mode a) => a -> a -> GetBool mode
(.==) a b =
  withMode @mode
    (withBaseSEq @mode @a $ a == b)
    (withBaseSEq @mode @a $ a Grisette.Internal.Core.Data.Class.SEq..== b)

(./=) ::
  forall mode a. (Typeable mode, UnifiedSEq mode a) => a -> a -> GetBool mode
(./=) a b =
  withMode @mode
    (withBaseSEq @mode @a $ a /= b)
    (withBaseSEq @mode @a $ a Grisette.Internal.Core.Data.Class.SEq../= b)

seq1 ::
  forall mode f a.
  (Typeable mode, UnifiedSEq mode a, UnifiedSEq1 mode f) =>
  f a ->
  f a ->
  GetBool mode
seq1 a b =
  withMode @mode
    (withBaseSEq1 @mode @f $ withBaseSEq @mode @a eq1 a b)
    ( withBaseSEq1 @mode @f $
        withBaseSEq @mode @a $
          Grisette.Internal.Core.Data.Class.SEq.seq1 a b
    )

seq2 ::
  forall mode f a b.
  (Typeable mode, UnifiedSEq mode a, UnifiedSEq mode b, UnifiedSEq2 mode f) =>
  f a b ->
  f a b ->
  GetBool mode
seq2 a b =
  withMode @mode
    ( withBaseSEq2 @mode @f $
        withBaseSEq @mode @a $
          withBaseSEq @mode @b eq2 a b
    )
    ( withBaseSEq2 @mode @f $
        withBaseSEq @mode @a $
          withBaseSEq @mode @b $
            Grisette.Internal.Core.Data.Class.SEq.seq2 a b
    )

-- | A class that provides a unified symbolic equality comparison for unified
-- types.
--
-- On all values with 'Eq' instance, the comparisons could return concrete
-- results.
--
-- On all values with 'Grisette.SEq' instance, the comparisons could return
-- symbolic results.
--
-- Note that you may sometimes need to write type annotations for the result
-- when the mode isn't clear:
--
-- > a .== b :: GetBool mode
class UnifiedSEq mode a where
  withBaseSEq :: ((If (IsConMode mode) (Eq a) (SEq a)) => r) -> r

class
  (forall a. (UnifiedSEq mode a) => UnifiedSEq mode (f a)) =>
  UnifiedSEq1 mode f
  where
  withBaseSEq1 :: ((If (IsConMode mode) (Eq1 f) (SEq1 f)) => r) -> r

class
  (forall a. (UnifiedSEq mode a) => UnifiedSEq1 mode (f a)) =>
  UnifiedSEq2 mode f
  where
  withBaseSEq2 :: ((If (IsConMode mode) (Eq2 f) (SEq2 f)) => r) -> r

instance
  {-# INCOHERENT #-}
  (Typeable mode, If (IsConMode mode) (Eq a) (SEq a)) =>
  UnifiedSEq mode a
  where
  withBaseSEq r = r

instance
  {-# INCOHERENT #-}
  ( Typeable mode,
    If (IsConMode mode) (Eq1 f) (SEq1 f),
    forall a. (UnifiedSEq mode a) => UnifiedSEq mode (f a)
  ) =>
  UnifiedSEq1 mode f
  where
  withBaseSEq1 r = r

deriveFunctorArgUnifiedInterfaces
  ''UnifiedSEq
  'withBaseSEq
  ''UnifiedSEq1
  'withBaseSEq1
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
    ''WordN,
    ''IntN,
    ''[],
    ''Maybe,
    ''Either,
    ''(),
    ''(,),
    ''(,,),
    ''(,,,),
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
    ''VerificationConditions,
    ''ExceptT,
    ''MaybeT,
    ''WriterLazy.WriterT,
    ''WriterStrict.WriterT,
    ''Identity
  ]

deriveUnifiedInterface1s
  ''UnifiedSEq
  'withBaseSEq
  ''UnifiedSEq1
  'withBaseSEq1
  [ ''[],
    ''Maybe,
    ''Either,
    ''(,),
    ''ExceptT,
    ''MaybeT,
    ''WriterLazy.WriterT,
    ''WriterStrict.WriterT,
    ''Identity
  ]

-- Sum
instance
  (Typeable mode, UnifiedSEq1 mode f, UnifiedSEq1 mode g, UnifiedSEq mode a) =>
  UnifiedSEq mode (Sum f g a)
  where
  withBaseSEq r =
    withMode @mode
      (withBaseSEq1 @mode @f $ withBaseSEq1 @mode @g $ withBaseSEq @mode @a r)
      (withBaseSEq1 @mode @f $ withBaseSEq1 @mode @g $ withBaseSEq @mode @a r)
  {-# INLINE withBaseSEq #-}

instance
  (Typeable mode, UnifiedSEq1 mode f, UnifiedSEq1 mode g) =>
  UnifiedSEq1 mode (Sum f g)
  where
  withBaseSEq1 r =
    withMode @mode
      (withBaseSEq1 @mode @f $ withBaseSEq1 @mode @g r)
      (withBaseSEq1 @mode @f $ withBaseSEq1 @mode @g r)
  {-# INLINE withBaseSEq1 #-}

-- IdentityT
instance
  (Typeable mode, UnifiedSEq1 mode m, UnifiedSEq mode a) =>
  UnifiedSEq mode (IdentityT m a)
  where
  withBaseSEq r =
    withMode @mode
      (withBaseSEq1 @mode @m $ withBaseSEq @mode @a r)
      (withBaseSEq1 @mode @m $ withBaseSEq @mode @a r)
  {-# INLINE withBaseSEq #-}

instance
  (Typeable mode, UnifiedSEq1 mode m) =>
  UnifiedSEq1 mode (IdentityT m)
  where
  withBaseSEq1 r =
    withMode @mode (withBaseSEq1 @mode @m r) (withBaseSEq1 @mode @m r)
  {-# INLINE withBaseSEq1 #-}

instance (Typeable mode, ValidFP eb sb) => UnifiedSEq mode (FP eb sb) where
  withBaseSEq r = withMode @mode r r
  {-# INLINE withBaseSEq #-}

instance (Typeable mode) => UnifiedSEq2 mode Either where
  withBaseSEq2 r = withMode @mode r r
  {-# INLINE withBaseSEq2 #-}

instance (Typeable mode) => UnifiedSEq2 mode (,) where
  withBaseSEq2 r = withMode @mode r r
  {-# INLINE withBaseSEq2 #-}

#if MIN_VERSION_base(4,16,0)
deriveUnifiedInterface1s
  ''UnifiedSEq
  'withBaseSEq
  ''UnifiedSEq1
  'withBaseSEq1
  [ ''(,,),
    ''(,,,)
  ]

instance (Typeable mode, UnifiedSEq mode a) => UnifiedSEq2 mode ((,,) a) where
  withBaseSEq2 r =
    withMode @mode (withBaseSEq @mode @a r) (withBaseSEq @mode @a r)
  {-# INLINE withBaseSEq2 #-}

instance
  (Typeable mode, UnifiedSEq mode a, UnifiedSEq mode b) =>
  UnifiedSEq2 mode ((,,,) a b)
  where
  withBaseSEq2 r =
    withMode @mode
      (withBaseSEq @mode @a $ withBaseSEq @mode @b r)
      (withBaseSEq @mode @a $ withBaseSEq @mode @b r)
  {-# INLINE withBaseSEq2 #-}
#endif