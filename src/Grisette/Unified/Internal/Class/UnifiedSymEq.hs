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

module Grisette.Unified.Internal.Class.UnifiedSymEq
  ( UnifiedSymEq (..),
    UnifiedSymEq1 (..),
    UnifiedSymEq2 (..),
    (.==),
    (./=),
    liftSymEq,
    symEq1,
    liftSymEq2,
    symEq2,
  )
where

import Control.Monad.Except (ExceptT)
import Control.Monad.Identity (Identity, IdentityT)
import Control.Monad.Trans.Maybe (MaybeT)
import qualified Control.Monad.Writer.Lazy as WriterLazy
import qualified Control.Monad.Writer.Strict as WriterStrict
import qualified Data.ByteString as B
import Data.Functor.Classes (Eq1 (liftEq), Eq2 (liftEq2), eq1, eq2)
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
import Grisette.Internal.Core.Data.Class.SymEq (SymEq, SymEq1, SymEq2)
import qualified Grisette.Internal.Core.Data.Class.SymEq
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

-- | Unified `(Grisette.Internal.Core.Data.Class.SymEq..==)`.
--
-- Note that you may sometimes need to write type annotations for the result
-- when the mode isn't clear:
--
-- > a .== b :: GetBool mode
--
-- One example when it isn't clear is when this is used in unified
-- `Grisette.Unified.Internal.Class.UnifiedBranching.mrgIf`.
(.==) ::
  forall mode a. (Typeable mode, UnifiedSymEq mode a) => a -> a -> GetBool mode
(.==) a b =
  withMode @mode
    (withBaseSymEq @mode @a $ a == b)
    (withBaseSymEq @mode @a $ a Grisette.Internal.Core.Data.Class.SymEq..== b)

-- | Unified `(Grisette.Internal.Core.Data.Class.SymEq../=)`.
--
-- Note that you may sometimes need to write type annotations for the result
-- when the mode isn't clear:
--
-- > a ./= b :: GetBool mode
--
-- One example when it isn't clear is when this is used in unified
-- `Grisette.Unified.Internal.Class.UnifiedBranching.mrgIf`.
(./=) ::
  forall mode a. (Typeable mode, UnifiedSymEq mode a) => a -> a -> GetBool mode
(./=) a b =
  withMode @mode
    (withBaseSymEq @mode @a $ a /= b)
    (withBaseSymEq @mode @a $ a Grisette.Internal.Core.Data.Class.SymEq../= b)

-- | Unified `Grisette.Internal.Core.Data.Class.SymEq.liftSymEq`.
liftSymEq ::
  forall mode f a b.
  (Typeable mode, UnifiedSymEq1 mode f) =>
  (a -> b -> GetBool mode) ->
  f a ->
  f b ->
  GetBool mode
liftSymEq f a b =
  withMode @mode
    (withBaseSymEq1 @mode @f $ liftEq f a b)
    ( withBaseSymEq1 @mode @f $
        Grisette.Internal.Core.Data.Class.SymEq.liftSymEq f a b
    )

-- | Unified `Grisette.Internal.Core.Data.Class.SymEq.symEq1`.
symEq1 ::
  forall mode f a.
  (Typeable mode, UnifiedSymEq mode a, UnifiedSymEq1 mode f) =>
  f a ->
  f a ->
  GetBool mode
symEq1 a b =
  withMode @mode
    (withBaseSymEq1 @mode @f $ withBaseSymEq @mode @a eq1 a b)
    ( withBaseSymEq1 @mode @f $
        withBaseSymEq @mode @a $
          Grisette.Internal.Core.Data.Class.SymEq.symEq1 a b
    )

-- | Unified `Grisette.Internal.Core.Data.Class.SymEq.liftSymEq2`.
liftSymEq2 ::
  forall mode f a b c d.
  (Typeable mode, UnifiedSymEq2 mode f) =>
  (a -> b -> GetBool mode) ->
  (c -> d -> GetBool mode) ->
  f a c ->
  f b d ->
  GetBool mode
liftSymEq2 f a b =
  withMode @mode
    (withBaseSymEq2 @mode @f $ liftEq2 f a b)
    ( withBaseSymEq2 @mode @f $
        Grisette.Internal.Core.Data.Class.SymEq.liftSymEq2 f a b
    )

-- | Unified `Grisette.Internal.Core.Data.Class.SymEq.symEq2`.
symEq2 ::
  forall mode f a b.
  ( Typeable mode,
    UnifiedSymEq mode a,
    UnifiedSymEq mode b,
    UnifiedSymEq2 mode f
  ) =>
  f a b ->
  f a b ->
  GetBool mode
symEq2 a b =
  withMode @mode
    ( withBaseSymEq2 @mode @f $
        withBaseSymEq @mode @a $
          withBaseSymEq @mode @b eq2 a b
    )
    ( withBaseSymEq2 @mode @f $
        withBaseSymEq @mode @a $
          withBaseSymEq @mode @b $
            Grisette.Internal.Core.Data.Class.SymEq.symEq2 a b
    )

-- | A class that provides unified equality comparison.
--
-- We use this type class to help resolve the constraints for `Eq` and `SymEq`.
class UnifiedSymEq mode a where
  withBaseSymEq :: ((If (IsConMode mode) (Eq a) (SymEq a)) => r) -> r

-- | A class that provides unified lifting of equality comparison.
--
-- We use this type class to help resolve the constraints for `Eq1` and
-- `SymEq1`.
class
  (forall a. (UnifiedSymEq mode a) => UnifiedSymEq mode (f a)) =>
  UnifiedSymEq1 mode f
  where
  withBaseSymEq1 :: ((If (IsConMode mode) (Eq1 f) (SymEq1 f)) => r) -> r

-- | A class that provides unified lifting of equality comparison.
--
-- We use this type class to help resolve the constraints for `Eq2` and
-- `SymEq2`.
class
  (forall a. (UnifiedSymEq mode a) => UnifiedSymEq1 mode (f a)) =>
  UnifiedSymEq2 mode f
  where
  withBaseSymEq2 :: ((If (IsConMode mode) (Eq2 f) (SymEq2 f)) => r) -> r

instance
  {-# INCOHERENT #-}
  (Typeable mode, If (IsConMode mode) (Eq a) (SymEq a)) =>
  UnifiedSymEq mode a
  where
  withBaseSymEq r = r
  {-# INLINE withBaseSymEq #-}

instance
  {-# INCOHERENT #-}
  ( Typeable mode,
    If (IsConMode mode) (Eq1 f) (SymEq1 f),
    forall a. (UnifiedSymEq mode a) => UnifiedSymEq mode (f a)
  ) =>
  UnifiedSymEq1 mode f
  where
  withBaseSymEq1 r = r
  {-# INLINE withBaseSymEq1 #-}

instance
  {-# INCOHERENT #-}
  ( Typeable mode,
    If (IsConMode mode) (Eq2 f) (SymEq2 f),
    forall a. (UnifiedSymEq mode a) => UnifiedSymEq1 mode (f a)
  ) =>
  UnifiedSymEq2 mode f
  where
  withBaseSymEq2 r = r
  {-# INLINE withBaseSymEq2 #-}

deriveFunctorArgUnifiedInterfaces
  ''UnifiedSymEq
  'withBaseSymEq
  ''UnifiedSymEq1
  'withBaseSymEq1
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
  ''UnifiedSymEq
  'withBaseSymEq
  ''UnifiedSymEq1
  'withBaseSymEq1
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
  ( Typeable mode,
    UnifiedSymEq1 mode f,
    UnifiedSymEq1 mode g,
    UnifiedSymEq mode a
  ) =>
  UnifiedSymEq mode (Sum f g a)
  where
  withBaseSymEq r =
    withMode @mode
      ( withBaseSymEq1 @mode @f $
          withBaseSymEq1 @mode @g $
            withBaseSymEq @mode @a r
      )
      ( withBaseSymEq1 @mode @f $
          withBaseSymEq1 @mode @g $
            withBaseSymEq @mode @a r
      )
  {-# INLINE withBaseSymEq #-}

instance
  (Typeable mode, UnifiedSymEq1 mode f, UnifiedSymEq1 mode g) =>
  UnifiedSymEq1 mode (Sum f g)
  where
  withBaseSymEq1 r =
    withMode @mode
      (withBaseSymEq1 @mode @f $ withBaseSymEq1 @mode @g r)
      (withBaseSymEq1 @mode @f $ withBaseSymEq1 @mode @g r)
  {-# INLINE withBaseSymEq1 #-}

-- IdentityT
instance
  (Typeable mode, UnifiedSymEq1 mode m, UnifiedSymEq mode a) =>
  UnifiedSymEq mode (IdentityT m a)
  where
  withBaseSymEq r =
    withMode @mode
      (withBaseSymEq1 @mode @m $ withBaseSymEq @mode @a r)
      (withBaseSymEq1 @mode @m $ withBaseSymEq @mode @a r)
  {-# INLINE withBaseSymEq #-}

instance
  (Typeable mode, UnifiedSymEq1 mode m) =>
  UnifiedSymEq1 mode (IdentityT m)
  where
  withBaseSymEq1 r =
    withMode @mode (withBaseSymEq1 @mode @m r) (withBaseSymEq1 @mode @m r)
  {-# INLINE withBaseSymEq1 #-}

instance (Typeable mode, ValidFP eb sb) => UnifiedSymEq mode (FP eb sb) where
  withBaseSymEq r = withMode @mode r r
  {-# INLINE withBaseSymEq #-}

instance (Typeable mode) => UnifiedSymEq2 mode Either where
  withBaseSymEq2 r = withMode @mode r r
  {-# INLINE withBaseSymEq2 #-}

instance (Typeable mode) => UnifiedSymEq2 mode (,) where
  withBaseSymEq2 r = withMode @mode r r
  {-# INLINE withBaseSymEq2 #-}

#if MIN_VERSION_base(4,16,0)
deriveUnifiedInterface1s
  ''UnifiedSymEq
  'withBaseSymEq
  ''UnifiedSymEq1
  'withBaseSymEq1
  [ ''(,,),
    ''(,,,)
  ]

instance (Typeable mode, UnifiedSymEq mode a) =>
  UnifiedSymEq2 mode ((,,) a) where
  withBaseSymEq2 r =
    withMode @mode (withBaseSymEq @mode @a r) (withBaseSymEq @mode @a r)
  {-# INLINE withBaseSymEq2 #-}

instance
  (Typeable mode, UnifiedSymEq mode a, UnifiedSymEq mode b) =>
  UnifiedSymEq2 mode ((,,,) a b)
  where
  withBaseSymEq2 r =
    withMode @mode
      (withBaseSymEq @mode @a $ withBaseSymEq @mode @b r)
      (withBaseSymEq @mode @a $ withBaseSymEq @mode @b r)
  {-# INLINE withBaseSymEq2 #-}
#endif
