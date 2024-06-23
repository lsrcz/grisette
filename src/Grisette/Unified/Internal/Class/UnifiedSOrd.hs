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
{-# LANGUAGE UndecidableSuperClasses #-}

module Grisette.Unified.Internal.Class.UnifiedSOrd
  ( UnifiedSOrd (..),
    UnifiedSOrd1 (..),
    UnifiedSOrd2 (..),
    (.<=),
    (.<),
    (.>=),
    (.>),
    symCompare,
    liftSymCompare,
    symCompare1,
    liftSymCompare2,
    symCompare2,
    symMax,
    symMin,
    mrgMax,
    mrgMin,
  )
where

import Control.Monad.Except (ExceptT)
import Control.Monad.Identity (Identity (runIdentity), IdentityT)
import Control.Monad.Trans.Maybe (MaybeT)
import qualified Control.Monad.Writer.Lazy as WriterLazy
import qualified Control.Monad.Writer.Strict as WriterStrict
import qualified Data.ByteString as B
import Data.Functor.Classes
  ( Ord1 (liftCompare),
    Ord2 (liftCompare2),
    compare1,
    compare2,
  )
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
import Grisette.Internal.Core.Data.Class.Mergeable (Mergeable)
import Grisette.Internal.Core.Data.Class.SOrd (SOrd, SOrd1, SOrd2)
import qualified Grisette.Internal.Core.Data.Class.SOrd
import Grisette.Internal.Core.Data.Class.TryMerge (tryMerge)
import Grisette.Internal.SymPrim.BV (IntN, WordN)
import Grisette.Internal.SymPrim.FP (FP, FPRoundingMode, ValidFP)
import Grisette.Internal.TH.DeriveUnifiedInterface
  ( deriveFunctorArgUnifiedInterfaces,
    deriveUnifiedInterface1s,
  )
import Grisette.Unified.Internal.BaseMonad (BaseMonad)
import Grisette.Unified.Internal.Class.UnifiedBranching
  ( UnifiedBranching (withBaseBranching),
  )
import Grisette.Unified.Internal.Class.UnifiedITEOp (UnifiedITEOp)
import Grisette.Unified.Internal.EvaluationMode
  ( IsConMode,
  )
import Grisette.Unified.Internal.UnifiedBool (UnifiedBool (GetBool))
import Grisette.Unified.Internal.Util (withMode)

-- | Unified less than or equal comparison.
--
-- Note that you may sometimes need to write type annotations for the result
-- when the mode isn't clear:
--
-- > a .<= b :: GetBool mode
--
-- One example when it isn't clear is when this is used in unified
-- `Grisette.Unified.Internal.Class.UnifiedBranching.mrgIf`.
(.<=) ::
  forall mode a. (Typeable mode, UnifiedSOrd mode a) => a -> a -> GetBool mode
(.<=) a b =
  withMode @mode
    (withBaseSOrd @mode @a $ a <= b)
    (withBaseSOrd @mode @a $ a Grisette.Internal.Core.Data.Class.SOrd..<= b)
{-# INLINE (.<=) #-}

-- | Unified less than comparison.
(.<) ::
  forall mode a. (Typeable mode, UnifiedSOrd mode a) => a -> a -> GetBool mode
(.<) a b =
  withMode @mode
    (withBaseSOrd @mode @a $ a < b)
    (withBaseSOrd @mode @a $ a Grisette.Internal.Core.Data.Class.SOrd..< b)
{-# INLINE (.<) #-}

-- | Unified greater than or equal comparison.
(.>=) ::
  forall mode a. (Typeable mode, UnifiedSOrd mode a) => a -> a -> GetBool mode
(.>=) a b =
  withMode @mode
    (withBaseSOrd @mode @a $ a >= b)
    (withBaseSOrd @mode @a $ a Grisette.Internal.Core.Data.Class.SOrd..>= b)
{-# INLINE (.>=) #-}

-- | Unified greater than comparison.
(.>) ::
  forall mode a. (Typeable mode, UnifiedSOrd mode a) => a -> a -> GetBool mode
(.>) a b =
  withMode @mode
    (withBaseSOrd @mode @a $ a > b)
    (withBaseSOrd @mode @a $ a Grisette.Internal.Core.Data.Class.SOrd..> b)
{-# INLINE (.>) #-}

-- | Unified comparison.
symCompare ::
  forall mode a ctx.
  (Typeable mode, UnifiedSOrd mode a, Monad ctx) =>
  a ->
  a ->
  BaseMonad mode Ordering
symCompare x y =
  withMode @mode
    (withBaseSOrd @mode @a $ return $ compare x y)
    ( withBaseSOrd @mode @a $
        Grisette.Internal.Core.Data.Class.SOrd.symCompare x y
    )
{-# INLINE symCompare #-}

-- | Unified lifting of a comparison for unary type constructors.
liftSymCompare ::
  forall mode f a b.
  (Typeable mode, UnifiedSOrd1 mode f) =>
  (a -> b -> BaseMonad mode Ordering) ->
  f a ->
  f b ->
  BaseMonad mode Ordering
liftSymCompare f a b =
  withMode @mode
    ( withBaseSOrd1 @mode @f $
        return $
          liftCompare (\x y -> runIdentity $ f x y) a b
    )
    ( withBaseSOrd1 @mode @f $
        Grisette.Internal.Core.Data.Class.SOrd.liftSymCompare f a b
    )
{-# INLINE liftSymCompare #-}

-- | Unified lifting of the default comparison for unary type constructors.
symCompare1 ::
  forall mode f a.
  (Typeable mode, UnifiedSOrd mode a, UnifiedSOrd1 mode f) =>
  f a ->
  f a ->
  BaseMonad mode Ordering
symCompare1 a b =
  withMode @mode
    (withBaseSOrd1 @mode @f $ withBaseSOrd @mode @a $ return $ compare1 a b)
    ( withBaseSOrd1 @mode @f $
        withBaseSOrd @mode @a $
          Grisette.Internal.Core.Data.Class.SOrd.symCompare1 a b
    )
{-# INLINE symCompare1 #-}

-- | Unified lifting of a comparison for binary type constructors.
liftSymCompare2 ::
  forall mode f a b c d.
  (Typeable mode, UnifiedSOrd2 mode f) =>
  (a -> b -> BaseMonad mode Ordering) ->
  (c -> d -> BaseMonad mode Ordering) ->
  f a c ->
  f b d ->
  BaseMonad mode Ordering
liftSymCompare2 f g a b =
  withMode @mode
    ( withBaseSOrd2 @mode @f $
        return $
          liftCompare2
            (\x y -> runIdentity $ f x y)
            (\x y -> runIdentity $ g x y)
            a
            b
    )
    ( withBaseSOrd2 @mode @f $
        Grisette.Internal.Core.Data.Class.SOrd.liftSymCompare2 f g a b
    )
{-# INLINE liftSymCompare2 #-}

-- | Unified lifting of the default comparison for binary type constructors.
symCompare2 ::
  forall mode f a b.
  ( Typeable mode,
    UnifiedSOrd mode a,
    UnifiedSOrd mode b,
    UnifiedSOrd2 mode f
  ) =>
  f a b ->
  f a b ->
  BaseMonad mode Ordering
symCompare2 a b =
  withMode @mode
    ( withBaseSOrd2 @mode @f $
        withBaseSOrd @mode @a $
          withBaseSOrd @mode @b $
            return $
              compare2 a b
    )
    ( withBaseSOrd2 @mode @f $
        withBaseSOrd @mode @a $
          withBaseSOrd @mode @b $
            Grisette.Internal.Core.Data.Class.SOrd.symCompare2 a b
    )
{-# INLINE symCompare2 #-}

-- | Unified maximum.
symMax ::
  forall mode a.
  (UnifiedSOrd mode a, UnifiedITEOp mode a, Typeable mode) =>
  a ->
  a ->
  a
symMax x y =
  withMode @mode
    (withBaseSOrd @mode @a $ max x y)
    ( withBaseSOrd @mode @a $
        Grisette.Internal.Core.Data.Class.SOrd.symMax x y
    )
{-# INLINE symMax #-}

-- | Unified minimum.
symMin ::
  forall mode a.
  (UnifiedSOrd mode a, UnifiedITEOp mode a, Typeable mode) =>
  a ->
  a ->
  a
symMin x y =
  withMode @mode
    (withBaseSOrd @mode @a $ min x y)
    ( withBaseSOrd @mode @a $
        Grisette.Internal.Core.Data.Class.SOrd.symMin x y
    )
{-# INLINE symMin #-}

-- | Unified maximum, merged in a monad.
mrgMax ::
  forall mode a m.
  ( UnifiedSOrd mode a,
    UnifiedBranching mode m,
    Typeable mode,
    Applicative m,
    Mergeable a
  ) =>
  a ->
  a ->
  m a
mrgMax x y =
  withMode @mode
    (withBaseSOrd @mode @a $ tryMerge $ pure $ max x y)
    ( withBaseSOrd @mode @a $
        withBaseBranching @mode @m $
          Grisette.Internal.Core.Data.Class.SOrd.mrgMax x y
    )
{-# INLINE mrgMax #-}

-- | Unified minimum, merged in a monad.
mrgMin ::
  forall mode a m.
  ( UnifiedSOrd mode a,
    UnifiedBranching mode m,
    Typeable mode,
    Applicative m,
    Mergeable a
  ) =>
  a ->
  a ->
  m a
mrgMin x y =
  withMode @mode
    (withBaseSOrd @mode @a $ tryMerge $ pure $ min x y)
    ( withBaseSOrd @mode @a $
        withBaseBranching @mode @m $
          Grisette.Internal.Core.Data.Class.SOrd.mrgMin x y
    )
{-# INLINE mrgMin #-}

-- | A class that provides unified comparison.
--
-- We use this type class to help resolve the constraints for `Ord` and `SOrd`.
class UnifiedSOrd mode a where
  withBaseSOrd :: (((If (IsConMode mode) (Ord a) (SOrd a)) => r)) -> r

-- | A class that provides unified lifting of comparison.
--
-- We use this type class to help resolve the constraints for `Ord1` and
-- `SOrd1`.
class UnifiedSOrd1 mode f where
  withBaseSOrd1 :: (((If (IsConMode mode) (Ord1 f) (SOrd1 f)) => r)) -> r

-- | A class that provides unified lifting of comparison.
--
-- We use this type class to help resolve the constraints for `Ord2` and
-- `SOrd2`.
class UnifiedSOrd2 mode f where
  withBaseSOrd2 :: (((If (IsConMode mode) (Ord2 f) (SOrd2 f)) => r)) -> r

instance
  {-# INCOHERENT #-}
  (Typeable mode, If (IsConMode mode) (Ord a) (SOrd a)) =>
  UnifiedSOrd mode a
  where
  withBaseSOrd r = r
  {-# INLINE withBaseSOrd #-}

instance
  {-# INCOHERENT #-}
  ( Typeable mode,
    If (IsConMode mode) (Ord1 f) (SOrd1 f),
    forall a. (UnifiedSOrd mode a) => UnifiedSOrd mode (f a)
  ) =>
  UnifiedSOrd1 mode f
  where
  withBaseSOrd1 r = r
  {-# INLINE withBaseSOrd1 #-}

instance
  {-# INCOHERENT #-}
  ( Typeable mode,
    If (IsConMode mode) (Ord2 f) (SOrd2 f),
    forall a. (UnifiedSOrd mode a) => UnifiedSOrd1 mode (f a)
  ) =>
  UnifiedSOrd2 mode f
  where
  withBaseSOrd2 r = r
  {-# INLINE withBaseSOrd2 #-}

deriveFunctorArgUnifiedInterfaces
  ''UnifiedSOrd
  'withBaseSOrd
  ''UnifiedSOrd1
  'withBaseSOrd1
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
  ''UnifiedSOrd
  'withBaseSOrd
  ''UnifiedSOrd1
  'withBaseSOrd1
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
  (Typeable mode, UnifiedSOrd1 mode f, UnifiedSOrd1 mode g, UnifiedSOrd mode a) =>
  UnifiedSOrd mode (Sum f g a)
  where
  withBaseSOrd r =
    withMode @mode
      (withBaseSOrd1 @mode @f $ withBaseSOrd1 @mode @g $ withBaseSOrd @mode @a r)
      (withBaseSOrd1 @mode @f $ withBaseSOrd1 @mode @g $ withBaseSOrd @mode @a r)
  {-# INLINE withBaseSOrd #-}

instance
  (Typeable mode, UnifiedSOrd1 mode f, UnifiedSOrd1 mode g) =>
  UnifiedSOrd1 mode (Sum f g)
  where
  withBaseSOrd1 r =
    withMode @mode
      (withBaseSOrd1 @mode @f $ withBaseSOrd1 @mode @g r)
      (withBaseSOrd1 @mode @f $ withBaseSOrd1 @mode @g r)
  {-# INLINE withBaseSOrd1 #-}

-- IdentityT
instance
  (Typeable mode, UnifiedSOrd1 mode m, UnifiedSOrd mode a) =>
  UnifiedSOrd mode (IdentityT m a)
  where
  withBaseSOrd r =
    withMode @mode
      (withBaseSOrd1 @mode @m $ withBaseSOrd @mode @a r)
      (withBaseSOrd1 @mode @m $ withBaseSOrd @mode @a r)
  {-# INLINE withBaseSOrd #-}

instance
  (Typeable mode, UnifiedSOrd1 mode m) =>
  UnifiedSOrd1 mode (IdentityT m)
  where
  withBaseSOrd1 r =
    withMode @mode (withBaseSOrd1 @mode @m r) (withBaseSOrd1 @mode @m r)
  {-# INLINE withBaseSOrd1 #-}

instance (Typeable mode, ValidFP eb sb) => UnifiedSOrd mode (FP eb sb) where
  withBaseSOrd r = withMode @mode r r
  {-# INLINE withBaseSOrd #-}

instance (Typeable mode) => UnifiedSOrd2 mode Either where
  withBaseSOrd2 r = withMode @mode r r
  {-# INLINE withBaseSOrd2 #-}

instance (Typeable mode) => UnifiedSOrd2 mode (,) where
  withBaseSOrd2 r = withMode @mode r r
  {-# INLINE withBaseSOrd2 #-}

#if MIN_VERSION_base(4,16,0)
deriveUnifiedInterface1s
  ''UnifiedSOrd
  'withBaseSOrd
  ''UnifiedSOrd1
  'withBaseSOrd1
  [ ''(,,),
    ''(,,,)
  ]

instance (Typeable mode, UnifiedSOrd mode a) => UnifiedSOrd2 mode ((,,) a) where
  withBaseSOrd2 r =
    withMode @mode (withBaseSOrd @mode @a r) (withBaseSOrd @mode @a r)
  {-# INLINE withBaseSOrd2 #-}

instance
  (Typeable mode, UnifiedSOrd mode a, UnifiedSOrd mode b) =>
  UnifiedSOrd2 mode ((,,,) a b)
  where
  withBaseSOrd2 r =
    withMode @mode
      (withBaseSOrd @mode @a $ withBaseSOrd @mode @b r)
      (withBaseSOrd @mode @a $ withBaseSOrd @mode @b r)
  {-# INLINE withBaseSOrd2 #-}
#endif
