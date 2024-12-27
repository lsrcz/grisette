{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
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
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Eta reduce" #-}

module Grisette.Internal.Unified.Class.Internal.Instances.UnifiedSymEq
  ( (.==),
    (./=),
    symDistinct,
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
import Data.Ratio (Ratio)
import qualified Data.Text as T
import Data.Type.Bool (If)
import Data.Word (Word16, Word32, Word64, Word8)
import Grisette.Internal.Core.Control.Exception
  ( AssertionError,
    VerificationConditions,
  )
import Grisette.Internal.Core.Control.Monad.Union (Union)
import Grisette.Internal.Internal.Decl.Core.Data.Class.SymEq
  ( SymEq,
    SymEq1,
    SymEq2,
  )
import qualified Grisette.Internal.Internal.Decl.Core.Data.Class.SymEq as SymEq
import Grisette.Internal.SymPrim.BV (IntN, WordN)
import Grisette.Internal.SymPrim.FP (FP, FPRoundingMode, ValidFP)
import Grisette.Internal.TH.DeriveUnifiedInterface
  ( deriveFunctorArgUnifiedInterfaces,
    deriveUnifiedInterface1s,
  )
import Grisette.Internal.Unified.Class.Internal.UnifiedSymEq
  ( UnifiedSymEq (withBaseSymEq),
    UnifiedSymEq1 (withBaseSymEq1),
    UnifiedSymEq2 (withBaseSymEq2),
  )
import Grisette.Internal.Unified.EvalModeTag (EvalModeTag (S), IsConMode)
import Grisette.Internal.Unified.UnifiedBool (UnifiedBool (GetBool))
import Grisette.Internal.Unified.Util (DecideEvalMode, withMode)

-- | Unified `(Grisette.Internal.Core.Data.Class.SymEq..==)`.
--
-- Note that you may sometimes need to write type annotations for the result
-- when the mode isn't clear:
--
-- > a .== b :: GetBool mode
--
-- One example when it isn't clear is when this is used in unified
-- `Grisette.Internal.Unified.Class.UnifiedBranching.mrgIf`.
(.==) ::
  forall mode a. (DecideEvalMode mode, UnifiedSymEq mode a) => a -> a -> GetBool mode
(.==) a b =
  withMode @mode
    (withBaseSymEq @mode @a $ a == b)
    (withBaseSymEq @mode @a $ a SymEq..== b)

-- | Unified `(Grisette.Internal.Core.Data.Class.SymEq../=)`.
--
-- Note that you may sometimes need to write type annotations for the result
-- when the mode isn't clear:
--
-- > a ./= b :: GetBool mode
--
-- One example when it isn't clear is when this is used in unified
-- `Grisette.Internal.Unified.Class.UnifiedBranching.mrgIf`.
(./=) ::
  forall mode a. (DecideEvalMode mode, UnifiedSymEq mode a) => a -> a -> GetBool mode
(./=) a b =
  withMode @mode
    (withBaseSymEq @mode @a $ a /= b)
    (withBaseSymEq @mode @a $ a SymEq../= b)

-- | Unified `Grisette.Internal.Core.Data.Class.SymEq.symDistinct`.
--
-- Note that you may sometimes need to write type annotations for the result
-- when the mode isn't clear:
--
-- > symDistinct l :: GetBool mode
--
-- One example when it isn't clear is when this is used in unified
-- `Grisette.Internal.Unified.Class.UnifiedBranching.mrgIf`.
symDistinct ::
  forall mode a. (DecideEvalMode mode, UnifiedSymEq mode a) => [a] -> GetBool mode
symDistinct l =
  withMode @mode
    ( withBaseSymEq @mode @a $
        SymEq.distinct l
    )
    ( withBaseSymEq @mode @a $
        SymEq.symDistinct l
    )

-- | Unified `Grisette.Internal.Core.Data.Class.SymEq.liftSymEq`.
liftSymEq ::
  forall mode f a b.
  (DecideEvalMode mode, UnifiedSymEq1 mode f) =>
  (a -> b -> GetBool mode) ->
  f a ->
  f b ->
  GetBool mode
liftSymEq f a b =
  withMode @mode
    (withBaseSymEq1 @mode @f $ liftEq f a b)
    ( withBaseSymEq1 @mode @f $
        SymEq.liftSymEq f a b
    )

-- | Unified `Grisette.Internal.Core.Data.Class.SymEq.symEq1`.
symEq1 ::
  forall mode f a.
  (DecideEvalMode mode, UnifiedSymEq mode a, UnifiedSymEq1 mode f) =>
  f a ->
  f a ->
  GetBool mode
symEq1 a b =
  withMode @mode
    (withBaseSymEq1 @mode @f $ withBaseSymEq @mode @a eq1 a b)
    ( withBaseSymEq1 @mode @f $
        withBaseSymEq @mode @a $
          SymEq.symEq1 a b
    )

-- | Unified `Grisette.Internal.Core.Data.Class.SymEq.liftSymEq2`.
liftSymEq2 ::
  forall mode f a b c d.
  (DecideEvalMode mode, UnifiedSymEq2 mode f) =>
  (a -> b -> GetBool mode) ->
  (c -> d -> GetBool mode) ->
  f a c ->
  f b d ->
  GetBool mode
liftSymEq2 f a b =
  withMode @mode
    (withBaseSymEq2 @mode @f $ liftEq2 f a b)
    ( withBaseSymEq2 @mode @f $
        SymEq.liftSymEq2 f a b
    )

-- | Unified `Grisette.Internal.Core.Data.Class.SymEq.symEq2`.
symEq2 ::
  forall mode f a b.
  ( DecideEvalMode mode,
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
            SymEq.symEq2 a b
    )

instance
  {-# INCOHERENT #-}
  (DecideEvalMode mode, If (IsConMode mode) (Eq a) (SymEq a)) =>
  UnifiedSymEq mode a
  where
  withBaseSymEq r = r
  {-# INLINE withBaseSymEq #-}

instance
  {-# INCOHERENT #-}
  ( DecideEvalMode mode,
    If (IsConMode mode) (Eq1 f) (SymEq1 f),
    forall a. (UnifiedSymEq mode a) => UnifiedSymEq mode (f a)
  ) =>
  UnifiedSymEq1 mode f
  where
  withBaseSymEq1 r = r
  {-# INLINE withBaseSymEq1 #-}

instance
  {-# INCOHERENT #-}
  ( DecideEvalMode mode,
    If (IsConMode mode) (Eq2 f) (SymEq2 f),
    forall a. (UnifiedSymEq mode a) => UnifiedSymEq1 mode (f a)
  ) =>
  UnifiedSymEq2 mode f
  where
  withBaseSymEq2 r = r
  {-# INLINE withBaseSymEq2 #-}

instance (UnifiedSymEq 'S v) => UnifiedSymEq 'S (Union v) where
  withBaseSymEq r = withBaseSymEq @'S @v r
  {-# INLINE withBaseSymEq #-}

instance
  (DecideEvalMode mode, UnifiedSymEq mode a) =>
  UnifiedSymEq mode (Ratio a)
  where
  withBaseSymEq r =
    withMode @mode (withBaseSymEq @mode @a r) (withBaseSymEq @mode @a r)
  {-# INLINE withBaseSymEq #-}

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
  ( DecideEvalMode mode,
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
  (DecideEvalMode mode, UnifiedSymEq1 mode f, UnifiedSymEq1 mode g) =>
  UnifiedSymEq1 mode (Sum f g)
  where
  withBaseSymEq1 r =
    withMode @mode
      (withBaseSymEq1 @mode @f $ withBaseSymEq1 @mode @g r)
      (withBaseSymEq1 @mode @f $ withBaseSymEq1 @mode @g r)
  {-# INLINE withBaseSymEq1 #-}

-- IdentityT
instance
  (DecideEvalMode mode, UnifiedSymEq1 mode m, UnifiedSymEq mode a) =>
  UnifiedSymEq mode (IdentityT m a)
  where
  withBaseSymEq r =
    withMode @mode
      (withBaseSymEq1 @mode @m $ withBaseSymEq @mode @a r)
      (withBaseSymEq1 @mode @m $ withBaseSymEq @mode @a r)
  {-# INLINE withBaseSymEq #-}

instance
  (DecideEvalMode mode, UnifiedSymEq1 mode m) =>
  UnifiedSymEq1 mode (IdentityT m)
  where
  withBaseSymEq1 r =
    withMode @mode (withBaseSymEq1 @mode @m r) (withBaseSymEq1 @mode @m r)
  {-# INLINE withBaseSymEq1 #-}

instance (DecideEvalMode mode, ValidFP eb sb) => UnifiedSymEq mode (FP eb sb) where
  withBaseSymEq r = withMode @mode r r
  {-# INLINE withBaseSymEq #-}

instance (DecideEvalMode mode) => UnifiedSymEq2 mode Either where
  withBaseSymEq2 r = withMode @mode r r
  {-# INLINE withBaseSymEq2 #-}

instance (DecideEvalMode mode) => UnifiedSymEq2 mode (,) where
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

instance (DecideEvalMode mode, UnifiedSymEq mode a) =>
  UnifiedSymEq2 mode ((,,) a) where
  withBaseSymEq2 r =
    withMode @mode (withBaseSymEq @mode @a r) (withBaseSymEq @mode @a r)
  {-# INLINE withBaseSymEq2 #-}

instance
  (DecideEvalMode mode, UnifiedSymEq mode a, UnifiedSymEq mode b) =>
  UnifiedSymEq2 mode ((,,,) a b)
  where
  withBaseSymEq2 r =
    withMode @mode
      (withBaseSymEq @mode @a $ withBaseSymEq @mode @b r)
      (withBaseSymEq @mode @a $ withBaseSymEq @mode @b r)
  {-# INLINE withBaseSymEq2 #-}
#endif
