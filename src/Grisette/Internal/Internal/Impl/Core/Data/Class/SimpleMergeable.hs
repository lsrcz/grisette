{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      :   Grisette.Internal.Internal.Impl.Core.Data.Class.SimpleMergeable
-- Copyright   :   (c) Sirui Lu 2021-2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Internal.Internal.Impl.Core.Data.Class.SimpleMergeable () where

import Control.Monad.Except (ExceptT (ExceptT))
import Control.Monad.Identity
  ( Identity,
    IdentityT (IdentityT),
  )
import qualified Control.Monad.RWS.Lazy as RWSLazy
import qualified Control.Monad.RWS.Strict as RWSStrict
import Control.Monad.Reader (ReaderT (ReaderT))
import qualified Control.Monad.State.Lazy as StateLazy
import qualified Control.Monad.State.Strict as StateStrict
import Control.Monad.Trans.Cont (ContT (ContT))
import Control.Monad.Trans.Maybe (MaybeT (MaybeT))
import qualified Control.Monad.Writer.Lazy as WriterLazy
import qualified Control.Monad.Writer.Strict as WriterStrict
import Data.Functor.Compose (Compose (Compose))
import Data.Functor.Const (Const)
import Data.Functor.Product (Product)
import qualified Data.HashSet as HS
import Data.Monoid (Alt, Ap, Endo (Endo))
import qualified Data.Monoid as Monoid
import Data.Ord (Down)
import Data.Proxy (Proxy)
import GHC.Generics
  ( K1 (K1),
    M1 (M1),
    Par1 (Par1),
    Rec1 (Rec1),
    U1,
    V1,
    (:.:) (Comp1),
    type (:*:),
  )
import GHC.TypeNats (KnownNat, type (<=))
import Generics.Deriving (Default (Default), Default1 (Default1))
import Grisette.Internal.Core.Control.Exception (AssertionError)
import Grisette.Internal.Core.Data.Class.Mergeable
  ( Mergeable (rootStrategy),
    Mergeable1 (liftRootStrategy),
    Mergeable2 (liftRootStrategy2),
    Mergeable3 (liftRootStrategy3),
    MergingStrategy (SimpleStrategy),
  )
import Grisette.Internal.Internal.Decl.Core.Data.Class.SimpleMergeable
  ( SimpleMergeable (mrgIte),
    SimpleMergeable1 (liftMrgIte),
    SimpleMergeable2 (liftMrgIte2),
    SymBranching (mrgIfPropagatedStrategy, mrgIfWithStrategy),
    mrgIf,
    mrgIte1,
  )
import Grisette.Internal.Internal.Impl.Core.Data.Class.TryMerge ()
import Grisette.Internal.SymPrim.FP (ValidFP)
import Grisette.Internal.SymPrim.GeneralFun (freshArgSymbol, substTerm, type (-->) (GeneralFun))
import Grisette.Internal.SymPrim.Prim.Internal.Term (SupportedPrim (pevalITETerm), symTerm)
import Grisette.Internal.SymPrim.Prim.SomeTerm (SomeTerm (SomeTerm))
import Grisette.Internal.SymPrim.Prim.Term (TypedConstantSymbol)
import Grisette.Internal.SymPrim.SymAlgReal (SymAlgReal (SymAlgReal))
import Grisette.Internal.SymPrim.SymBV
  ( SymIntN (SymIntN),
    SymWordN (SymWordN),
  )
import Grisette.Internal.SymPrim.SymBool (SymBool (SymBool))
import Grisette.Internal.SymPrim.SymFP (SymFP (SymFP), SymFPRoundingMode (SymFPRoundingMode))
import Grisette.Internal.SymPrim.SymGeneralFun (type (-~>) (SymGeneralFun))
import Grisette.Internal.SymPrim.SymInteger (SymInteger (SymInteger))
import Grisette.Internal.SymPrim.SymTabularFun (type (=~>) (SymTabularFun))
import Grisette.Internal.TH.Derivation.Derive (derive)

-- $setup
-- >>> import Grisette.Core
-- >>> import Grisette.SymPrim
-- >>> import Control.Monad.Identity

derive
  [ ''(,),
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
    ''(,,,,,,,,,,,,,,)
  ]
  [''SimpleMergeable, ''SimpleMergeable1, ''SimpleMergeable2]

derive
  [ ''Identity,
    ''Monoid.Dual,
    ''Monoid.Sum,
    ''Monoid.Product,
    ''Down
  ]
  [''SimpleMergeable, ''SimpleMergeable1]

derive
  [''(), ''AssertionError]
  [''SimpleMergeable]

instance SimpleMergeable (Proxy a) where
  mrgIte _ l _ = l
  {-# INLINE mrgIte #-}

instance SimpleMergeable1 Proxy where
  liftMrgIte _ _ l _ = l
  {-# INLINE liftMrgIte #-}

instance (SimpleMergeable b) => SimpleMergeable (a -> b) where
  mrgIte = mrgIte1
  {-# INLINE mrgIte #-}

instance SimpleMergeable1 ((->) a) where
  liftMrgIte ms cond t f v = ms cond (t v) (f v)
  {-# INLINE liftMrgIte #-}

instance SimpleMergeable2 (->) where
  liftMrgIte2 _ ms cond t f v = ms cond (t v) (f v)
  {-# INLINE liftMrgIte2 #-}

-- MaybeT
instance (SymBranching m, Mergeable a) => SimpleMergeable (MaybeT m a) where
  mrgIte = mrgIf
  {-# INLINE mrgIte #-}

instance (SymBranching m) => SimpleMergeable1 (MaybeT m) where
  liftMrgIte m = mrgIfWithStrategy (SimpleStrategy m)
  {-# INLINE liftMrgIte #-}

instance (SymBranching m) => SymBranching (MaybeT m) where
  mrgIfWithStrategy strategy cond (MaybeT l) (MaybeT r) =
    MaybeT $ mrgIfWithStrategy (liftRootStrategy strategy) cond l r
  {-# INLINE mrgIfWithStrategy #-}
  mrgIfPropagatedStrategy cond (MaybeT l) (MaybeT r) =
    MaybeT $ mrgIfPropagatedStrategy cond l r
  {-# INLINE mrgIfPropagatedStrategy #-}

-- ExceptT
instance
  (SymBranching m, Mergeable e, Mergeable a) =>
  SimpleMergeable (ExceptT e m a)
  where
  mrgIte = mrgIf
  {-# INLINE mrgIte #-}

instance
  (SymBranching m, Mergeable e) =>
  SimpleMergeable1 (ExceptT e m)
  where
  liftMrgIte m = mrgIfWithStrategy (SimpleStrategy m)
  {-# INLINE liftMrgIte #-}

instance
  (SymBranching m, Mergeable e) =>
  SymBranching (ExceptT e m)
  where
  mrgIfWithStrategy s cond (ExceptT t) (ExceptT f) =
    ExceptT $ mrgIfWithStrategy (liftRootStrategy s) cond t f
  {-# INLINE mrgIfWithStrategy #-}
  mrgIfPropagatedStrategy cond (ExceptT t) (ExceptT f) =
    ExceptT $ mrgIfPropagatedStrategy cond t f
  {-# INLINE mrgIfPropagatedStrategy #-}

-- StateT
instance
  (Mergeable s, Mergeable a, SymBranching m) =>
  SimpleMergeable (StateLazy.StateT s m a)
  where
  mrgIte = mrgIf
  {-# INLINE mrgIte #-}

instance
  (Mergeable s, SymBranching m) =>
  SimpleMergeable1 (StateLazy.StateT s m)
  where
  liftMrgIte m = mrgIfWithStrategy (SimpleStrategy m)
  {-# INLINE liftMrgIte #-}

instance
  (Mergeable s, SymBranching m) =>
  SymBranching (StateLazy.StateT s m)
  where
  mrgIfWithStrategy s cond (StateLazy.StateT t) (StateLazy.StateT f) =
    StateLazy.StateT $ \v ->
      mrgIfWithStrategy
        (liftRootStrategy2 s rootStrategy)
        cond
        (t v)
        (f v)
  {-# INLINE mrgIfWithStrategy #-}
  mrgIfPropagatedStrategy cond (StateLazy.StateT t) (StateLazy.StateT f) =
    StateLazy.StateT $ \v -> mrgIfPropagatedStrategy cond (t v) (f v)
  {-# INLINE mrgIfPropagatedStrategy #-}

instance
  (Mergeable s, Mergeable a, SymBranching m) =>
  SimpleMergeable (StateStrict.StateT s m a)
  where
  mrgIte = mrgIf
  {-# INLINE mrgIte #-}

instance
  (Mergeable s, SymBranching m) =>
  SimpleMergeable1 (StateStrict.StateT s m)
  where
  liftMrgIte m = mrgIfWithStrategy (SimpleStrategy m)
  {-# INLINE liftMrgIte #-}

instance
  (Mergeable s, SymBranching m) =>
  SymBranching (StateStrict.StateT s m)
  where
  mrgIfWithStrategy s cond (StateStrict.StateT t) (StateStrict.StateT f) =
    StateStrict.StateT $
      \v ->
        mrgIfWithStrategy (liftRootStrategy2 s rootStrategy) cond (t v) (f v)
  {-# INLINE mrgIfWithStrategy #-}
  mrgIfPropagatedStrategy cond (StateStrict.StateT t) (StateStrict.StateT f) =
    StateStrict.StateT $ \v -> mrgIfPropagatedStrategy cond (t v) (f v)
  {-# INLINE mrgIfPropagatedStrategy #-}

-- WriterT
instance
  (Mergeable s, Mergeable a, SymBranching m, Monoid s) =>
  SimpleMergeable (WriterLazy.WriterT s m a)
  where
  mrgIte = mrgIf
  {-# INLINE mrgIte #-}

instance
  (Mergeable s, SymBranching m, Monoid s) =>
  SimpleMergeable1 (WriterLazy.WriterT s m)
  where
  liftMrgIte m = mrgIfWithStrategy (SimpleStrategy m)
  {-# INLINE liftMrgIte #-}

instance
  (Mergeable s, SymBranching m, Monoid s) =>
  SymBranching (WriterLazy.WriterT s m)
  where
  mrgIfWithStrategy s cond (WriterLazy.WriterT t) (WriterLazy.WriterT f) =
    WriterLazy.WriterT $
      mrgIfWithStrategy (liftRootStrategy2 s rootStrategy) cond t f
  {-# INLINE mrgIfWithStrategy #-}
  mrgIfPropagatedStrategy cond (WriterLazy.WriterT t) (WriterLazy.WriterT f) =
    WriterLazy.WriterT $ mrgIfPropagatedStrategy cond t f
  {-# INLINE mrgIfPropagatedStrategy #-}

instance
  (Mergeable s, Mergeable a, SymBranching m, Monoid s) =>
  SimpleMergeable (WriterStrict.WriterT s m a)
  where
  mrgIte = mrgIf
  {-# INLINE mrgIte #-}

instance
  (Mergeable s, SymBranching m, Monoid s) =>
  SimpleMergeable1 (WriterStrict.WriterT s m)
  where
  liftMrgIte m = mrgIfWithStrategy (SimpleStrategy m)
  {-# INLINE liftMrgIte #-}

instance
  (Mergeable s, SymBranching m, Monoid s) =>
  SymBranching (WriterStrict.WriterT s m)
  where
  mrgIfWithStrategy s cond (WriterStrict.WriterT t) (WriterStrict.WriterT f) =
    WriterStrict.WriterT $
      mrgIfWithStrategy (liftRootStrategy2 s rootStrategy) cond t f
  {-# INLINE mrgIfWithStrategy #-}
  mrgIfPropagatedStrategy
    cond
    (WriterStrict.WriterT t)
    (WriterStrict.WriterT f) =
      WriterStrict.WriterT $ mrgIfPropagatedStrategy cond t f
  {-# INLINE mrgIfPropagatedStrategy #-}

-- ReaderT
instance
  (Mergeable a, SymBranching m) =>
  SimpleMergeable (ReaderT s m a)
  where
  mrgIte = mrgIf
  {-# INLINE mrgIte #-}

instance
  (SymBranching m) =>
  SimpleMergeable1 (ReaderT s m)
  where
  liftMrgIte m = mrgIfWithStrategy (SimpleStrategy m)
  {-# INLINE liftMrgIte #-}

instance
  (SymBranching m) =>
  SymBranching (ReaderT s m)
  where
  mrgIfWithStrategy s cond (ReaderT t) (ReaderT f) =
    ReaderT $ \v -> mrgIfWithStrategy s cond (t v) (f v)
  {-# INLINE mrgIfWithStrategy #-}
  mrgIfPropagatedStrategy cond (ReaderT t) (ReaderT f) =
    ReaderT $ \v -> mrgIfPropagatedStrategy cond (t v) (f v)
  {-# INLINE mrgIfPropagatedStrategy #-}

-- IdentityT
instance
  (SymBranching m, Mergeable a) =>
  SimpleMergeable (IdentityT m a)
  where
  mrgIte = mrgIf
  {-# INLINE mrgIte #-}

instance (SymBranching m) => SimpleMergeable1 (IdentityT m) where
  liftMrgIte m = mrgIfWithStrategy (SimpleStrategy m)
  {-# INLINE liftMrgIte #-}

instance (SymBranching m) => SymBranching (IdentityT m) where
  mrgIfWithStrategy s cond (IdentityT l) (IdentityT r) =
    IdentityT $ mrgIfWithStrategy s cond l r
  {-# INLINE mrgIfWithStrategy #-}
  mrgIfPropagatedStrategy cond (IdentityT l) (IdentityT r) =
    IdentityT $ mrgIfPropagatedStrategy cond l r
  {-# INLINE mrgIfPropagatedStrategy #-}

-- ContT
instance (SymBranching m, Mergeable r) => SimpleMergeable (ContT r m a) where
  mrgIte cond (ContT l) (ContT r) = ContT $ \c -> mrgIf cond (l c) (r c)
  {-# INLINE mrgIte #-}

instance (SymBranching m, Mergeable r) => SimpleMergeable1 (ContT r m) where
  liftMrgIte m = mrgIfWithStrategy (SimpleStrategy m)
  {-# INLINE liftMrgIte #-}

instance (SymBranching m, Mergeable r) => SymBranching (ContT r m) where
  mrgIfWithStrategy _ cond (ContT l) (ContT r) =
    ContT $ \c -> mrgIf cond (l c) (r c)
  {-# INLINE mrgIfWithStrategy #-}
  mrgIfPropagatedStrategy cond (ContT l) (ContT r) =
    ContT $ \c -> mrgIfPropagatedStrategy cond (l c) (r c)
  {-# INLINE mrgIfPropagatedStrategy #-}

-- RWST
instance
  (Mergeable s, Mergeable w, Monoid w, Mergeable a, SymBranching m) =>
  SimpleMergeable (RWSLazy.RWST r w s m a)
  where
  mrgIte = mrgIf
  {-# INLINE mrgIte #-}

instance
  (Mergeable s, Mergeable w, Monoid w, SymBranching m) =>
  SimpleMergeable1 (RWSLazy.RWST r w s m)
  where
  liftMrgIte m = mrgIfWithStrategy (SimpleStrategy m)
  {-# INLINE liftMrgIte #-}

instance
  (Mergeable s, Mergeable w, Monoid w, SymBranching m) =>
  SymBranching (RWSLazy.RWST r w s m)
  where
  mrgIfWithStrategy ms cond (RWSLazy.RWST t) (RWSLazy.RWST f) =
    RWSLazy.RWST $ \r s ->
      mrgIfWithStrategy
        (liftRootStrategy3 ms rootStrategy rootStrategy)
        cond
        (t r s)
        (f r s)
  {-# INLINE mrgIfWithStrategy #-}
  mrgIfPropagatedStrategy cond (RWSLazy.RWST t) (RWSLazy.RWST f) =
    RWSLazy.RWST $ \r s -> mrgIfPropagatedStrategy cond (t r s) (f r s)
  {-# INLINE mrgIfPropagatedStrategy #-}

instance
  (Mergeable s, Mergeable w, Monoid w, Mergeable a, SymBranching m) =>
  SimpleMergeable (RWSStrict.RWST r w s m a)
  where
  mrgIte = mrgIf
  {-# INLINE mrgIte #-}

instance
  (Mergeable s, Mergeable w, Monoid w, SymBranching m) =>
  SimpleMergeable1 (RWSStrict.RWST r w s m)
  where
  liftMrgIte m = mrgIfWithStrategy (SimpleStrategy m)
  {-# INLINE liftMrgIte #-}

instance
  (Mergeable s, Mergeable w, Monoid w, SymBranching m) =>
  SymBranching (RWSStrict.RWST r w s m)
  where
  mrgIfWithStrategy ms cond (RWSStrict.RWST t) (RWSStrict.RWST f) =
    RWSStrict.RWST $ \r s ->
      mrgIfWithStrategy
        (liftRootStrategy3 ms rootStrategy rootStrategy)
        cond
        (t r s)
        (f r s)
  {-# INLINE mrgIfWithStrategy #-}
  mrgIfPropagatedStrategy cond (RWSStrict.RWST t) (RWSStrict.RWST f) =
    RWSStrict.RWST $ \r s -> mrgIfPropagatedStrategy cond (t r s) (f r s)
  {-# INLINE mrgIfPropagatedStrategy #-}

-- Product
deriving via
  (Default (Product l r a))
  instance
    (SimpleMergeable (l a), SimpleMergeable (r a)) =>
    SimpleMergeable (Product l r a)

deriving via
  (Default1 (Product l r))
  instance
    (SimpleMergeable1 l, SimpleMergeable1 r) => SimpleMergeable1 (Product l r)

-- Compose
deriving via
  (Default (Compose f g a))
  instance
    (SimpleMergeable (f (g a))) =>
    SimpleMergeable (Compose f g a)

instance
  (SimpleMergeable1 f, SimpleMergeable1 g) =>
  SimpleMergeable1 (Compose f g)
  where
  liftMrgIte m cond (Compose l) (Compose r) =
    Compose $ liftMrgIte (liftMrgIte m) cond l r

-- Const
deriving via
  (Default (Const a b))
  instance
    (SimpleMergeable a) => SimpleMergeable (Const a b)

deriving via
  (Default1 (Const a))
  instance
    (SimpleMergeable a) => SimpleMergeable1 (Const a)

-- Alt
deriving via
  (Default (Alt f a))
  instance
    (SimpleMergeable (f a)) => SimpleMergeable (Alt f a)

deriving via
  (Default1 (Alt f))
  instance
    (SimpleMergeable1 f) => SimpleMergeable1 (Alt f)

-- Ap
deriving via
  (Default (Ap f a))
  instance
    (SimpleMergeable (f a)) => SimpleMergeable (Ap f a)

deriving via
  (Default1 (Ap f))
  instance
    (SimpleMergeable1 f) => SimpleMergeable1 (Ap f)

-- Endo
instance (SimpleMergeable a) => SimpleMergeable (Endo a) where
  mrgIte = mrgIte1
  {-# INLINE mrgIte #-}

instance SimpleMergeable1 Endo where
  liftMrgIte m cond (Endo l) (Endo r) = Endo $ liftMrgIte m cond l r
  {-# INLINE liftMrgIte #-}

-- Generic
deriving via (Default (U1 p)) instance SimpleMergeable (U1 p)

deriving via (Default (V1 p)) instance SimpleMergeable (V1 p)

deriving via
  (Default (K1 i c p))
  instance
    (SimpleMergeable c) => SimpleMergeable (K1 i c p)

deriving via
  (Default (M1 i c f p))
  instance
    (SimpleMergeable (f p)) => SimpleMergeable (M1 i c f p)

deriving via
  (Default ((f :*: g) p))
  instance
    (SimpleMergeable (f p), SimpleMergeable (g p)) =>
    SimpleMergeable ((f :*: g) p)

deriving via
  (Default (Par1 p))
  instance
    (SimpleMergeable p) => SimpleMergeable (Par1 p)

deriving via
  (Default (Rec1 f p))
  instance
    (SimpleMergeable (f p)) => SimpleMergeable (Rec1 f p)

deriving via
  (Default ((f :.: g) p))
  instance
    (SimpleMergeable (f (g p))) => SimpleMergeable ((f :.: g) p)

#define SIMPLE_MERGEABLE_SIMPLE(symtype) \
instance SimpleMergeable symtype where \
  mrgIte (SymBool c) (symtype t) (symtype f) = symtype $ pevalITETerm c t f; \
  {-# INLINE mrgIte #-}

#define SIMPLE_MERGEABLE_BV(symtype) \
instance (KnownNat n, 1 <= n) => SimpleMergeable (symtype n) where \
  mrgIte (SymBool c) (symtype t) (symtype f) = symtype $ pevalITETerm c t f; \
  {-# INLINE mrgIte #-}

#define SIMPLE_MERGEABLE_FUN(cop, op, cons) \
instance SimpleMergeable (op sa sb) where \
  mrgIte (SymBool c) (cons t) (cons f) = cons $ pevalITETerm c t f; \
  {-# INLINE mrgIte #-}

#if 1
SIMPLE_MERGEABLE_SIMPLE(SymInteger)
SIMPLE_MERGEABLE_SIMPLE(SymFPRoundingMode)
SIMPLE_MERGEABLE_SIMPLE(SymAlgReal)
SIMPLE_MERGEABLE_BV(SymIntN)
SIMPLE_MERGEABLE_BV(SymWordN)
SIMPLE_MERGEABLE_FUN((=->), (=~>), SymTabularFun)
SIMPLE_MERGEABLE_FUN((-->), (-~>), SymGeneralFun)
#endif

instance SimpleMergeable (a --> b) where
  mrgIte
    (SymBool c)
    (GeneralFun (ta :: TypedConstantSymbol a) a)
    (GeneralFun tb b) =
      GeneralFun argSymbol $
        pevalITETerm
          c
          (substTerm ta (symTerm argSymbol) HS.empty a)
          (substTerm tb (symTerm argSymbol) HS.empty b)
      where
        argSymbol :: TypedConstantSymbol a
        argSymbol = freshArgSymbol [SomeTerm a, SomeTerm b]
  {-# INLINE mrgIte #-}

instance (ValidFP eb sb) => SimpleMergeable (SymFP eb sb) where
  mrgIte (SymBool c) (SymFP t) (SymFP f) = SymFP $ pevalITETerm c t f
  {-# INLINE mrgIte #-}
