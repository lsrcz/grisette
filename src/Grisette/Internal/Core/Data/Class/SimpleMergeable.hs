{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      :   Grisette.Internal.Core.Data.Class.SimpleMergeable
-- Copyright   :   (c) Sirui Lu 2021-2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Internal.Core.Data.Class.SimpleMergeable
  ( -- * Simple mergeable types
    SimpleMergeable (..),
    SimpleMergeable1 (..),
    mrgIte1,
    SimpleMergeable2 (..),
    mrgIte2,
    UnionMergeable1 (..),
    mrgIf,
    mergeWithStrategy,
    merge,
  )
where

import Control.Monad.Except (ExceptT (ExceptT))
import Control.Monad.Identity
  ( Identity (Identity),
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
import Data.Kind (Type)
import GHC.Generics
  ( Generic (Rep, from, to),
    K1 (K1),
    M1 (M1),
    U1,
    V1,
    type (:*:) ((:*:)),
  )
import GHC.TypeNats (KnownNat, type (<=))
import Generics.Deriving (Default (Default))
import Grisette.Internal.Core.Control.Exception (AssertionError)
import Grisette.Internal.Core.Data.Class.ITEOp (ITEOp (symIte))
import Grisette.Internal.Core.Data.Class.Mergeable
  ( Mergeable (rootStrategy),
    Mergeable',
    Mergeable1 (liftRootStrategy),
    Mergeable2 (liftRootStrategy2),
    Mergeable3 (liftRootStrategy3),
    MergingStrategy (SimpleStrategy),
  )
import Grisette.Internal.Core.Data.Class.TryMerge
  ( TryMerge (tryMergeWithStrategy),
  )
import Grisette.Internal.SymPrim.FP (ValidFP)
import Grisette.Internal.SymPrim.GeneralFun (type (-->))
import Grisette.Internal.SymPrim.Prim.Term
  ( LinkedRep,
    SupportedPrim,
  )
import Grisette.Internal.SymPrim.SymBV
  ( SymIntN,
    SymWordN,
  )
import Grisette.Internal.SymPrim.SymBool (SymBool)
import Grisette.Internal.SymPrim.SymFP (SymFP, SymFPRoundingMode)
import Grisette.Internal.SymPrim.SymGeneralFun (type (-~>))
import Grisette.Internal.SymPrim.SymInteger (SymInteger)
import Grisette.Internal.SymPrim.SymTabularFun (type (=~>))
import Grisette.Internal.SymPrim.TabularFun (type (=->))

-- $setup
-- >>> import Grisette.Core
-- >>> import Grisette.SymPrim
-- >>> import Control.Monad.Identity

-- | Auxiliary class for the generic derivation for the 'SimpleMergeable' class.
class SimpleMergeable' f where
  mrgIte' :: SymBool -> f a -> f a -> f a

instance (SimpleMergeable' U1) where
  mrgIte' _ t _ = t
  {-# INLINE mrgIte' #-}

instance (SimpleMergeable' V1) where
  mrgIte' _ t _ = t
  {-# INLINE mrgIte' #-}

instance (SimpleMergeable c) => (SimpleMergeable' (K1 i c)) where
  mrgIte' cond (K1 a) (K1 b) = K1 $ mrgIte cond a b
  {-# INLINE mrgIte' #-}

instance (SimpleMergeable' a) => (SimpleMergeable' (M1 i c a)) where
  mrgIte' cond (M1 a) (M1 b) = M1 $ mrgIte' cond a b
  {-# INLINE mrgIte' #-}

instance (SimpleMergeable' a, SimpleMergeable' b) => (SimpleMergeable' (a :*: b)) where
  mrgIte' cond (a1 :*: a2) (b1 :*: b2) = mrgIte' cond a1 b1 :*: mrgIte' cond a2 b2
  {-# INLINE mrgIte' #-}

-- | This class indicates that a type has a simple root merge strategy.
--
-- __Note:__ This type class can be derived for algebraic data types.
-- You may need the @DerivingVia@ and @DerivingStrategies@ extensions.
--
-- > data X = ...
-- >   deriving Generic
-- >   deriving (Mergeable, SimpleMergeable) via (Default X)
class (Mergeable a) => SimpleMergeable a where
  -- | Performs if-then-else with the simple root merge strategy.
  --
  -- >>> mrgIte "a" "b" "c" :: SymInteger
  -- (ite a b c)
  mrgIte :: SymBool -> a -> a -> a

instance (Generic a, Mergeable' (Rep a), SimpleMergeable' (Rep a)) => SimpleMergeable (Default a) where
  mrgIte cond (Default a) (Default b) = Default $ to $ mrgIte' cond (from a) (from b)
  {-# INLINE mrgIte #-}

-- | Lifting of the 'SimpleMergeable' class to unary type constructors.
class (Mergeable1 u) => SimpleMergeable1 u where
  -- | Lift 'mrgIte' through the type constructor.
  --
  -- >>> liftMrgIte mrgIte "a" (Identity "b") (Identity "c") :: Identity SymInteger
  -- Identity (ite a b c)
  liftMrgIte :: (SymBool -> a -> a -> a) -> SymBool -> u a -> u a -> u a

-- | Lift the standard 'mrgIte' function through the type constructor.
--
-- >>> mrgIte1 "a" (Identity "b") (Identity "c") :: Identity SymInteger
-- Identity (ite a b c)
mrgIte1 :: (SimpleMergeable1 u, SimpleMergeable a) => SymBool -> u a -> u a -> u a
mrgIte1 = liftMrgIte mrgIte
{-# INLINE mrgIte1 #-}

-- | Lifting of the 'SimpleMergeable' class to binary type constructors.
class (Mergeable2 u) => SimpleMergeable2 u where
  -- | Lift 'mrgIte' through the type constructor.
  --
  -- >>> liftMrgIte2 mrgIte mrgIte "a" ("b", "c") ("d", "e") :: (SymInteger, SymBool)
  -- ((ite a b d),(ite a c e))
  liftMrgIte2 :: (SymBool -> a -> a -> a) -> (SymBool -> b -> b -> b) -> SymBool -> u a b -> u a b -> u a b

-- | Lift the standard 'mrgIte' function through the type constructor.
--
-- >>> mrgIte2 "a" ("b", "c") ("d", "e") :: (SymInteger, SymBool)
-- ((ite a b d),(ite a c e))
mrgIte2 :: (SimpleMergeable2 u, SimpleMergeable a, SimpleMergeable b) => SymBool -> u a b -> u a b -> u a b
mrgIte2 = liftMrgIte2 mrgIte mrgIte
{-# INLINE mrgIte2 #-}

-- | Special case of the 'Mergeable1' and 'SimpleMergeable1' class for type
-- constructors that are 'SimpleMergeable' when applied to any 'Mergeable'
-- types.
--
-- This type class is used to generalize the 'mrgIf' function to other
-- containers, for example, monad transformer transformed Unions.
class (SimpleMergeable1 u, TryMerge u) => UnionMergeable1 (u :: Type -> Type) where
  -- | Symbolic @if@ control flow with the result merged with some merge strategy.
  --
  -- >>> mrgIfWithStrategy rootStrategy "a" (mrgSingle "b") (return "c") :: UnionM SymInteger
  -- {(ite a b c)}
  --
  -- __Note:__ Be careful to call this directly in your code.
  -- The supplied merge strategy should be consistent with the type's root merge strategy,
  -- or some internal invariants would be broken and the program can crash.
  --
  -- This function is to be called when the 'Mergeable' constraint can not be resolved,
  -- e.g., the merge strategy for the contained type is given with 'Mergeable1'.
  -- In other cases, 'mrgIf' is usually a better alternative.
  mrgIfWithStrategy :: MergingStrategy a -> SymBool -> u a -> u a -> u a

  mrgIfPropagatedStrategy :: SymBool -> u a -> u a -> u a

mergeWithStrategy :: (UnionMergeable1 m) => MergingStrategy a -> m a -> m a
mergeWithStrategy = tryMergeWithStrategy
{-# INLINE mergeWithStrategy #-}

-- | Try to merge the container with the root strategy.
merge :: (UnionMergeable1 m, Mergeable a) => m a -> m a
merge = mergeWithStrategy rootStrategy
{-# INLINE merge #-}

-- | Symbolic @if@ control flow with the result merged with the type's root merge strategy.
--
-- Equivalent to @'mrgIfWithStrategy' 'rootStrategy'@.
--
-- >>> mrgIf "a" (return "b") (return "c") :: UnionM SymInteger
-- {(ite a b c)}
mrgIf :: (UnionMergeable1 u, Mergeable a) => SymBool -> u a -> u a -> u a
mrgIf = mrgIfWithStrategy rootStrategy
{-# INLINE mrgIf #-}

instance SimpleMergeable () where
  mrgIte _ t _ = t
  {-# INLINE mrgIte #-}

instance (SimpleMergeable a, SimpleMergeable b) => SimpleMergeable (a, b) where
  mrgIte cond (a1, b1) (a2, b2) = (mrgIte cond a1 a2, mrgIte cond b1 b2)
  {-# INLINE mrgIte #-}

instance (SimpleMergeable a) => SimpleMergeable1 ((,) a) where
  liftMrgIte mb cond (a1, b1) (a2, b2) = (mrgIte cond a1 a2, mb cond b1 b2)
  {-# INLINE liftMrgIte #-}

instance SimpleMergeable2 (,) where
  liftMrgIte2 ma mb cond (a1, b1) (a2, b2) = (ma cond a1 a2, mb cond b1 b2)
  {-# INLINE liftMrgIte2 #-}

instance
  (SimpleMergeable a, SimpleMergeable b, SimpleMergeable c) =>
  SimpleMergeable (a, b, c)
  where
  mrgIte cond (a1, b1, c1) (a2, b2, c2) = (mrgIte cond a1 a2, mrgIte cond b1 b2, mrgIte cond c1 c2)
  {-# INLINE mrgIte #-}

instance
  ( SimpleMergeable a,
    SimpleMergeable b,
    SimpleMergeable c,
    SimpleMergeable d
  ) =>
  SimpleMergeable (a, b, c, d)
  where
  mrgIte cond (a1, b1, c1, d1) (a2, b2, c2, d2) =
    (mrgIte cond a1 a2, mrgIte cond b1 b2, mrgIte cond c1 c2, mrgIte cond d1 d2)
  {-# INLINE mrgIte #-}

instance
  ( SimpleMergeable a,
    SimpleMergeable b,
    SimpleMergeable c,
    SimpleMergeable d,
    SimpleMergeable e
  ) =>
  SimpleMergeable (a, b, c, d, e)
  where
  mrgIte cond (a1, b1, c1, d1, e1) (a2, b2, c2, d2, e2) =
    (mrgIte cond a1 a2, mrgIte cond b1 b2, mrgIte cond c1 c2, mrgIte cond d1 d2, mrgIte cond e1 e2)
  {-# INLINE mrgIte #-}

instance
  ( SimpleMergeable a,
    SimpleMergeable b,
    SimpleMergeable c,
    SimpleMergeable d,
    SimpleMergeable e,
    SimpleMergeable f
  ) =>
  SimpleMergeable (a, b, c, d, e, f)
  where
  mrgIte cond (a1, b1, c1, d1, e1, f1) (a2, b2, c2, d2, e2, f2) =
    (mrgIte cond a1 a2, mrgIte cond b1 b2, mrgIte cond c1 c2, mrgIte cond d1 d2, mrgIte cond e1 e2, mrgIte cond f1 f2)
  {-# INLINE mrgIte #-}

instance
  ( SimpleMergeable a,
    SimpleMergeable b,
    SimpleMergeable c,
    SimpleMergeable d,
    SimpleMergeable e,
    SimpleMergeable f,
    SimpleMergeable g
  ) =>
  SimpleMergeable (a, b, c, d, e, f, g)
  where
  mrgIte cond (a1, b1, c1, d1, e1, f1, g1) (a2, b2, c2, d2, e2, f2, g2) =
    ( mrgIte cond a1 a2,
      mrgIte cond b1 b2,
      mrgIte cond c1 c2,
      mrgIte cond d1 d2,
      mrgIte cond e1 e2,
      mrgIte cond f1 f2,
      mrgIte cond g1 g2
    )
  {-# INLINE mrgIte #-}

instance
  ( SimpleMergeable a,
    SimpleMergeable b,
    SimpleMergeable c,
    SimpleMergeable d,
    SimpleMergeable e,
    SimpleMergeable f,
    SimpleMergeable g,
    SimpleMergeable h
  ) =>
  SimpleMergeable (a, b, c, d, e, f, g, h)
  where
  mrgIte cond (a1, b1, c1, d1, e1, f1, g1, h1) (a2, b2, c2, d2, e2, f2, g2, h2) =
    ( mrgIte cond a1 a2,
      mrgIte cond b1 b2,
      mrgIte cond c1 c2,
      mrgIte cond d1 d2,
      mrgIte cond e1 e2,
      mrgIte cond f1 f2,
      mrgIte cond g1 g2,
      mrgIte cond h1 h2
    )
  {-# INLINE mrgIte #-}

instance (SimpleMergeable b) => SimpleMergeable (a -> b) where
  mrgIte = mrgIte1
  {-# INLINE mrgIte #-}

instance SimpleMergeable1 ((->) a) where
  liftMrgIte ms cond t f v = ms cond (t v) (f v)
  {-# INLINE liftMrgIte #-}

instance (UnionMergeable1 m, Mergeable a) => SimpleMergeable (MaybeT m a) where
  mrgIte = mrgIf
  {-# INLINE mrgIte #-}

instance (UnionMergeable1 m) => SimpleMergeable1 (MaybeT m) where
  liftMrgIte m = mrgIfWithStrategy (SimpleStrategy m)
  {-# INLINE liftMrgIte #-}

instance (UnionMergeable1 m) => UnionMergeable1 (MaybeT m) where
  mrgIfWithStrategy strategy cond (MaybeT l) (MaybeT r) =
    MaybeT $ mrgIfWithStrategy (liftRootStrategy strategy) cond l r
  {-# INLINE mrgIfWithStrategy #-}
  mrgIfPropagatedStrategy cond (MaybeT l) (MaybeT r) =
    MaybeT $ mrgIfPropagatedStrategy cond l r
  {-# INLINE mrgIfPropagatedStrategy #-}

instance
  (UnionMergeable1 m, Mergeable e, Mergeable a) =>
  SimpleMergeable (ExceptT e m a)
  where
  mrgIte = mrgIf
  {-# INLINE mrgIte #-}

instance
  (UnionMergeable1 m, Mergeable e) =>
  SimpleMergeable1 (ExceptT e m)
  where
  liftMrgIte m = mrgIfWithStrategy (SimpleStrategy m)
  {-# INLINE liftMrgIte #-}

instance
  (UnionMergeable1 m, Mergeable e) =>
  UnionMergeable1 (ExceptT e m)
  where
  mrgIfWithStrategy s cond (ExceptT t) (ExceptT f) =
    ExceptT $ mrgIfWithStrategy (liftRootStrategy s) cond t f
  {-# INLINE mrgIfWithStrategy #-}
  mrgIfPropagatedStrategy cond (ExceptT t) (ExceptT f) =
    ExceptT $ mrgIfPropagatedStrategy cond t f
  {-# INLINE mrgIfPropagatedStrategy #-}

instance
  (Mergeable s, Mergeable a, UnionMergeable1 m) =>
  SimpleMergeable (StateLazy.StateT s m a)
  where
  mrgIte = mrgIf
  {-# INLINE mrgIte #-}

instance
  (Mergeable s, UnionMergeable1 m) =>
  SimpleMergeable1 (StateLazy.StateT s m)
  where
  liftMrgIte m = mrgIfWithStrategy (SimpleStrategy m)
  {-# INLINE liftMrgIte #-}

instance
  (Mergeable s, UnionMergeable1 m) =>
  UnionMergeable1 (StateLazy.StateT s m)
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
  (Mergeable s, Mergeable a, UnionMergeable1 m) =>
  SimpleMergeable (StateStrict.StateT s m a)
  where
  mrgIte = mrgIf
  {-# INLINE mrgIte #-}

instance
  (Mergeable s, UnionMergeable1 m) =>
  SimpleMergeable1 (StateStrict.StateT s m)
  where
  liftMrgIte m = mrgIfWithStrategy (SimpleStrategy m)
  {-# INLINE liftMrgIte #-}

instance
  (Mergeable s, UnionMergeable1 m) =>
  UnionMergeable1 (StateStrict.StateT s m)
  where
  mrgIfWithStrategy s cond (StateStrict.StateT t) (StateStrict.StateT f) =
    StateStrict.StateT $
      \v ->
        mrgIfWithStrategy (liftRootStrategy2 s rootStrategy) cond (t v) (f v)
  {-# INLINE mrgIfWithStrategy #-}
  mrgIfPropagatedStrategy cond (StateStrict.StateT t) (StateStrict.StateT f) =
    StateStrict.StateT $ \v -> mrgIfPropagatedStrategy cond (t v) (f v)
  {-# INLINE mrgIfPropagatedStrategy #-}

instance
  (Mergeable s, Mergeable a, UnionMergeable1 m, Monoid s) =>
  SimpleMergeable (WriterLazy.WriterT s m a)
  where
  mrgIte = mrgIf
  {-# INLINE mrgIte #-}

instance
  (Mergeable s, UnionMergeable1 m, Monoid s) =>
  SimpleMergeable1 (WriterLazy.WriterT s m)
  where
  liftMrgIte m = mrgIfWithStrategy (SimpleStrategy m)
  {-# INLINE liftMrgIte #-}

instance
  (Mergeable s, UnionMergeable1 m, Monoid s) =>
  UnionMergeable1 (WriterLazy.WriterT s m)
  where
  mrgIfWithStrategy s cond (WriterLazy.WriterT t) (WriterLazy.WriterT f) =
    WriterLazy.WriterT $
      mrgIfWithStrategy (liftRootStrategy2 s rootStrategy) cond t f
  {-# INLINE mrgIfWithStrategy #-}
  mrgIfPropagatedStrategy cond (WriterLazy.WriterT t) (WriterLazy.WriterT f) =
    WriterLazy.WriterT $ mrgIfPropagatedStrategy cond t f
  {-# INLINE mrgIfPropagatedStrategy #-}

instance
  (Mergeable s, Mergeable a, UnionMergeable1 m, Monoid s) =>
  SimpleMergeable (WriterStrict.WriterT s m a)
  where
  mrgIte = mrgIf
  {-# INLINE mrgIte #-}

instance
  (Mergeable s, UnionMergeable1 m, Monoid s) =>
  SimpleMergeable1 (WriterStrict.WriterT s m)
  where
  liftMrgIte m = mrgIfWithStrategy (SimpleStrategy m)
  {-# INLINE liftMrgIte #-}

instance
  (Mergeable s, UnionMergeable1 m, Monoid s) =>
  UnionMergeable1 (WriterStrict.WriterT s m)
  where
  mrgIfWithStrategy s cond (WriterStrict.WriterT t) (WriterStrict.WriterT f) =
    WriterStrict.WriterT $
      mrgIfWithStrategy (liftRootStrategy2 s rootStrategy) cond t f
  {-# INLINE mrgIfWithStrategy #-}
  mrgIfPropagatedStrategy cond (WriterStrict.WriterT t) (WriterStrict.WriterT f) =
    WriterStrict.WriterT $ mrgIfPropagatedStrategy cond t f
  {-# INLINE mrgIfPropagatedStrategy #-}

instance
  (Mergeable a, UnionMergeable1 m) =>
  SimpleMergeable (ReaderT s m a)
  where
  mrgIte = mrgIf
  {-# INLINE mrgIte #-}

instance
  (UnionMergeable1 m) =>
  SimpleMergeable1 (ReaderT s m)
  where
  liftMrgIte m = mrgIfWithStrategy (SimpleStrategy m)
  {-# INLINE liftMrgIte #-}

instance
  (UnionMergeable1 m) =>
  UnionMergeable1 (ReaderT s m)
  where
  mrgIfWithStrategy s cond (ReaderT t) (ReaderT f) =
    ReaderT $ \v -> mrgIfWithStrategy s cond (t v) (f v)
  {-# INLINE mrgIfWithStrategy #-}
  mrgIfPropagatedStrategy cond (ReaderT t) (ReaderT f) =
    ReaderT $ \v -> mrgIfPropagatedStrategy cond (t v) (f v)
  {-# INLINE mrgIfPropagatedStrategy #-}

instance (SimpleMergeable a) => SimpleMergeable (Identity a) where
  mrgIte = mrgIte1
  {-# INLINE mrgIte #-}

instance SimpleMergeable1 Identity where
  liftMrgIte mite cond (Identity l) (Identity r) = Identity $ mite cond l r
  {-# INLINE liftMrgIte #-}

instance
  (UnionMergeable1 m, Mergeable a) =>
  SimpleMergeable (IdentityT m a)
  where
  mrgIte = mrgIf
  {-# INLINE mrgIte #-}

instance (UnionMergeable1 m) => SimpleMergeable1 (IdentityT m) where
  liftMrgIte m = mrgIfWithStrategy (SimpleStrategy m)
  {-# INLINE liftMrgIte #-}

instance (UnionMergeable1 m) => UnionMergeable1 (IdentityT m) where
  mrgIfWithStrategy s cond (IdentityT l) (IdentityT r) =
    IdentityT $ mrgIfWithStrategy s cond l r
  {-# INLINE mrgIfWithStrategy #-}
  mrgIfPropagatedStrategy cond (IdentityT l) (IdentityT r) =
    IdentityT $ mrgIfPropagatedStrategy cond l r
  {-# INLINE mrgIfPropagatedStrategy #-}

instance (UnionMergeable1 m, Mergeable r) => SimpleMergeable (ContT r m a) where
  mrgIte cond (ContT l) (ContT r) = ContT $ \c -> mrgIf cond (l c) (r c)
  {-# INLINE mrgIte #-}

instance (UnionMergeable1 m, Mergeable r) => SimpleMergeable1 (ContT r m) where
  liftMrgIte m = mrgIfWithStrategy (SimpleStrategy m)
  {-# INLINE liftMrgIte #-}

instance (UnionMergeable1 m, Mergeable r) => UnionMergeable1 (ContT r m) where
  mrgIfWithStrategy _ cond (ContT l) (ContT r) =
    ContT $ \c -> mrgIf cond (l c) (r c)
  {-# INLINE mrgIfWithStrategy #-}
  mrgIfPropagatedStrategy cond (ContT l) (ContT r) =
    ContT $ \c -> mrgIfPropagatedStrategy cond (l c) (r c)
  {-# INLINE mrgIfPropagatedStrategy #-}

instance
  (Mergeable s, Mergeable w, Monoid w, Mergeable a, UnionMergeable1 m) =>
  SimpleMergeable (RWSLazy.RWST r w s m a)
  where
  mrgIte = mrgIf
  {-# INLINE mrgIte #-}

instance
  (Mergeable s, Mergeable w, Monoid w, UnionMergeable1 m) =>
  SimpleMergeable1 (RWSLazy.RWST r w s m)
  where
  liftMrgIte m = mrgIfWithStrategy (SimpleStrategy m)
  {-# INLINE liftMrgIte #-}

instance
  (Mergeable s, Mergeable w, Monoid w, UnionMergeable1 m) =>
  UnionMergeable1 (RWSLazy.RWST r w s m)
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
  (Mergeable s, Mergeable w, Monoid w, Mergeable a, UnionMergeable1 m) =>
  SimpleMergeable (RWSStrict.RWST r w s m a)
  where
  mrgIte = mrgIf
  {-# INLINE mrgIte #-}

instance
  (Mergeable s, Mergeable w, Monoid w, UnionMergeable1 m) =>
  SimpleMergeable1 (RWSStrict.RWST r w s m)
  where
  liftMrgIte m = mrgIfWithStrategy (SimpleStrategy m)
  {-# INLINE liftMrgIte #-}

instance
  (Mergeable s, Mergeable w, Monoid w, UnionMergeable1 m) =>
  UnionMergeable1 (RWSStrict.RWST r w s m)
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

#define SIMPLE_MERGEABLE_SIMPLE(symtype) \
instance SimpleMergeable symtype where \
  mrgIte = symIte; \
  {-# INLINE mrgIte #-}

#define SIMPLE_MERGEABLE_BV(symtype) \
instance (KnownNat n, 1 <= n) => SimpleMergeable (symtype n) where \
  mrgIte = symIte; \
  {-# INLINE mrgIte #-}

#define SIMPLE_MERGEABLE_FUN(cop, op) \
instance (SupportedPrim (cop ca cb), LinkedRep ca sa, LinkedRep cb sb) => \
  SimpleMergeable (op sa sb) where \
  mrgIte = symIte; \
  {-# INLINE mrgIte #-}

#if 1
SIMPLE_MERGEABLE_SIMPLE(SymBool)
SIMPLE_MERGEABLE_SIMPLE(SymInteger)
SIMPLE_MERGEABLE_SIMPLE(SymFPRoundingMode)
SIMPLE_MERGEABLE_BV(SymIntN)
SIMPLE_MERGEABLE_BV(SymWordN)
SIMPLE_MERGEABLE_FUN((=->), (=~>))
SIMPLE_MERGEABLE_FUN((-->), (-~>))
#endif

instance (ValidFP eb sb) => SimpleMergeable (SymFP eb sb) where
  mrgIte = symIte
  {-# INLINE mrgIte #-}

-- Exception
deriving via (Default AssertionError) instance SimpleMergeable AssertionError
