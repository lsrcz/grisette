{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- Module      :   Grisette.Core.Data.Class.SimpleMergeable
-- Copyright   :   (c) Sirui Lu 2021-2023
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Core.Data.Class.SimpleMergeable
  ( -- * Simple mergeable types
    SimpleMergeable (..),
    SimpleMergeable1 (..),
    mrgIte1,
    SimpleMergeable2 (..),
    mrgIte2,

    -- * UnionLike operations
    UnionLike (..),
    mrgIf,
    merge,
    mrgSingle,
    UnionPrjOp (..),
    pattern Single,
    pattern If,
    simpleMerge,
    onUnion,
    onUnion2,
    onUnion3,
    onUnion4,
    (#~),
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
import Data.Bifunctor (Bifunctor (first))
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
import Grisette.Core.Data.Class.Bool (ITEOp (ites), LogicalOp (nots, (&&~)))
import Grisette.Core.Data.Class.Function (Function (Arg, Ret, (#)))
import Grisette.Core.Data.Class.Mergeable
  ( Mergeable (rootStrategy),
    Mergeable',
    Mergeable1 (liftRootStrategy),
    Mergeable2 (liftRootStrategy2),
    Mergeable3 (liftRootStrategy3),
    MergingStrategy (SimpleStrategy),
  )
import Grisette.Core.Data.Class.Solvable (Solvable (con))
import Grisette.IR.SymPrim.Data.Prim.InternedTerm.Term
  ( LinkedRep,
    SupportedPrim,
  )
import {-# SOURCE #-} Grisette.IR.SymPrim.Data.SymPrim
  ( SymBool,
    SymIntN,
    SymInteger,
    SymWordN,
    type (-~>),
    type (=~>),
  )

-- $setup
-- >>> import Grisette.Core
-- >>> import Grisette.IR.SymPrim
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
class SimpleMergeable1 u where
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
class (SimpleMergeable1 u, Mergeable1 u) => UnionLike u where
  -- | Wrap a single value in the union.
  --
  -- Note that this function cannot propagate the 'Mergeable' knowledge.
  --
  -- >>> single "a" :: UnionM SymInteger
  -- <a>
  -- >>> mrgSingle "a" :: UnionM SymInteger
  -- {a}
  single :: a -> u a

  -- | If-then-else on two union values.
  --
  -- Note that this function cannot capture the 'Mergeable' knowledge. However,
  -- it may use the merging strategy from the branches to merge the results.
  --
  -- >>> unionIf "a" (single "b") (single "c") :: UnionM SymInteger
  -- <If a b c>
  -- >>> unionIf "a" (mrgSingle "b") (single "c") :: UnionM SymInteger
  -- {(ite a b c)}
  unionIf :: SymBool -> u a -> u a -> u a

  -- | Merge the contents with some merge strategy.
  --
  -- >>> mergeWithStrategy rootStrategy $ unionIf "a" (single "b") (single "c") :: UnionM SymInteger
  -- {(ite a b c)}
  --
  -- __Note:__ Be careful to call this directly in your code.
  -- The supplied merge strategy should be consistent with the type's root merge strategy,
  -- or some internal invariants would be broken and the program can crash.
  --
  -- This function is to be called when the 'Mergeable' constraint can not be resolved,
  -- e.g., the merge strategy for the contained type is given with 'Mergeable1'.
  -- In other cases, 'merge' is usually a better alternative.
  mergeWithStrategy :: MergingStrategy a -> u a -> u a

  -- | Symbolic @if@ control flow with the result merged with some merge strategy.
  --
  -- >>> mrgIfWithStrategy rootStrategy "a" (mrgSingle "b") (single "c") :: UnionM SymInteger
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
  mrgIfWithStrategy s cond l r = mergeWithStrategy s $ unionIf cond l r
  {-# INLINE mrgIfWithStrategy #-}

  -- | Wrap a single value in the union and capture the 'Mergeable' knowledge.
  --
  -- >>> mrgSingleWithStrategy rootStrategy "a" :: UnionM SymInteger
  -- {a}
  --
  -- __Note:__ Be careful to call this directly in your code.
  -- The supplied merge strategy should be consistent with the type's root merge strategy,
  -- or some internal invariants would be broken and the program can crash.
  --
  -- This function is to be called when the 'Mergeable' constraint can not be resolved,
  -- e.g., the merge strategy for the contained type is given with 'Mergeable1'.
  -- In other cases, 'mrgSingle' is usually a better alternative.
  mrgSingleWithStrategy :: MergingStrategy a -> a -> u a
  mrgSingleWithStrategy s = mergeWithStrategy s . single
  {-# INLINE mrgSingleWithStrategy #-}

-- | Symbolic @if@ control flow with the result merged with the type's root merge strategy.
--
-- Equivalent to @'mrgIfWithStrategy' 'rootStrategy'@.
--
-- >>> mrgIf "a" (single "b") (single "c") :: UnionM SymInteger
-- {(ite a b c)}
mrgIf :: (UnionLike u, Mergeable a) => SymBool -> u a -> u a -> u a
mrgIf = mrgIfWithStrategy rootStrategy
{-# INLINE mrgIf #-}

-- | Merge the contents with the type's root merge strategy.
--
-- Equivalent to @'mergeWithStrategy' 'rootStrategy'@.
--
-- >>> merge $ unionIf "a" (single "b") (single "c") :: UnionM SymInteger
-- {(ite a b c)}
merge :: (UnionLike u, Mergeable a) => u a -> u a
merge = mergeWithStrategy rootStrategy
{-# INLINE merge #-}

-- | Wrap a single value in the type and propagate the type's root merge strategy.
--
-- Equivalent to @'mrgSingleWithStrategy' 'rootStrategy'@.
--
-- >>> mrgSingle "a" :: UnionM SymInteger
-- {a}
mrgSingle :: (UnionLike u, Mergeable a) => a -> u a
mrgSingle = mrgSingleWithStrategy rootStrategy
{-# INLINE mrgSingle #-}

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

instance (UnionLike m, Mergeable a) => SimpleMergeable (MaybeT m a) where
  mrgIte = mrgIf
  {-# INLINE mrgIte #-}

instance (UnionLike m) => SimpleMergeable1 (MaybeT m) where
  liftMrgIte m = mrgIfWithStrategy (SimpleStrategy m)
  {-# INLINE liftMrgIte #-}

instance (UnionLike m) => UnionLike (MaybeT m) where
  mergeWithStrategy s (MaybeT v) = MaybeT $ mergeWithStrategy (liftRootStrategy s) v
  {-# INLINE mergeWithStrategy #-}
  mrgIfWithStrategy s cond (MaybeT t) (MaybeT f) = MaybeT $ mrgIfWithStrategy (liftRootStrategy s) cond t f
  {-# INLINE mrgIfWithStrategy #-}
  single = MaybeT . single . return
  {-# INLINE single #-}
  unionIf cond (MaybeT l) (MaybeT r) = MaybeT $ unionIf cond l r
  {-# INLINE unionIf #-}

instance
  (UnionLike m, Mergeable e, Mergeable a) =>
  SimpleMergeable (ExceptT e m a)
  where
  mrgIte = mrgIf
  {-# INLINE mrgIte #-}

instance
  (UnionLike m, Mergeable e) =>
  SimpleMergeable1 (ExceptT e m)
  where
  liftMrgIte m = mrgIfWithStrategy (SimpleStrategy m)
  {-# INLINE liftMrgIte #-}

instance
  (UnionLike m, Mergeable e) =>
  UnionLike (ExceptT e m)
  where
  mergeWithStrategy s (ExceptT v) = ExceptT $ mergeWithStrategy (liftRootStrategy s) v
  {-# INLINE mergeWithStrategy #-}
  mrgIfWithStrategy s cond (ExceptT t) (ExceptT f) = ExceptT $ mrgIfWithStrategy (liftRootStrategy s) cond t f
  {-# INLINE mrgIfWithStrategy #-}
  single = ExceptT . single . return
  {-# INLINE single #-}
  unionIf cond (ExceptT l) (ExceptT r) = ExceptT $ unionIf cond l r
  {-# INLINE unionIf #-}

instance
  (Mergeable s, Mergeable a, UnionLike m) =>
  SimpleMergeable (StateLazy.StateT s m a)
  where
  mrgIte = mrgIf
  {-# INLINE mrgIte #-}

instance
  (Mergeable s, UnionLike m) =>
  SimpleMergeable1 (StateLazy.StateT s m)
  where
  liftMrgIte m = mrgIfWithStrategy (SimpleStrategy m)
  {-# INLINE liftMrgIte #-}

instance
  (Mergeable s, UnionLike m) =>
  UnionLike (StateLazy.StateT s m)
  where
  mergeWithStrategy ms (StateLazy.StateT f) =
    StateLazy.StateT $ \v -> mergeWithStrategy (liftRootStrategy2 ms rootStrategy) $ f v
  {-# INLINE mergeWithStrategy #-}
  mrgIfWithStrategy s cond (StateLazy.StateT t) (StateLazy.StateT f) =
    StateLazy.StateT $ \v -> mrgIfWithStrategy (liftRootStrategy2 s rootStrategy) cond (t v) (f v)
  {-# INLINE mrgIfWithStrategy #-}
  single x = StateLazy.StateT $ \s -> single (x, s)
  {-# INLINE single #-}
  unionIf cond (StateLazy.StateT l) (StateLazy.StateT r) =
    StateLazy.StateT $ \s -> unionIf cond (l s) (r s)
  {-# INLINE unionIf #-}

instance
  (Mergeable s, Mergeable a, UnionLike m) =>
  SimpleMergeable (StateStrict.StateT s m a)
  where
  mrgIte = mrgIf
  {-# INLINE mrgIte #-}

instance
  (Mergeable s, UnionLike m) =>
  SimpleMergeable1 (StateStrict.StateT s m)
  where
  liftMrgIte m = mrgIfWithStrategy (SimpleStrategy m)
  {-# INLINE liftMrgIte #-}

instance
  (Mergeable s, UnionLike m) =>
  UnionLike (StateStrict.StateT s m)
  where
  mergeWithStrategy ms (StateStrict.StateT f) =
    StateStrict.StateT $ \v -> mergeWithStrategy (liftRootStrategy2 ms rootStrategy) $ f v
  {-# INLINE mergeWithStrategy #-}
  mrgIfWithStrategy s cond (StateStrict.StateT t) (StateStrict.StateT f) =
    StateStrict.StateT $ \v -> mrgIfWithStrategy (liftRootStrategy2 s rootStrategy) cond (t v) (f v)
  {-# INLINE mrgIfWithStrategy #-}
  single x = StateStrict.StateT $ \s -> single (x, s)
  {-# INLINE single #-}
  unionIf cond (StateStrict.StateT l) (StateStrict.StateT r) =
    StateStrict.StateT $ \s -> unionIf cond (l s) (r s)
  {-# INLINE unionIf #-}

instance
  (Mergeable s, Mergeable a, UnionLike m, Monoid s) =>
  SimpleMergeable (WriterLazy.WriterT s m a)
  where
  mrgIte = mrgIf
  {-# INLINE mrgIte #-}

instance
  (Mergeable s, UnionLike m, Monoid s) =>
  SimpleMergeable1 (WriterLazy.WriterT s m)
  where
  liftMrgIte m = mrgIfWithStrategy (SimpleStrategy m)
  {-# INLINE liftMrgIte #-}

instance
  (Mergeable s, UnionLike m, Monoid s) =>
  UnionLike (WriterLazy.WriterT s m)
  where
  mergeWithStrategy ms (WriterLazy.WriterT f) = WriterLazy.WriterT $ mergeWithStrategy (liftRootStrategy2 ms rootStrategy) f
  {-# INLINE mergeWithStrategy #-}
  mrgIfWithStrategy s cond (WriterLazy.WriterT t) (WriterLazy.WriterT f) =
    WriterLazy.WriterT $ mrgIfWithStrategy (liftRootStrategy2 s rootStrategy) cond t f
  {-# INLINE mrgIfWithStrategy #-}
  single x = WriterLazy.WriterT $ single (x, mempty)
  {-# INLINE single #-}
  unionIf cond (WriterLazy.WriterT l) (WriterLazy.WriterT r) =
    WriterLazy.WriterT $ unionIf cond l r
  {-# INLINE unionIf #-}

instance
  (Mergeable s, Mergeable a, UnionLike m, Monoid s) =>
  SimpleMergeable (WriterStrict.WriterT s m a)
  where
  mrgIte = mrgIf
  {-# INLINE mrgIte #-}

instance
  (Mergeable s, UnionLike m, Monoid s) =>
  SimpleMergeable1 (WriterStrict.WriterT s m)
  where
  liftMrgIte m = mrgIfWithStrategy (SimpleStrategy m)
  {-# INLINE liftMrgIte #-}

instance
  (Mergeable s, UnionLike m, Monoid s) =>
  UnionLike (WriterStrict.WriterT s m)
  where
  mergeWithStrategy ms (WriterStrict.WriterT f) = WriterStrict.WriterT $ mergeWithStrategy (liftRootStrategy2 ms rootStrategy) f
  {-# INLINE mergeWithStrategy #-}
  mrgIfWithStrategy s cond (WriterStrict.WriterT t) (WriterStrict.WriterT f) =
    WriterStrict.WriterT $ mrgIfWithStrategy (liftRootStrategy2 s rootStrategy) cond t f
  {-# INLINE mrgIfWithStrategy #-}
  single x = WriterStrict.WriterT $ single (x, mempty)
  {-# INLINE single #-}
  unionIf cond (WriterStrict.WriterT l) (WriterStrict.WriterT r) =
    WriterStrict.WriterT $ unionIf cond l r
  {-# INLINE unionIf #-}

instance
  (Mergeable a, UnionLike m) =>
  SimpleMergeable (ReaderT s m a)
  where
  mrgIte = mrgIf
  {-# INLINE mrgIte #-}

instance
  (UnionLike m) =>
  SimpleMergeable1 (ReaderT s m)
  where
  liftMrgIte m = mrgIfWithStrategy (SimpleStrategy m)
  {-# INLINE liftMrgIte #-}

instance
  (UnionLike m) =>
  UnionLike (ReaderT s m)
  where
  mergeWithStrategy ms (ReaderT f) = ReaderT $ \v -> mergeWithStrategy ms $ f v
  {-# INLINE mergeWithStrategy #-}
  mrgIfWithStrategy s cond (ReaderT t) (ReaderT f) =
    ReaderT $ \v -> mrgIfWithStrategy s cond (t v) (f v)
  {-# INLINE mrgIfWithStrategy #-}
  single x = ReaderT $ \_ -> single x
  {-# INLINE single #-}
  unionIf cond (ReaderT l) (ReaderT r) = ReaderT $ \s -> unionIf cond (l s) (r s)
  {-# INLINE unionIf #-}

instance (SimpleMergeable a) => SimpleMergeable (Identity a) where
  mrgIte cond (Identity l) (Identity r) = Identity $ mrgIte cond l r
  {-# INLINE mrgIte #-}

instance SimpleMergeable1 Identity where
  liftMrgIte mite cond (Identity l) (Identity r) = Identity $ mite cond l r
  {-# INLINE liftMrgIte #-}

instance (UnionLike m, Mergeable a) => SimpleMergeable (IdentityT m a) where
  mrgIte = mrgIf
  {-# INLINE mrgIte #-}

instance (UnionLike m) => SimpleMergeable1 (IdentityT m) where
  liftMrgIte m = mrgIfWithStrategy (SimpleStrategy m)
  {-# INLINE liftMrgIte #-}

instance (UnionLike m) => UnionLike (IdentityT m) where
  mergeWithStrategy ms (IdentityT f) =
    IdentityT $ mergeWithStrategy ms f
  {-# INLINE mergeWithStrategy #-}
  mrgIfWithStrategy s cond (IdentityT l) (IdentityT r) = IdentityT $ mrgIfWithStrategy s cond l r
  {-# INLINE mrgIfWithStrategy #-}
  single x = IdentityT $ single x
  {-# INLINE single #-}
  unionIf cond (IdentityT l) (IdentityT r) = IdentityT $ unionIf cond l r
  {-# INLINE unionIf #-}

instance (UnionLike m, Mergeable r) => SimpleMergeable (ContT r m a) where
  mrgIte cond (ContT l) (ContT r) = ContT $ \c -> mrgIf cond (l c) (r c)
  {-# INLINE mrgIte #-}

instance (UnionLike m, Mergeable r) => SimpleMergeable1 (ContT r m) where
  liftMrgIte m = mrgIfWithStrategy (SimpleStrategy m)
  {-# INLINE liftMrgIte #-}

instance (UnionLike m, Mergeable r) => UnionLike (ContT r m) where
  mergeWithStrategy _ (ContT f) = ContT $ \c -> merge (f c)
  {-# INLINE mergeWithStrategy #-}
  mrgIfWithStrategy _ cond (ContT l) (ContT r) = ContT $ \c -> mrgIf cond (l c) (r c)
  {-# INLINE mrgIfWithStrategy #-}
  single x = ContT $ \c -> c x
  {-# INLINE single #-}
  unionIf cond (ContT l) (ContT r) = ContT $ \c -> unionIf cond (l c) (r c)
  {-# INLINE unionIf #-}

instance
  (Mergeable s, Mergeable w, Monoid w, Mergeable a, UnionLike m) =>
  SimpleMergeable (RWSLazy.RWST r w s m a)
  where
  mrgIte = mrgIf
  {-# INLINE mrgIte #-}

instance
  (Mergeable s, Mergeable w, Monoid w, UnionLike m) =>
  SimpleMergeable1 (RWSLazy.RWST r w s m)
  where
  liftMrgIte m = mrgIfWithStrategy (SimpleStrategy m)
  {-# INLINE liftMrgIte #-}

instance
  (Mergeable s, Mergeable w, Monoid w, UnionLike m) =>
  UnionLike (RWSLazy.RWST r w s m)
  where
  mergeWithStrategy ms (RWSLazy.RWST f) =
    RWSLazy.RWST $ \r s -> mergeWithStrategy (liftRootStrategy3 ms rootStrategy rootStrategy) $ f r s
  {-# INLINE mergeWithStrategy #-}
  mrgIfWithStrategy ms cond (RWSLazy.RWST t) (RWSLazy.RWST f) =
    RWSLazy.RWST $ \r s -> mrgIfWithStrategy (liftRootStrategy3 ms rootStrategy rootStrategy) cond (t r s) (f r s)
  {-# INLINE mrgIfWithStrategy #-}
  single x = RWSLazy.RWST $ \_ s -> single (x, s, mempty)
  {-# INLINE single #-}
  unionIf cond (RWSLazy.RWST t) (RWSLazy.RWST f) =
    RWSLazy.RWST $ \r s -> unionIf cond (t r s) (f r s)
  {-# INLINE unionIf #-}

instance
  (Mergeable s, Mergeable w, Monoid w, Mergeable a, UnionLike m) =>
  SimpleMergeable (RWSStrict.RWST r w s m a)
  where
  mrgIte = mrgIf
  {-# INLINE mrgIte #-}

instance
  (Mergeable s, Mergeable w, Monoid w, UnionLike m) =>
  SimpleMergeable1 (RWSStrict.RWST r w s m)
  where
  liftMrgIte m = mrgIfWithStrategy (SimpleStrategy m)
  {-# INLINE liftMrgIte #-}

instance
  (Mergeable s, Mergeable w, Monoid w, UnionLike m) =>
  UnionLike (RWSStrict.RWST r w s m)
  where
  mergeWithStrategy ms (RWSStrict.RWST f) =
    RWSStrict.RWST $ \r s -> mergeWithStrategy (liftRootStrategy3 ms rootStrategy rootStrategy) $ f r s
  {-# INLINE mergeWithStrategy #-}
  mrgIfWithStrategy ms cond (RWSStrict.RWST t) (RWSStrict.RWST f) =
    RWSStrict.RWST $ \r s -> mrgIfWithStrategy (liftRootStrategy3 ms rootStrategy rootStrategy) cond (t r s) (f r s)
  {-# INLINE mrgIfWithStrategy #-}
  single x = RWSStrict.RWST $ \_ s -> single (x, s, mempty)
  {-# INLINE single #-}
  unionIf cond (RWSStrict.RWST t) (RWSStrict.RWST f) =
    RWSStrict.RWST $ \r s -> unionIf cond (t r s) (f r s)
  {-# INLINE unionIf #-}

-- | Union containers that can be projected back into single value or
-- if-guarded values.
class (UnionLike u) => UnionPrjOp (u :: Type -> Type) where
  -- | Pattern match to extract single values.
  --
  -- >>> singleView (single 1 :: UnionM Integer)
  -- Just 1
  -- >>> singleView (unionIf "a" (single 1) (single 2) :: UnionM Integer)
  -- Nothing
  singleView :: u a -> Maybe a

  -- | Pattern match to extract if values.
  --
  -- >>> ifView (single 1 :: UnionM Integer)
  -- Nothing
  -- >>> ifView (unionIf "a" (single 1) (single 2) :: UnionM Integer)
  -- Just (a,<1>,<2>)
  -- >>> ifView (mrgIf "a" (single 1) (single 2) :: UnionM Integer)
  -- Just (a,{1},{2})
  ifView :: u a -> Maybe (SymBool, u a, u a)

  -- | The leftmost value in the union.
  --
  -- >>> leftMost (unionIf "a" (single 1) (single 2) :: UnionM Integer)
  -- 1
  leftMost :: u a -> a

  -- | Convert the union to a guarded list.
  --
  -- >>> toGuardedList (mrgIf "a" (single 1) (mrgIf "b" (single 2) (single 3)) :: UnionM Integer)
  -- [(a,1),((&& b (! a)),2),((! (|| b a)),3)]
  toGuardedList :: u a -> [(SymBool, a)]
  toGuardedList u =
    case (singleView u, ifView u) of
      (Just x, _) -> [(con True, x)]
      (_, Just (c, l, r)) ->
        fmap (first (&&~ c)) (toGuardedList l)
          ++ fmap (first (&&~ nots c)) (toGuardedList r)
      _ -> error "Should not happen"

-- | Pattern match to extract single values with 'singleView'.
--
-- >>> case (single 1 :: UnionM Integer) of Single v -> v
-- 1
pattern Single :: (UnionPrjOp u, Mergeable a) => a -> u a
pattern Single x <-
  (singleView -> Just x)
  where
    Single x = mrgSingle x

-- | Pattern match to extract guard values with 'ifView'
-- >>> case (unionIf "a" (single 1) (single 2) :: UnionM Integer) of If c t f -> (c,t,f)
-- (a,<1>,<2>)
pattern If :: (UnionPrjOp u, Mergeable a) => SymBool -> u a -> u a -> u a
pattern If c t f <-
  (ifView -> Just (c, t, f))
  where
    If c t f = unionIf c t f

-- | Merge the simply mergeable values in a union, and extract the merged value.
--
-- In the following example, 'unionIf' will not merge the results, and
-- 'simpleMerge' will merge it and extract the single merged value.
--
-- >>> unionIf (ssym "a") (return $ ssym "b") (return $ ssym "c") :: UnionM SymBool
-- <If a b c>
-- >>> simpleMerge $ (unionIf (ssym "a") (return $ ssym "b") (return $ ssym "c") :: UnionM SymBool)
-- (ite a b c)
simpleMerge :: forall u a. (SimpleMergeable a, UnionLike u, UnionPrjOp u) => u a -> a
simpleMerge u = case merge u of
  Single x -> x
  _ -> error "Should not happen"
{-# INLINE simpleMerge #-}

-- | Lift a function to work on union values.
--
-- >>> sumU = onUnion sum
-- >>> sumU (unionIf "cond" (return ["a"]) (return ["b","c"]) :: UnionM [SymInteger])
-- (ite cond a (+ b c))
onUnion ::
  forall u a r.
  (SimpleMergeable r, UnionLike u, UnionPrjOp u, Monad u) =>
  (a -> r) ->
  (u a -> r)
onUnion f = simpleMerge . fmap f

-- | Lift a function to work on union values.
onUnion2 ::
  forall u a b r.
  (SimpleMergeable r, UnionLike u, UnionPrjOp u, Monad u) =>
  (a -> b -> r) ->
  (u a -> u b -> r)
onUnion2 f ua ub = simpleMerge $ f <$> ua <*> ub

-- | Lift a function to work on union values.
onUnion3 ::
  forall u a b c r.
  (SimpleMergeable r, UnionLike u, UnionPrjOp u, Monad u) =>
  (a -> b -> c -> r) ->
  (u a -> u b -> u c -> r)
onUnion3 f ua ub uc = simpleMerge $ f <$> ua <*> ub <*> uc

-- | Lift a function to work on union values.
onUnion4 ::
  forall u a b c d r.
  (SimpleMergeable r, UnionLike u, UnionPrjOp u, Monad u) =>
  (a -> b -> c -> d -> r) ->
  (u a -> u b -> u c -> u d -> r)
onUnion4 f ua ub uc ud = simpleMerge $ f <$> ua <*> ub <*> uc <*> ud

-- | Helper for applying functions on 'UnionPrjOp' and 'SimpleMergeable'.
--
-- >>> let f :: Integer -> UnionM Integer = \x -> mrgIf (ssym "a") (mrgSingle $ x + 1) (mrgSingle $ x + 2)
-- >>> f #~ (mrgIf (ssym "b" :: SymBool) (mrgSingle 0) (mrgSingle 2) :: UnionM Integer)
-- {If (&& b a) 1 (If b 2 (If a 3 4))}
(#~) ::
  (Function f, SimpleMergeable (Ret f), UnionPrjOp u, Functor u) =>
  f ->
  u (Arg f) ->
  Ret f
(#~) f u = simpleMerge $ fmap (f #) u
{-# INLINE (#~) #-}

infixl 9 #~

#define SIMPLE_MERGEABLE_SIMPLE(symtype) \
instance SimpleMergeable symtype where \
  mrgIte = ites; \
  {-# INLINE mrgIte #-}

#define SIMPLE_MERGEABLE_BV(symtype) \
instance (KnownNat n, 1 <= n) => SimpleMergeable (symtype n) where \
  mrgIte = ites; \
  {-# INLINE mrgIte #-}

#define SIMPLE_MERGEABLE_FUN(op) \
instance (SupportedPrim ca, SupportedPrim cb, LinkedRep ca sa, LinkedRep cb sb) => SimpleMergeable (sa op sb) where \
  mrgIte = ites; \
  {-# INLINE mrgIte #-}

#if 1
SIMPLE_MERGEABLE_SIMPLE(SymBool)
SIMPLE_MERGEABLE_SIMPLE(SymInteger)
SIMPLE_MERGEABLE_BV(SymIntN)
SIMPLE_MERGEABLE_BV(SymWordN)
SIMPLE_MERGEABLE_FUN(=~>)
SIMPLE_MERGEABLE_FUN(-~>)
#endif
