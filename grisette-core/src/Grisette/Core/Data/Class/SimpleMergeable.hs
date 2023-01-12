{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
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
-- Copyright   :   (c) Sirui Lu 2021-2022
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Core.Data.Class.SimpleMergeable
  ( -- * Note for the examples

    --

    -- | This module does not contain the implementation for solvable (see "Grisette.Core#solvable")
    -- types, and the examples in this module rely on the implementations in
    -- the [grisette-symir](https://hackage.haskell.org/package/grisette-symir) package.

    -- * Simple mergeable types
    GSimpleMergeable (..),
    GSimpleMergeable1 (..),
    gmrgIte1,
    GSimpleMergeable2 (..),
    gmrgIte2,

    -- * UnionLike operations
    GUnionLike (..),
    mrgIf,
    merge,
    mrgSingle,
    GUnionPrjOp (..),
    pattern SingleU,
    pattern IfU,
    simpleMerge,
    onUnion,
    onUnion2,
    onUnion3,
    onUnion4,
    (#~),
  )
where

import Control.Monad.Except
import Control.Monad.Identity
import qualified Control.Monad.RWS.Lazy as RWSLazy
import qualified Control.Monad.RWS.Strict as RWSStrict
import Control.Monad.Reader
import qualified Control.Monad.State.Lazy as StateLazy
import qualified Control.Monad.State.Strict as StateStrict
import Control.Monad.Trans.Cont
import Control.Monad.Trans.Maybe
import qualified Control.Monad.Writer.Lazy as WriterLazy
import qualified Control.Monad.Writer.Strict as WriterStrict
import Data.Kind
import GHC.Generics
import Generics.Deriving
import Grisette.Core.Data.Class.Bool
import Grisette.Core.Data.Class.Function
import Grisette.Core.Data.Class.Mergeable

-- $setup
-- >>> import Grisette.Core
-- >>> import Grisette.IR.SymPrim
-- >>> import Control.Monad.Identity

-- | Auxiliary class for the generic derivation for the 'GSimpleMergeable' class.
class GSimpleMergeable' bool f where
  gmrgIte' :: bool -> f a -> f a -> f a

instance (GSimpleMergeable' bool U1) where
  gmrgIte' _ t _ = t
  {-# INLINE gmrgIte' #-}

instance (GSimpleMergeable' bool V1) where
  gmrgIte' _ t _ = t
  {-# INLINE gmrgIte' #-}

instance (GSimpleMergeable bool c) => (GSimpleMergeable' bool (K1 i c)) where
  gmrgIte' cond (K1 a) (K1 b) = K1 $ gmrgIte cond a b
  {-# INLINE gmrgIte' #-}

instance (GSimpleMergeable' bool a) => (GSimpleMergeable' bool (M1 i c a)) where
  gmrgIte' cond (M1 a) (M1 b) = M1 $ gmrgIte' cond a b
  {-# INLINE gmrgIte' #-}

instance (GSimpleMergeable' bool a, GSimpleMergeable' bool b) => (GSimpleMergeable' bool (a :*: b)) where
  gmrgIte' cond (a1 :*: a2) (b1 :*: b2) = gmrgIte' cond a1 b1 :*: gmrgIte' cond a2 b2
  {-# INLINE gmrgIte' #-}

-- | This class indicates that a type has a simple root merge strategy.
--
-- __Note 1:__ This type class can be derived for algebraic data types.
-- You may need the @DerivingVia@ and @DerivingStrategies@ extensions.
--
-- > data X = ...
-- >   deriving Generic
-- >   deriving (GMergeable SymBool, GSimpleMergeable SymBool) via (Default X)
--
-- __Note 2:__ The @bool@ type is the symbolic boolean type to use. It should
-- be an instance of `SymBoolOp`. If you do not need to use an alternative
-- symbolic Boolean type, and will use the 'SymBool' type provided by the
-- [grisette-symir](https://hackage.haskell.org/package/grisette-symir) package, you can use the specialized `SimpleMergeable` type
-- synonym for constraints.
-- The specialized combinators like 'mrgIte' are also provided.
-- However, you still need @'GMergeable' SymBool@ for implementing or deriving the
-- type class due to GHC's limitation.
class GMergeable bool a => GSimpleMergeable bool a where
  -- | Performs if-then-else with the simple root merge strategy.
  --
  -- >>> gmrgIte ("a" :: SymBool) "b" "c" :: SymInteger
  -- (ite a b c)
  gmrgIte :: bool -> a -> a -> a

instance (Generic a, GMergeable' bool (Rep a), GSimpleMergeable' bool (Rep a)) => GSimpleMergeable bool (Default a) where
  gmrgIte cond (Default a) (Default b) = Default $ to $ gmrgIte' cond (from a) (from b)
  {-# INLINE gmrgIte #-}

-- | Lifting of the 'GSimpleMergeable' class to unary type constructors.
class GSimpleMergeable1 bool u where
  -- | Lift 'gmrgIte' through the type constructor.
  --
  -- >>> liftGMrgIte gmrgIte ("a" :: SymBool) (Identity "b") (Identity "c") :: Identity SymInteger
  -- Identity (ite a b c)
  liftGMrgIte :: (bool -> a -> a -> a) -> bool -> u a -> u a -> u a

-- | Lift the standard 'gmrgIte' function through the type constructor.
--
-- >>> gmrgIte1 ("a" :: SymBool) (Identity "b") (Identity "c") :: Identity SymInteger
-- Identity (ite a b c)
gmrgIte1 :: (GSimpleMergeable1 bool u, GSimpleMergeable bool a) => bool -> u a -> u a -> u a
gmrgIte1 = liftGMrgIte gmrgIte
{-# INLINE gmrgIte1 #-}

-- | Lifting of the 'GSimpleMergeable' class to binary type constructors.
class (GMergeable2 bool u) => GSimpleMergeable2 bool u where
  -- | Lift 'gmrgIte' through the type constructor.
  --
  -- >>> liftGMrgIte2 gmrgIte gmrgIte ("a" :: SymBool) ("b", "c") ("d", "e") :: (SymInteger, SymBool)
  -- ((ite a b d),(ite a c e))
  liftGMrgIte2 :: (bool -> a -> a -> a) -> (bool -> b -> b -> b) -> bool -> u a b -> u a b -> u a b

-- | Lift the standard 'gmrgIte' function through the type constructor.
--
-- >>> gmrgIte2 ("a" :: SymBool) ("b", "c") ("d", "e") :: (SymInteger, SymBool)
-- ((ite a b d),(ite a c e))
gmrgIte2 :: (GSimpleMergeable2 bool u, GSimpleMergeable bool a, GSimpleMergeable bool b) => bool -> u a b -> u a b -> u a b
gmrgIte2 = liftGMrgIte2 gmrgIte gmrgIte
{-# INLINE gmrgIte2 #-}

-- | Special case of the 'GMergeable1' and 'GSimpleMergeable1' class for type
-- constructors that are 'GSimpleMergeable' when applied to any 'GMergeable'
-- types.
--
-- This type class is used to generalize the 'mrgIf' function to other
-- containers, for example, monad transformer transformed Unions.
class (GSimpleMergeable1 bool u, GMergeable1 bool u, SymBoolOp bool) => GUnionLike bool u | u -> bool where
  -- | Wrap a single value in the union.
  --
  -- Note that this function cannot propagate the 'GMergeable' knowledge.
  --
  -- >>> single "a" :: UnionM SymInteger
  -- UAny (Single a)
  -- >>> mrgSingle "a" :: UnionM SymInteger
  -- UMrg (Single a)
  single :: a -> u a

  -- | If-then-else on two union values.
  --
  -- Note that this function cannot capture the 'GMergeable' knowledge. However,
  -- it may use the merging strategy from the branches to merge the results.
  --
  -- >>> unionIf "a" (single "b") (single "c") :: UnionM SymInteger
  -- UAny (If a (Single b) (Single c))
  -- >>> unionIf "a" (mrgSingle "b") (single "c") :: UnionM SymInteger
  -- UMrg (Single (ite a b c))
  unionIf :: bool -> u a -> u a -> u a

  -- | Merge the contents with some merge strategy.
  --
  -- >>> mergeWithStrategy grootStrategy $ unionIf "a" (single "b") (single "c") :: UnionM SymInteger
  -- UMrg (Single (ite a b c))
  --
  -- __Note:__ Be careful to call this directly in your code.
  -- The supplied merge strategy should be consistent with the type's root merge strategy,
  -- or some internal invariants would be broken and the program can crash.
  --
  -- This function is to be called when the 'GMergeable' constraint can not be resolved,
  -- e.g., the merge strategy for the contained type is given with 'GMergeable1'.
  -- In other cases, 'merge' is usually a better alternative.
  mergeWithStrategy :: GMergingStrategy bool a -> u a -> u a

  -- | Symbolic @if@ control flow with the result merged with some merge strategy.
  --
  -- >>> mrgIfWithStrategy grootStrategy "a" (mrgSingle "b") (single "c") :: UnionM SymInteger
  -- UMrg (Single (ite a b c))
  --
  -- __Note:__ Be careful to call this directly in your code.
  -- The supplied merge strategy should be consistent with the type's root merge strategy,
  -- or some internal invariants would be broken and the program can crash.
  --
  -- This function is to be called when the 'GMergeable' constraint can not be resolved,
  -- e.g., the merge strategy for the contained type is given with 'GMergeable1'.
  -- In other cases, 'mrgIf' is usually a better alternative.
  mrgIfWithStrategy :: GMergingStrategy bool a -> bool -> u a -> u a -> u a
  mrgIfWithStrategy s cond l r = mergeWithStrategy s $ unionIf cond l r
  {-# INLINE mrgIfWithStrategy #-}

  -- | Wrap a single value in the union and capture the 'GMergeable' knowledge.
  --
  -- >>> mrgSingleWithStrategy grootStrategy "a" :: UnionM SymInteger
  -- UMrg (Single a)
  --
  -- __Note:__ Be careful to call this directly in your code.
  -- The supplied merge strategy should be consistent with the type's root merge strategy,
  -- or some internal invariants would be broken and the program can crash.
  --
  -- This function is to be called when the 'GMergeable' constraint can not be resolved,
  -- e.g., the merge strategy for the contained type is given with 'GMergeable1'.
  -- In other cases, 'mrgSingle' is usually a better alternative.
  mrgSingleWithStrategy :: GMergingStrategy bool a -> a -> u a
  mrgSingleWithStrategy s = mergeWithStrategy s . single
  {-# INLINE mrgSingleWithStrategy #-}

-- | Symbolic @if@ control flow with the result merged with the type's root merge strategy.
--
-- Equivalent to @'mrgIfWithStrategy' 'grootStrategy'@.
--
-- >>> mrgIf "a" (single "b") (single "c") :: UnionM SymInteger
-- UMrg (Single (ite a b c))
mrgIf :: (GUnionLike bool u, GMergeable bool a) => bool -> u a -> u a -> u a
mrgIf = mrgIfWithStrategy grootStrategy
{-# INLINE mrgIf #-}

-- | Merge the contents with the type's root merge strategy.
--
-- Equivalent to @'mergeWithStrategy' 'grootStrategy'@.
--
-- >>> merge $ unionIf "a" (single "b") (single "c") :: UnionM SymInteger
-- UMrg (Single (ite a b c))
merge :: (GUnionLike bool u, GMergeable bool a) => u a -> u a
merge = mergeWithStrategy grootStrategy
{-# INLINE merge #-}

-- | Wrap a single value in the type and propagate the type's root merge strategy.
--
-- Equivalent to @'mrgSingleWithStrategy' 'grootStrategy'@.
--
-- >>> mrgSingle "a" :: UnionM SymInteger
-- UMrg (Single a)
mrgSingle :: (GUnionLike bool u, GMergeable bool a) => a -> u a
mrgSingle = mrgSingleWithStrategy grootStrategy
{-# INLINE mrgSingle #-}

instance (SymBoolOp bool) => GSimpleMergeable bool () where
  gmrgIte _ t _ = t
  {-# INLINE gmrgIte #-}

instance (SymBoolOp bool, GSimpleMergeable bool a, GSimpleMergeable bool b) => GSimpleMergeable bool (a, b) where
  gmrgIte cond (a1, b1) (a2, b2) = (gmrgIte cond a1 a2, gmrgIte cond b1 b2)
  {-# INLINE gmrgIte #-}

instance (SymBoolOp bool, GSimpleMergeable bool a) => GSimpleMergeable1 bool ((,) a) where
  liftGMrgIte mb cond (a1, b1) (a2, b2) = (gmrgIte cond a1 a2, mb cond b1 b2)
  {-# INLINE liftGMrgIte #-}

instance (SymBoolOp bool) => GSimpleMergeable2 bool (,) where
  liftGMrgIte2 ma mb cond (a1, b1) (a2, b2) = (ma cond a1 a2, mb cond b1 b2)
  {-# INLINE liftGMrgIte2 #-}

instance
  (SymBoolOp bool, GSimpleMergeable bool a, GSimpleMergeable bool b, GSimpleMergeable bool c) =>
  GSimpleMergeable bool (a, b, c)
  where
  gmrgIte cond (a1, b1, c1) (a2, b2, c2) = (gmrgIte cond a1 a2, gmrgIte cond b1 b2, gmrgIte cond c1 c2)
  {-# INLINE gmrgIte #-}

instance
  ( SymBoolOp bool,
    GSimpleMergeable bool a,
    GSimpleMergeable bool b,
    GSimpleMergeable bool c,
    GSimpleMergeable bool d
  ) =>
  GSimpleMergeable bool (a, b, c, d)
  where
  gmrgIte cond (a1, b1, c1, d1) (a2, b2, c2, d2) =
    (gmrgIte cond a1 a2, gmrgIte cond b1 b2, gmrgIte cond c1 c2, gmrgIte cond d1 d2)
  {-# INLINE gmrgIte #-}

instance
  ( SymBoolOp bool,
    GSimpleMergeable bool a,
    GSimpleMergeable bool b,
    GSimpleMergeable bool c,
    GSimpleMergeable bool d,
    GSimpleMergeable bool e
  ) =>
  GSimpleMergeable bool (a, b, c, d, e)
  where
  gmrgIte cond (a1, b1, c1, d1, e1) (a2, b2, c2, d2, e2) =
    (gmrgIte cond a1 a2, gmrgIte cond b1 b2, gmrgIte cond c1 c2, gmrgIte cond d1 d2, gmrgIte cond e1 e2)
  {-# INLINE gmrgIte #-}

instance
  ( SymBoolOp bool,
    GSimpleMergeable bool a,
    GSimpleMergeable bool b,
    GSimpleMergeable bool c,
    GSimpleMergeable bool d,
    GSimpleMergeable bool e,
    GSimpleMergeable bool f
  ) =>
  GSimpleMergeable bool (a, b, c, d, e, f)
  where
  gmrgIte cond (a1, b1, c1, d1, e1, f1) (a2, b2, c2, d2, e2, f2) =
    (gmrgIte cond a1 a2, gmrgIte cond b1 b2, gmrgIte cond c1 c2, gmrgIte cond d1 d2, gmrgIte cond e1 e2, gmrgIte cond f1 f2)
  {-# INLINE gmrgIte #-}

instance
  ( SymBoolOp bool,
    GSimpleMergeable bool a,
    GSimpleMergeable bool b,
    GSimpleMergeable bool c,
    GSimpleMergeable bool d,
    GSimpleMergeable bool e,
    GSimpleMergeable bool f,
    GSimpleMergeable bool g
  ) =>
  GSimpleMergeable bool (a, b, c, d, e, f, g)
  where
  gmrgIte cond (a1, b1, c1, d1, e1, f1, g1) (a2, b2, c2, d2, e2, f2, g2) =
    ( gmrgIte cond a1 a2,
      gmrgIte cond b1 b2,
      gmrgIte cond c1 c2,
      gmrgIte cond d1 d2,
      gmrgIte cond e1 e2,
      gmrgIte cond f1 f2,
      gmrgIte cond g1 g2
    )
  {-# INLINE gmrgIte #-}

instance
  ( SymBoolOp bool,
    GSimpleMergeable bool a,
    GSimpleMergeable bool b,
    GSimpleMergeable bool c,
    GSimpleMergeable bool d,
    GSimpleMergeable bool e,
    GSimpleMergeable bool f,
    GSimpleMergeable bool g,
    GSimpleMergeable bool h
  ) =>
  GSimpleMergeable bool (a, b, c, d, e, f, g, h)
  where
  gmrgIte cond (a1, b1, c1, d1, e1, f1, g1, h1) (a2, b2, c2, d2, e2, f2, g2, h2) =
    ( gmrgIte cond a1 a2,
      gmrgIte cond b1 b2,
      gmrgIte cond c1 c2,
      gmrgIte cond d1 d2,
      gmrgIte cond e1 e2,
      gmrgIte cond f1 f2,
      gmrgIte cond g1 g2,
      gmrgIte cond h1 h2
    )
  {-# INLINE gmrgIte #-}

instance (SymBoolOp bool, GSimpleMergeable bool b) => GSimpleMergeable bool (a -> b) where
  gmrgIte = gmrgIte1
  {-# INLINE gmrgIte #-}

instance (SymBoolOp bool) => GSimpleMergeable1 bool ((->) a) where
  liftGMrgIte ms cond t f v = ms cond (t v) (f v)
  {-# INLINE liftGMrgIte #-}

instance (SymBoolOp bool, GUnionLike bool m, GMergeable bool a) => GSimpleMergeable bool (MaybeT m a) where
  gmrgIte = mrgIf
  {-# INLINE gmrgIte #-}

instance (SymBoolOp bool, GUnionLike bool m) => GSimpleMergeable1 bool (MaybeT m) where
  liftGMrgIte m = mrgIfWithStrategy (SimpleStrategy m)
  {-# INLINE liftGMrgIte #-}

instance (SymBoolOp bool, GUnionLike bool m) => GUnionLike bool (MaybeT m) where
  mergeWithStrategy s (MaybeT v) = MaybeT $ mergeWithStrategy (liftGRootStrategy s) v
  {-# INLINE mergeWithStrategy #-}
  mrgIfWithStrategy s cond (MaybeT t) (MaybeT f) = MaybeT $ mrgIfWithStrategy (liftGRootStrategy s) cond t f
  {-# INLINE mrgIfWithStrategy #-}
  single = MaybeT . single . return
  {-# INLINE single #-}
  unionIf cond (MaybeT l) (MaybeT r) = MaybeT $ unionIf cond l r
  {-# INLINE unionIf #-}

instance
  (SymBoolOp bool, GUnionLike bool m, GMergeable bool e, GMergeable bool a) =>
  GSimpleMergeable bool (ExceptT e m a)
  where
  gmrgIte = mrgIf
  {-# INLINE gmrgIte #-}

instance
  (SymBoolOp bool, GUnionLike bool m, GMergeable bool e) =>
  GSimpleMergeable1 bool (ExceptT e m)
  where
  liftGMrgIte m = mrgIfWithStrategy (SimpleStrategy m)
  {-# INLINE liftGMrgIte #-}

instance
  (SymBoolOp bool, GUnionLike bool m, GMergeable bool e) =>
  GUnionLike bool (ExceptT e m)
  where
  mergeWithStrategy s (ExceptT v) = ExceptT $ mergeWithStrategy (liftGRootStrategy s) v
  {-# INLINE mergeWithStrategy #-}
  mrgIfWithStrategy s cond (ExceptT t) (ExceptT f) = ExceptT $ mrgIfWithStrategy (liftGRootStrategy s) cond t f
  {-# INLINE mrgIfWithStrategy #-}
  single = ExceptT . single . return
  {-# INLINE single #-}
  unionIf cond (ExceptT l) (ExceptT r) = ExceptT $ unionIf cond l r
  {-# INLINE unionIf #-}

instance
  (SymBoolOp bool, GMergeable bool s, GMergeable bool a, GUnionLike bool m) =>
  GSimpleMergeable bool (StateLazy.StateT s m a)
  where
  gmrgIte = mrgIf
  {-# INLINE gmrgIte #-}

instance
  (SymBoolOp bool, GMergeable bool s, GUnionLike bool m) =>
  GSimpleMergeable1 bool (StateLazy.StateT s m)
  where
  liftGMrgIte m = mrgIfWithStrategy (SimpleStrategy m)
  {-# INLINE liftGMrgIte #-}

instance
  (SymBoolOp bool, GMergeable bool s, GUnionLike bool m) =>
  GUnionLike bool (StateLazy.StateT s m)
  where
  mergeWithStrategy ms (StateLazy.StateT f) =
    StateLazy.StateT $ \v -> mergeWithStrategy (liftGRootStrategy2 ms grootStrategy) $ f v
  {-# INLINE mergeWithStrategy #-}
  mrgIfWithStrategy s cond (StateLazy.StateT t) (StateLazy.StateT f) =
    StateLazy.StateT $ \v -> mrgIfWithStrategy (liftGRootStrategy2 s grootStrategy) cond (t v) (f v)
  {-# INLINE mrgIfWithStrategy #-}
  single x = StateLazy.StateT $ \s -> single (x, s)
  {-# INLINE single #-}
  unionIf cond (StateLazy.StateT l) (StateLazy.StateT r) =
    StateLazy.StateT $ \s -> unionIf cond (l s) (r s)
  {-# INLINE unionIf #-}

instance
  (SymBoolOp bool, GMergeable bool s, GMergeable bool a, GUnionLike bool m) =>
  GSimpleMergeable bool (StateStrict.StateT s m a)
  where
  gmrgIte = mrgIf
  {-# INLINE gmrgIte #-}

instance
  (SymBoolOp bool, GMergeable bool s, GUnionLike bool m) =>
  GSimpleMergeable1 bool (StateStrict.StateT s m)
  where
  liftGMrgIte m = mrgIfWithStrategy (SimpleStrategy m)
  {-# INLINE liftGMrgIte #-}

instance
  (SymBoolOp bool, GMergeable bool s, GUnionLike bool m) =>
  GUnionLike bool (StateStrict.StateT s m)
  where
  mergeWithStrategy ms (StateStrict.StateT f) =
    StateStrict.StateT $ \v -> mergeWithStrategy (liftGRootStrategy2 ms grootStrategy) $ f v
  {-# INLINE mergeWithStrategy #-}
  mrgIfWithStrategy s cond (StateStrict.StateT t) (StateStrict.StateT f) =
    StateStrict.StateT $ \v -> mrgIfWithStrategy (liftGRootStrategy2 s grootStrategy) cond (t v) (f v)
  {-# INLINE mrgIfWithStrategy #-}
  single x = StateStrict.StateT $ \s -> single (x, s)
  {-# INLINE single #-}
  unionIf cond (StateStrict.StateT l) (StateStrict.StateT r) =
    StateStrict.StateT $ \s -> unionIf cond (l s) (r s)
  {-# INLINE unionIf #-}

instance
  (SymBoolOp bool, GMergeable bool s, GMergeable bool a, GUnionLike bool m, Monoid s) =>
  GSimpleMergeable bool (WriterLazy.WriterT s m a)
  where
  gmrgIte = mrgIf
  {-# INLINE gmrgIte #-}

instance
  (SymBoolOp bool, GMergeable bool s, GUnionLike bool m, Monoid s) =>
  GSimpleMergeable1 bool (WriterLazy.WriterT s m)
  where
  liftGMrgIte m = mrgIfWithStrategy (SimpleStrategy m)
  {-# INLINE liftGMrgIte #-}

instance
  (SymBoolOp bool, GMergeable bool s, GUnionLike bool m, Monoid s) =>
  GUnionLike bool (WriterLazy.WriterT s m)
  where
  mergeWithStrategy ms (WriterLazy.WriterT f) = WriterLazy.WriterT $ mergeWithStrategy (liftGRootStrategy2 ms grootStrategy) f
  {-# INLINE mergeWithStrategy #-}
  mrgIfWithStrategy s cond (WriterLazy.WriterT t) (WriterLazy.WriterT f) =
    WriterLazy.WriterT $ mrgIfWithStrategy (liftGRootStrategy2 s grootStrategy) cond t f
  {-# INLINE mrgIfWithStrategy #-}
  single x = WriterLazy.WriterT $ single (x, mempty)
  {-# INLINE single #-}
  unionIf cond (WriterLazy.WriterT l) (WriterLazy.WriterT r) =
    WriterLazy.WriterT $ unionIf cond l r
  {-# INLINE unionIf #-}

instance
  (SymBoolOp bool, GMergeable bool s, GMergeable bool a, GUnionLike bool m, Monoid s) =>
  GSimpleMergeable bool (WriterStrict.WriterT s m a)
  where
  gmrgIte = mrgIf
  {-# INLINE gmrgIte #-}

instance
  (SymBoolOp bool, GMergeable bool s, GUnionLike bool m, Monoid s) =>
  GSimpleMergeable1 bool (WriterStrict.WriterT s m)
  where
  liftGMrgIte m = mrgIfWithStrategy (SimpleStrategy m)
  {-# INLINE liftGMrgIte #-}

instance
  (SymBoolOp bool, GMergeable bool s, GUnionLike bool m, Monoid s) =>
  GUnionLike bool (WriterStrict.WriterT s m)
  where
  mergeWithStrategy ms (WriterStrict.WriterT f) = WriterStrict.WriterT $ mergeWithStrategy (liftGRootStrategy2 ms grootStrategy) f
  {-# INLINE mergeWithStrategy #-}
  mrgIfWithStrategy s cond (WriterStrict.WriterT t) (WriterStrict.WriterT f) =
    WriterStrict.WriterT $ mrgIfWithStrategy (liftGRootStrategy2 s grootStrategy) cond t f
  {-# INLINE mrgIfWithStrategy #-}
  single x = WriterStrict.WriterT $ single (x, mempty)
  {-# INLINE single #-}
  unionIf cond (WriterStrict.WriterT l) (WriterStrict.WriterT r) =
    WriterStrict.WriterT $ unionIf cond l r
  {-# INLINE unionIf #-}

instance
  (SymBoolOp bool, GMergeable bool a, GUnionLike bool m) =>
  GSimpleMergeable bool (ReaderT s m a)
  where
  gmrgIte = mrgIf
  {-# INLINE gmrgIte #-}

instance
  (SymBoolOp bool, GUnionLike bool m) =>
  GSimpleMergeable1 bool (ReaderT s m)
  where
  liftGMrgIte m = mrgIfWithStrategy (SimpleStrategy m)
  {-# INLINE liftGMrgIte #-}

instance
  (SymBoolOp bool, GUnionLike bool m) =>
  GUnionLike bool (ReaderT s m)
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

instance (SymBoolOp bool, GSimpleMergeable bool a) => GSimpleMergeable bool (Identity a) where
  gmrgIte cond (Identity l) (Identity r) = Identity $ gmrgIte cond l r
  {-# INLINE gmrgIte #-}

instance (SymBoolOp bool) => GSimpleMergeable1 bool Identity where
  liftGMrgIte mite cond (Identity l) (Identity r) = Identity $ mite cond l r
  {-# INLINE liftGMrgIte #-}

instance (SymBoolOp bool, GUnionLike bool m, GMergeable bool a) => GSimpleMergeable bool (IdentityT m a) where
  gmrgIte = mrgIf
  {-# INLINE gmrgIte #-}

instance (SymBoolOp bool, GUnionLike bool m) => GSimpleMergeable1 bool (IdentityT m) where
  liftGMrgIte m = mrgIfWithStrategy (SimpleStrategy m)
  {-# INLINE liftGMrgIte #-}

instance (SymBoolOp bool, GUnionLike bool m) => GUnionLike bool (IdentityT m) where
  mergeWithStrategy ms (IdentityT f) =
    IdentityT $ mergeWithStrategy ms f
  {-# INLINE mergeWithStrategy #-}
  mrgIfWithStrategy s cond (IdentityT l) (IdentityT r) = IdentityT $ mrgIfWithStrategy s cond l r
  {-# INLINE mrgIfWithStrategy #-}
  single x = IdentityT $ single x
  {-# INLINE single #-}
  unionIf cond (IdentityT l) (IdentityT r) = IdentityT $ unionIf cond l r
  {-# INLINE unionIf #-}

instance (SymBoolOp bool, GUnionLike bool m, GMergeable bool r) => GSimpleMergeable bool (ContT r m a) where
  gmrgIte cond (ContT l) (ContT r) = ContT $ \c -> mrgIf cond (l c) (r c)
  {-# INLINE gmrgIte #-}

instance (SymBoolOp bool, GUnionLike bool m, GMergeable bool r) => GSimpleMergeable1 bool (ContT r m) where
  liftGMrgIte m = mrgIfWithStrategy (SimpleStrategy m)
  {-# INLINE liftGMrgIte #-}

instance (SymBoolOp bool, GUnionLike bool m, GMergeable bool r) => GUnionLike bool (ContT r m) where
  mergeWithStrategy _ (ContT f) = ContT $ \c -> merge (f c)
  {-# INLINE mergeWithStrategy #-}
  mrgIfWithStrategy _ cond (ContT l) (ContT r) = ContT $ \c -> mrgIf cond (l c) (r c)
  {-# INLINE mrgIfWithStrategy #-}
  single x = ContT $ \c -> c x
  {-# INLINE single #-}
  unionIf cond (ContT l) (ContT r) = ContT $ \c -> unionIf cond (l c) (r c)
  {-# INLINE unionIf #-}

instance
  (SymBoolOp bool, GMergeable bool s, GMergeable bool w, Monoid w, GMergeable bool a, GUnionLike bool m) =>
  GSimpleMergeable bool (RWSLazy.RWST r w s m a)
  where
  gmrgIte = mrgIf
  {-# INLINE gmrgIte #-}

instance
  (SymBoolOp bool, GMergeable bool s, GMergeable bool w, Monoid w, GUnionLike bool m) =>
  GSimpleMergeable1 bool (RWSLazy.RWST r w s m)
  where
  liftGMrgIte m = mrgIfWithStrategy (SimpleStrategy m)
  {-# INLINE liftGMrgIte #-}

instance
  (SymBoolOp bool, GMergeable bool s, GMergeable bool w, Monoid w, GUnionLike bool m) =>
  GUnionLike bool (RWSLazy.RWST r w s m)
  where
  mergeWithStrategy ms (RWSLazy.RWST f) =
    RWSLazy.RWST $ \r s -> mergeWithStrategy (liftGRootStrategy3 ms grootStrategy grootStrategy) $ f r s
  {-# INLINE mergeWithStrategy #-}
  mrgIfWithStrategy ms cond (RWSLazy.RWST t) (RWSLazy.RWST f) =
    RWSLazy.RWST $ \r s -> mrgIfWithStrategy (liftGRootStrategy3 ms grootStrategy grootStrategy) cond (t r s) (f r s)
  {-# INLINE mrgIfWithStrategy #-}
  single x = RWSLazy.RWST $ \_ s -> single (x, s, mempty)
  {-# INLINE single #-}
  unionIf cond (RWSLazy.RWST t) (RWSLazy.RWST f) =
    RWSLazy.RWST $ \r s -> unionIf cond (t r s) (f r s)
  {-# INLINE unionIf #-}

instance
  (SymBoolOp bool, GMergeable bool s, GMergeable bool w, Monoid w, GMergeable bool a, GUnionLike bool m) =>
  GSimpleMergeable bool (RWSStrict.RWST r w s m a)
  where
  gmrgIte = mrgIf
  {-# INLINE gmrgIte #-}

instance
  (SymBoolOp bool, GMergeable bool s, GMergeable bool w, Monoid w, GUnionLike bool m) =>
  GSimpleMergeable1 bool (RWSStrict.RWST r w s m)
  where
  liftGMrgIte m = mrgIfWithStrategy (SimpleStrategy m)
  {-# INLINE liftGMrgIte #-}

instance
  (SymBoolOp bool, GMergeable bool s, GMergeable bool w, Monoid w, GUnionLike bool m) =>
  GUnionLike bool (RWSStrict.RWST r w s m)
  where
  mergeWithStrategy ms (RWSStrict.RWST f) =
    RWSStrict.RWST $ \r s -> mergeWithStrategy (liftGRootStrategy3 ms grootStrategy grootStrategy) $ f r s
  {-# INLINE mergeWithStrategy #-}
  mrgIfWithStrategy ms cond (RWSStrict.RWST t) (RWSStrict.RWST f) =
    RWSStrict.RWST $ \r s -> mrgIfWithStrategy (liftGRootStrategy3 ms grootStrategy grootStrategy) cond (t r s) (f r s)
  {-# INLINE mrgIfWithStrategy #-}
  single x = RWSStrict.RWST $ \_ s -> single (x, s, mempty)
  {-# INLINE single #-}
  unionIf cond (RWSStrict.RWST t) (RWSStrict.RWST f) =
    RWSStrict.RWST $ \r s -> unionIf cond (t r s) (f r s)
  {-# INLINE unionIf #-}

-- | Union containers that can be projected back into single value or
-- if-guarded values.
class (GUnionLike bool u) => GUnionPrjOp bool (u :: Type -> Type) | u -> bool where
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
  -- Just (a,UAny (Single 1),UAny (Single 2))
  -- >>> ifView (mrgIf "a" (single 1) (single 2) :: UnionM Integer)
  -- Just (a,UMrg (Single 1),UMrg (Single 2))
  ifView :: u a -> Maybe (bool, u a, u a)

  -- | The leftmost value in the union.
  --
  -- >>> leftMost (unionIf "a" (single 1) (single 2) :: UnionM Integer)
  -- 1
  leftMost :: u a -> a

-- | Pattern match to extract single values with 'singleView'.
--
-- >>> case (single 1 :: UnionM Integer) of SingleU v -> v
-- 1
pattern SingleU :: GUnionPrjOp bool u => a -> u a
pattern SingleU x <-
  (singleView -> Just x)
  where
    SingleU x = single x

-- | Pattern match to extract guard values with 'ifView'
-- >>> case (unionIf "a" (single 1) (single 2) :: UnionM Integer) of IfU c t f -> (c,t,f)
-- (a,UAny (Single 1),UAny (Single 2))
pattern IfU :: GUnionPrjOp bool u => bool -> u a -> u a -> u a
pattern IfU c t f <-
  (ifView -> Just (c, t, f))
  where
    IfU c t f = unionIf c t f

-- | Merge the simply mergeable values in a union, and extract the merged value.
--
-- In the following example, 'unionIf' will not merge the results, and
-- 'simpleMerge' will merge it and extract the single merged value.
--
-- >>> unionIf (ssymb "a") (return $ ssymb "b") (return $ ssymb "c") :: UnionM SymBool
-- UAny (If a (Single b) (Single c))
-- >>> simpleMerge $ (unionIf (ssymb "a") (return $ ssymb "b") (return $ ssymb "c") :: UnionM SymBool)
-- (ite a b c)
simpleMerge :: forall bool u a. (GSimpleMergeable bool a, GUnionLike bool u, GUnionPrjOp bool u) => u a -> a
simpleMerge u = case merge u of
  SingleU x -> x
  _ -> error "Should not happen"
{-# INLINE simpleMerge #-}

-- | Lift a function to work on union values.
--
-- >>> sumU = onUnion sum
-- >>> sumU (unionIf "cond" (return ["a"]) (return ["b","c"]) :: UnionM [SymInteger])
-- (ite cond a (+ b c))
onUnion ::
  forall bool u a r.
  (GSimpleMergeable bool r, GUnionLike bool u, GUnionPrjOp bool u, Monad u) =>
  (a -> r) ->
  (u a -> r)
onUnion f = simpleMerge . fmap f

-- | Lift a function to work on union values.
onUnion2 ::
  forall bool u a b r.
  (GSimpleMergeable bool r, GUnionLike bool u, GUnionPrjOp bool u, Monad u) =>
  (a -> b -> r) ->
  (u a -> u b -> r)
onUnion2 f ua ub = simpleMerge $ f <$> ua <*> ub

-- | Lift a function to work on union values.
onUnion3 ::
  forall bool u a b c r.
  (GSimpleMergeable bool r, GUnionLike bool u, GUnionPrjOp bool u, Monad u) =>
  (a -> b -> c -> r) ->
  (u a -> u b -> u c -> r)
onUnion3 f ua ub uc = simpleMerge $ f <$> ua <*> ub <*> uc

-- | Lift a function to work on union values.
onUnion4 ::
  forall bool u a b c d r.
  (GSimpleMergeable bool r, GUnionLike bool u, GUnionPrjOp bool u, Monad u) =>
  (a -> b -> c -> d -> r) ->
  (u a -> u b -> u c -> u d -> r)
onUnion4 f ua ub uc ud = simpleMerge $ f <$> ua <*> ub <*> uc <*> ud

-- | Helper for applying functions on 'GUnionPrjOp' and 'GSimpleMergeable'.
--
-- >>> let f :: Integer -> UnionM Integer = \x -> mrgIf (ssymb "a") (mrgSingle $ x + 1) (mrgSingle $ x + 2)
-- >>> f #~ (mrgIf (ssymb "b" :: SymBool) (mrgSingle 0) (mrgSingle 2) :: UnionM Integer)
-- UMrg (If (&& b a) (Single 1) (If b (Single 2) (If a (Single 3) (Single 4))))
(#~) ::
  (SymBoolOp bool, Function f, GSimpleMergeable bool (Ret f), GUnionPrjOp bool u, Functor u) =>
  f ->
  u (Arg f) ->
  Ret f
(#~) f u = simpleMerge $ fmap (f #) u
{-# INLINE (#~) #-}

infixl 9 #~
