{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Grisette.Core.Data.Class.SimpleMergeable
  ( GSimpleMergeable (..),
    GSimpleMergeable1 (..),
    gmrgIte1,
    GSimpleMergeable2 (..),
    gmrgIte2,
    GUnionLike (..),
    mrgIf,
    merge,
    mrgSingle,
    GUnionPrjOp (..),
    pattern SingleU,
    pattern IfU,
    getSingle,
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
class GMergeable bool a => GSimpleMergeable bool a where
  -- | Performs if-then-else with the simple root merge strategy.
  gmrgIte :: bool -> a -> a -> a

instance (Generic a, GMergeable' bool (Rep a), GSimpleMergeable' bool (Rep a)) => GSimpleMergeable bool (Default a) where
  gmrgIte cond (Default a) (Default b) = Default $ to $ gmrgIte' cond (from a) (from b)
  {-# INLINE gmrgIte #-}

-- | Lifting of the 'GSimpleMergeable' class to unary type constructors.
class GSimpleMergeable1 bool u where
  -- | Lift 'gmrgIte' through the type constructor.
  liftGMrgIte :: (bool -> a -> a -> a) -> bool -> u a -> u a -> u a

-- | Lift the standard 'gmrgIte' function through the type constructor.
gmrgIte1 :: (GSimpleMergeable1 bool u, GSimpleMergeable bool a) => bool -> u a -> u a -> u a
gmrgIte1 = liftGMrgIte gmrgIte
{-# INLINE gmrgIte1 #-}

-- | Lifting of the 'GSimpleMergeable' class to binary type constructors.
class (GMergeable2 bool u) => GSimpleMergeable2 bool u where
  -- | Lift 'gmrgIte' through the type constructor.
  liftGMrgIte2 :: (bool -> a -> a -> a) -> (bool -> b -> b -> b) -> bool -> u a b -> u a b -> u a b

-- | Lift the standard 'gmrgIte' function through the type constructor.
gmrgIte2 :: (GSimpleMergeable2 bool u, GSimpleMergeable bool a, GSimpleMergeable bool b) => bool -> u a b -> u a b -> u a b
gmrgIte2 = liftGMrgIte2 gmrgIte gmrgIte
{-# INLINE gmrgIte2 #-}

-- | Special case of the 'Mergeable1' and 'GSimpleMergeable1' class for type constructors
-- that are 'GSimpleMergeable' when applied to any 'Mergeable' types.
--
-- Usually it is Union-like structures.
class (GSimpleMergeable1 bool u, GMergeable1 bool u, SymBoolOp bool) => GUnionLike bool u | u -> bool where
  -- | Wrap a single value in the union.
  single :: a -> u a

  -- | If-then-else on two union values.
  unionIf :: bool -> u a -> u a -> u a

  -- | Merge the contents with some merge strategy.
  --
  -- Be careful to call this directly in your code.
  -- The supplied merge strategy should be consistent with the type's root merge strategy,
  -- or some internal invariants would be broken and the program can crash.
  --
  -- This function is to be called when the 'Mergeable' constraint can not be resolved,
  -- e.g., the merge strategy for the contained type is given with 'Mergeable1'.
  -- In other cases, 'merge' is usually a better alternative.
  mergeWithStrategy :: GMergingStrategy bool a -> u a -> u a

  -- | Symbolic @if@ control flow with the result merged with some merge strategy.
  --
  -- Be careful to call this directly in your code.
  -- The supplied merge strategy should be consistent with the type's root merge strategy,
  -- or some internal invariants would be broken and the program can crash.
  --
  -- This function to to be called when the 'Mergeable' constraint can not be resolved,
  -- e.g., the merge strategy for the contained type is given with 'Mergeable1'.
  -- In other cases, 'mrgIf' is usually a better alternative.
  mrgIfWithStrategy :: GMergingStrategy bool a -> bool -> u a -> u a -> u a
  mrgIfWithStrategy s cond l r = mergeWithStrategy s $ unionIf cond l r
  {-# INLINE mrgIfWithStrategy #-}

  mrgSingleWithStrategy :: GMergingStrategy bool a -> a -> u a
  mrgSingleWithStrategy s = mergeWithStrategy s . single
  {-# INLINE mrgSingleWithStrategy #-}

-- | Symbolic @if@ control flow with the result merged with the type's root merge strategy.
--
-- | Equivalent to @mrgIfWithStrategy gmergingStrategy@.
mrgIf :: (GUnionLike bool u, GMergeable bool a) => bool -> u a -> u a -> u a
mrgIf = mrgIfWithStrategy gmergingStrategy
{-# INLINE mrgIf #-}

-- | Merge the contents with the type's root merge strategy.
--
-- | Equivalent to @mergeWithStrategy gmergingStrategy@.
merge :: (GUnionLike bool u, GMergeable bool a) => u a -> u a
merge = mergeWithStrategy gmergingStrategy
{-# INLINE merge #-}

-- | Wrap a single value in the type and propagate the type's root merge strategy.
--
-- | Equivalent to @mrgSingleWithStrategy gmergingStrategy@.
mrgSingle :: (GUnionLike bool u, GMergeable bool a) => a -> u a
mrgSingle = mrgSingleWithStrategy gmergingStrategy
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
  mergeWithStrategy s (MaybeT v) = MaybeT $ mergeWithStrategy (liftGMergingStrategy s) v
  {-# INLINE mergeWithStrategy #-}
  mrgIfWithStrategy s cond (MaybeT t) (MaybeT f) = MaybeT $ mrgIfWithStrategy (liftGMergingStrategy s) cond t f
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
  mergeWithStrategy s (ExceptT v) = ExceptT $ mergeWithStrategy (liftGMergingStrategy s) v
  {-# INLINE mergeWithStrategy #-}
  mrgIfWithStrategy s cond (ExceptT t) (ExceptT f) = ExceptT $ mrgIfWithStrategy (liftGMergingStrategy s) cond t f
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
    StateLazy.StateT $ \v -> mergeWithStrategy (liftGMergingStrategy2 ms gmergingStrategy) $ f v
  {-# INLINE mergeWithStrategy #-}
  mrgIfWithStrategy s cond (StateLazy.StateT t) (StateLazy.StateT f) =
    StateLazy.StateT $ \v -> mrgIfWithStrategy (liftGMergingStrategy2 s gmergingStrategy) cond (t v) (f v)
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
    StateStrict.StateT $ \v -> mergeWithStrategy (liftGMergingStrategy2 ms gmergingStrategy) $ f v
  {-# INLINE mergeWithStrategy #-}
  mrgIfWithStrategy s cond (StateStrict.StateT t) (StateStrict.StateT f) =
    StateStrict.StateT $ \v -> mrgIfWithStrategy (liftGMergingStrategy2 s gmergingStrategy) cond (t v) (f v)
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
  mergeWithStrategy ms (WriterLazy.WriterT f) = WriterLazy.WriterT $ mergeWithStrategy (liftGMergingStrategy2 ms gmergingStrategy) f
  {-# INLINE mergeWithStrategy #-}
  mrgIfWithStrategy s cond (WriterLazy.WriterT t) (WriterLazy.WriterT f) =
    WriterLazy.WriterT $ mrgIfWithStrategy (liftGMergingStrategy2 s gmergingStrategy) cond t f
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
  mergeWithStrategy ms (WriterStrict.WriterT f) = WriterStrict.WriterT $ mergeWithStrategy (liftGMergingStrategy2 ms gmergingStrategy) f
  {-# INLINE mergeWithStrategy #-}
  mrgIfWithStrategy s cond (WriterStrict.WriterT t) (WriterStrict.WriterT f) =
    WriterStrict.WriterT $ mrgIfWithStrategy (liftGMergingStrategy2 s gmergingStrategy) cond t f
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
    RWSLazy.RWST $ \r s -> mergeWithStrategy (liftGMergingStrategy3 ms gmergingStrategy gmergingStrategy) $ f r s
  {-# INLINE mergeWithStrategy #-}
  mrgIfWithStrategy ms cond (RWSLazy.RWST t) (RWSLazy.RWST f) =
    RWSLazy.RWST $ \r s -> mrgIfWithStrategy (liftGMergingStrategy3 ms gmergingStrategy gmergingStrategy) cond (t r s) (f r s)
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
    RWSStrict.RWST $ \r s -> mergeWithStrategy (liftGMergingStrategy3 ms gmergingStrategy gmergingStrategy) $ f r s
  {-# INLINE mergeWithStrategy #-}
  mrgIfWithStrategy ms cond (RWSStrict.RWST t) (RWSStrict.RWST f) =
    RWSStrict.RWST $ \r s -> mrgIfWithStrategy (liftGMergingStrategy3 ms gmergingStrategy gmergingStrategy) cond (t r s) (f r s)
  {-# INLINE mrgIfWithStrategy #-}
  single x = RWSStrict.RWST $ \_ s -> single (x, s, mempty)
  {-# INLINE single #-}
  unionIf cond (RWSStrict.RWST t) (RWSStrict.RWST f) =
    RWSStrict.RWST $ \r s -> unionIf cond (t r s) (f r s)
  {-# INLINE unionIf #-}

class (GUnionLike bool u) => GUnionPrjOp bool (u :: Type -> Type) | u -> bool where
  -- | Pattern match to extract single values.
  singleView :: u a -> Maybe a

  -- | Pattern match to extract if values.
  ifView :: u a -> Maybe (bool, u a, u a)

  -- | The leftmost value in the union.
  leftMost :: u a -> a

-- | Pattern match to extract single values with 'singleView'.
pattern SingleU :: GUnionPrjOp bool u => a -> u a
pattern SingleU x <-
  (singleView -> Just x)
  where
    SingleU x = single x

-- | Pattern match to extract guard values with 'guardView'
pattern IfU :: GUnionPrjOp bool u => bool -> u a -> u a -> u a
pattern IfU c t f <-
  (ifView -> Just (c, t, f))
  where
    IfU c t f = unionIf c t f

-- | Extract the value from a union-like monad if the value has a simply mergeable type.
--
-- 'unionIf' will not merge the results.
-- 'getSingle' will merge it and extract the single value.
--
-- >>> unionIf (ssymb "a") (return $ ssymb "b") (return $ ssymb "c") :: UnionM SymBool
-- UAny (If a (Single b) (Single c))
-- >>> getSingle $ (unionIf (ssymb "a") (return $ ssymb "b") (return $ ssymb "c") :: UnionM SymBool)
-- (ite a b c)
getSingle :: forall bool u a. (GSimpleMergeable bool a, GUnionLike bool u, GUnionPrjOp bool u) => u a -> a
getSingle u = case merge u of
  SingleU x -> x
  _ -> error "Should not happen"
{-# INLINE getSingle #-}

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
(#~) f u = getSingle $ fmap (f #) u
{-# INLINE (#~) #-}

infixl 9 #~
