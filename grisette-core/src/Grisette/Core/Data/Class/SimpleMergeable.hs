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
  ( SimpleMergeable (..),
    SimpleMergeable1 (..),
    mrgIte1,
    SimpleMergeable2 (..),
    mrgIte2,
    UnionLike (..),
    mrgIf,
    merge,
    mrgSingle,
    UnionPrjOp (..),
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

-- | Auxiliary class for the generic derivation for the 'SimpleMergeable' class.
class SimpleMergeable' bool f where
  mrgIte' :: bool -> f a -> f a -> f a

instance (SimpleMergeable' bool U1) where
  mrgIte' _ t _ = t
  {-# INLINE mrgIte' #-}

instance (SimpleMergeable' bool V1) where
  mrgIte' _ t _ = t
  {-# INLINE mrgIte' #-}

instance (SimpleMergeable bool c) => (SimpleMergeable' bool (K1 i c)) where
  mrgIte' cond (K1 a) (K1 b) = K1 $ mrgIte cond a b
  {-# INLINE mrgIte' #-}

instance (SimpleMergeable' bool a) => (SimpleMergeable' bool (M1 i c a)) where
  mrgIte' cond (M1 a) (M1 b) = M1 $ mrgIte' cond a b
  {-# INLINE mrgIte' #-}

instance (SimpleMergeable' bool a, SimpleMergeable' bool b) => (SimpleMergeable' bool (a :*: b)) where
  mrgIte' cond (a1 :*: a2) (b1 :*: b2) = mrgIte' cond a1 b1 :*: mrgIte' cond a2 b2
  {-# INLINE mrgIte' #-}

-- | This class indicates that a type has a simple root merge strategy.
class GMergeable bool a => SimpleMergeable bool a where
  -- | Performs if-then-else with the simple root merge strategy.
  mrgIte :: bool -> a -> a -> a

instance (Generic a, GMergeable' bool (Rep a), SimpleMergeable' bool (Rep a)) => SimpleMergeable bool (Default a) where
  mrgIte cond (Default a) (Default b) = Default $ to $ mrgIte' cond (from a) (from b)
  {-# INLINE mrgIte #-}

-- | Lifting of the 'SimpleMergeable' class to unary type constructors.
class SimpleMergeable1 bool u where
  -- | Lift 'mrgIte' through the type constructor.
  liftMrgIte :: (bool -> a -> a -> a) -> bool -> u a -> u a -> u a

-- | Lift the standard 'mrgIte' function through the type constructor.
mrgIte1 :: (SimpleMergeable1 bool u, SimpleMergeable bool a) => bool -> u a -> u a -> u a
mrgIte1 = liftMrgIte mrgIte
{-# INLINE mrgIte1 #-}

-- | Lifting of the 'SimpleMergeable' class to binary type constructors.
class (GMergeable2 bool u) => SimpleMergeable2 bool u where
  -- | Lift 'mrgIte' through the type constructor.
  liftMrgIte2 :: (bool -> a -> a -> a) -> (bool -> b -> b -> b) -> bool -> u a b -> u a b -> u a b

-- | Lift the standard 'mrgIte' function through the type constructor.
mrgIte2 :: (SimpleMergeable2 bool u, SimpleMergeable bool a, SimpleMergeable bool b) => bool -> u a b -> u a b -> u a b
mrgIte2 = liftMrgIte2 mrgIte mrgIte
{-# INLINE mrgIte2 #-}

-- | Special case of the 'Mergeable1' and 'SimpleMergeable1' class for type constructors
-- that are 'SimpleMergeable' when applied to any 'Mergeable' types.
--
-- Usually it is Union-like structures.
class (SimpleMergeable1 bool u, GMergeable1 bool u, SymBoolOp bool) => UnionLike bool u | u -> bool where
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
mrgIf :: (UnionLike bool u, GMergeable bool a) => bool -> u a -> u a -> u a
mrgIf = mrgIfWithStrategy gmergingStrategy
{-# INLINE mrgIf #-}

-- | Merge the contents with the type's root merge strategy.
--
-- | Equivalent to @mergeWithStrategy gmergingStrategy@.
merge :: (UnionLike bool u, GMergeable bool a) => u a -> u a
merge = mergeWithStrategy gmergingStrategy
{-# INLINE merge #-}

-- | Wrap a single value in the type and propagate the type's root merge strategy.
--
-- | Equivalent to @mrgSingleWithStrategy gmergingStrategy@.
mrgSingle :: (UnionLike bool u, GMergeable bool a) => a -> u a
mrgSingle = mrgSingleWithStrategy gmergingStrategy
{-# INLINE mrgSingle #-}

instance (SymBoolOp bool) => SimpleMergeable bool () where
  mrgIte _ t _ = t
  {-# INLINE mrgIte #-}

instance (SymBoolOp bool, SimpleMergeable bool a, SimpleMergeable bool b) => SimpleMergeable bool (a, b) where
  mrgIte cond (a1, b1) (a2, b2) = (mrgIte cond a1 a2, mrgIte cond b1 b2)
  {-# INLINE mrgIte #-}

instance (SymBoolOp bool, SimpleMergeable bool a) => SimpleMergeable1 bool ((,) a) where
  liftMrgIte mb cond (a1, b1) (a2, b2) = (mrgIte cond a1 a2, mb cond b1 b2)
  {-# INLINE liftMrgIte #-}

instance (SymBoolOp bool) => SimpleMergeable2 bool (,) where
  liftMrgIte2 ma mb cond (a1, b1) (a2, b2) = (ma cond a1 a2, mb cond b1 b2)
  {-# INLINE liftMrgIte2 #-}

instance
  (SymBoolOp bool, SimpleMergeable bool a, SimpleMergeable bool b, SimpleMergeable bool c) =>
  SimpleMergeable bool (a, b, c)
  where
  mrgIte cond (a1, b1, c1) (a2, b2, c2) = (mrgIte cond a1 a2, mrgIte cond b1 b2, mrgIte cond c1 c2)
  {-# INLINE mrgIte #-}

instance
  ( SymBoolOp bool,
    SimpleMergeable bool a,
    SimpleMergeable bool b,
    SimpleMergeable bool c,
    SimpleMergeable bool d
  ) =>
  SimpleMergeable bool (a, b, c, d)
  where
  mrgIte cond (a1, b1, c1, d1) (a2, b2, c2, d2) =
    (mrgIte cond a1 a2, mrgIte cond b1 b2, mrgIte cond c1 c2, mrgIte cond d1 d2)
  {-# INLINE mrgIte #-}

instance
  ( SymBoolOp bool,
    SimpleMergeable bool a,
    SimpleMergeable bool b,
    SimpleMergeable bool c,
    SimpleMergeable bool d,
    SimpleMergeable bool e
  ) =>
  SimpleMergeable bool (a, b, c, d, e)
  where
  mrgIte cond (a1, b1, c1, d1, e1) (a2, b2, c2, d2, e2) =
    (mrgIte cond a1 a2, mrgIte cond b1 b2, mrgIte cond c1 c2, mrgIte cond d1 d2, mrgIte cond e1 e2)
  {-# INLINE mrgIte #-}

instance
  ( SymBoolOp bool,
    SimpleMergeable bool a,
    SimpleMergeable bool b,
    SimpleMergeable bool c,
    SimpleMergeable bool d,
    SimpleMergeable bool e,
    SimpleMergeable bool f
  ) =>
  SimpleMergeable bool (a, b, c, d, e, f)
  where
  mrgIte cond (a1, b1, c1, d1, e1, f1) (a2, b2, c2, d2, e2, f2) =
    (mrgIte cond a1 a2, mrgIte cond b1 b2, mrgIte cond c1 c2, mrgIte cond d1 d2, mrgIte cond e1 e2, mrgIte cond f1 f2)
  {-# INLINE mrgIte #-}

instance
  ( SymBoolOp bool,
    SimpleMergeable bool a,
    SimpleMergeable bool b,
    SimpleMergeable bool c,
    SimpleMergeable bool d,
    SimpleMergeable bool e,
    SimpleMergeable bool f,
    SimpleMergeable bool g
  ) =>
  SimpleMergeable bool (a, b, c, d, e, f, g)
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
  ( SymBoolOp bool,
    SimpleMergeable bool a,
    SimpleMergeable bool b,
    SimpleMergeable bool c,
    SimpleMergeable bool d,
    SimpleMergeable bool e,
    SimpleMergeable bool f,
    SimpleMergeable bool g,
    SimpleMergeable bool h
  ) =>
  SimpleMergeable bool (a, b, c, d, e, f, g, h)
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

instance (SymBoolOp bool, SimpleMergeable bool b) => SimpleMergeable bool (a -> b) where
  mrgIte = mrgIte1
  {-# INLINE mrgIte #-}

instance (SymBoolOp bool) => SimpleMergeable1 bool ((->) a) where
  liftMrgIte ms cond t f v = ms cond (t v) (f v)
  {-# INLINE liftMrgIte #-}

instance (SymBoolOp bool, UnionLike bool m, GMergeable bool a) => SimpleMergeable bool (MaybeT m a) where
  mrgIte = mrgIf
  {-# INLINE mrgIte #-}

instance (SymBoolOp bool, UnionLike bool m) => SimpleMergeable1 bool (MaybeT m) where
  liftMrgIte m = mrgIfWithStrategy (SimpleStrategy m)
  {-# INLINE liftMrgIte #-}

instance (SymBoolOp bool, UnionLike bool m) => UnionLike bool (MaybeT m) where
  mergeWithStrategy s (MaybeT v) = MaybeT $ mergeWithStrategy (liftGMergingStrategy s) v
  {-# INLINE mergeWithStrategy #-}
  mrgIfWithStrategy s cond (MaybeT t) (MaybeT f) = MaybeT $ mrgIfWithStrategy (liftGMergingStrategy s) cond t f
  {-# INLINE mrgIfWithStrategy #-}
  single = MaybeT . single . return
  {-# INLINE single #-}
  unionIf cond (MaybeT l) (MaybeT r) = MaybeT $ unionIf cond l r
  {-# INLINE unionIf #-}

instance
  (SymBoolOp bool, UnionLike bool m, GMergeable bool e, GMergeable bool a) =>
  SimpleMergeable bool (ExceptT e m a)
  where
  mrgIte = mrgIf
  {-# INLINE mrgIte #-}

instance
  (SymBoolOp bool, UnionLike bool m, GMergeable bool e) =>
  SimpleMergeable1 bool (ExceptT e m)
  where
  liftMrgIte m = mrgIfWithStrategy (SimpleStrategy m)
  {-# INLINE liftMrgIte #-}

instance
  (SymBoolOp bool, UnionLike bool m, GMergeable bool e) =>
  UnionLike bool (ExceptT e m)
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
  (SymBoolOp bool, GMergeable bool s, GMergeable bool a, UnionLike bool m) =>
  SimpleMergeable bool (StateLazy.StateT s m a)
  where
  mrgIte = mrgIf
  {-# INLINE mrgIte #-}

instance
  (SymBoolOp bool, GMergeable bool s, UnionLike bool m) =>
  SimpleMergeable1 bool (StateLazy.StateT s m)
  where
  liftMrgIte m = mrgIfWithStrategy (SimpleStrategy m)
  {-# INLINE liftMrgIte #-}

instance
  (SymBoolOp bool, GMergeable bool s, UnionLike bool m) =>
  UnionLike bool (StateLazy.StateT s m)
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
  (SymBoolOp bool, GMergeable bool s, GMergeable bool a, UnionLike bool m) =>
  SimpleMergeable bool (StateStrict.StateT s m a)
  where
  mrgIte = mrgIf
  {-# INLINE mrgIte #-}

instance
  (SymBoolOp bool, GMergeable bool s, UnionLike bool m) =>
  SimpleMergeable1 bool (StateStrict.StateT s m)
  where
  liftMrgIte m = mrgIfWithStrategy (SimpleStrategy m)
  {-# INLINE liftMrgIte #-}

instance
  (SymBoolOp bool, GMergeable bool s, UnionLike bool m) =>
  UnionLike bool (StateStrict.StateT s m)
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
  (SymBoolOp bool, GMergeable bool s, GMergeable bool a, UnionLike bool m, Monoid s) =>
  SimpleMergeable bool (WriterLazy.WriterT s m a)
  where
  mrgIte = mrgIf
  {-# INLINE mrgIte #-}

instance
  (SymBoolOp bool, GMergeable bool s, UnionLike bool m, Monoid s) =>
  SimpleMergeable1 bool (WriterLazy.WriterT s m)
  where
  liftMrgIte m = mrgIfWithStrategy (SimpleStrategy m)
  {-# INLINE liftMrgIte #-}

instance
  (SymBoolOp bool, GMergeable bool s, UnionLike bool m, Monoid s) =>
  UnionLike bool (WriterLazy.WriterT s m)
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
  (SymBoolOp bool, GMergeable bool s, GMergeable bool a, UnionLike bool m, Monoid s) =>
  SimpleMergeable bool (WriterStrict.WriterT s m a)
  where
  mrgIte = mrgIf
  {-# INLINE mrgIte #-}

instance
  (SymBoolOp bool, GMergeable bool s, UnionLike bool m, Monoid s) =>
  SimpleMergeable1 bool (WriterStrict.WriterT s m)
  where
  liftMrgIte m = mrgIfWithStrategy (SimpleStrategy m)
  {-# INLINE liftMrgIte #-}

instance
  (SymBoolOp bool, GMergeable bool s, UnionLike bool m, Monoid s) =>
  UnionLike bool (WriterStrict.WriterT s m)
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
  (SymBoolOp bool, GMergeable bool a, UnionLike bool m) =>
  SimpleMergeable bool (ReaderT s m a)
  where
  mrgIte = mrgIf
  {-# INLINE mrgIte #-}

instance
  (SymBoolOp bool, UnionLike bool m) =>
  SimpleMergeable1 bool (ReaderT s m)
  where
  liftMrgIte m = mrgIfWithStrategy (SimpleStrategy m)
  {-# INLINE liftMrgIte #-}

instance
  (SymBoolOp bool, UnionLike bool m) =>
  UnionLike bool (ReaderT s m)
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

instance (SymBoolOp bool, SimpleMergeable bool a) => SimpleMergeable bool (Identity a) where
  mrgIte cond (Identity l) (Identity r) = Identity $ mrgIte cond l r
  {-# INLINE mrgIte #-}

instance (SymBoolOp bool) => SimpleMergeable1 bool Identity where
  liftMrgIte mite cond (Identity l) (Identity r) = Identity $ mite cond l r
  {-# INLINE liftMrgIte #-}

instance (SymBoolOp bool, UnionLike bool m, GMergeable bool a) => SimpleMergeable bool (IdentityT m a) where
  mrgIte = mrgIf
  {-# INLINE mrgIte #-}

instance (SymBoolOp bool, UnionLike bool m) => SimpleMergeable1 bool (IdentityT m) where
  liftMrgIte m = mrgIfWithStrategy (SimpleStrategy m)
  {-# INLINE liftMrgIte #-}

instance (SymBoolOp bool, UnionLike bool m) => UnionLike bool (IdentityT m) where
  mergeWithStrategy ms (IdentityT f) =
    IdentityT $ mergeWithStrategy ms f
  {-# INLINE mergeWithStrategy #-}
  mrgIfWithStrategy s cond (IdentityT l) (IdentityT r) = IdentityT $ mrgIfWithStrategy s cond l r
  {-# INLINE mrgIfWithStrategy #-}
  single x = IdentityT $ single x
  {-# INLINE single #-}
  unionIf cond (IdentityT l) (IdentityT r) = IdentityT $ unionIf cond l r
  {-# INLINE unionIf #-}

instance (SymBoolOp bool, UnionLike bool m, GMergeable bool r) => SimpleMergeable bool (ContT r m a) where
  mrgIte cond (ContT l) (ContT r) = ContT $ \c -> mrgIf cond (l c) (r c)
  {-# INLINE mrgIte #-}

instance (SymBoolOp bool, UnionLike bool m, GMergeable bool r) => SimpleMergeable1 bool (ContT r m) where
  liftMrgIte m = mrgIfWithStrategy (SimpleStrategy m)
  {-# INLINE liftMrgIte #-}

instance (SymBoolOp bool, UnionLike bool m, GMergeable bool r) => UnionLike bool (ContT r m) where
  mergeWithStrategy _ (ContT f) = ContT $ \c -> merge (f c)
  {-# INLINE mergeWithStrategy #-}
  mrgIfWithStrategy _ cond (ContT l) (ContT r) = ContT $ \c -> mrgIf cond (l c) (r c)
  {-# INLINE mrgIfWithStrategy #-}
  single x = ContT $ \c -> c x
  {-# INLINE single #-}
  unionIf cond (ContT l) (ContT r) = ContT $ \c -> unionIf cond (l c) (r c)
  {-# INLINE unionIf #-}

instance
  (SymBoolOp bool, GMergeable bool s, GMergeable bool w, Monoid w, GMergeable bool a, UnionLike bool m) =>
  SimpleMergeable bool (RWSLazy.RWST r w s m a)
  where
  mrgIte = mrgIf
  {-# INLINE mrgIte #-}

instance
  (SymBoolOp bool, GMergeable bool s, GMergeable bool w, Monoid w, UnionLike bool m) =>
  SimpleMergeable1 bool (RWSLazy.RWST r w s m)
  where
  liftMrgIte m = mrgIfWithStrategy (SimpleStrategy m)
  {-# INLINE liftMrgIte #-}

instance
  (SymBoolOp bool, GMergeable bool s, GMergeable bool w, Monoid w, UnionLike bool m) =>
  UnionLike bool (RWSLazy.RWST r w s m)
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
  (SymBoolOp bool, GMergeable bool s, GMergeable bool w, Monoid w, GMergeable bool a, UnionLike bool m) =>
  SimpleMergeable bool (RWSStrict.RWST r w s m a)
  where
  mrgIte = mrgIf
  {-# INLINE mrgIte #-}

instance
  (SymBoolOp bool, GMergeable bool s, GMergeable bool w, Monoid w, UnionLike bool m) =>
  SimpleMergeable1 bool (RWSStrict.RWST r w s m)
  where
  liftMrgIte m = mrgIfWithStrategy (SimpleStrategy m)
  {-# INLINE liftMrgIte #-}

instance
  (SymBoolOp bool, GMergeable bool s, GMergeable bool w, Monoid w, UnionLike bool m) =>
  UnionLike bool (RWSStrict.RWST r w s m)
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

class (UnionLike bool u) => UnionPrjOp bool (u :: Type -> Type) | u -> bool where
  -- | Pattern match to extract single values.
  singleView :: u a -> Maybe a

  -- | Pattern match to extract if values.
  ifView :: u a -> Maybe (bool, u a, u a)

  -- | The leftmost value in the union.
  leftMost :: u a -> a

-- | Pattern match to extract single values with 'singleView'.
pattern SingleU :: UnionPrjOp bool u => a -> u a
pattern SingleU x <-
  (singleView -> Just x)
  where
    SingleU x = single x

-- | Pattern match to extract guard values with 'guardView'
pattern IfU :: UnionPrjOp bool u => bool -> u a -> u a -> u a
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
getSingle :: forall bool u a. (SimpleMergeable bool a, UnionLike bool u, UnionPrjOp bool u) => u a -> a
getSingle u = case merge u of
  SingleU x -> x
  _ -> error "Should not happen"
{-# INLINE getSingle #-}

-- | Helper for applying functions on 'UnionPrjOp' and 'SimpleMergeable'.
--
-- >>> let f :: Integer -> UnionM Integer = \x -> mrgIf (ssymb "a") (mrgSingle $ x + 1) (mrgSingle $ x + 2)
-- >>> f #~ (mrgIf (ssymb "b" :: SymBool) (mrgSingle 0) (mrgSingle 2) :: UnionM Integer)
-- UMrg (If (&& b a) (Single 1) (If b (Single 2) (If a (Single 3) (Single 4))))
(#~) ::
  (SymBoolOp bool, Function f, SimpleMergeable bool (Ret f), UnionPrjOp bool u, Functor u) =>
  f ->
  u (Arg f) ->
  Ret f
(#~) f u = getSingle $ fmap (f #) u
{-# INLINE (#~) #-}

infixl 9 #~
