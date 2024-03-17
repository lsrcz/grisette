{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Grisette.Lib.Lens.Micro
  ( (.%~),
    mrgOver,
    (.+~),
    (.-~),
    (.<>~),
    (..~),
    mrgSet,
    (.?~),
    (..?~),
    (.<%~),
    (.<<%~),
    (.<<.~),
    mrgRewriteOf,
    mrgTransformOf,
    (.^.),
    (.^..),
    (.^?),
    mrgTraverseOf_,
    mrgForOf_,
  )
where

import Data.Monoid (Endo, First)
import Grisette.Core.Control.Monad.UnionM (UnionM)
import Grisette.Core.Data.Class.Mergeable (Mergeable)
import Grisette.Core.Data.Class.PlainUnion (simpleMerge)
import Grisette.Core.Data.Class.SimpleMergeable
  ( SimpleMergeable,
  )
import Grisette.Core.Data.Class.TryMerge (TryMerge)
import Grisette.Lib.Control.Applicative (mrgPure, (.*>))
import Grisette.Lib.Control.Monad (mrgFmap, mrgReturn, mrgSequence, mrgVoid)
import Lens.Micro
  ( ASetter,
    Getting,
    LensLike,
    over,
    (%~),
    (.~),
    (<%~),
    (<<%~),
    (<<.~),
    (?~),
    (^.),
    (^..),
    (^?),
  )
import Lens.Micro.Internal (foldMapOf, (#.))

(.%~) ::
  (Mergeable a, Mergeable b) =>
  ASetter s t (UnionM a) (UnionM b) ->
  (a -> b) ->
  s ->
  t
setter .%~ f = setter %~ (mrgFmap f)
{-# INLINE (.%~) #-}

mrgOver ::
  (Mergeable a, Mergeable b) =>
  ASetter s t (UnionM a) (UnionM b) ->
  (a -> b) ->
  s ->
  t
mrgOver = (.%~)
{-# INLINE mrgOver #-}

(.+~) ::
  (Num a, Mergeable a) => ASetter s t (UnionM a) (UnionM a) -> a -> s -> t
setter .+~ a = setter .%~ (+ a)
{-# INLINE (.+~) #-}

(.-~) ::
  (Num a, Mergeable a) => ASetter s t (UnionM a) (UnionM a) -> a -> s -> t
setter .-~ a = setter .%~ (\x -> x - a)
{-# INLINE (.-~) #-}

(.<>~) ::
  (Monoid a, Mergeable a) =>
  ASetter s t (UnionM a) (UnionM a) ->
  a ->
  s ->
  t
setter .<>~ a = setter .%~ (<> a)
{-# INLINE (.<>~) #-}

(..~) :: (Mergeable b) => ASetter s t a (UnionM b) -> b -> s -> t
setter ..~ x = setter .~ (mrgReturn x)
{-# INLINE (..~) #-}

mrgSet :: (Mergeable b) => ASetter s t a (UnionM b) -> b -> s -> t
mrgSet = (..~)
{-# INLINE mrgSet #-}

(.?~) :: (Mergeable b) => ASetter s t a (UnionM (Maybe b)) -> b -> s -> t
setter .?~ b = setter .~ mrgReturn (Just b)
{-# INLINE (.?~) #-}

(..?~) :: (Mergeable b) => ASetter s t a (Maybe (UnionM b)) -> b -> s -> t
setter ..?~ b = setter ?~ mrgReturn b
{-# INLINE (..?~) #-}

(.<%~) ::
  (Mergeable a, Mergeable b) =>
  LensLike ((,) (UnionM b)) s t (UnionM a) (UnionM b) ->
  (a -> b) ->
  s ->
  (UnionM b, t)
setter .<%~ f = setter <%~ (mrgFmap f)
{-# INLINE (.<%~) #-}

(.<<%~) ::
  (Mergeable a, Mergeable b) =>
  LensLike ((,) (UnionM a)) s t (UnionM a) (UnionM b) ->
  (a -> b) ->
  s ->
  (UnionM a, t)
setter .<<%~ f = setter <<%~ (mrgFmap f)
{-# INLINE (.<<%~) #-}

(.<<.~) ::
  (Mergeable b) =>
  LensLike ((,) (UnionM a)) s t (UnionM a) (UnionM b) ->
  b ->
  s ->
  (UnionM a, t)
setter .<<.~ b = setter <<.~ mrgReturn b
{-# INLINE (.<<.~) #-}

mrgRewriteOf ::
  (Mergeable b) =>
  ASetter a b (UnionM a) (UnionM b) ->
  (b -> UnionM (Maybe a)) ->
  a ->
  UnionM b
mrgRewriteOf l f = go
  where
    go = mrgTransformOf l $ \x -> do
      res <- f x
      case res of
        Nothing -> mrgReturn x
        Just y -> go y
{-# INLINE mrgRewriteOf #-}

mrgTransformOf ::
  (Mergeable b) =>
  ASetter a b (UnionM a) (UnionM b) ->
  (b -> UnionM b) ->
  a ->
  UnionM b
mrgTransformOf l f = go
  where
    go = f . over l (>>= go)
{-# INLINE mrgTransformOf #-}

(.^.) :: (SimpleMergeable a) => UnionM s -> Getting a s a -> a
s .^. getter = simpleMerge $ (^. getter) <$> s
{-# INLINE (.^.) #-}

(.^..) ::
  (Mergeable s, Mergeable a) =>
  s ->
  Getting (Endo [UnionM a]) s (UnionM a) ->
  UnionM [a]
s .^.. getter = mrgSequence (s ^.. getter)
{-# INLINE (.^..) #-}

(.^?) ::
  (Mergeable s, Mergeable a) =>
  s ->
  Getting (First (UnionM a)) s (UnionM a) ->
  UnionM (Maybe a)
s .^? getter = mrgSequence (s ^? getter)
{-# INLINE (.^?) #-}

newtype MrgTraversed_ f = MrgTraversed_ {getMrgTraversed_ :: f ()}

instance (Applicative f, TryMerge f) => Monoid (MrgTraversed_ f) where
  mempty = MrgTraversed_ (mrgPure ())
  {-# INLINE mempty #-}

instance (Applicative f, TryMerge f) => Semigroup (MrgTraversed_ f) where
  MrgTraversed_ ma <> MrgTraversed_ mb = MrgTraversed_ (ma .*> mb)
  {-# INLINE (<>) #-}

mrgTraverseOf_ ::
  (Functor f, TryMerge f) =>
  Getting (MrgTraversed_ f) s a ->
  (a -> f r) ->
  s ->
  f ()
mrgTraverseOf_ l f =
  mrgVoid . getMrgTraversed_ #. foldMapOf l (MrgTraversed_ #. mrgVoid . f)
{-# INLINE mrgTraverseOf_ #-}

mrgForOf_ ::
  (Functor f, TryMerge f) =>
  Getting (MrgTraversed_ f) s a ->
  s ->
  (a -> f r) ->
  f ()
mrgForOf_ l s f = mrgTraverseOf_ l f s
{-# INLINE mrgForOf_ #-}
