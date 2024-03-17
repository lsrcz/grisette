module Grisette.Lib.Data.Function ((.$), (.&), mrgOn) where

import Grisette.Core.Control.Monad.UnionM (UnionM)
import Grisette.Core.Data.Class.Mergeable (Mergeable)
import Grisette.Core.Data.Class.PlainUnion (simpleMerge)
import Grisette.Core.Data.Class.SimpleMergeable (SimpleMergeable)
import Grisette.Lib.Control.Applicative ((.<*>))
import Grisette.Lib.Data.Functor (mrgFmap, (.<$>))

(.$) :: (Mergeable a, SimpleMergeable b) => (a -> b) -> UnionM a -> b
(.$) f u = simpleMerge $ mrgFmap f u
{-# INLINE (.$) #-}

infixr 0 .$

(.&) :: (Mergeable a, SimpleMergeable b) => UnionM a -> (a -> b) -> b
(.&) = flip (.$)
{-# INLINE (.&) #-}

infixl 1 .&

mrgOn ::
  (Mergeable a, Mergeable b, SimpleMergeable c) =>
  (b -> b -> c) ->
  (a -> b) ->
  UnionM a ->
  UnionM a ->
  c
mrgOn f u l r = simpleMerge $ f .<$> mrgFmap u l .<*> mrgFmap u r
