{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Grisette.IR.SymPrim.Data.Class.SimpleMergeable
  ( SimpleMergeable,
    mrgIte,
    SimpleMergeable1,
    liftMrgIte,
    mrgIte1,
    SimpleMergeable2,
    liftMrgIte2,
    mrgIte2,
    UnionLike,
    UnionPrjOp,
  )
where

import Grisette.Core.Data.Class.SimpleMergeable
import Grisette.IR.SymPrim.Data.SymPrim

type SimpleMergeable a = GSimpleMergeable SymBool a

mrgIte :: (SimpleMergeable a) => SymBool -> a -> a -> a
mrgIte = gmrgIte

type SimpleMergeable1 f = GSimpleMergeable1 SymBool f

liftMrgIte :: (SimpleMergeable1 f) => (SymBool -> a -> a -> a) -> SymBool -> f a -> f a -> f a
liftMrgIte = liftGMrgIte

mrgIte1 :: (SimpleMergeable1 f, SimpleMergeable a) => SymBool -> f a -> f a -> f a
mrgIte1 = gmrgIte1

type SimpleMergeable2 f = GSimpleMergeable2 SymBool f

liftMrgIte2 ::
  (SimpleMergeable2 f) =>
  (SymBool -> a -> a -> a) ->
  (SymBool -> b -> b -> b) ->
  SymBool ->
  f a b ->
  f a b ->
  f a b
liftMrgIte2 = liftGMrgIte2

mrgIte2 ::
  ( SimpleMergeable2 f,
    SimpleMergeable a,
    SimpleMergeable b
  ) =>
  SymBool ->
  f a b ->
  f a b ->
  f a b
mrgIte2 = gmrgIte2

type UnionLike u = GUnionLike SymBool u

type UnionPrjOp u = GUnionPrjOp SymBool u
