{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

-- |
-- Module      :   Grisette.IR.SymPrim.Data.Class.SimpleMergeable
-- Copyright   :   (c) Sirui Lu 2021-2022
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
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

-- | 'GSimpleMergeable' specialized with 'SymBool'
type SimpleMergeable a = GSimpleMergeable SymBool a

-- | 'gmrgIte' specialized with 'SymBool'
mrgIte :: (SimpleMergeable a) => SymBool -> a -> a -> a
mrgIte = gmrgIte
{-# INLINE mrgIte #-}

-- | 'GSimpleMergeable1' specialized with 'SymBool'
type SimpleMergeable1 f = GSimpleMergeable1 SymBool f

-- | 'liftGMrgIte' specialized with 'SymBool'
liftMrgIte :: (SimpleMergeable1 f) => (SymBool -> a -> a -> a) -> SymBool -> f a -> f a -> f a
liftMrgIte = liftGMrgIte
{-# INLINE liftMrgIte #-}

-- | 'gmrgIte1' specialized with 'SymBool'
mrgIte1 :: (SimpleMergeable1 f, SimpleMergeable a) => SymBool -> f a -> f a -> f a
mrgIte1 = gmrgIte1
{-# INLINE mrgIte1 #-}

-- | 'GSimpleMergeable2' specialized with 'SymBool'
type SimpleMergeable2 f = GSimpleMergeable2 SymBool f

-- | 'liftGMrgIte2' specialized with 'SymBool'
liftMrgIte2 ::
  (SimpleMergeable2 f) =>
  (SymBool -> a -> a -> a) ->
  (SymBool -> b -> b -> b) ->
  SymBool ->
  f a b ->
  f a b ->
  f a b
liftMrgIte2 = liftGMrgIte2
{-# INLINE liftMrgIte2 #-}

-- | 'gmrgIte2' specialized with 'SymBool'
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
{-# INLINE mrgIte2 #-}

-- | 'GUnionLike' specialized with 'SymBool'
type UnionLike u = GUnionLike SymBool u

-- | 'GUnionPrjOp' specialized with 'SymBool'
type UnionPrjOp u = GUnionPrjOp SymBool u
