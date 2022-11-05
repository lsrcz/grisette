{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Grisette.IR.SymPrim.Data.Class.SOrd
  ( SOrd,
    symlt,
    symle,
    symgt,
    symge,
    symCompare,
    (>~),
    (>=~),
    (<~),
    (<=~),
  )
where

import Grisette.Core.Data.Class.SOrd
import Grisette.Core.Data.Class.SimpleMergeable
import Grisette.IR.SymPrim.Data.SymPrim

type SOrd a = GSOrd SymBool a

symlt :: (SOrd a) => a -> a -> SymBool
symlt = gsymlt
{-# INLINE symlt #-}

symle :: (SOrd a) => a -> a -> SymBool
symle = gsymle
{-# INLINE symle #-}

symgt :: (SOrd a) => a -> a -> SymBool
symgt = gsymgt
{-# INLINE symgt #-}

symge :: (SOrd a) => a -> a -> SymBool
symge = gsymge
{-# INLINE symge #-}

symCompare :: (UnionLike SymBool u, Monad u, SOrd a) => a -> a -> u Ordering
symCompare = gsymCompare
{-# INLINE symCompare #-}

(>~) :: (SOrd a) => a -> a -> SymBool
(>~) = symgt
{-# INLINE (>~) #-}

(>=~) :: (SOrd a) => a -> a -> SymBool
(>=~) = symge
{-# INLINE (>=~) #-}

(<~) :: (SOrd a) => a -> a -> SymBool
(<~) = symlt
{-# INLINE (<~) #-}

(<=~) :: (SOrd a) => a -> a -> SymBool
(<=~) = symle
{-# INLINE (<=~) #-}
