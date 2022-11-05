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

symle :: (SOrd a) => a -> a -> SymBool
symle = gsymle

symgt :: (SOrd a) => a -> a -> SymBool
symgt = gsymgt

symge :: (SOrd a) => a -> a -> SymBool
symge = gsymge

symCompare :: (UnionLike SymBool u, Monad u, SOrd a) => a -> a -> u Ordering
symCompare = gsymCompare

(>~) :: (SOrd a) => a -> a -> SymBool
(>~) = symgt

(>=~) :: (SOrd a) => a -> a -> SymBool
(>=~) = symge

(<~) :: (SOrd a) => a -> a -> SymBool
(<~) = symlt

(<=~) :: (SOrd a) => a -> a -> SymBool
(<=~) = symle
