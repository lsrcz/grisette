{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

-- |
-- Module      :   Grisette.IR.SymPrim.Data.Class.SOrd
-- Copyright   :   (c) Sirui Lu 2021-2022
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
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
import Grisette.IR.SymPrim.Data.Class.SimpleMergeable
import Grisette.IR.SymPrim.Data.SymPrim

-- | 'GSOrd' specialized with 'SymBool'
type SOrd a = GSOrd SymBool a

-- | 'gsymlt' specialized with 'SymBool'
symlt :: (SOrd a) => a -> a -> SymBool
symlt = gsymlt
{-# INLINE symlt #-}

-- | 'gsymle' specialized with 'SymBool'
symle :: (SOrd a) => a -> a -> SymBool
symle = gsymle
{-# INLINE symle #-}

-- | 'gsymgt' specialized with 'SymBool'
symgt :: (SOrd a) => a -> a -> SymBool
symgt = gsymgt
{-# INLINE symgt #-}

-- | 'gsymge' specialized with 'SymBool'
symge :: (SOrd a) => a -> a -> SymBool
symge = gsymge
{-# INLINE symge #-}

-- | 'gsymCompare' specialized with 'SymBool'
symCompare :: (UnionLike u, Monad u, SOrd a) => a -> a -> u Ordering
symCompare = gsymCompare
{-# INLINE symCompare #-}

-- | Infix version of 'symgt'
(>~) :: (SOrd a) => a -> a -> SymBool
(>~) = symgt
{-# INLINE (>~) #-}

infix 4 >~

-- | Infix version of 'symge'
(>=~) :: (SOrd a) => a -> a -> SymBool
(>=~) = symge
{-# INLINE (>=~) #-}

infix 4 >=~

-- | Infix version of 'symlt'
(<~) :: (SOrd a) => a -> a -> SymBool
(<~) = symlt
{-# INLINE (<~) #-}

infix 4 <~

-- | Infix version of 'symle'
(<=~) :: (SOrd a) => a -> a -> SymBool
(<=~) = symle
{-# INLINE (<=~) #-}

infix 4 <=~
