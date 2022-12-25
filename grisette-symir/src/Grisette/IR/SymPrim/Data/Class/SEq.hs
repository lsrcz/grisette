{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

-- |
-- Module      :   Grisette.IR.SymPrim.Data.Class.SEq
-- Copyright   :   (c) Sirui Lu 2021-2022
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.IR.SymPrim.Data.Class.SEq
  ( SEq,
    symeq,
    symne,
    (==~),
    (/=~),
  )
where

import Grisette.Core.Data.Class.Bool
import Grisette.IR.SymPrim.Data.SymPrim

-- | 'GSEq' specialized with 'SymBool'
type SEq a = GSEq SymBool a

-- | 'gsymeq' specialized with 'SymBool'
symeq :: (SEq a) => a -> a -> SymBool
symeq = gsymeq
{-# INLINE symeq #-}

-- | 'gsymne' specialized with 'SymBool'
symne :: (SEq a) => a -> a -> SymBool
symne = gsymne
{-# INLINE symne #-}

-- | Infix version of 'symeq'
(==~) :: (SEq a) => a -> a -> SymBool
(==~) = symeq
{-# INLINE (==~) #-}

infix 4 ==~

-- | Infix version of 'symne'
(/=~) :: (SEq a) => a -> a -> SymBool
(/=~) = symne
{-# INLINE (/=~) #-}

infix 4 /=~
