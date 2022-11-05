{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

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

type SEq a = GSEq SymBool a

symeq :: (SEq a) => a -> a -> SymBool
symeq = gsymeq
{-# INLINE symeq #-}

symne :: (SEq a) => a -> a -> SymBool
symne = gsymne
{-# INLINE symne #-}

(==~) :: (SEq a) => a -> a -> SymBool
(==~) = symeq
{-# INLINE (==~) #-}

(/=~) :: (SEq a) => a -> a -> SymBool
(/=~) = symne
{-# INLINE (/=~) #-}
