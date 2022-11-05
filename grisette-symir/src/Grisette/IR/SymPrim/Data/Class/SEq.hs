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

symne :: (SEq a) => a -> a -> SymBool
symne = gsymne

(==~) :: (SEq a) => a -> a -> SymBool
(==~) = symeq

(/=~) :: (SEq a) => a -> a -> SymBool
(/=~) = symne
