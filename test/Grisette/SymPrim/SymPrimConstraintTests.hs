{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module Grisette.SymPrim.SymPrimConstraintTests
  ( symBool,
    symInteger,
    symWordN8,
    symIntN8,
    symFP32,
    symAlgReal,
    someSymWordN,
    someSymIntN,
    bool,
    integer,
    wordN8,
    intN8,
    fp32,
    algReal,
    someWordN,
    someIntN,
  )
where

import Grisette
  ( AlgReal,
    BasicSymPrim,
    FP32,
    IntN8,
    Prim,
    SomeSymWordN,
    SymAlgReal,
    SymBool,
    SymFP32,
    SymIntN8,
    SymInteger,
    SymPrim,
    SymWordN8,
    WordN8,
  )
import Grisette.Internal.SymPrim.SomeBV (SomeIntN, SomeSymIntN, SomeWordN)

data BasicSymPrimType a where
  BasicSymPrimType :: (BasicSymPrim a) => BasicSymPrimType a

symBool :: BasicSymPrimType SymBool
symBool = BasicSymPrimType

symInteger :: BasicSymPrimType SymInteger
symInteger = BasicSymPrimType

symWordN8 :: BasicSymPrimType SymWordN8
symWordN8 = BasicSymPrimType

symIntN8 :: BasicSymPrimType SymIntN8
symIntN8 = BasicSymPrimType

symFP32 :: BasicSymPrimType SymFP32
symFP32 = BasicSymPrimType

symAlgReal :: BasicSymPrimType SymAlgReal
symAlgReal = BasicSymPrimType

data SymPrimType a where
  SymPrimType :: (SymPrim a) => SymPrimType a

someSymWordN :: SymPrimType SomeSymWordN
someSymWordN = SymPrimType

someSymIntN :: SymPrimType SomeSymIntN
someSymIntN = SymPrimType

data PrimType a where
  PrimType :: (Prim a) => PrimType a

bool :: PrimType Bool
bool = PrimType

integer :: PrimType Integer
integer = PrimType

wordN8 :: PrimType WordN8
wordN8 = PrimType

intN8 :: PrimType IntN8
intN8 = PrimType

fp32 :: PrimType FP32
fp32 = PrimType

algReal :: PrimType AlgReal
algReal = PrimType

someWordN :: PrimType SomeWordN
someWordN = PrimType

someIntN :: PrimType SomeIntN
someIntN = PrimType
