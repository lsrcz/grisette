{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      :   Grisette.IR.SymPrim.Data.SymPrim
-- Copyright   :   (c) Sirui Lu 2021-2023
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.IR.SymPrim.Data.SymPrim
  ( SymBool (..),
    SymInteger (..),
    SymWordN (..),
    SymIntN (..),
    SomeSymWordN (..),
    SomeSymIntN (..),
    type (=~>) (..),
    type (-~>) (..),
    (-->),
    ModelSymPair (..),
    symSize,
    symsSize,
  )
where

import Control.DeepSeq
import Control.Monad.Except
import Data.Bits
import Data.Hashable
import Data.Int
import Data.Proxy
import Data.String
import Data.Typeable
import Data.Word
import GHC.Generics
import GHC.TypeNats
import Grisette.Core.Data.Class.BitVector
import Grisette.Core.Data.Class.Bool
import Grisette.Core.Data.Class.Error
import Grisette.Core.Data.Class.Evaluate
import Grisette.Core.Data.Class.ExtractSymbolics
import Grisette.Core.Data.Class.Function
import Grisette.Core.Data.Class.GenSym
import Grisette.Core.Data.Class.Integer
import Grisette.Core.Data.Class.Mergeable
import Grisette.Core.Data.Class.ModelOps
import Grisette.Core.Data.Class.SOrd
import Grisette.Core.Data.Class.SimpleMergeable
import Grisette.Core.Data.Class.Solvable
import Grisette.Core.Data.Class.Substitute
import Grisette.Core.Data.Class.ToCon
import Grisette.Core.Data.Class.ToSym
import Grisette.IR.SymPrim.Data.BV
import Grisette.IR.SymPrim.Data.IntBitwidth
import Grisette.IR.SymPrim.Data.Prim.InternedTerm.InternedCtors
import Grisette.IR.SymPrim.Data.Prim.InternedTerm.Term
import Grisette.IR.SymPrim.Data.Prim.InternedTerm.TermSubstitution
import Grisette.IR.SymPrim.Data.Prim.InternedTerm.TermUtils
import Grisette.IR.SymPrim.Data.Prim.Model
import Grisette.IR.SymPrim.Data.Prim.PartialEval.BV
import Grisette.IR.SymPrim.Data.Prim.PartialEval.Bits
import Grisette.IR.SymPrim.Data.Prim.PartialEval.Bool
import Grisette.IR.SymPrim.Data.Prim.PartialEval.GeneralFun
import Grisette.IR.SymPrim.Data.Prim.PartialEval.Integer
import Grisette.IR.SymPrim.Data.Prim.PartialEval.Num
import Grisette.IR.SymPrim.Data.Prim.PartialEval.TabularFun
import Grisette.IR.SymPrim.Data.TabularFun
import Grisette.Lib.Control.Monad
import Grisette.Utils.Parameterized
import Language.Haskell.TH.Syntax

-- $setup
-- >>> import Grisette.Core
-- >>> import Grisette.IR.SymPrim
-- >>> import Grisette.Backend.SBV
-- >>> import Data.Proxy

-- | Symbolic Boolean type.
--
-- >>> :set -XOverloadedStrings
-- >>> "a" :: SymBool
-- a
-- >>> "a" &&~ "b" :: SymBool
-- (&& a b)
--
-- More symbolic operations are available. Please refer to the documentation
-- for the type class instances.
newtype SymBool = SymBool {underlyingBoolTerm :: Term Bool}
  deriving (Lift, NFData, Generic)

-- | Symbolic (unbounded, mathematical) integer type.
--
-- >>> "a" + 1 :: SymInteger
-- (+ 1 a)
--
-- More symbolic operations are available. Please refer to the documentation
-- for the type class instances.
newtype SymInteger = SymInteger {underlyingIntegerTerm :: Term Integer}
  deriving (Lift, NFData, Generic)

instance SignedDivMod SymInteger where
  divs (SymInteger l) rs@(SymInteger r) =
    mrgIf
      (rs ==~ con 0)
      (throwError $ transformError DivideByZero)
      (mrgReturn $ SymInteger $ pevalDivIntegerTerm l r)
  mods (SymInteger l) rs@(SymInteger r) =
    mrgIf
      (rs ==~ con 0)
      (throwError $ transformError DivideByZero)
      (mrgReturn $ SymInteger $ pevalModIntegerTerm l r)

instance SymIntegerOp SymInteger

-- | Symbolic signed bit vector type. Indexed with the bit width.
-- Signedness affects the semantics of the operations, including
-- comparison/extension, etc.
--
-- >>> :set -XOverloadedStrings -XDataKinds -XBinaryLiterals
-- >>> "a" + 5 :: SymIntN 5
-- (+ 0b00101 a)
-- >>> sizedBVConcat (con 0b101 :: SymIntN 3) (con 0b110 :: SymIntN 3)
-- 0b101110
-- >>> sizedBVExt (Proxy @6) (con 0b101 :: SymIntN 3)
-- 0b111101
-- >>> (8 :: SymIntN 4) <~ (7 :: SymIntN 4)
-- true
--
-- More symbolic operations are available. Please refer to the documentation
-- for the type class instances.
newtype SymIntN (n :: Nat) = SymIntN {underlyingIntNTerm :: Term (IntN n)}
  deriving (Lift, NFData, Generic)

-- | Symbolic signed bit vector type. Not indexed, but the bit width is
-- fixed at the creation time.
--
-- A 'SomeSymIntN' must be created by wrapping a 'SymIntN' with the
-- 'SomeSymIntN' constructor to fix the bit width:
--
-- >>> (SomeSymIntN ("a" :: SymIntN 5))
-- a
--
-- >>> :set -XOverloadedStrings -XDataKinds -XBinaryLiterals
-- >>> (SomeSymIntN ("a" :: SymIntN 5)) + (SomeSymIntN (5 :: SymIntN 5))
-- (+ 0b00101 a)
-- >>> someBVConcat (SomeSymIntN (con 0b101 :: SymIntN 3)) (SomeSymIntN (con 0b110 :: SymIntN 3))
-- 0b101110
--
-- More symbolic operations are available. Please refer to the documentation
-- for the type class instances.
data SomeSymIntN where
  SomeSymIntN :: (KnownNat n, 1 <= n) => SymIntN n -> SomeSymIntN

unarySomeSymIntN :: (forall n. (KnownNat n, 1 <= n) => SymIntN n -> r) -> String -> SomeSymIntN -> r
unarySomeSymIntN op str (SomeSymIntN (w :: SymIntN w)) = op w
{-# INLINE unarySomeSymIntN #-}

unarySomeSymIntNR1 :: (forall n. (KnownNat n, 1 <= n) => SymIntN n -> SymIntN n) -> String -> SomeSymIntN -> SomeSymIntN
unarySomeSymIntNR1 op str (SomeSymIntN (w :: SymIntN w)) = SomeSymIntN $ op w
{-# INLINE unarySomeSymIntNR1 #-}

binSomeSymIntN :: (forall n. (KnownNat n, 1 <= n) => SymIntN n -> SymIntN n -> r) -> String -> SomeSymIntN -> SomeSymIntN -> r
binSomeSymIntN op str (SomeSymIntN (l :: SymIntN l)) (SomeSymIntN (r :: SymIntN r)) =
  case sameNat (Proxy @l) (Proxy @r) of
    Just Refl -> op l r
    Nothing -> error $ "Operation " ++ str ++ " on SymIntN with different bitwidth"
{-# INLINE binSomeSymIntN #-}

binSomeSymIntNR1 :: (forall n. (KnownNat n, 1 <= n) => SymIntN n -> SymIntN n -> SymIntN n) -> String -> SomeSymIntN -> SomeSymIntN -> SomeSymIntN
binSomeSymIntNR1 op str (SomeSymIntN (l :: SymIntN l)) (SomeSymIntN (r :: SymIntN r)) =
  case sameNat (Proxy @l) (Proxy @r) of
    Just Refl -> SomeSymIntN $ op l r
    Nothing -> error $ "Operation " ++ str ++ " on SymIntN with different bitwidth"
{-# INLINE binSomeSymIntNR1 #-}

binSomeSymIntNR2 :: (forall n. (KnownNat n, 1 <= n) => SymIntN n -> SymIntN n -> (SymIntN n, SymIntN n)) -> String -> SomeSymIntN -> SomeSymIntN -> (SomeSymIntN, SomeSymIntN)
binSomeSymIntNR2 op str (SomeSymIntN (l :: SymIntN l)) (SomeSymIntN (r :: SymIntN r)) =
  case sameNat (Proxy @l) (Proxy @r) of
    Just Refl ->
      case op l r of
        (a, b) -> (SomeSymIntN a, SomeSymIntN b)
    Nothing -> error $ "Operation " ++ str ++ " on SymIntN with different bitwidth"
{-# INLINE binSomeSymIntNR2 #-}

-- | Symbolic unsigned bit vector type. Indexed with the bit width.
-- Signedness affects the semantics of the operations, including
-- comparison/extension, etc.
--
-- >>> :set -XOverloadedStrings -XDataKinds -XBinaryLiterals
-- >>> "a" + 5 :: SymWordN 5
-- (+ 0b00101 a)
-- >>> sizedBVConcat (con 0b101 :: SymWordN 3) (con 0b110 :: SymWordN 3)
-- 0b101110
-- >>> sizedBVExt (Proxy @6) (con 0b101 :: SymWordN 3)
-- 0b000101
-- >>> (8 :: SymWordN 4) <~ (7 :: SymWordN 4)
-- false
--
-- More symbolic operations are available. Please refer to the documentation
-- for the type class instances.
newtype SymWordN (n :: Nat) = SymWordN {underlyingWordNTerm :: Term (WordN n)}
  deriving (Lift, NFData, Generic)

-- | Symbolic unsigned bit vector type. Not indexed, but the bit width is
-- fixed at the creation time.
--
-- A 'SomeSymWordN' must be created by wrapping a 'SymWordN' with the
-- 'SomeSymWordN' constructor to fix the bit width:
--
-- >>> (SomeSymWordN ("a" :: SymWordN 5))
-- a
--
-- >>> :set -XOverloadedStrings -XDataKinds -XBinaryLiterals
-- >>> (SomeSymWordN ("a" :: SymWordN 5)) + (SomeSymWordN (5 :: SymWordN 5))
-- (+ 0b00101 a)
-- >>> someBVConcat (SomeSymWordN (con 0b101 :: SymWordN 3)) (SomeSymWordN (con 0b110 :: SymWordN 3))
-- 0b101110
--
-- More symbolic operations are available. Please refer to the documentation
-- for the type class instances.
data SomeSymWordN where
  SomeSymWordN :: (KnownNat n, 1 <= n) => SymWordN n -> SomeSymWordN

unarySomeSymWordN :: (forall n. (KnownNat n, 1 <= n) => SymWordN n -> r) -> String -> SomeSymWordN -> r
unarySomeSymWordN op str (SomeSymWordN (w :: SymWordN w)) = op w
{-# INLINE unarySomeSymWordN #-}

unarySomeSymWordNR1 :: (forall n. (KnownNat n, 1 <= n) => SymWordN n -> SymWordN n) -> String -> SomeSymWordN -> SomeSymWordN
unarySomeSymWordNR1 op str (SomeSymWordN (w :: SymWordN w)) = SomeSymWordN $ op w
{-# INLINE unarySomeSymWordNR1 #-}

binSomeSymWordN :: (forall n. (KnownNat n, 1 <= n) => SymWordN n -> SymWordN n -> r) -> String -> SomeSymWordN -> SomeSymWordN -> r
binSomeSymWordN op str (SomeSymWordN (l :: SymWordN l)) (SomeSymWordN (r :: SymWordN r)) =
  case sameNat (Proxy @l) (Proxy @r) of
    Just Refl -> op l r
    Nothing -> error $ "Operation " ++ str ++ " on SymWordN with different bitwidth"
{-# INLINE binSomeSymWordN #-}

binSomeSymWordNR1 :: (forall n. (KnownNat n, 1 <= n) => SymWordN n -> SymWordN n -> SymWordN n) -> String -> SomeSymWordN -> SomeSymWordN -> SomeSymWordN
binSomeSymWordNR1 op str (SomeSymWordN (l :: SymWordN l)) (SomeSymWordN (r :: SymWordN r)) =
  case sameNat (Proxy @l) (Proxy @r) of
    Just Refl -> SomeSymWordN $ op l r
    Nothing -> error $ "Operation " ++ str ++ " on SymWordN with different bitwidth"
{-# INLINE binSomeSymWordNR1 #-}

binSomeSymWordNR2 :: (forall n. (KnownNat n, 1 <= n) => SymWordN n -> SymWordN n -> (SymWordN n, SymWordN n)) -> String -> SomeSymWordN -> SomeSymWordN -> (SomeSymWordN, SomeSymWordN)
binSomeSymWordNR2 op str (SomeSymWordN (l :: SymWordN l)) (SomeSymWordN (r :: SymWordN r)) =
  case sameNat (Proxy @l) (Proxy @r) of
    Just Refl ->
      case op l r of
        (a, b) -> (SomeSymWordN a, SomeSymWordN b)
    Nothing -> error $ "Operation " ++ str ++ " on SymWordN with different bitwidth"
{-# INLINE binSomeSymWordNR2 #-}

instance ConRep SymBool where
  type ConType SymBool = Bool

instance SymRep Bool where
  type SymType Bool = SymBool

instance LinkedRep Bool SymBool where
  underlyingTerm (SymBool a) = a
  wrapTerm = SymBool

instance ConRep SymInteger where
  type ConType SymInteger = Integer

instance SymRep Integer where
  type SymType Integer = SymInteger

instance LinkedRep Integer SymInteger where
  underlyingTerm (SymInteger a) = a
  wrapTerm = SymInteger

instance (KnownNat n, 1 <= n) => ConRep (SymIntN n) where
  type ConType (SymIntN n) = IntN n

instance (KnownNat n, 1 <= n) => SymRep (IntN n) where
  type SymType (IntN n) = SymIntN n

instance (KnownNat n, 1 <= n) => LinkedRep (IntN n) (SymIntN n) where
  underlyingTerm (SymIntN a) = a
  wrapTerm = SymIntN

instance (KnownNat n, 1 <= n) => ConRep (SymWordN n) where
  type ConType (SymWordN n) = WordN n

instance (KnownNat n, 1 <= n) => SymRep (WordN n) where
  type SymType (WordN n) = SymWordN n

instance (KnownNat n, 1 <= n) => LinkedRep (WordN n) (SymWordN n) where
  underlyingTerm (SymWordN a) = a
  wrapTerm = SymWordN

-- | Symbolic tabular function type.
--
-- >>> :set -XTypeOperators -XOverloadedStrings
-- >>> f' = "f" :: SymInteger =~> SymInteger
-- >>> f = (f' #)
-- >>> f 1
-- (apply f 1)
--
-- >>> f' = con (TabularFun [(1, 2), (2, 3)] 4) :: SymInteger =~> SymInteger
-- >>> f = (f' #)
-- >>> f 1
-- 2
-- >>> f 2
-- 3
-- >>> f 3
-- 4
-- >>> f "b"
-- (ite (= b 1) 2 (ite (= b 2) 3 4))
data sa =~> sb where
  SymTabularFun :: (LinkedRep ca sa, LinkedRep cb sb) => Term (ca =-> cb) -> sa =~> sb

infixr 0 =~>

instance (ConRep a, ConRep b) => ConRep (a =~> b) where
  type ConType (a =~> b) = ConType a =-> ConType b

instance (SymRep a, SymRep b) => SymRep (a =-> b) where
  type SymType (a =-> b) = SymType a =~> SymType b

instance (LinkedRep ca sa, LinkedRep cb sb) => LinkedRep (ca =-> cb) (sa =~> sb) where
  underlyingTerm (SymTabularFun a) = a
  wrapTerm = SymTabularFun

instance (SupportedPrim ca, SupportedPrim cb, LinkedRep ca sa, LinkedRep cb sb) => Function (sa =~> sb) where
  type Arg (sa =~> sb) = sa
  type Ret (sa =~> sb) = sb
  (SymTabularFun f) # t = wrapTerm $ pevalTabularFunApplyTerm f (underlyingTerm t)

-- |
-- Symbolic general function type.
--
-- >>> :set -XTypeOperators -XOverloadedStrings
-- >>> f' = "f" :: SymInteger -~> SymInteger
-- >>> f = (f' #)
-- >>> f 1
-- (apply f 1)
--
-- >>> f' = con ("a" --> "a" + 1) :: SymInteger -~> SymInteger
-- >>> f'
-- \(a:ARG :: Integer) -> (+ 1 a:ARG)
-- >>> f = (f' #)
-- >>> f 1
-- 2
-- >>> f 2
-- 3
-- >>> f 3
-- 4
-- >>> f "b"
-- (+ 1 b)
data sa -~> sb where
  SymGeneralFun :: (LinkedRep ca sa, LinkedRep cb sb) => Term (ca --> cb) -> sa -~> sb

infixr 0 -~>

instance (ConRep a, ConRep b) => ConRep (a -~> b) where
  type ConType (a -~> b) = ConType a --> ConType b

instance (SymRep ca, SymRep cb) => SymRep (ca --> cb) where
  type SymType (ca --> cb) = SymType ca -~> SymType cb

instance (LinkedRep ca sa, LinkedRep cb sb) => LinkedRep (ca --> cb) (sa -~> sb) where
  underlyingTerm (SymGeneralFun a) = a
  wrapTerm = SymGeneralFun

instance (SupportedPrim ca, SupportedPrim cb, LinkedRep ca sa, LinkedRep cb sb) => Function (sa -~> sb) where
  type Arg (sa -~> sb) = sa
  type Ret (sa -~> sb) = sb
  (SymGeneralFun f) # t = wrapTerm $ pevalGeneralFunApplyTerm f (underlyingTerm t)

-- | Construction of general symbolic functions.
--
-- >>> f = "a" --> "a" + 1 :: Integer --> Integer
-- >>> f
-- \(a:ARG :: Integer) -> (+ 1 a:ARG)
--
-- This general symbolic function needs to be applied to symbolic values:
-- >>> f # ("a" :: SymInteger)
-- (+ 1 a)
(-->) :: (SupportedPrim ca, SupportedPrim cb, LinkedRep cb sb) => TypedSymbol ca -> sb -> ca --> cb
(-->) arg v = GeneralFun newarg (substTerm arg (symTerm newarg) (underlyingTerm v))
  where
    newarg = WithInfo arg ARG

infixr 0 -->

data ARG = ARG
  deriving (Eq, Ord, Lift, Show, Generic)

instance NFData ARG where
  rnf ARG = ()

instance Hashable ARG where
  hashWithSalt s ARG = s `hashWithSalt` (0 :: Int)

-- Aggregate instances

-- Num

#define NUM_BV(symtype) \
instance (KnownNat n, 1 <= n) => Num (symtype n) where \
  (symtype l) + (symtype r) = symtype $ pevalAddNumTerm l r; \
  (symtype l) - (symtype r) = symtype $ pevalMinusNumTerm l r; \
  (symtype l) * (symtype r) = symtype $ pevalTimesNumTerm l r; \
  negate (symtype v) = symtype $ pevalUMinusNumTerm v; \
  abs (symtype v) = symtype $ pevalAbsNumTerm v; \
  signum (symtype v) = symtype $ pevalSignumNumTerm v; \
  fromInteger i = con $ fromInteger i

#define NUM_SOME_BV(somety, br1, ur1) \
instance Num somety where \
  (+) = br1 (+) "+"; \
  {-# INLINE (+) #-}; \
  (-) = br1 (-) "-"; \
  {-# INLINE (-) #-}; \
  (*) = br1 (*) "*"; \
  {-# INLINE (*) #-}; \
  negate = ur1 negate "negate"; \
  {-# INLINE negate #-}; \
  abs = ur1 abs "abs"; \
  {-# INLINE abs #-}; \
  signum = ur1 signum "signum"; \
  {-# INLINE signum #-}; \
  fromInteger = error "fromInteger is not defined for SomeSymWordN as no bitwidth is known"; \
  {-# INLINE fromInteger #-}

#if 1
NUM_BV(SymIntN)
NUM_BV(SymWordN)
NUM_SOME_BV(SomeSymWordN, binSomeSymWordNR1, unarySomeSymWordNR1)
NUM_SOME_BV(SomeSymIntN, binSomeSymIntNR1, unarySomeSymIntNR1)
#endif

instance Num SymInteger where
  (SymInteger l) + (SymInteger r) = SymInteger $ pevalAddNumTerm l r
  (SymInteger l) - (SymInteger r) = SymInteger $ pevalMinusNumTerm l r
  (SymInteger l) * (SymInteger r) = SymInteger $ pevalTimesNumTerm l r
  negate (SymInteger v) = SymInteger $ pevalUMinusNumTerm v
  abs (SymInteger v) = SymInteger $ pevalAbsNumTerm v
  signum (SymInteger v) = SymInteger $ pevalSignumNumTerm v
  fromInteger = con

-- Bits

#define BITS_BV(symtype, signed) \
instance (KnownNat n, 1 <= n) => Bits (symtype n) where \
  symtype l .&. symtype r = symtype $ pevalAndBitsTerm l r; \
  symtype l .|. symtype r = symtype $ pevalOrBitsTerm l r; \
  symtype l `xor` symtype r = symtype $ pevalXorBitsTerm l r; \
  complement (symtype n) = symtype $ pevalComplementBitsTerm n; \
  shift (symtype n) i = symtype $ pevalShiftBitsTerm n i; \
  rotate (symtype n) i = symtype $ pevalRotateBitsTerm n i; \
  bitSize _ = fromIntegral $ natVal (Proxy @n); \
  bitSizeMaybe _ = Just $ fromIntegral $ natVal (Proxy @n); \
  isSigned _ = signed; \
  testBit (Con n) =  testBit n; \
  testBit _ = error "You cannot call testBit on symbolic variables"; \
  bit = con . bit; \
  popCount (Con n) = popCount n; \
  popCount _ = error "You cannot call popCount on symbolic variables"

#define BITS_BV_SOME(somety, origty, br1, uf, ur1) \
instance Bits somety where \
  (.&.) = br1 (.&.) ".&."; \
  {-# INLINE (.&.) #-}; \
  (.|.) = br1 (.|.) ".|."; \
  {-# INLINE (.|.) #-}; \
  xor = br1 xor "xor"; \
  {-# INLINE xor #-}; \
  complement = ur1 complement "complement"; \
  {-# INLINE complement #-}; \
  shift s i = ur1 (`shift` i) "shift" s; \
  {-# INLINE shift #-}; \
  rotate s i = ur1 (`rotate` i) "rotate" s; \
  {-# INLINE rotate #-}; \
  zeroBits = error ("zeroBits is not defined for " ++ show (typeRep (Proxy @somety)) ++ " as no bitwidth is known"); \
  {-# INLINE zeroBits #-}; \
  bit = error ("bit is not defined for " ++ show (typeRep (Proxy @somety)) ++ " as no bitwidth is known"); \
  {-# INLINE bit #-}; \
  setBit s i = ur1 (`setBit` i) "setBit" s; \
  {-# INLINE setBit #-}; \
  clearBit s i = ur1 (`clearBit` i) "clearBit" s; \
  {-# INLINE clearBit #-}; \
  complementBit s i = ur1 (`complementBit` i) "complementBit" s; \
  {-# INLINE complementBit #-}; \
  testBit s i = uf (`testBit` i) "testBit" s; \
  {-# INLINE testBit #-}; \
  bitSizeMaybe (somety (n :: origty n)) = Just $ fromIntegral $ natVal n; \
  {-# INLINE bitSizeMaybe #-}; \
  bitSize (somety (n :: origty n)) = fromIntegral $ natVal n; \
  {-# INLINE bitSize #-}; \
  isSigned _ = False; \
  {-# INLINE isSigned #-}; \
  shiftL s i = ur1 (`shiftL` i) "shiftL" s; \
  {-# INLINE shiftL #-}; \
  unsafeShiftL s i = ur1 (`unsafeShiftL` i) "unsafeShiftL" s; \
  {-# INLINE unsafeShiftL #-}; \
  shiftR s i = ur1 (`shiftR` i) "shiftR" s; \
  {-# INLINE shiftR #-}; \
  unsafeShiftR s i = ur1 (`unsafeShiftR` i) "unsafeShiftR" s; \
  {-# INLINE unsafeShiftR #-}; \
  rotateL s i = ur1 (`rotateL` i) "rotateL" s; \
  {-# INLINE rotateL #-}; \
  rotateR s i = ur1 (`rotateR` i) "rotateR" s; \
  {-# INLINE rotateR #-}; \
  popCount = uf popCount "popCount"; \
  {-# INLINE popCount #-}

#if 1
BITS_BV(SymIntN, True)
BITS_BV(SymWordN, False)
BITS_BV_SOME(SomeSymIntN, SymIntN, binSomeSymIntNR1, unarySomeSymIntN, unarySomeSymIntNR1)
BITS_BV_SOME(SomeSymWordN, SymWordN, binSomeSymWordNR1, unarySomeSymWordN, unarySomeSymWordNR1)
#endif

-- Show

#define SHOW_SIMPLE(symtype) \
instance Show symtype where \
  show (symtype t) = pformat t

#define SHOW_BV(symtype) \
instance (KnownNat n, 1 <= n) => Show (symtype n) where \
  show (symtype t) = pformat t

#define SHOW_FUN(op, cons) \
instance (SupportedPrim ca, SupportedPrim cb, LinkedRep ca sa, LinkedRep cb sb) => Show (sa op sb) where \
  show (cons t) = pformat t

#define SHOW_BV_SOME(somety) \
instance Show somety where \
  show (somety t) = show t

#if 1
SHOW_SIMPLE(SymBool)
SHOW_SIMPLE(SymInteger)
SHOW_BV(SymIntN)
SHOW_BV(SymWordN)
SHOW_FUN(=~>, SymTabularFun)
SHOW_FUN(-~>, SymGeneralFun)
SHOW_BV_SOME(SomeSymIntN)
SHOW_BV_SOME(SomeSymWordN)
#endif

-- Hashable

#define HASHABLE_SIMPLE(symtype) \
instance Hashable symtype where \
  hashWithSalt s (symtype v) = s `hashWithSalt` v

#define HASHABLE_BV(symtype) \
instance (KnownNat n, 1 <= n) => Hashable (symtype n) where \
  hashWithSalt s (symtype v) = s `hashWithSalt` v

#define HASHABLE_FUN(op, cons) \
instance (SupportedPrim ca, SupportedPrim cb, LinkedRep ca sa, LinkedRep cb sb) => Hashable (sa op sb) where \
  hashWithSalt s (cons v) = s `hashWithSalt` v

#define HASHABLE_BV_SOME(somety, origty) \
instance Hashable somety where \
  s `hashWithSalt` (somety (w :: origty n)) = s `hashWithSalt` natVal (Proxy @n) `hashWithSalt` w

#if 1
HASHABLE_SIMPLE(SymBool)
HASHABLE_SIMPLE(SymInteger)
HASHABLE_BV(SymIntN)
HASHABLE_BV(SymWordN)
HASHABLE_FUN(=~>, SymTabularFun)
HASHABLE_FUN(-~>, SymGeneralFun)
HASHABLE_BV_SOME(SomeSymIntN, SymIntN)
HASHABLE_BV_SOME(SomeSymWordN, SymWordN)
#endif

-- Eq

#define EQ_SIMPLE(symtype) \
instance Eq symtype where \
  (symtype l) == (symtype r) = l == r

#define EQ_BV(symtype) \
instance (KnownNat n, 1 <= n) => Eq (symtype n) where \
  (symtype l) == (symtype r) = l == r

#define EQ_BV_SOME(symtype, bf) \
instance Eq symtype where; \
  (==) = bf (==) "=="; \
  {-# INLINE (==) #-}; \
  (/=) = bf (/=) "/="; \
  {-# INLINE (/=) #-}

#define EQ_FUN(op, cons) \
instance (SupportedPrim ca, SupportedPrim cb, LinkedRep ca sa, LinkedRep cb sb) => Eq (sa op sb) where \
  (cons l) == (cons r) = l == r

#if 1
EQ_SIMPLE(SymBool)
EQ_SIMPLE(SymInteger)
EQ_BV(SymIntN)
EQ_BV(SymWordN)
EQ_FUN(=~>, SymTabularFun)
EQ_FUN(-~>, SymGeneralFun)
EQ_BV_SOME(SomeSymIntN, binSomeSymIntN)
EQ_BV_SOME(SomeSymWordN, binSomeSymWordN)
#endif

-- IsString

#define IS_STRING_SIMPLE(symtype) \
instance IsString symtype where \
  fromString = ssym

#define IS_STRING_BV(symtype) \
instance (KnownNat n, 1 <= n) => IsString (symtype n) where \
  fromString = ssym

#define IS_STRING_FUN(op, cons) \
instance (SupportedPrim ca, SupportedPrim cb, LinkedRep ca sa, LinkedRep cb sb) => IsString (sa op sb) where \
  fromString = ssym

#if 1
IS_STRING_SIMPLE(SymBool)
IS_STRING_SIMPLE(SymInteger)
IS_STRING_BV(SymIntN)
IS_STRING_BV(SymWordN)
IS_STRING_FUN(=~>, SymTabularFunc)
IS_STRING_FUN(-~>, SymGeneralFun)
#endif

-- Solvable

#define SOLVABLE_SIMPLE(contype, symtype) \
instance Solvable contype symtype where \
  con = symtype . conTerm; \
  ssym = symtype . ssymTerm; \
  isym str i = symtype $ isymTerm str i; \
  sinfosym str info = symtype $ sinfosymTerm str info; \
  iinfosym str i info = symtype $ iinfosymTerm str i info; \
  conView (symtype (ConTerm _ t)) = Just t; \
  conView _ = Nothing

#define SOLVABLE_BV(contype, symtype) \
instance (KnownNat n, 1 <= n) => Solvable (contype n) (symtype n) where \
  con = symtype . conTerm; \
  ssym = symtype . ssymTerm; \
  isym str i = symtype $ isymTerm str i; \
  sinfosym str info = symtype $ sinfosymTerm str info; \
  iinfosym str i info = symtype $ iinfosymTerm str i info; \
  conView (symtype (ConTerm _ t)) = Just t; \
  conView _ = Nothing

#define SOLVABLE_FUN(symop, conop, symcons) \
instance (SupportedPrim ca, SupportedPrim cb, LinkedRep ca sa, LinkedRep cb sb) => Solvable (conop ca cb) (symop sa sb) where \
  con = symcons . conTerm; \
  ssym = symcons . ssymTerm; \
  isym str i = symcons $ isymTerm str i; \
  sinfosym str info = symcons $ sinfosymTerm str info; \
  iinfosym str i info = symcons $ iinfosymTerm str i info; \
  conView (symcons (ConTerm _ t)) = Just t; \
  conView _ = Nothing

#if 1
SOLVABLE_SIMPLE(Bool, SymBool)
SOLVABLE_SIMPLE(Integer, SymInteger)
SOLVABLE_BV(IntN, SymIntN)
SOLVABLE_BV(WordN, SymWordN)
SOLVABLE_FUN((=~>), (=->), SymTabularFun)
SOLVABLE_FUN((-~>), (-->), SymGeneralFun)
#endif

-- ToSym and ToCon

#define TO_SYM_SYMID_SIMPLE(symtype) \
instance ToSym symtype symtype where \
  toSym = id

#define TO_SYM_SYMID_BV(symtype) \
instance (KnownNat n, 1 <= n) => ToSym (symtype n) (symtype n) where \
  toSym = id

#define TO_SYM_SYMID_FUN(op) \
instance (SupportedPrim a, SupportedPrim b) => ToSym (a op b) (a op b) where \
  toSym = id

#if 1
TO_SYM_SYMID_SIMPLE(SymBool)
TO_SYM_SYMID_SIMPLE(SymInteger)
TO_SYM_SYMID_BV(SymIntN)
TO_SYM_SYMID_BV(SymWordN)
TO_SYM_SYMID_FUN(=~>)
TO_SYM_SYMID_FUN(-~>)
TO_SYM_SYMID_SIMPLE(SomeSymIntN)
TO_SYM_SYMID_SIMPLE(SomeWordN)
#endif

#define TO_SYM_FROMCON_SIMPLE(contype, symtype) \
instance ToSym contype symtype where \
  toSym = con

#define TO_SYM_FROMCON_BV(contype, symtype) \
instance (KnownNat n, 1 <= n) => ToSym (contype n) (symtype n) where \
  toSym = con

#define TO_SYM_FROMCON_FUN(conop, symop) \
instance (SupportedPrim ca, SupportedPrim cb, LinkedRep ca sa, LinkedRep cb sb) => ToSym (conop ca cb) (symop sa sb) where \
  toSym = con

#define TO_SYM_FROMCON_BV_SOME(contype, symtype) \
instance ToSym contype symtype where \
  toSym (contype v) = symtype (con v)

#if 1
TO_SYM_FROMCON_SIMPLE(Bool, SymBool)
TO_SYM_FROMCON_SIMPLE(Integer, SymInteger)
TO_SYM_FROMCON_BV(IntN, SymIntN)
TO_SYM_FROMCON_BV(WordN, SymWordN)
TO_SYM_FROMCON_FUN((=->), (=~>))
TO_SYM_FROMCON_FUN((-->), (-~>))
TO_SYM_FROMCON_BV_SOME(SomeIntN, SomeSymIntN)
TO_SYM_FROMCON_BV_SOME(SomeWordN, SomeSymWordN)
#endif

#define TO_CON_SYMID_SIMPLE(symtype) \
instance ToCon symtype symtype where \
  toCon = Just

#define TO_CON_SYMID_BV(symtype) \
instance (KnownNat n, 1 <= n) => ToCon (symtype n) (symtype n) where \
  toCon = Just

#define TO_CON_SYMID_FUN(op) \
instance (SupportedPrim a, SupportedPrim b) => ToCon (a op b) (a op b) where \
  toCon = Just

#if 1
TO_CON_SYMID_SIMPLE(SymBool)
TO_CON_SYMID_SIMPLE(SymInteger)
TO_CON_SYMID_BV(SymIntN)
TO_CON_SYMID_BV(SymWordN)
TO_CON_SYMID_FUN(=~>)
TO_CON_SYMID_FUN(-~>)
TO_CON_SYMID_SIMPLE(SomeSymIntN)
TO_CON_SYMID_SIMPLE(SomeSymWordN)
#endif

#define TO_CON_FROMSYM_SIMPLE(contype, symtype) \
instance ToCon symtype contype where \
  toCon = conView

#define TO_CON_FROMSYM_BV(contype, symtype) \
instance (KnownNat n, 1 <= n) => ToCon (symtype n) (contype n) where \
  toCon = conView

#define TO_CON_FROMSYM_FUN(conop, symop) \
instance (SupportedPrim ca, SupportedPrim cb, LinkedRep ca sa, LinkedRep cb sb) => ToCon (symop sa sb) (conop ca cb) where \
  toCon = conView

#define TO_CON_FROMSYM_BV_SOME(contype, symtype) \
instance ToCon symtype contype where \
  toCon (symtype v) = contype <$> conView v

#if 1
TO_CON_FROMSYM_SIMPLE(Bool, SymBool)
TO_CON_FROMSYM_SIMPLE(Integer, SymInteger)
TO_CON_FROMSYM_BV(IntN, SymIntN)
TO_CON_FROMSYM_BV(WordN, SymWordN)
TO_CON_FROMSYM_FUN((=->), (=~>))
TO_CON_FROMSYM_FUN((-->), (-~>))
TO_CON_FROMSYM_BV_SOME(SomeIntN, SomeSymIntN)
TO_CON_FROMSYM_BV_SOME(SomeWordN, SomeSymWordN)
#endif

#define TO_SYM_FROMBV_SOME(somesymbv, bv) \
instance (KnownNat n, 1 <= n) => ToSym (bv n) somesymbv where \
  toSym = somesymbv . con

#if 1
TO_SYM_FROMBV_SOME(SomeSymIntN, IntN)
TO_SYM_FROMBV_SOME(SomeSymWordN, WordN)
#endif

#define TOSYM_MACHINE_INTEGER(int, bv) \
instance ToSym int (bv) where \
  toSym = fromIntegral

#define TOSYM_MACHINE_INTEGER_SOME(int, somesymbv, bv, bitwidth) \
instance ToSym int somesymbv where \
  toSym v = somesymbv (con (fromIntegral v :: bv bitwidth))

#define TOCON_MACHINE_INTEGER(sbvw, bvw, n, int) \
instance ToCon (sbvw n) int where \
  toCon (Con (bvw v :: bvw n)) = Just $ fromIntegral v; \
  toCon _ = Nothing

#if 1
TOSYM_MACHINE_INTEGER(Int8, SymIntN 8)
TOSYM_MACHINE_INTEGER(Int16, SymIntN 16)
TOSYM_MACHINE_INTEGER(Int32, SymIntN 32)
TOSYM_MACHINE_INTEGER(Int64, SymIntN 64)
TOSYM_MACHINE_INTEGER(Word8, SymWordN 8)
TOSYM_MACHINE_INTEGER(Word16, SymWordN 16)
TOSYM_MACHINE_INTEGER(Word32, SymWordN 32)
TOSYM_MACHINE_INTEGER(Word64, SymWordN 64)
TOSYM_MACHINE_INTEGER(Int, SymIntN $intBitwidthQ)
TOSYM_MACHINE_INTEGER(Word, SymWordN $intBitwidthQ)

TOSYM_MACHINE_INTEGER_SOME(Int8, SomeSymIntN, IntN, 8)
TOSYM_MACHINE_INTEGER_SOME(Int16, SomeSymIntN, IntN, 16)
TOSYM_MACHINE_INTEGER_SOME(Int32, SomeSymIntN, IntN, 32)
TOSYM_MACHINE_INTEGER_SOME(Int64, SomeSymIntN, IntN, 64)
TOSYM_MACHINE_INTEGER_SOME(Word8, SomeSymWordN, WordN, 8)
TOSYM_MACHINE_INTEGER_SOME(Word16, SomeSymWordN, WordN, 16)
TOSYM_MACHINE_INTEGER_SOME(Word32, SomeSymWordN, WordN, 32)
TOSYM_MACHINE_INTEGER_SOME(Word64, SomeSymWordN, WordN, 64)
TOSYM_MACHINE_INTEGER_SOME(Int, SomeSymIntN, IntN, $intBitwidthQ)
TOSYM_MACHINE_INTEGER_SOME(Word, SomeSymWordN, WordN, $intBitwidthQ)

TOCON_MACHINE_INTEGER(SymIntN, IntN, 8, Int8)
TOCON_MACHINE_INTEGER(SymIntN, IntN, 16, Int16)
TOCON_MACHINE_INTEGER(SymIntN, IntN, 32, Int32)
TOCON_MACHINE_INTEGER(SymIntN, IntN, 64, Int64)
TOCON_MACHINE_INTEGER(SymWordN, WordN, 8, Word8)
TOCON_MACHINE_INTEGER(SymWordN, WordN, 16, Word16)
TOCON_MACHINE_INTEGER(SymWordN, WordN, 32, Word32)
TOCON_MACHINE_INTEGER(SymWordN, WordN, 64, Word64)
TOCON_MACHINE_INTEGER(SymIntN, IntN, $intBitwidthQ, Int)
TOCON_MACHINE_INTEGER(SymWordN, WordN, $intBitwidthQ, Word)
#endif

-- Evaluate

#define EVALUATE_SYM_SIMPLE(symtype) \
instance EvaluateSym symtype where \
  evaluateSym fillDefault model (symtype t) = symtype $ evaluateTerm fillDefault model t

#define EVALUATE_SYM_BV(symtype) \
instance (KnownNat n, 1 <= n) => EvaluateSym (symtype n) where \
  evaluateSym fillDefault model (symtype t) = symtype $ evaluateTerm fillDefault model t

#define EVALUATE_SYM_FUN(op, cons) \
instance (SupportedPrim ca, SupportedPrim cb, LinkedRep ca sa, LinkedRep cb sb) => EvaluateSym (sa op sb) where \
  evaluateSym fillDefault model (cons t) = cons $ evaluateTerm fillDefault model t

#define EVALUATE_SYM_BV_SOME(somety, origty) \
instance EvaluateSym somety where \
  evaluateSym fillDefault model (somety (origty t)) = somety $ origty $ evaluateTerm fillDefault model t

#if 1
EVALUATE_SYM_SIMPLE(SymBool)
EVALUATE_SYM_SIMPLE(SymInteger)
EVALUATE_SYM_BV(SymIntN)
EVALUATE_SYM_BV(SymWordN)
EVALUATE_SYM_FUN(=~>, SymTabularFun)
EVALUATE_SYM_FUN(-~>, SymGeneralFun)
EVALUATE_SYM_BV_SOME(SomeSymIntN, SymIntN)
EVALUATE_SYM_BV_SOME(SomeSymWordN, SymWordN)
#endif

-- ExtractSymbolics

#define EXTRACT_SYMBOLICS_SIMPLE(symtype) \
instance ExtractSymbolics symtype where \
  extractSymbolics (symtype t) = SymbolSet $ extractSymbolicsTerm t

#define EXTRACT_SYMBOLICS_BV(symtype) \
instance (KnownNat n, 1 <= n) => ExtractSymbolics (symtype n) where \
  extractSymbolics (symtype t) = SymbolSet $ extractSymbolicsTerm t

#define EXTRACT_SYMBOLICS_FUN(op, cons) \
instance (SupportedPrim ca, SupportedPrim cb, LinkedRep ca sa, LinkedRep cb sb) => ExtractSymbolics (sa op sb) where \
  extractSymbolics (cons t) = SymbolSet $ extractSymbolicsTerm t

#define EXTRACT_SYMBOLICS_BV_SOME(somety, origty) \
instance ExtractSymbolics somety where \
  extractSymbolics (somety (origty t)) = SymbolSet $ extractSymbolicsTerm t

#if 1
EXTRACT_SYMBOLICS_SIMPLE(SymBool)
EXTRACT_SYMBOLICS_SIMPLE(SymInteger)
EXTRACT_SYMBOLICS_BV(SymIntN)
EXTRACT_SYMBOLICS_BV(SymWordN)
EXTRACT_SYMBOLICS_FUN(=~>, SymTabularFun)
EXTRACT_SYMBOLICS_FUN(-~>, SymGeneralFun)
EXTRACT_SYMBOLICS_BV_SOME(SomeSymIntN, SymIntN)
EXTRACT_SYMBOLICS_BV_SOME(SomeSymWordN, SymWordN)
#endif

-- SEq

#define SEQ_SIMPLE(symtype) \
instance SEq symtype where \
  (symtype l) ==~ (symtype r) = SymBool $ pevalEqvTerm l r

#define SEQ_BV(symtype) \
instance (KnownNat n, 1 <= n) => SEq (symtype n) where \
  (symtype l) ==~ (symtype r) = SymBool $ pevalEqvTerm l r

#define SEQ_BV_SOME(somety, bf) \
instance SEq somety where \
  (==~) = bf (==~) "==~"; \
  {-# INLINE (==~) #-}; \
  (/=~) = bf (/=~) "/=~"; \
  {-# INLINE (/=~) #-}

#if 1
SEQ_SIMPLE(SymBool)
SEQ_SIMPLE(SymInteger)
SEQ_BV(SymIntN)
SEQ_BV(SymWordN)
SEQ_BV_SOME(SomeSymIntN, binSomeSymIntN)
SEQ_BV_SOME(SomeSymWordN, binSomeSymWordN)
#endif

-- SOrd

#define SORD_SIMPLE(symtype) \
instance SOrd symtype where \
  (symtype a) <=~ (symtype b) = SymBool $ pevalLeNumTerm a b; \
  (symtype a) <~ (symtype b) = SymBool $ pevalLtNumTerm a b; \
  (symtype a) >=~ (symtype b) = SymBool $ pevalGeNumTerm a b; \
  (symtype a) >~ (symtype b) = SymBool $ pevalGtNumTerm a b; \
  a `symCompare` b = mrgIf \
    (a <~ b) \
    (mrgReturn LT) \
    (mrgIf (a ==~ b) (mrgReturn EQ) (mrgReturn GT))

#define SORD_BV(symtype) \
instance (KnownNat n, 1 <= n) => SOrd (symtype n) where \
  (symtype a) <=~ (symtype b) = SymBool $ pevalLeNumTerm a b; \
  (symtype a) <~ (symtype b) = SymBool $ pevalLtNumTerm a b; \
  (symtype a) >=~ (symtype b) = SymBool $ pevalGeNumTerm a b; \
  (symtype a) >~ (symtype b) = SymBool $ pevalGtNumTerm a b; \
  a `symCompare` b = mrgIf \
    (a <~ b) \
    (mrgReturn LT) \
    (mrgIf (a ==~ b) (mrgReturn EQ) (mrgReturn GT))

#define SORD_BV_SOME(somety, bf) \
instance SOrd somety where \
  (<=~) = bf (<=~) "<=~"; \
  {-# INLINE (<=~) #-}; \
  (<~) = bf (<~) "<~"; \
  {-# INLINE (<~) #-}; \
  (>=~) = bf (>=~) ">=~"; \
  {-# INLINE (>=~) #-}; \
  (>~) = bf (>~) ">~"; \
  {-# INLINE (>~) #-}; \
  symCompare = bf symCompare "symCompare"; \
  {-# INLINE symCompare #-}

instance SOrd SymBool where
  l <=~ r = nots l ||~ r
  l <~ r = nots l &&~ r
  l >=~ r = l ||~ nots r
  l >~ r = l &&~ nots r
  symCompare l r =
    mrgIf
      (nots l &&~ r)
      (mrgReturn LT)
      (mrgIf (l ==~ r) (mrgReturn EQ) (mrgReturn GT))

#if 1
SORD_SIMPLE(SymInteger)
SORD_BV(SymIntN)
SORD_BV(SymWordN)
SORD_BV_SOME(SomeSymIntN, binSomeSymIntN)
SORD_BV_SOME(SomeSymWordN, binSomeSymWordN)
#endif

-- SubstituteSym

#define SUBSTITUTE_SYM_SIMPLE(symtype) \
instance SubstituteSym symtype where \
  substituteSym sym v (symtype t) = symtype $ substTerm sym (underlyingTerm v) t

#define SUBSTITUTE_SYM_BV(symtype) \
instance (KnownNat n, 1 <= n) => SubstituteSym (symtype n) where \
  substituteSym sym v (symtype t) = symtype $ substTerm sym (underlyingTerm v) t

#define SUBSTITUTE_SYM_FUN(op, cons) \
instance (SupportedPrim ca, SupportedPrim cb, LinkedRep ca sa, LinkedRep cb sb) => SubstituteSym (sa op sb) where \
  substituteSym sym v (cons t) = cons $ substTerm sym (underlyingTerm v) t

#define SUBSTITUTE_SYM_BV_SOME(somety, origty) \
instance SubstituteSym somety where \
  substituteSym sym v (somety (origty t)) = somety $ origty $ substTerm sym (underlyingTerm v) t

#if 1
SUBSTITUTE_SYM_SIMPLE(SymBool)
SUBSTITUTE_SYM_SIMPLE(SymInteger)
SUBSTITUTE_SYM_BV(SymIntN)
SUBSTITUTE_SYM_BV(SymWordN)
SUBSTITUTE_SYM_FUN(=~>, SymTabularFun)
SUBSTITUTE_SYM_FUN(-~>, SymGeneralFun)
SUBSTITUTE_SYM_BV_SOME(SomeSymIntN, SymIntN)
SUBSTITUTE_SYM_BV_SOME(SomeSymWordN, SymWordN)
#endif

-- SizedBV

#define BVCONCAT_SIZED(symtype) \
sizedBVConcat :: forall l r. (KnownNat l, KnownNat r, 1 <= l, 1 <= r) => symtype l -> symtype r -> symtype (l + r); \
sizedBVConcat (symtype l) (symtype r) = \
  case (leqAddPos pl pr, knownAdd (KnownProof @l) (KnownProof @r)) of \
    (LeqProof, KnownProof) -> \
      symtype (pevalBVConcatTerm l r); \
  where; \
    pl = Proxy :: Proxy l; \
    pr = Proxy :: Proxy r

#define BVZEXT_SIZED(symtype) \
sizedBVZext :: forall l r proxy. (KnownNat l, KnownNat r, 1 <= l, KnownNat r, l <= r) => proxy r -> symtype l -> symtype r; \
sizedBVZext _ (symtype v) = \
  case leqTrans (LeqProof @1 @l) (LeqProof @l @r) of \
    LeqProof -> symtype $ pevalBVExtendTerm False (Proxy @r) v

#define BVSEXT_SIZED(symtype) \
sizedBVSext :: forall l r proxy. (KnownNat l, KnownNat r, 1 <= l, KnownNat r, l <= r) => proxy r -> symtype l -> symtype r; \
sizedBVSext _ (symtype v) = \
  case leqTrans (LeqProof @1 @l) (LeqProof @l @r) of \
    LeqProof -> symtype $ pevalBVExtendTerm True (Proxy @r) v

#define BVSELECT_SIZED(symtype) \
sizedBVSelect :: forall n ix w proxy. (KnownNat n, KnownNat ix, KnownNat w, 1 <= n, 1 <= w, ix + w <= n) => \
  proxy ix -> proxy w -> symtype n -> symtype w; \
sizedBVSelect pix pw (symtype v) = symtype $ pevalBVSelectTerm pix pw v

#if 1
instance SizedBV SymIntN where
  BVCONCAT_SIZED(SymIntN)
  BVZEXT_SIZED(SymIntN)
  BVSEXT_SIZED(SymIntN)
  sizedBVExt = sizedBVSext
  BVSELECT_SIZED(SymIntN)

instance SizedBV SymWordN where
  BVCONCAT_SIZED(SymWordN)
  BVZEXT_SIZED(SymWordN)
  BVSEXT_SIZED(SymWordN)
  sizedBVExt = sizedBVZext
  BVSELECT_SIZED(SymWordN)
#endif

-- BV

#define BVCONCAT(somety, origty) \
someBVConcat (somety (a :: origty l)) (somety (b :: origty r)) = \
  case (leqAddPos (Proxy @l) (Proxy @r), knownAdd @l @r KnownProof KnownProof) of \
    (LeqProof, KnownProof) -> \
      somety $ sizedBVConcat a b

#define BVZEXT(somety, origty) \
someBVZext (p :: p l) (somety (a :: origty n)) \
  | natVal p < natVal (Proxy @n) = error "zextBV: trying to zero extend a value to a smaller size" \
  | otherwise = \
    case (unsafeLeqProof @1 @l, unsafeLeqProof @n @l) of \
      (LeqProof, LeqProof) -> somety $ sizedBVZext p a

#define BVSEXT(somety, origty) \
someBVSext (p :: p l) (somety (a :: origty n)) \
  | natVal p < natVal (Proxy @n) = error "zextBV: trying to zero extend a value to a smaller size" \
  | otherwise = \
    case (unsafeLeqProof @1 @l, unsafeLeqProof @n @l) of \
      (LeqProof, LeqProof) -> somety $ sizedBVSext p a

#define BVSELECT(somety, origty) \
someBVSelect (p :: p ix) (q :: q w) (somety (a :: origty n)) \
  | natVal p + natVal q > natVal (Proxy @n) = error "selectBV: trying to select a bitvector outside the bounds of the input" \
  | natVal q == 0 = error "selectBV: trying to select a bitvector of size 0" \
  | otherwise = \
    case (unsafeLeqProof @1 @w, unsafeLeqProof @(ix + w) @n) of \
      (LeqProof, LeqProof) -> somety $ sizedBVSelect (Proxy @ix) (Proxy @w) a

#if 1
instance SomeBV SomeSymIntN where
  BVCONCAT(SomeSymIntN, SymIntN)
  BVZEXT(SomeSymIntN, SymIntN)
  BVSEXT(SomeSymIntN, SymIntN)
  someBVExt = someBVSext
  BVSELECT(SomeSymIntN, SymIntN)

instance SomeBV SomeSymWordN where
  BVCONCAT(SomeSymWordN, SymWordN)
  BVZEXT(SomeSymWordN, SymWordN)
  BVSEXT(SomeSymWordN, SymWordN)
  someBVExt = someBVZext
  BVSELECT(SomeSymWordN, SymWordN)
#endif

-- ModelRep

-- | A pair of a symbolic constant and its value.
-- This is used to build a model from a list of symbolic constants and their values.
--
-- >>> buildModel ("a" := (1 :: Integer), "b" := True) :: Model
-- Model {a -> 1 :: Integer, b -> True :: Bool}
data ModelSymPair ct st where
  (:=) :: LinkedRep ct st => st -> ct -> ModelSymPair ct st

instance ModelRep (ModelSymPair ct st) Model where
  buildModel (sym := val) =
    case underlyingTerm sym of
      SymTerm _ symbol -> insertValue symbol val emptyModel
      _ -> error "buildModel: should only use symbolic constants"

-- | Get the sum of the sizes of a list of symbolic terms.
-- Duplicate sub-terms are counted for only once.
--
-- >>> symsSize [1, "a" :: SymInteger, "a" + 1 :: SymInteger]
-- 3
symsSize :: forall con sym. LinkedRep con sym => [sym] -> Int
symsSize = termsSize . fmap (underlyingTerm @con)

-- | Get the size of a symbolic term.
-- Duplicate sub-terms are counted for only once.
--
-- >>> symSize (1 :: SymInteger)
-- 1
-- >>> symSize ("a" :: SymInteger)
-- 1
-- >>> symSize ("a" + 1 :: SymInteger)
-- 3
-- >>> symSize (("a" + 1) * ("a" + 1) :: SymInteger)
-- 4
symSize :: forall con sym. LinkedRep con sym => sym -> Int
symSize = termSize . underlyingTerm @con
