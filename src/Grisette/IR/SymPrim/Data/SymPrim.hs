{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
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
    SomeSym (..),
    AllSyms (..),
    allSymsSize,
    unarySomeSymIntN,
    unarySomeSymIntNR1,
    binSomeSymIntN,
    binSomeSymIntNR1,
    binSomeSymIntNR2,
    unarySomeSymWordN,
    unarySomeSymWordNR1,
    binSomeSymWordN,
    binSomeSymWordNR1,
    binSomeSymWordNR2,
  )
where

import Control.DeepSeq (NFData (rnf))
import Control.Monad.Except (ExceptT (ExceptT), MonadError (throwError))
import Control.Monad.Identity
  ( Identity (Identity),
    IdentityT (IdentityT),
  )
import Control.Monad.Trans.Maybe (MaybeT (MaybeT))
import qualified Control.Monad.Writer.Lazy as WriterLazy
import qualified Control.Monad.Writer.Strict as WriterStrict
import Data.Bits
  ( Bits
      ( bit,
        bitSize,
        bitSizeMaybe,
        clearBit,
        complement,
        complementBit,
        isSigned,
        popCount,
        rotate,
        rotateL,
        rotateR,
        setBit,
        shift,
        shiftL,
        shiftR,
        testBit,
        unsafeShiftL,
        unsafeShiftR,
        xor,
        zeroBits,
        (.&.),
        (.|.)
      ),
    FiniteBits (finiteBitSize),
  )
import qualified Data.ByteString as B
import Data.Functor.Sum (Sum)
import Data.Hashable (Hashable (hashWithSalt))
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Proxy (Proxy (Proxy))
import Data.String (IsString (fromString))
import qualified Data.Text as T
import Data.Typeable (typeRep, type (:~:) (Refl))
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.Generics
  ( Generic (Rep, from),
    K1 (K1),
    M1 (M1),
    U1,
    type (:*:) ((:*:)),
    type (:+:) (L1, R1),
  )
import GHC.TypeNats
  ( KnownNat,
    Nat,
    natVal,
    sameNat,
    type (+),
    type (<=),
  )
import Generics.Deriving (Default (Default, unDefault))
import Grisette.Core.Control.Exception
  ( AssertionError,
    VerificationConditions,
  )
import Grisette.Core.Data.BV
  ( IntN,
    SomeIntN (SomeIntN),
    SomeWordN (SomeWordN),
    WordN,
  )
import Grisette.Core.Data.Class.BitVector
  ( BV (bvConcat, bvExt, bvSelect, bvSext, bvZext),
    SizedBV (sizedBVConcat, sizedBVExt, sizedBVSelect, sizedBVSext, sizedBVZext),
  )
import Grisette.Core.Data.Class.ExtractSymbolics
  ( ExtractSymbolics (extractSymbolics),
  )
import Grisette.Core.Data.Class.Function (Function (Arg, Ret, (#)))
import Grisette.Core.Data.Class.LogicalOp (LogicalOp ((&&~), (||~)))
import Grisette.Core.Data.Class.ModelOps
  ( ModelOps (emptyModel, insertValue),
    ModelRep (buildModel),
  )
import Grisette.Core.Data.Class.SEq (SEq ((/=~), (==~)))
import Grisette.Core.Data.Class.SOrd (SOrd ((<~), (>=~), (>~)))
import Grisette.Core.Data.Class.SafeArith
  ( ArithException (DivideByZero, Overflow, Underflow),
    SafeDivision
      ( safeDiv,
        safeDiv',
        safeDivMod,
        safeDivMod',
        safeMod,
        safeMod',
        safeQuot,
        safeQuot',
        safeQuotRem,
        safeQuotRem',
        safeRem,
        safeRem'
      ),
    SafeLinearArith
      ( safeAdd,
        safeAdd',
        safeMinus,
        safeMinus',
        safeNeg,
        safeNeg'
      ),
  )
import Grisette.Core.Data.Class.SignConversion (SignConversion (toSigned, toUnsigned))
import Grisette.Core.Data.Class.SimpleMergeable (mrgIf)
import Grisette.Core.Data.Class.Solvable
  ( Solvable (con, ssym),
    pattern Con,
  )
import Grisette.Core.Data.Class.Substitute (SubstituteSym (substituteSym))
import Grisette.Core.Data.Class.ToSym (ToSym (toSym))
import Grisette.IR.SymPrim.Data.IntBitwidth (intBitwidthQ)
import Grisette.IR.SymPrim.Data.Prim.InternedTerm.InternedCtors (symTerm)
import Grisette.IR.SymPrim.Data.Prim.InternedTerm.SomeTerm
  ( SomeTerm (SomeTerm),
  )
import Grisette.IR.SymPrim.Data.Prim.InternedTerm.Term
  ( ConRep (ConType),
    LinkedRep (underlyingTerm, wrapTerm),
    SupportedPrim,
    SymRep (SymType),
    Term (SymTerm),
    TypedSymbol (WithInfo),
    type (-->) (GeneralFun),
  )
import Grisette.IR.SymPrim.Data.Prim.InternedTerm.TermSubstitution
  ( substTerm,
  )
import Grisette.IR.SymPrim.Data.Prim.InternedTerm.TermUtils
  ( extractSymbolicsTerm,
    pformat,
    someTermsSize,
    termSize,
    termsSize,
  )
import Grisette.IR.SymPrim.Data.Prim.Model
  ( Model,
    SymbolSet (SymbolSet),
  )
import Grisette.IR.SymPrim.Data.Prim.PartialEval.BV
  ( pevalBVConcatTerm,
    pevalBVExtendTerm,
    pevalBVSelectTerm,
    pevalToSignedTerm,
    pevalToUnsignedTerm,
  )
import Grisette.IR.SymPrim.Data.Prim.PartialEval.Bits
  ( pevalAndBitsTerm,
    pevalComplementBitsTerm,
    pevalOrBitsTerm,
    pevalRotateBitsTerm,
    pevalShiftBitsTerm,
    pevalXorBitsTerm,
  )
import Grisette.IR.SymPrim.Data.Prim.PartialEval.GeneralFun
  ( pevalGeneralFunApplyTerm,
  )
import Grisette.IR.SymPrim.Data.Prim.PartialEval.Integral
  ( pevalDivBoundedIntegralTerm,
    pevalDivIntegralTerm,
    pevalModBoundedIntegralTerm,
    pevalModIntegralTerm,
    pevalQuotBoundedIntegralTerm,
    pevalQuotIntegralTerm,
    pevalRemBoundedIntegralTerm,
    pevalRemIntegralTerm,
  )
import Grisette.IR.SymPrim.Data.Prim.PartialEval.Num
  ( pevalAbsNumTerm,
    pevalAddNumTerm,
    pevalMinusNumTerm,
    pevalSignumNumTerm,
    pevalTimesNumTerm,
    pevalUMinusNumTerm,
  )
import Grisette.IR.SymPrim.Data.Prim.PartialEval.TabularFun
  ( pevalTabularFunApplyTerm,
  )
import Grisette.IR.SymPrim.Data.TabularFun (type (=->))
import Grisette.Lib.Control.Monad (mrgReturn)
import Grisette.Utils.Parameterized
  ( KnownProof (KnownProof),
    LeqProof (LeqProof),
    knownAdd,
    leqAddPos,
    leqTrans,
    unsafeKnownProof,
    unsafeLeqProof,
  )
import Language.Haskell.TH.Syntax (Lift)

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

#define QUOTE() '
#define QID(a) a
#define QRIGHT(a) QID(a)'

#define SAFE_DIVISION_FUNC(name, type, op) \
name (type l) rs@(type r) = \
  mrgIf \
    (rs ==~ con 0) \
    (throwError DivideByZero) \
    (mrgReturn $ type $ op l r); \
QRIGHT(name) t (type l) rs@(type r) = \
  mrgIf \
    (rs ==~ con 0) \
    (throwError (t DivideByZero)) \
    (mrgReturn $ type $ op l r)

#define SAFE_DIVISION_FUNC2(name, type, op1, op2) \
name (type l) rs@(type r) = \
  mrgIf \
    (rs ==~ con 0) \
    (throwError DivideByZero) \
    (mrgReturn (type $ op1 l r, type $ op2 l r)); \
QRIGHT(name) t (type l) rs@(type r) = \
  mrgIf \
    (rs ==~ con 0) \
    (throwError (t DivideByZero)) \
    (mrgReturn (type $ op1 l r, type $ op2 l r))

#if 1
instance SafeDivision ArithException SymInteger where
  SAFE_DIVISION_FUNC(safeDiv, SymInteger, pevalDivIntegralTerm)
  SAFE_DIVISION_FUNC(safeMod, SymInteger, pevalModIntegralTerm)
  SAFE_DIVISION_FUNC(safeQuot, SymInteger, pevalQuotIntegralTerm)
  SAFE_DIVISION_FUNC(safeRem, SymInteger, pevalRemIntegralTerm)
  SAFE_DIVISION_FUNC2(safeDivMod, SymInteger, pevalDivIntegralTerm, pevalModIntegralTerm)
  SAFE_DIVISION_FUNC2(safeQuotRem, SymInteger, pevalQuotIntegralTerm, pevalRemIntegralTerm)
#endif

instance SafeLinearArith ArithException SymInteger where
  safeAdd ls rs = mrgReturn $ ls + rs
  safeAdd' _ ls rs = mrgReturn $ ls + rs
  safeNeg v = mrgReturn $ -v
  safeNeg' _ v = mrgReturn $ -v
  safeMinus ls rs = mrgReturn $ ls - rs
  safeMinus' _ ls rs = mrgReturn $ ls - rs

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

#define SAFE_DIVISION_FUNC_BOUNDED_SIGNED(name, type, op) \
name ls@(type l) rs@(type r) = \
  mrgIf \
    (rs ==~ con 0) \
    (throwError DivideByZero) \
    (mrgIf (rs ==~ con (-1) &&~ ls ==~ con minBound) \
      (throwError Overflow) \
      (mrgReturn $ type $ op l r)); \
QRIGHT(name) t ls@(type l) rs@(type r) = \
  mrgIf \
    (rs ==~ con 0) \
    (throwError (t DivideByZero)) \
    (mrgIf (rs ==~ con (-1) &&~ ls ==~ con minBound) \
      (throwError (t Overflow)) \
      (mrgReturn $ type $ op l r))

#define SAFE_DIVISION_FUNC2_BOUNDED_SIGNED(name, type, op1, op2) \
name ls@(type l) rs@(type r) = \
  mrgIf \
    (rs ==~ con 0) \
    (throwError DivideByZero) \
    (mrgIf (rs ==~ con (-1) &&~ ls ==~ con minBound) \
      (throwError Overflow) \
      (mrgReturn (type $ op1 l r, type $ op2 l r))); \
QRIGHT(name) t ls@(type l) rs@(type r) = \
  mrgIf \
    (rs ==~ con 0) \
    (throwError (t DivideByZero)) \
    (mrgIf (rs ==~ con (-1) &&~ ls ==~ con minBound) \
      (throwError (t Overflow)) \
      (mrgReturn (type $ op1 l r, type $ op2 l r)))

#if 1
instance (KnownNat n, 1 <= n) => SafeDivision ArithException (SymIntN n) where
  SAFE_DIVISION_FUNC_BOUNDED_SIGNED(safeDiv, SymIntN, pevalDivBoundedIntegralTerm)
  SAFE_DIVISION_FUNC(safeMod, SymIntN, pevalModBoundedIntegralTerm)
  SAFE_DIVISION_FUNC_BOUNDED_SIGNED(safeQuot, SymIntN, pevalQuotBoundedIntegralTerm)
  SAFE_DIVISION_FUNC(safeRem, SymIntN, pevalRemBoundedIntegralTerm)
  SAFE_DIVISION_FUNC2_BOUNDED_SIGNED(safeDivMod, SymIntN, pevalDivBoundedIntegralTerm, pevalModBoundedIntegralTerm)
  SAFE_DIVISION_FUNC2_BOUNDED_SIGNED(safeQuotRem, SymIntN, pevalQuotBoundedIntegralTerm, pevalRemBoundedIntegralTerm)
#endif

instance (KnownNat n, 1 <= n) => SafeLinearArith ArithException (SymIntN n) where
  safeAdd ls rs =
    mrgIf
      (ls >~ 0)
      (mrgIf (rs >~ 0 &&~ res <~ 0) (throwError Overflow) (return res))
      ( mrgIf
          (ls <~ 0 &&~ rs <~ 0 &&~ res >=~ 0)
          (throwError Underflow)
          (mrgReturn res)
      )
    where
      res = ls + rs
  safeAdd' f ls rs =
    mrgIf
      (ls >~ 0)
      (mrgIf (rs >~ 0 &&~ res <~ 0) (throwError $ f Overflow) (return res))
      ( mrgIf
          (ls <~ 0 &&~ rs <~ 0 &&~ res >=~ 0)
          (throwError $ f Underflow)
          (mrgReturn res)
      )
    where
      res = ls + rs
  safeNeg v = mrgIf (v ==~ con minBound) (throwError Overflow) (mrgReturn $ -v)
  safeNeg' f v = mrgIf (v ==~ con minBound) (throwError $ f Overflow) (mrgReturn $ -v)
  safeMinus ls rs =
    mrgIf
      (ls >=~ 0)
      (mrgIf (rs <~ 0 &&~ res <~ 0) (throwError Overflow) (return res))
      ( mrgIf
          (ls <~ 0 &&~ rs >~ 0 &&~ res >~ 0)
          (throwError Underflow)
          (mrgReturn res)
      )
    where
      res = ls - rs
  safeMinus' f ls rs =
    mrgIf
      (ls >=~ 0)
      (mrgIf (rs <~ 0 &&~ res <~ 0) (throwError $ f Overflow) (return res))
      ( mrgIf
          (ls <~ 0 &&~ rs >~ 0 &&~ res >~ 0)
          (throwError $ f Underflow)
          (mrgReturn res)
      )
    where
      res = ls - rs

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
-- >>> bvConcat (SomeSymIntN (con 0b101 :: SymIntN 3)) (SomeSymIntN (con 0b110 :: SymIntN 3))
-- 0b101110
--
-- More symbolic operations are available. Please refer to the documentation
-- for the type class instances.
data SomeSymIntN where
  SomeSymIntN :: (KnownNat n, 1 <= n) => SymIntN n -> SomeSymIntN

unarySomeSymIntN :: (forall n. (KnownNat n, 1 <= n) => SymIntN n -> r) -> String -> SomeSymIntN -> r
unarySomeSymIntN op _ (SomeSymIntN (w :: SymIntN w)) = op w
{-# INLINE unarySomeSymIntN #-}

unarySomeSymIntNR1 :: (forall n. (KnownNat n, 1 <= n) => SymIntN n -> SymIntN n) -> String -> SomeSymIntN -> SomeSymIntN
unarySomeSymIntNR1 op _ (SomeSymIntN (w :: SymIntN w)) = SomeSymIntN $ op w
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

#if 1
instance (KnownNat n, 1 <= n) => SafeDivision ArithException (SymWordN n) where
  SAFE_DIVISION_FUNC(safeDiv, SymWordN, pevalDivIntegralTerm)
  SAFE_DIVISION_FUNC(safeMod, SymWordN, pevalModIntegralTerm)
  SAFE_DIVISION_FUNC(safeQuot, SymWordN, pevalQuotIntegralTerm)
  SAFE_DIVISION_FUNC(safeRem, SymWordN, pevalRemIntegralTerm)
  SAFE_DIVISION_FUNC2(safeDivMod, SymWordN, pevalDivIntegralTerm, pevalModIntegralTerm)
  SAFE_DIVISION_FUNC2(safeQuotRem, SymWordN, pevalQuotIntegralTerm, pevalRemIntegralTerm)
#endif

instance (KnownNat n, 1 <= n) => SafeLinearArith ArithException (SymWordN n) where
  safeAdd ls rs =
    mrgIf
      (ls >~ res ||~ rs >~ res)
      (throwError Overflow)
      (mrgReturn res)
    where
      res = ls + rs
  safeAdd' f ls rs =
    mrgIf
      (ls >~ res ||~ rs >~ res)
      (throwError $ f Overflow)
      (mrgReturn res)
    where
      res = ls + rs
  safeNeg v = mrgIf (v /=~ 0) (throwError Underflow) (mrgReturn v)
  safeNeg' f v = mrgIf (v /=~ 0) (throwError $ f Underflow) (mrgReturn v)
  safeMinus ls rs =
    mrgIf
      (rs >~ ls)
      (throwError Underflow)
      (mrgReturn res)
    where
      res = ls - rs
  safeMinus' f ls rs =
    mrgIf
      (rs >~ ls)
      (throwError $ f Underflow)
      (mrgReturn res)
    where
      res = ls - rs

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
-- >>> bvConcat (SomeSymWordN (con 0b101 :: SymWordN 3)) (SomeSymWordN (con 0b110 :: SymWordN 3))
-- 0b101110
--
-- More symbolic operations are available. Please refer to the documentation
-- for the type class instances.
data SomeSymWordN where
  SomeSymWordN :: (KnownNat n, 1 <= n) => SymWordN n -> SomeSymWordN

unarySomeSymWordN :: (forall n. (KnownNat n, 1 <= n) => SymWordN n -> r) -> String -> SomeSymWordN -> r
unarySomeSymWordN op _ (SomeSymWordN (w :: SymWordN w)) = op w
{-# INLINE unarySomeSymWordN #-}

unarySomeSymWordNR1 :: (forall n. (KnownNat n, 1 <= n) => SymWordN n -> SymWordN n) -> String -> SomeSymWordN -> SomeSymWordN
unarySomeSymWordNR1 op _ (SomeSymWordN (w :: SymWordN w)) = SomeSymWordN $ op w
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
  {-# INLINE (.&.) #-}; \
  symtype l .|. symtype r = symtype $ pevalOrBitsTerm l r; \
  {-# INLINE (.|.) #-}; \
  symtype l `xor` symtype r = symtype $ pevalXorBitsTerm l r; \
  {-# INLINE xor #-}; \
  complement (symtype n) = symtype $ pevalComplementBitsTerm n; \
  {-# INLINE complement #-}; \
  shift (symtype n) i = symtype $ pevalShiftBitsTerm n i; \
  {-# INLINE shift #-}; \
  rotate (symtype n) i = symtype $ pevalRotateBitsTerm n i; \
  {-# INLINE rotate #-}; \
  bitSize = finiteBitSize; \
  {-# INLINE bitSize #-}; \
  bitSizeMaybe = Just . finiteBitSize; \
  {-# INLINE bitSizeMaybe #-}; \
  isSigned _ = signed; \
  {-# INLINE isSigned #-}; \
  testBit (Con n) =  testBit n; \
  testBit _ = error "You cannot call testBit on symbolic variables"; \
  {-# INLINE testBit #-}; \
  bit = con . bit; \
  {-# INLINE bit #-}; \
  popCount (Con n) = popCount n; \
  popCount _ = error "You cannot call popCount on symbolic variables"; \
  {-# INLINE popCount #-}

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
  bitSizeMaybe = Just . finiteBitSize; \
  {-# INLINE bitSizeMaybe #-}; \
  bitSize = finiteBitSize; \
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

-- FiniteBits

#define FINITE_BITS_BV(symtype) \
instance (KnownNat n, 1 <= n) => FiniteBits (symtype n) where \
  finiteBitSize _ = fromIntegral $ natVal (Proxy @n); \
  {-# INLINE finiteBitSize #-}; \

#define FINITE_BITS_BV_SOME(somety, origty) \
instance FiniteBits somety where \
  finiteBitSize (somety (n :: origty n)) = fromIntegral $ natVal n; \
  {-# INLINE finiteBitSize #-}

#if 1
FINITE_BITS_BV(SymIntN)
FINITE_BITS_BV(SymWordN)
FINITE_BITS_BV_SOME(SomeSymIntN, SymIntN)
FINITE_BITS_BV_SOME(SomeSymWordN, SymWordN)
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
  fromString = ssym . fromString

#define IS_STRING_BV(symtype) \
instance (KnownNat n, 1 <= n) => IsString (symtype n) where \
  fromString = ssym . fromString

#define IS_STRING_FUN(op, cons) \
instance (SupportedPrim ca, SupportedPrim cb, LinkedRep ca sa, LinkedRep cb sb) => IsString (sa op sb) where \
  fromString = ssym . fromString

#if 1
IS_STRING_SIMPLE(SymBool)
IS_STRING_SIMPLE(SymInteger)
IS_STRING_BV(SymIntN)
IS_STRING_BV(SymWordN)
IS_STRING_FUN(=~>, SymTabularFunc)
IS_STRING_FUN(-~>, SymGeneralFun)
#endif

-- Solvable

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
TO_SYM_SYMID_SIMPLE(SomeSymWordN)
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
sizedBVSelect :: forall n ix w p q. (KnownNat n, KnownNat ix, KnownNat w, 1 <= n, 1 <= w, ix + w <= n) => \
  p ix -> q w -> symtype n -> symtype w; \
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
bvConcat (somety (a :: origty l)) (somety (b :: origty r)) = \
  case (leqAddPos (Proxy @l) (Proxy @r), knownAdd @l @r KnownProof KnownProof) of \
    (LeqProof, KnownProof) -> \
      somety $ sizedBVConcat a b

#define BVZEXT(somety, origty) \
bvZext l (somety (a :: origty n)) \
  | l < n = error "bvZext: trying to zero extend a value to a smaller size" \
  | otherwise = res (Proxy @n) \
  where \
    n = fromIntegral $ natVal (Proxy @n); \
    res :: forall (l :: Nat). Proxy l -> somety; \
    res p = \
      case (unsafeKnownProof @l (fromIntegral l), unsafeLeqProof @1 @l, unsafeLeqProof @n @l) of \
        (KnownProof, LeqProof, LeqProof) -> somety $ sizedBVZext p a

#define BVSEXT(somety, origty) \
bvSext l (somety (a :: origty n)) \
  | l < n = error "bvZext: trying to zero extend a value to a smaller size" \
  | otherwise = res (Proxy @n) \
  where \
    n = fromIntegral $ natVal (Proxy @n); \
    res :: forall (l :: Nat). Proxy l -> somety; \
    res p = \
      case (unsafeKnownProof @l (fromIntegral l), unsafeLeqProof @1 @l, unsafeLeqProof @n @l) of \
        (KnownProof, LeqProof, LeqProof) -> somety $ sizedBVSext p a

#define BVSELECT(somety, origty) \
bvSelect ix w (somety (a :: origty n)) \
    | ix + w > n = error "bvSelect: trying to select a bitvector outside the bounds of the input" \
    | w == 0 = error "bvSelect: trying to select a bitvector of size 0" \
    | otherwise = res (Proxy @n) (Proxy @n) \
    where \
      n = fromIntegral $ natVal (Proxy @n); \
      res :: forall (w :: Nat) (ix :: Nat). Proxy w -> Proxy ix -> somety; \
      res _ _ = \
        case ( unsafeKnownProof @ix (fromIntegral ix), \
               unsafeKnownProof @w (fromIntegral w), \
               unsafeLeqProof @1 @w, \
               unsafeLeqProof @(ix + w) @n \
             ) of \
          (KnownProof, KnownProof, LeqProof, LeqProof) -> \
            somety $ sizedBVSelect (Proxy @ix) (Proxy @w) a

#if 1
instance BV SomeSymIntN where
  BVCONCAT(SomeSymIntN, SymIntN)
  {-# INLINE bvConcat #-}
  BVZEXT(SomeSymIntN, SymIntN)
  {-# INLINE bvZext #-}
  BVSEXT(SomeSymIntN, SymIntN)
  {-# INLINE bvSext #-}
  bvExt = bvSext
  {-# INLINE bvExt #-}
  BVSELECT(SomeSymIntN, SymIntN)
  {-# INLINE bvSelect #-}

instance BV SomeSymWordN where
  BVCONCAT(SomeSymWordN, SymWordN)
  {-# INLINE bvConcat #-}
  BVZEXT(SomeSymWordN, SymWordN)
  {-# INLINE bvZext #-}
  BVSEXT(SomeSymWordN, SymWordN)
  {-# INLINE bvSext #-}
  bvExt = bvZext
  {-# INLINE bvExt #-}
  BVSELECT(SomeSymWordN, SymWordN)
  {-# INLINE bvSelect #-}
#endif

-- BVSignConversion

instance (KnownNat n, 1 <= n) => SignConversion (SymWordN n) (SymIntN n) where
  toSigned (SymWordN n) = SymIntN $ pevalToSignedTerm n
  toUnsigned (SymIntN n) = SymWordN $ pevalToUnsignedTerm n

instance SignConversion SomeSymWordN SomeSymIntN where
  toSigned (SomeSymWordN n) = SomeSymIntN $ toSigned n
  toUnsigned (SomeSymIntN n) = SomeSymWordN $ toUnsigned n

-- ModelRep

-- | A pair of a symbolic constant and its value.
-- This is used to build a model from a list of symbolic constants and their values.
--
-- >>> buildModel ("a" := (1 :: Integer), "b" := True) :: Model
-- Model {a -> 1 :: Integer, b -> True :: Bool}
data ModelSymPair ct st where
  (:=) :: (LinkedRep ct st) => st -> ct -> ModelSymPair ct st

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
symsSize :: forall con sym. (LinkedRep con sym) => [sym] -> Int
symsSize = termsSize . fmap (underlyingTerm @con)
{-# INLINE symsSize #-}

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
symSize :: forall con sym. (LinkedRep con sym) => sym -> Int
symSize = termSize . underlyingTerm @con
{-# INLINE symSize #-}

-- | Some symbolic value with 'LinkedRep' constraint.
data SomeSym where
  SomeSym :: (LinkedRep con sym) => sym -> SomeSym

someUnderlyingTerm :: SomeSym -> SomeTerm
someUnderlyingTerm (SomeSym s) = SomeTerm $ underlyingTerm s

someSymsSize :: [SomeSym] -> Int
someSymsSize = someTermsSize . fmap someUnderlyingTerm
{-# INLINE someSymsSize #-}

-- | Extract all symbolic primitive values that are represented as SMT terms.
--
-- __Note:__ This type class can be derived for algebraic data types. You may
-- need the @DerivingVia@ and @DerivingStrategies@ extenstions.
--
-- > data X = ... deriving Generic deriving AllSyms via (Default X)
class AllSyms a where
  -- | Convert a value to a list of symbolic primitive values. It should
  -- prepend to an existing list of symbolic primitive values.
  allSymsS :: a -> [SomeSym] -> [SomeSym]
  allSymsS a l = allSyms a ++ l

  -- | Specialized 'allSymsS' that prepends to an empty list.
  allSyms :: a -> [SomeSym]
  allSyms a = allSymsS a []

  {-# MINIMAL allSymsS | allSyms #-}

-- | Get the total size of symbolic terms in a value.
-- Duplicate sub-terms are counted for only once.
--
-- >>> allSymsSize ("a" :: SymInteger, "a" + "b" :: SymInteger, ("a" + "b") * "c" :: SymInteger)
-- 5
allSymsSize :: (AllSyms a) => a -> Int
allSymsSize = someSymsSize . allSyms

class AllSyms' a where
  allSymsS' :: a c -> [SomeSym] -> [SomeSym]

instance (Generic a, AllSyms' (Rep a)) => AllSyms (Default a) where
  allSymsS = allSymsS' . from . unDefault

instance AllSyms' U1 where
  allSymsS' _ = id

instance (AllSyms c) => AllSyms' (K1 i c) where
  allSymsS' (K1 v) = allSymsS v

instance (AllSyms' a) => AllSyms' (M1 i c a) where
  allSymsS' (M1 v) = allSymsS' v

instance (AllSyms' a, AllSyms' b) => AllSyms' (a :+: b) where
  allSymsS' (L1 l) = allSymsS' l
  allSymsS' (R1 r) = allSymsS' r

instance (AllSyms' a, AllSyms' b) => AllSyms' (a :*: b) where
  allSymsS' (a :*: b) = allSymsS' a . allSymsS' b

#define CONCRETE_ALLSYMS(type) \
instance AllSyms type where \
  allSymsS _ = id

#define ALLSYMS_SIMPLE(t) \
instance AllSyms t where \
  allSymsS v = (SomeSym v :)

#define ALLSYMS_BV(t) \
instance (KnownNat n, 1 <= n) => AllSyms (t n) where \
  allSymsS v = (SomeSym v :)

#define ALLSYMS_SOME_BV(t) \
instance AllSyms t where \
  allSymsS (t v) = (SomeSym v :)

#define ALLSYMS_FUN(op) \
instance (SupportedPrim ca, SupportedPrim cb, LinkedRep ca sa, LinkedRep cb sb) => AllSyms (sa op sb) where \
  allSymsS v = (SomeSym v :)

#if 1
CONCRETE_ALLSYMS(Bool)
CONCRETE_ALLSYMS(Integer)
CONCRETE_ALLSYMS(Char)
CONCRETE_ALLSYMS(Int)
CONCRETE_ALLSYMS(Int8)
CONCRETE_ALLSYMS(Int16)
CONCRETE_ALLSYMS(Int32)
CONCRETE_ALLSYMS(Int64)
CONCRETE_ALLSYMS(Word)
CONCRETE_ALLSYMS(Word8)
CONCRETE_ALLSYMS(Word16)
CONCRETE_ALLSYMS(Word32)
CONCRETE_ALLSYMS(Word64)
CONCRETE_ALLSYMS(B.ByteString)
CONCRETE_ALLSYMS(T.Text)
ALLSYMS_SIMPLE(SymBool)
ALLSYMS_SIMPLE(SymInteger)
ALLSYMS_BV(SymIntN)
ALLSYMS_BV(SymWordN)
ALLSYMS_SOME_BV(SomeSymIntN)
ALLSYMS_SOME_BV(SomeSymWordN)
ALLSYMS_FUN(=~>)
ALLSYMS_FUN(-~>)
#endif

instance AllSyms () where
  allSymsS _ = id

-- Either
deriving via
  (Default (Either a b))
  instance
    ( AllSyms a,
      AllSyms b
    ) =>
    AllSyms (Either a b)

-- Maybe
deriving via (Default (Maybe a)) instance (AllSyms a) => AllSyms (Maybe a)

-- List
deriving via (Default [a]) instance (AllSyms a) => AllSyms [a]

-- (,)
deriving via
  (Default (a, b))
  instance
    (AllSyms a, AllSyms b) =>
    AllSyms (a, b)

-- (,,)
deriving via
  (Default (a, b, c))
  instance
    ( AllSyms a,
      AllSyms b,
      AllSyms c
    ) =>
    AllSyms (a, b, c)

-- (,,,)
deriving via
  (Default (a, b, c, d))
  instance
    ( AllSyms a,
      AllSyms b,
      AllSyms c,
      AllSyms d
    ) =>
    AllSyms (a, b, c, d)

-- (,,,,)
deriving via
  (Default (a, b, c, d, e))
  instance
    ( AllSyms a,
      AllSyms b,
      AllSyms c,
      AllSyms d,
      AllSyms e
    ) =>
    AllSyms (a, b, c, d, e)

-- (,,,,,)
deriving via
  (Default (a, b, c, d, e, f))
  instance
    ( AllSyms a,
      AllSyms b,
      AllSyms c,
      AllSyms d,
      AllSyms e,
      AllSyms f
    ) =>
    AllSyms (a, b, c, d, e, f)

-- (,,,,,,)
deriving via
  (Default (a, b, c, d, e, f, g))
  instance
    ( AllSyms a,
      AllSyms b,
      AllSyms c,
      AllSyms d,
      AllSyms e,
      AllSyms f,
      AllSyms g
    ) =>
    AllSyms (a, b, c, d, e, f, g)

-- (,,,,,,,)
deriving via
  (Default (a, b, c, d, e, f, g, h))
  instance
    ( AllSyms a,
      AllSyms b,
      AllSyms c,
      AllSyms d,
      AllSyms e,
      AllSyms f,
      AllSyms g,
      AllSyms h
    ) =>
    AllSyms ((,,,,,,,) a b c d e f g h)

-- MaybeT
instance
  (AllSyms (m (Maybe a))) =>
  AllSyms (MaybeT m a)
  where
  allSymsS (MaybeT v) = allSymsS v

-- ExceptT
instance
  (AllSyms (m (Either e a))) =>
  AllSyms (ExceptT e m a)
  where
  allSymsS (ExceptT v) = allSymsS v

-- Sum
deriving via
  (Default (Sum f g a))
  instance
    (AllSyms (f a), AllSyms (g a)) =>
    AllSyms (Sum f g a)

-- WriterT
instance
  (AllSyms (m (a, s))) =>
  AllSyms (WriterLazy.WriterT s m a)
  where
  allSymsS (WriterLazy.WriterT v) = allSymsS v

instance
  (AllSyms (m (a, s))) =>
  AllSyms (WriterStrict.WriterT s m a)
  where
  allSymsS (WriterStrict.WriterT v) = allSymsS v

-- Identity
instance (AllSyms a) => AllSyms (Identity a) where
  allSymsS (Identity a) = allSymsS a

-- IdentityT
instance (AllSyms (m a)) => AllSyms (IdentityT m a) where
  allSymsS (IdentityT a) = allSymsS a

-- VerificationConditions
deriving via (Default VerificationConditions) instance AllSyms VerificationConditions

-- AssertionError
deriving via (Default AssertionError) instance AllSyms AssertionError
