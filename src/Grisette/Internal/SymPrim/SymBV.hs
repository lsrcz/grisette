{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      :   Grisette.Internal.SymPrim.SymBV
-- Copyright   :   (c) Sirui Lu 2021-2023
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Internal.SymPrim.SymBV
  ( SymWordN (SymWordN),
    SymWordN8,
    SymWordN16,
    SymWordN32,
    SymWordN64,
    SymIntN (SymIntN),
    SymIntN8,
    SymIntN16,
    SymIntN32,
    SymIntN64,
  )
where

import Control.DeepSeq (NFData)
import qualified Data.Binary as Binary
import Data.Bits
  ( Bits
      ( bit,
        bitSize,
        bitSizeMaybe,
        complement,
        isSigned,
        popCount,
        rotate,
        shift,
        testBit,
        xor,
        (.&.),
        (.|.)
      ),
    FiniteBits (finiteBitSize),
  )
import Data.Bytes.Serial (Serial (deserialize, serialize))
import Data.Hashable (Hashable (hashWithSalt))
import Data.Proxy (Proxy (Proxy))
import qualified Data.Serialize as Cereal
import Data.String (IsString (fromString))
import GHC.Generics (Generic)
import GHC.TypeNats
  ( KnownNat,
    Nat,
    natVal,
    type (+),
    type (<=),
  )
import Grisette.Internal.Core.Data.Class.BitCast (BitCast (bitCast))
import Grisette.Internal.Core.Data.Class.BitVector
  ( SizedBV
      ( sizedBVConcat,
        sizedBVExt,
        sizedBVSelect,
        sizedBVSext,
        sizedBVZext
      ),
  )
import Grisette.Internal.Core.Data.Class.Function
  ( Apply (FunType, apply),
  )
import Grisette.Internal.Core.Data.Class.SignConversion
  ( SignConversion (toSigned, toUnsigned),
  )
import Grisette.Internal.Core.Data.Class.Solvable
  ( Solvable (con, conView, ssym, sym),
    pattern Con,
  )
import Grisette.Internal.Core.Data.Class.SymRotate
  ( SymRotate (symRotate, symRotateNegated),
  )
import Grisette.Internal.Core.Data.Class.SymShift
  ( SymShift (symShift, symShiftNegated),
  )
import Grisette.Internal.Internal.Decl.SymPrim.AllSyms
  ( AllSyms (allSymsS),
    SomeSym (SomeSym),
  )
import Grisette.Internal.SymPrim.BV
  ( IntN,
    WordN,
  )
import Grisette.Internal.SymPrim.Prim.Term
  ( ConRep (ConType),
    LinkedRep (underlyingTerm, wrapTerm),
    PEvalBVTerm (pevalBVConcatTerm, pevalBVExtendTerm, pevalBVSelectTerm),
    PEvalBitCastTerm (pevalBitCastTerm),
    PEvalBitwiseTerm
      ( pevalAndBitsTerm,
        pevalComplementBitsTerm,
        pevalOrBitsTerm,
        pevalXorBitsTerm
      ),
    PEvalNumTerm
      ( pevalAbsNumTerm,
        pevalAddNumTerm,
        pevalMulNumTerm,
        pevalNegNumTerm,
        pevalSignumNumTerm
      ),
    PEvalOrdTerm (pevalLeOrdTerm),
    PEvalRotateTerm
      ( pevalRotateLeftTerm,
        pevalRotateRightTerm
      ),
    PEvalShiftTerm (pevalShiftLeftTerm, pevalShiftRightTerm),
    SupportedPrim (pevalITETerm),
    SymRep (SymType),
    Term,
    conTerm,
    pevalDivIntegralTerm,
    pevalEqTerm,
    pevalGeOrdTerm,
    pevalModIntegralTerm,
    pevalOrTerm,
    pevalQuotIntegralTerm,
    pevalRemIntegralTerm,
    pevalSubNumTerm,
    pformatTerm,
    symTerm,
    typedConstantSymbol,
    pattern ConTerm,
  )
import Grisette.Internal.SymPrim.SymBool (SymBool (SymBool))
import Grisette.Internal.Utils.Parameterized
  ( KnownProof (KnownProof),
    LeqProof (LeqProof),
    knownAdd,
    leqAddPos,
    leqTrans,
  )
import Language.Haskell.TH.Syntax (Lift)

-- $setup
-- >>> import Grisette.Core
-- >>> import Grisette.SymPrim
-- >>> import Grisette.Backend
-- >>> import Data.Proxy

-- | Symbolic signed bit vector type. Indexed with the bit width.
-- Signedness affects the semantics of the operations, including
-- comparison/extension, etc.
--
-- >>> "a" + 5 :: SymIntN 5
-- (+ 0b00101 a)
-- >>> sizedBVConcat (con 0b101 :: SymIntN 3) (con 0b110 :: SymIntN 3)
-- 0b101110
-- >>> sizedBVExt (Proxy @6) (con 0b101 :: SymIntN 3)
-- 0b111101
-- >>> (8 :: SymIntN 4) .< (7 :: SymIntN 4)
-- true
--
-- More operations are available. Please refer to "Grisette.Core#g:symops" for
-- more information.
newtype SymIntN (n :: Nat) = SymIntN {underlyingIntNTerm :: Term (IntN n)}
  deriving (Lift, NFData, Generic)

-- | Symbolic 8-bit signed bit-vector.
type SymIntN8 = SymIntN 8

-- | Symbolic 16-bit signed bit-vector.
type SymIntN16 = SymIntN 16

-- | Symbolic 32-bit signed bit-vector.
type SymIntN32 = SymIntN 32

-- | Symbolic 64-bit signed bit-vector.
type SymIntN64 = SymIntN 64

-- | Symbolic unsigned bit vector type. Indexed with the bit width.
-- Signedness affects the semantics of the operations, including
-- comparison/extension, etc.
--
-- >>> "a" + 5 :: SymWordN 5
-- (+ 0b00101 a)
-- >>> sizedBVConcat (con 0b101 :: SymWordN 3) (con 0b110 :: SymWordN 3)
-- 0b101110
-- >>> sizedBVExt (Proxy @6) (con 0b101 :: SymWordN 3)
-- 0b000101
-- >>> (8 :: SymWordN 4) .< (7 :: SymWordN 4)
-- false
--
-- More operations are available. Please refer to "Grisette.Core#g:symops" for
-- more information.
newtype SymWordN (n :: Nat) = SymWordN {underlyingWordNTerm :: Term (WordN n)}
  deriving (Lift, NFData, Generic)

-- | Symbolic 8-bit unsigned bit-vector.
type SymWordN8 = SymWordN 8

-- | Symbolic 16-bit unsigned bit-vector.
type SymWordN16 = SymWordN 16

-- | Symbolic 32-bit unsigned bit-vector.
type SymWordN32 = SymWordN 32

-- | Symbolic 64-bit unsigned bit-vector.
type SymWordN64 = SymWordN 64

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

-- Aggregate instances

instance (KnownNat n, 1 <= n) => Apply (SymIntN n) where
  type FunType (SymIntN n) = SymIntN n
  apply = id

instance (KnownNat n, 1 <= n) => Apply (SymWordN n) where
  type FunType (SymWordN n) = SymWordN n
  apply = id

#define SOLVABLE_BV(contype, symtype) \
instance (KnownNat n, 1 <= n) => Solvable (contype n) (symtype n) where \
  con = symtype . conTerm; \
  sym = symtype . symTerm . typedConstantSymbol; \
  conView (symtype (ConTerm  t)) = Just t; \
  conView _ = Nothing

#if 1
SOLVABLE_BV(IntN, SymIntN)
SOLVABLE_BV(WordN, SymWordN)
#endif

-- Num

#define NUM_BV(symtype) \
instance (KnownNat n, 1 <= n) => Num (symtype n) where \
  (symtype l) + (symtype r) = symtype $ pevalAddNumTerm l r; \
  (symtype l) - (symtype r) = symtype $ pevalSubNumTerm l r; \
  (symtype l) * (symtype r) = symtype $ pevalMulNumTerm l r; \
  negate (symtype v) = symtype $ pevalNegNumTerm v; \
  abs (symtype v) = symtype $ pevalAbsNumTerm v; \
  signum (symtype v) = symtype $ pevalSignumNumTerm v; \
  fromInteger i = con $ fromInteger i

#if 1
NUM_BV(SymIntN)
NUM_BV(SymWordN)
#endif

instance (KnownNat n, 1 <= n) => Bounded (SymIntN n) where
  minBound = con $ minBound @(IntN n)
  maxBound = con $ maxBound @(IntN n)
  {-# INLINE minBound #-}
  {-# INLINE maxBound #-}

#define ENUM_BV(symtype, contype, symtypestring) \
instance (KnownNat n, 1 <= n) => Enum (symtype n) where \
  succ _ = error $ "succ: succ isn't supported for " ++ symtypestring; \
  pred _ = error $ "pred: pred isn't supported for " ++ symtypestring; \
  toEnum i \
    | (fromIntegral i :: Integer) >= fromIntegral (minBound @(contype n)) \
        && (fromIntegral i :: Integer) <= fromIntegral (maxBound @(contype n)) = \
        con $ fromIntegral i \
    | otherwise = error "toEnum: toEnum is out of bounds"; \
  fromEnum _ = \
    error $ "fromEnum: fromEnum isn't supported for " ++ symtypestring; \
  enumFrom = \
    error $ "enumFrom: enumFrom isn't supported for " ++ symtypestring; \
  enumFromThen = \
    error $ "enumFromThen: enumFromThen isn't supported for " ++ symtypestring; \
  enumFromTo = \
    error $ "enumFromTo: enumFromTo isn't supported for " ++ symtypestring; \
  enumFromThenTo = \
    error $ "enumFromThenTo: enumFromThenTo isn't supported for " ++ symtypestring

#if 1
ENUM_BV (SymIntN, IntN, "SymIntN")
ENUM_BV (SymWordN, WordN, "SymWordN")
#endif

#define ORD_BV(symtype, symtypestring) \
instance (KnownNat n, 1 <= n) => Ord (symtype n) where \
  (<) = error $ "Ord: < isn't supported for " ++ symtypestring ++ ". Consider using the symbolic comparison operators (.<)."; \
  (<=) = error $ "Ord: <= isn't supported for " ++ symtypestring ++ ". Consider using the symbolic comparison operators (.<=)."; \
  (>=) = error $ "Ord: >= isn't supported for " ++ symtypestring ++ ". Consider using the symbolic comparison operators (.>=)."; \
  (>) = error $ "Ord: > isn't supported for " ++ symtypestring ++ ". Consider using the symbolic comparison operators (.>)."; \
  max (symtype l) (symtype r) = symtype $ pevalITETerm (pevalLeOrdTerm l r) r l; \
  {-# INLINE max #-}; \
  min (symtype l) (symtype r) = symtype $ pevalITETerm (pevalLeOrdTerm l r) l r; \
  {-# INLINE min #-}; \
  compare _ _ = \
    error $ "compare: compare isn't supported for " ++ symtypestring ++ ". Consider using the symbolic comparison operators (symCompare)."

#if 1
ORD_BV(SymIntN, "SymIntN")
ORD_BV(SymWordN, "SymWordN")
#endif

instance (KnownNat n, 1 <= n) => Real (SymIntN n) where
  toRational _ = error $ "toRational: toRational isn't supported for " ++ "SymIntN"

instance (KnownNat n, 1 <= n) => Real (SymWordN n) where
  toRational _ = error $ "toRational: toRational isn't supported for " ++ "SymWordN"

#define INTEGRAL_BV(symtype, symtypestring) \
instance (KnownNat n, 1 <= n) => Integral (symtype n) where \
  toInteger = error $ "toInteger: toInteger isn't supported for " ++ symtypestring; \
  div (symtype l) (symtype r) = symtype $ pevalDivIntegralTerm l r; \
  {-# INLINE div #-}; \
  mod (symtype l) (symtype r) = symtype $ pevalModIntegralTerm l r; \
  {-# INLINE mod #-}; \
  quot (symtype l) (symtype r) = symtype $ pevalQuotIntegralTerm l r; \
  {-# INLINE quot #-}; \
  rem (symtype l) (symtype r) = symtype $ pevalRemIntegralTerm l r; \
  {-# INLINE rem #-}; \
  divMod (symtype l) (symtype r) = \
    (symtype $ pevalDivIntegralTerm l r, symtype $ pevalModIntegralTerm l r); \
  {-# INLINE divMod #-}; \
  quotRem (symtype l) (symtype r) = \
    (symtype $ pevalQuotIntegralTerm l r, symtype $ pevalRemIntegralTerm l r); \
  {-# INLINE quotRem #-}

#if 1
-- | The functions are total and will not throw errors. The result is considered
-- undefined if the divisor is 0.
--
-- It is the responsibility of the caller to ensure that the divisor is not
-- zero with the symbolic constraints, or use the t'Grisette.Core.DivOr' or
-- t'Grisette.Core.SafeDiv' classes.
INTEGRAL_BV(SymIntN, "SymIntN")
-- | The functions are total and will not throw errors. The result is considered
-- undefined if the divisor is 0.
--
-- It is the responsibility of the caller to ensure that the divisor is not
-- zero with the symbolic constraints, or use the t'Grisette.Core.DivOr' or
-- t'Grisette.Core.SafeDiv' classes.
INTEGRAL_BV(SymWordN, "SymWordN")
#endif

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
  shift (symtype n) i | i > 0 = symtype $ pevalShiftLeftTerm n (conTerm $ fromIntegral i); \
  shift (symtype n) i | i < 0 = symtype $ pevalShiftRightTerm n (conTerm $ fromIntegral (-i)); \
  shift (symtype n) _ = symtype n; \
  {-# INLINE shift #-}; \
  rotate (symtype n) i | i > 0 = symtype $ pevalRotateLeftTerm n (conTerm $ fromIntegral i); \
  rotate (symtype n) i | i < 0 = symtype $ pevalRotateRightTerm n (conTerm $ fromIntegral (-i)); \
  rotate (symtype n) _ = symtype n; \
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

#if 1
BITS_BV(SymIntN, True)
BITS_BV(SymWordN, False)
#endif

-- FiniteBits

#define FINITE_BITS_BV(symtype) \
instance (KnownNat n, 1 <= n) => FiniteBits (symtype n) where \
  finiteBitSize _ = fromIntegral $ natVal (Proxy @n); \
  {-# INLINE finiteBitSize #-}; \

#if 1
FINITE_BITS_BV(SymIntN)
FINITE_BITS_BV(SymWordN)
#endif

-- Show

#define SHOW_BV(symtype) \
instance (KnownNat n, 1 <= n) => Show (symtype n) where \
  show (symtype t) = pformatTerm t

#if 1
SHOW_BV(SymIntN)
SHOW_BV(SymWordN)
#endif

-- Hashable

#define HASHABLE_BV(symtype) \
instance (KnownNat n, 1 <= n) => Hashable (symtype n) where \
  hashWithSalt s (symtype v) = s `hashWithSalt` v

#if 1
HASHABLE_BV(SymIntN)
HASHABLE_BV(SymWordN)
#endif

-- Eq

#define EQ_BV(symtype) \
instance (KnownNat n, 1 <= n) => Eq (symtype n) where \
  (symtype l) == (symtype r) = l == r

#if 1
-- | Checks if two formulas are the same. Not building the actual symbolic
-- equality formula.
--
-- The reason why we choose this behavior is to allow symbolic variables to be
-- used as keys in hash maps, which can be useful for memoization.
--
-- Use with caution. Usually you should use t'Grisette.Core.SymEq' instead.
EQ_BV(SymIntN)
-- | Checks if two formulas are the same. Not building the actual symbolic
-- equality formula.
--
-- The reason why we choose this behavior is to allow symbolic variables to be
-- used as keys in hash maps, which can be useful for memoization.
--
-- Use with caution. Usually you should use t'Grisette.Core.SymEq' instead.
EQ_BV(SymWordN)
#endif

-- IsString

#define IS_STRING_BV(symtype) \
instance (KnownNat n, 1 <= n) => IsString (symtype n) where \
  fromString = ssym . fromString

#if 1
IS_STRING_BV(SymIntN)
IS_STRING_BV(SymWordN)
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

#define BVBV(somety, origty) \
  bv n i = case mkNatRepr n of \
    Some (natRepr :: NatRepr x) -> \
      case unsafeLeqProof @1 @x of \
        LeqProof -> withKnownNat natRepr $ \
          somety (fromIntegral i :: origty x)

-- BVSignConversion

instance (KnownNat n, 1 <= n) => SignConversion (SymWordN n) (SymIntN n) where
  toSigned (SymWordN n) = SymIntN $ pevalBitCastTerm n
  toUnsigned (SymIntN n) = SymWordN $ pevalBitCastTerm n

-- SymShift
instance (KnownNat n, 1 <= n) => SymShift (SymWordN n) where
  symShift (SymWordN a) (SymWordN s) = SymWordN $ pevalShiftLeftTerm a s
  symShiftNegated (SymWordN a) (SymWordN s) = SymWordN $ pevalShiftRightTerm a s

instance (KnownNat n, 1 <= n) => SymShift (SymIntN n) where
  symShift a _ | finiteBitSize a == 1 = a
  symShift as@(SymIntN a) (SymIntN s)
    | finiteBitSize as == 2 =
        SymIntN $
          pevalITETerm
            (pevalGeOrdTerm s (conTerm 0))
            (pevalShiftLeftTerm a s)
            ( pevalITETerm
                (pevalEqTerm s (conTerm (-2)))
                ( pevalITETerm
                    (pevalGeOrdTerm a (conTerm 0))
                    (conTerm 0)
                    (conTerm (-1))
                )
                (pevalShiftRightTerm a (pevalNegNumTerm s))
            )
  symShift (SymIntN a) (SymIntN s) =
    SymIntN $
      pevalITETerm
        (pevalGeOrdTerm s (conTerm 0))
        (pevalShiftLeftTerm a s)
        ( pevalITETerm
            (pevalLeOrdTerm s (conTerm (-bs)))
            (pevalShiftRightTerm a (conTerm bs))
            (pevalShiftRightTerm a (pevalNegNumTerm s))
        )
    where
      bs = fromIntegral (finiteBitSize (0 :: IntN n)) :: IntN n
  symShiftNegated (SymIntN a) (SymIntN s) =
    SymIntN $
      pevalITETerm
        (pevalGeOrdTerm s (conTerm 0))
        (pevalShiftRightTerm a s)
        ( pevalITETerm
            (pevalLeOrdTerm s (conTerm (-bs)))
            (conTerm 0)
            (pevalShiftLeftTerm a (pevalNegNumTerm s))
        )
    where
      bs = fromIntegral (finiteBitSize (0 :: IntN n)) :: IntN n

-- SymRotate
instance (KnownNat n, 1 <= n) => SymRotate (SymWordN n) where
  symRotate (SymWordN a) (SymWordN s) = SymWordN (pevalRotateLeftTerm a s)
  symRotateNegated (SymWordN a) (SymWordN s) =
    SymWordN (pevalRotateRightTerm a s)

instance (KnownNat n, 1 <= n) => SymRotate (SymIntN n) where
  symRotate as@(SymIntN a) (SymIntN s)
    | finiteBitSize as == 1 = as
    | finiteBitSize as == 2 =
        SymIntN $
          pevalITETerm
            ( pevalOrTerm
                (pevalEqTerm s (conTerm 0))
                (pevalEqTerm s (conTerm (-2)))
            )
            a
            (pevalRotateLeftTerm a (conTerm 1))
    | otherwise =
        SymIntN $
          pevalRotateLeftTerm
            a
            ( pevalModIntegralTerm
                s
                (conTerm (fromIntegral $ finiteBitSize as))
            )
  symRotateNegated as@(SymIntN a) (SymIntN s)
    | finiteBitSize as == 1 = as
    | finiteBitSize as == 2 =
        SymIntN $
          pevalITETerm
            ( pevalOrTerm
                (pevalEqTerm s (conTerm 0))
                (pevalEqTerm s (conTerm (-2)))
            )
            a
            (pevalRotateLeftTerm a (conTerm 1))
    | otherwise =
        SymIntN $
          pevalRotateRightTerm
            a
            ( pevalModIntegralTerm
                s
                (conTerm (fromIntegral $ finiteBitSize as))
            )

#define ALLSYMS_BV(t) \
instance (KnownNat n, 1 <= n) => AllSyms (t n) where \
  allSymsS v = (SomeSym v :)

#if 1
ALLSYMS_BV(SymIntN)
ALLSYMS_BV(SymWordN)
#endif

instance (KnownNat n, 1 <= n) => BitCast (SymIntN n) (SymWordN n) where
  bitCast (SymIntN n) = SymWordN $ pevalBitCastTerm n

instance (KnownNat n, 1 <= n) => BitCast (SymWordN n) (SymIntN n) where
  bitCast (SymWordN n) = SymIntN $ pevalBitCastTerm n

instance BitCast (SymIntN 1) SymBool where
  bitCast (SymIntN v) = SymBool $ pevalBitCastTerm v

instance BitCast (SymWordN 1) SymBool where
  bitCast (SymWordN v) = SymBool $ pevalBitCastTerm v

instance BitCast SymBool (SymIntN 1) where
  bitCast (SymBool v) = SymIntN $ pevalBitCastTerm v

instance BitCast SymBool (SymWordN 1) where
  bitCast (SymBool v) = SymWordN $ pevalBitCastTerm v

instance (KnownNat n, 1 <= n) => Serial (SymWordN n) where
  serialize = serialize . underlyingWordNTerm
  deserialize = SymWordN <$> deserialize

instance (KnownNat n, 1 <= n) => Serial (SymIntN n) where
  serialize = serialize . underlyingIntNTerm
  deserialize = SymIntN <$> deserialize

instance (KnownNat n, 1 <= n) => Cereal.Serialize (SymWordN n) where
  put = serialize
  get = deserialize

instance (KnownNat n, 1 <= n) => Binary.Binary (SymWordN n) where
  put = serialize
  get = deserialize

instance (KnownNat n, 1 <= n) => Cereal.Serialize (SymIntN n) where
  put = serialize
  get = deserialize

instance (KnownNat n, 1 <= n) => Binary.Binary (SymIntN n) where
  put = serialize
  get = deserialize
