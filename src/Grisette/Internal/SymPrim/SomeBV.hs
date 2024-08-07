{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- Module      :   Grisette.Internal.SymPrim.SomeBV
-- Copyright   :   (c) Sirui Lu 2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Internal.SymPrim.SomeBV
  ( SomeBV (..),

    -- * Constructing and pattern matching on SomeBV
    unsafeSomeBV,
    conBV,
    conBVView,
    pattern ConBV,
    symBV,
    ssymBV,
    isymBV,
    arbitraryBV,

    -- * Synonyms
    pattern SomeIntN,
    type SomeIntN,
    pattern SomeWordN,
    type SomeWordN,
    pattern SomeSymIntN,
    type SomeSymIntN,
    pattern SomeSymWordN,
    type SomeSymWordN,

    -- * Helpers for manipulating SomeBV
    unarySomeBV,
    unarySomeBVR1,
    binSomeBV,
    binSomeBVR1,
    binSomeBVR2,
    binSomeBVSafe,
    binSomeBVSafeR1,
    binSomeBVSafeR2,
  )
where

import Control.DeepSeq (NFData (rnf))
import Control.Exception (throw)
import Control.Monad.Except (ExceptT, MonadError (throwError), runExceptT)
import Data.Bifunctor (Bifunctor (bimap))
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
    FiniteBits (countLeadingZeros, countTrailingZeros, finiteBitSize),
  )
import Data.Data (Proxy (Proxy))
import Data.Hashable (Hashable (hashWithSalt))
import Data.Maybe (fromJust)
import Data.Type.Equality (type (:~:) (Refl))
import GHC.TypeNats
  ( KnownNat,
    Nat,
    natVal,
    sameNat,
    type (+),
    type (<=),
  )
import Grisette.Internal.Core.Control.Monad.Union (Union)
import Grisette.Internal.Core.Data.Class.BitVector
  ( BV (bv, bvConcat, bvExt, bvSelect, bvSext, bvZext),
    SizedBV
      ( sizedBVConcat,
        sizedBVExt,
        sizedBVFromIntegral,
        sizedBVSelect,
        sizedBVSext,
        sizedBVZext
      ),
  )
import Grisette.Internal.Core.Data.Class.EvalSym
  ( EvalSym (evalSym),
  )
import Grisette.Internal.Core.Data.Class.ExtractSym
  ( ExtractSym (extractSymMaybe),
  )
import Grisette.Internal.Core.Data.Class.GenSym
  ( GenSym (fresh),
    GenSymSimple (simpleFresh),
  )
import Grisette.Internal.Core.Data.Class.ITEOp (ITEOp (symIte))
import Grisette.Internal.Core.Data.Class.Mergeable
  ( Mergeable (rootStrategy),
    MergingStrategy (SortedStrategy),
    wrapStrategy,
  )
import Grisette.Internal.Core.Data.Class.PPrint
  ( PPrint (pformat),
  )
import Grisette.Internal.Core.Data.Class.SafeDiv
  ( DivOr (divModOr, divOr, modOr, quotOr, quotRemOr, remOr),
    SafeDiv (safeDiv, safeDivMod, safeMod, safeQuot, safeQuotRem, safeRem),
  )
import Grisette.Internal.Core.Data.Class.SafeLinearArith
  ( SafeLinearArith (safeAdd, safeNeg, safeSub),
  )
import Grisette.Internal.Core.Data.Class.SafeSymRotate
  ( SafeSymRotate (safeSymRotateL, safeSymRotateR),
  )
import Grisette.Internal.Core.Data.Class.SafeSymShift
  ( SafeSymShift
      ( safeSymShiftL,
        safeSymShiftR,
        safeSymStrictShiftL,
        safeSymStrictShiftR
      ),
  )
import Grisette.Internal.Core.Data.Class.SignConversion
  ( SignConversion (toSigned, toUnsigned),
  )
import Grisette.Internal.Core.Data.Class.Solvable
  ( Solvable (con, conView, isym, ssym, sym),
  )
import Grisette.Internal.Core.Data.Class.SubstSym
  ( SubstSym (substSym),
  )
import Grisette.Internal.Core.Data.Class.SymEq (SymEq ((./=), (.==)))
import Grisette.Internal.Core.Data.Class.SymOrd
  ( SymOrd (symCompare, (.<), (.<=), (.>), (.>=)),
  )
import Grisette.Internal.Core.Data.Class.SymRotate
  ( SymRotate (symRotate, symRotateNegated),
  )
import Grisette.Internal.Core.Data.Class.SymShift
  ( SymShift (symShift, symShiftNegated),
  )
import Grisette.Internal.Core.Data.Class.ToCon (ToCon (toCon))
import Grisette.Internal.Core.Data.Class.ToSym (ToSym (toSym))
import Grisette.Internal.Core.Data.Class.TryMerge (TryMerge, tryMerge)
import Grisette.Internal.Core.Data.Symbol (Identifier, Symbol)
import Grisette.Internal.SymPrim.AllSyms (AllSyms (allSyms, allSymsS))
import Grisette.Internal.SymPrim.BV
  ( BitwidthMismatch (BitwidthMismatch),
    IntN,
    WordN,
  )
import Grisette.Internal.SymPrim.SymBV
  ( SymIntN,
    SymWordN,
  )
import Grisette.Internal.Utils.Parameterized
  ( KnownProof (KnownProof),
    LeqProof (LeqProof),
    NatRepr,
    SomePositiveNatRepr (SomePositiveNatRepr),
    knownAdd,
    leqAddPos,
    mkPositiveNatRepr,
    unsafeKnownProof,
    unsafeLeqProof,
  )
import Grisette.Lib.Data.Functor (mrgFmap)
import Language.Haskell.TH.Syntax (Lift (liftTyped))
import Test.QuickCheck (Arbitrary (arbitrary), Gen)
import Unsafe.Coerce (unsafeCoerce)

-- $setup
-- >>> import Grisette.Core
-- >>> import Grisette.SymPrim

-- | Non-indexed bitvectors.
--
-- The creation of t'SomeBV' can be done with the `bv` function with a positive
-- bit width and a value:
--
-- >>> bv 4 0xf :: SomeBV IntN
-- 0xf
--
-- Operations on two t'SomeBV' values require the bitwidths to be the same. So
-- you should check for the bit width (via `finiteBitSize`) before performing
-- operations:
--
-- >>> bv 4 0x3 + bv 4 0x3 :: SomeBV IntN
-- 0x6
-- >>> bv 4 0x3 + bv 8 0x3 :: SomeBV IntN
-- *** Exception: BitwidthMismatch
--
-- One exception is that the equality testing (both concrete and symbolic via
-- 'SymEq') does not require the bitwidths to be the same. Different bitwidths
-- means the values are not equal:
--
-- >>> (bv 4 0x3 :: SomeBV IntN) == (bv 8 0x3)
-- False
data SomeBV bv where
  SomeBV :: (KnownNat n, 1 <= n) => bv n -> SomeBV bv

instance
  (forall n. (KnownNat n, 1 <= n) => Hashable (bv n)) =>
  Hashable (SomeBV bv)
  where
  hashWithSalt s (SomeBV (bv :: bv n)) =
    s `hashWithSalt` (natVal (Proxy @n)) `hashWithSalt` bv
  {-# INLINE hashWithSalt #-}

instance
  (forall n. (KnownNat n, 1 <= n) => Lift (bv n)) =>
  Lift (SomeBV bv)
  where
  liftTyped (SomeBV bv) = [||SomeBV bv||]

instance
  (forall n. (KnownNat n, 1 <= n) => Show (bv n)) =>
  Show (SomeBV bv)
  where
  show (SomeBV bv) = show bv
  {-# INLINE show #-}

instance
  (forall n. (KnownNat n, 1 <= n) => NFData (bv n)) =>
  NFData (SomeBV bv)
  where
  rnf (SomeBV bv) = rnf bv
  {-# INLINE rnf #-}

instance (forall n. (KnownNat n, 1 <= n) => Eq (bv n)) => Eq (SomeBV bv) where
  SomeBV (l :: bv l) == SomeBV (r :: bv r) =
    case sameNat (Proxy @l) (Proxy @r) of
      Just Refl -> l == r
      Nothing -> False
  {-# INLINE (==) #-}
  SomeBV (l :: bv l) /= SomeBV (r :: bv r) =
    case sameNat (Proxy @l) (Proxy @r) of
      Just Refl -> l /= r
      Nothing -> True
  {-# INLINE (/=) #-}

instance (forall n. (KnownNat n, 1 <= n) => Ord (bv n)) => Ord (SomeBV bv) where
  (<) = binSomeBV (<)
  {-# INLINE (<) #-}
  (<=) = binSomeBV (<=)
  {-# INLINE (<=) #-}
  (>) = binSomeBV (>)
  {-# INLINE (>) #-}
  (>=) = binSomeBV (>=)
  {-# INLINE (>=) #-}
  max = binSomeBVR1 max
  {-# INLINE max #-}
  min = binSomeBVR1 min
  {-# INLINE min #-}
  compare = binSomeBV compare
  {-# INLINE compare #-}

instance (forall n. (KnownNat n, 1 <= n) => Num (bv n)) => Num (SomeBV bv) where
  (+) = binSomeBVR1 (+)
  {-# INLINE (+) #-}
  (-) = binSomeBVR1 (-)
  {-# INLINE (-) #-}
  (*) = binSomeBVR1 (*)
  {-# INLINE (*) #-}
  negate = unarySomeBVR1 negate
  {-# INLINE negate #-}
  abs = unarySomeBVR1 abs
  {-# INLINE abs #-}
  signum = unarySomeBVR1 signum
  {-# INLINE signum #-}
  fromInteger =
    error $
      "fromInteger is not defined for SomeBV as no bitwidth is known, use "
        <> "(bv <bitwidth> <value>) instead"
  {-# INLINE fromInteger #-}

instance
  (forall n. (KnownNat n, 1 <= n) => Bits (bv n)) =>
  Bits (SomeBV bv)
  where
  (.&.) = binSomeBVR1 (.&.)
  (.|.) = binSomeBVR1 (.|.)
  xor = binSomeBVR1 xor
  complement = unarySomeBVR1 complement
  shift s i = unarySomeBVR1 (`shift` i) s
  rotate s i = unarySomeBVR1 (`rotate` i) s
  zeroBits =
    error $
      "zeroBits is not defined for SomeBV as no bitwidth is known, use "
        <> "(bv <bitwidth> 0) or (SomeBV (zeroBits :: bv <bitwidth>)) instead"
  bit =
    error $
      "bit is not defined for SomeBV as no bitwidth is known, use "
        <> "(SomeBV (bit <bit> :: bv <bitwidth>)) instead"
  setBit s i = unarySomeBVR1 (`setBit` i) s
  clearBit s i = unarySomeBVR1 (`clearBit` i) s
  complementBit s i = unarySomeBVR1 (`complementBit` i) s
  testBit s i = unarySomeBV (`testBit` i) s
  bitSizeMaybe = unarySomeBV bitSizeMaybe
  bitSize = fromJust . unarySomeBV bitSizeMaybe
  isSigned _ = False
  shiftL s i = unarySomeBVR1 (`shiftL` i) s
  unsafeShiftL s i = unarySomeBVR1 (`unsafeShiftL` i) s
  shiftR s i = unarySomeBVR1 (`shiftR` i) s
  unsafeShiftR s i = unarySomeBVR1 (`unsafeShiftR` i) s
  rotateL s i = unarySomeBVR1 (`rotateL` i) s
  rotateR s i = unarySomeBVR1 (`rotateR` i) s
  popCount = unarySomeBV popCount

instance
  (forall n. (KnownNat n, 1 <= n) => FiniteBits (bv n)) =>
  FiniteBits (SomeBV bv)
  where
  finiteBitSize = unarySomeBV finiteBitSize
  {-# INLINE finiteBitSize #-}
  countLeadingZeros = unarySomeBV countLeadingZeros
  {-# INLINE countLeadingZeros #-}
  countTrailingZeros = unarySomeBV countTrailingZeros
  {-# INLINE countTrailingZeros #-}

instance
  (forall n. (KnownNat n, 1 <= n) => Enum (bv n)) =>
  Enum (SomeBV bv)
  where
  toEnum =
    error $
      "toEnum is not defined for SomeBV, use "
        <> "(SomeBV (toEnum <value> :: bv <bitwidth>)) instead"
  {-# INLINE toEnum #-}
  fromEnum = unarySomeBV fromEnum
  {-# INLINE fromEnum #-}

instance
  (forall n. (KnownNat n, 1 <= n) => Real (bv n)) =>
  Real (SomeBV bv)
  where
  toRational = unarySomeBV toRational
  {-# INLINE toRational #-}

instance
  (forall n. (KnownNat n, 1 <= n) => Integral (bv n)) =>
  Integral (SomeBV bv)
  where
  toInteger = unarySomeBV toInteger
  {-# INLINE toInteger #-}
  quot = binSomeBVR1 quot
  {-# INLINE quot #-}
  rem = binSomeBVR1 rem
  {-# INLINE rem #-}
  div = binSomeBVR1 div
  {-# INLINE div #-}
  mod = binSomeBVR1 mod
  {-# INLINE mod #-}
  quotRem = binSomeBVR2 quotRem
  {-# INLINE quotRem #-}
  divMod = binSomeBVR2 divMod
  {-# INLINE divMod #-}

instance (SizedBV bv) => BV (SomeBV bv) where
  bvConcat (SomeBV (a :: bv l)) (SomeBV (b :: bv r)) =
    case ( leqAddPos (Proxy @l) (Proxy @r),
           knownAdd @l @r KnownProof KnownProof
         ) of
      (LeqProof, KnownProof) ->
        SomeBV $ sizedBVConcat a b
  {-# INLINE bvConcat #-}
  bvZext l (SomeBV (a :: bv n))
    | l < n = error "bvZext: trying to zero extend a value to a smaller size"
    | otherwise = res (Proxy @n)
    where
      n = fromIntegral $ natVal (Proxy @n)
      res :: forall (l :: Nat). Proxy l -> SomeBV bv
      res p =
        case ( unsafeKnownProof @l (fromIntegral l),
               unsafeLeqProof @1 @l,
               unsafeLeqProof @n @l
             ) of
          (KnownProof, LeqProof, LeqProof) -> SomeBV $ sizedBVZext p a
  {-# INLINE bvZext #-}
  bvSext l (SomeBV (a :: bv n))
    | l < n = error "bvSext: trying to zero extend a value to a smaller size"
    | otherwise = res (Proxy @n)
    where
      n = fromIntegral $ natVal (Proxy @n)
      res :: forall (l :: Nat). Proxy l -> SomeBV bv
      res p =
        case ( unsafeKnownProof @l (fromIntegral l),
               unsafeLeqProof @1 @l,
               unsafeLeqProof @n @l
             ) of
          (KnownProof, LeqProof, LeqProof) -> SomeBV $ sizedBVSext p a
  {-# INLINE bvSext #-}
  bvExt l (SomeBV (a :: bv n))
    | l < n = error "bvExt: trying to zero extend a value to a smaller size"
    | otherwise = res (Proxy @n)
    where
      n = fromIntegral $ natVal (Proxy @n)
      res :: forall (l :: Nat). Proxy l -> SomeBV bv
      res p =
        case ( unsafeKnownProof @l (fromIntegral l),
               unsafeLeqProof @1 @l,
               unsafeLeqProof @n @l
             ) of
          (KnownProof, LeqProof, LeqProof) -> SomeBV $ sizedBVExt p a
  {-# INLINE bvExt #-}
  bvSelect ix w (SomeBV (a :: bv n))
    | ix + w > n =
        error $
          "bvSelect: trying to select a bitvector outside the bounds of the "
            <> "input"
    | w == 0 = error "bvSelect: trying to select a bitvector of size 0"
    | otherwise = res (Proxy @n) (Proxy @n)
    where
      n = fromIntegral $ natVal (Proxy @n)
      res :: forall (w :: Nat) (ix :: Nat). Proxy w -> Proxy ix -> SomeBV bv
      res _ _ =
        case ( unsafeKnownProof @ix (fromIntegral ix),
               unsafeKnownProof @w (fromIntegral w),
               unsafeLeqProof @1 @w,
               unsafeLeqProof @(ix + w) @n
             ) of
          (KnownProof, KnownProof, LeqProof, LeqProof) ->
            SomeBV $ sizedBVSelect (Proxy @ix) (Proxy @w) a
  bv n i = unsafeSomeBV n $ \_ -> sizedBVFromIntegral i
  {-# INLINE bv #-}

instance
  (forall n. (KnownNat n, 1 <= n) => EvalSym (bv n)) =>
  EvalSym (SomeBV bv)
  where
  evalSym fillDefault model = unarySomeBVR1 (evalSym fillDefault model)
  {-# INLINE evalSym #-}

instance
  (forall n. (KnownNat n, 1 <= n) => ExtractSym (bv n)) =>
  ExtractSym (SomeBV bv)
  where
  extractSymMaybe = unarySomeBV extractSymMaybe
  {-# INLINE extractSymMaybe #-}

instance
  (forall n. (KnownNat n, 1 <= n) => PPrint (bv n)) =>
  PPrint (SomeBV bv)
  where
  pformat (SomeBV bv) = pformat bv
  {-# INLINE pformat #-}

data CompileTimeNat where
  CompileTimeNat :: (KnownNat n, 1 <= n) => Proxy n -> CompileTimeNat

instance Show CompileTimeNat where
  show (CompileTimeNat (Proxy :: Proxy n)) = show (natVal (Proxy @n))
  {-# INLINE show #-}

instance Eq CompileTimeNat where
  CompileTimeNat (Proxy :: Proxy n) == CompileTimeNat (Proxy :: Proxy m) =
    case sameNat (Proxy @n) (Proxy @m) of
      Just Refl -> True
      Nothing -> False
  {-# INLINE (==) #-}

instance Ord CompileTimeNat where
  compare
    (CompileTimeNat (Proxy :: Proxy n))
    (CompileTimeNat (Proxy :: Proxy m)) =
      compare (natVal (Proxy @n)) (natVal (Proxy @m))
  {-# INLINE compare #-}

instance
  (forall n. (KnownNat n, 1 <= n) => Mergeable (bv n)) =>
  Mergeable (SomeBV bv)
  where
  rootStrategy =
    SortedStrategy @CompileTimeNat
      (\(SomeBV (_ :: bv n)) -> CompileTimeNat (Proxy @n))
      ( \(CompileTimeNat (_ :: proxy n)) ->
          wrapStrategy
            (rootStrategy @(bv n))
            SomeBV
            (\(SomeBV x) -> unsafeCoerce x)
      )

instance (forall n. (KnownNat n, 1 <= n) => SymEq (bv n)) => SymEq (SomeBV bv) where
  SomeBV (l :: bv l) .== SomeBV (r :: bv r) =
    case sameNat (Proxy @l) (Proxy @r) of
      Just Refl -> l .== r
      Nothing -> con False
  {-# INLINE (.==) #-}
  SomeBV (l :: bv l) ./= SomeBV (r :: bv r) =
    case sameNat (Proxy @l) (Proxy @r) of
      Just Refl -> l ./= r
      Nothing -> con True
  {-# INLINE (./=) #-}

instance
  (forall n. (KnownNat n, 1 <= n) => SymOrd (bv n)) =>
  SymOrd (SomeBV bv)
  where
  (.<) = binSomeBV (.<)
  {-# INLINE (.<) #-}
  (.<=) = binSomeBV (.<=)
  {-# INLINE (.<=) #-}
  (.>) = binSomeBV (.>)
  {-# INLINE (.>) #-}
  (.>=) = binSomeBV (.>=)
  {-# INLINE (.>=) #-}
  symCompare = binSomeBV symCompare
  {-# INLINE symCompare #-}

instance
  (forall n. (KnownNat n, 1 <= n) => SubstSym (bv n)) =>
  SubstSym (SomeBV bv)
  where
  substSym c s = unarySomeBVR1 (substSym c s)
  {-# INLINE substSym #-}

instance
  ( KnownNat n,
    1 <= n,
    forall m. (KnownNat m, 1 <= m) => GenSym () (bv m),
    Mergeable (SomeBV bv)
  ) =>
  GenSym (Proxy n) (SomeBV bv)
  where
  fresh _ =
    (\(i :: Union (bv n)) -> mrgFmap SomeBV i) <$> fresh ()
  {-# INLINE fresh #-}

instance
  ( KnownNat n,
    1 <= n,
    forall m. (KnownNat m, 1 <= m) => GenSymSimple () (bv m),
    Mergeable (SomeBV bv)
  ) =>
  GenSymSimple (Proxy n) (SomeBV bv)
  where
  simpleFresh _ = (\(i :: bv n) -> SomeBV i) <$> simpleFresh ()
  {-# INLINE simpleFresh #-}

instance
  ( forall m. (KnownNat m, 1 <= m) => GenSym () (bv m),
    Mergeable (SomeBV bv)
  ) =>
  GenSym (SomeBV bv) (SomeBV bv)
  where
  fresh (SomeBV (_ :: bv x)) = fresh (Proxy @x)
  {-# INLINE fresh #-}

instance
  ( forall m. (KnownNat m, 1 <= m) => GenSymSimple () (bv m),
    Mergeable (SomeBV bv)
  ) =>
  GenSymSimple (SomeBV bv) (SomeBV bv)
  where
  simpleFresh (SomeBV (_ :: bv x)) = simpleFresh (Proxy @x)
  {-# INLINE simpleFresh #-}

instance
  ( forall n. (KnownNat n, 1 <= n) => GenSym () (bv n),
    Mergeable (SomeBV bv)
  ) =>
  GenSym Int (SomeBV bv)
  where
  fresh n
    | n <= 0 = error "fresh: cannot generate a bitvector of non-positive size"
    | otherwise = case mkPositiveNatRepr (fromIntegral n) of
        SomePositiveNatRepr (_ :: NatRepr x) -> fresh (Proxy @x)
  {-# INLINE fresh #-}

instance
  ( forall n. (KnownNat n, 1 <= n) => GenSymSimple () (bv n),
    Mergeable (SomeBV bv)
  ) =>
  GenSymSimple Int (SomeBV bv)
  where
  simpleFresh n
    | n <= 0 = error "fresh: cannot generate a bitvector of non-positive size"
    | otherwise = case mkPositiveNatRepr (fromIntegral n) of
        SomePositiveNatRepr (_ :: NatRepr x) -> simpleFresh (Proxy @x)
  {-# INLINE simpleFresh #-}

instance
  ( forall n. (KnownNat n, 1 <= n) => SignConversion (ubv n) (sbv n),
    -- Add this to help the type checker resolve the functional dependency
    SignConversion (ubv 1) (sbv 1)
  ) =>
  SignConversion (SomeBV ubv) (SomeBV sbv)
  where
  toSigned (SomeBV (n :: ubv n)) = SomeBV (toSigned n :: sbv n)
  {-# INLINE toSigned #-}
  toUnsigned (SomeBV (n :: sbv n)) = SomeBV (toUnsigned n :: ubv n)
  {-# INLINE toUnsigned #-}

instance
  (forall n. (KnownNat n, 1 <= n) => ToCon (sbv n) (cbv n)) =>
  ToCon (SomeBV sbv) (SomeBV cbv)
  where
  toCon (SomeBV (n :: sbv n)) = SomeBV <$> (toCon n :: Maybe (cbv n))
  {-# INLINE toCon #-}

instance
  (forall n. (KnownNat n, 1 <= n) => ToSym (cbv n) (sbv n)) =>
  ToSym (SomeBV cbv) (SomeBV sbv)
  where
  toSym (SomeBV (n :: cbv n)) = SomeBV (toSym n :: sbv n)
  {-# INLINE toSym #-}

divRemOrBase ::
  ( forall n.
    (KnownNat n, 1 <= n) =>
    (bv n, bv n) ->
    bv n ->
    bv n ->
    (bv n, bv n)
  ) ->
  (SomeBV bv, SomeBV bv) ->
  SomeBV bv ->
  SomeBV bv ->
  (SomeBV bv, SomeBV bv)
divRemOrBase
  f
  (SomeBV (dd :: bv dd), SomeBV (dm :: bv dm))
  (SomeBV (a :: bv a))
  (SomeBV (b :: bv b)) =
    case ( sameNat (Proxy @a) (Proxy @b),
           sameNat (Proxy @a) (Proxy @dd),
           sameNat (Proxy @a) (Proxy @dm)
         ) of
      (Just Refl, Just Refl, Just Refl) -> bimap SomeBV SomeBV $ f (dd, dm) a b
      _ -> throw BitwidthMismatch
{-# INLINE divRemOrBase #-}

instance
  (forall n. (KnownNat n, 1 <= n) => DivOr (bv n)) =>
  DivOr (SomeBV bv)
  where
  divOr = ternSomeBVR1 divOr
  {-# INLINE divOr #-}
  modOr = ternSomeBVR1 modOr
  {-# INLINE modOr #-}
  quotOr = ternSomeBVR1 quotOr
  {-# INLINE quotOr #-}
  remOr = ternSomeBVR1 remOr
  {-# INLINE remOr #-}
  divModOr = divRemOrBase divModOr
  {-# INLINE divModOr #-}
  quotRemOr = divRemOrBase quotRemOr
  {-# INLINE quotRemOr #-}

instance
  ( forall n.
    (KnownNat n, 1 <= n) =>
    SafeDiv e (bv n) (ExceptT e m),
    MonadError (Either BitwidthMismatch e) m,
    TryMerge m,
    Mergeable e
  ) =>
  SafeDiv (Either BitwidthMismatch e) (SomeBV bv) m
  where
  safeDiv = binSomeBVSafeR1 (safeDiv @e)
  {-# INLINE safeDiv #-}
  safeMod = binSomeBVSafeR1 (safeMod @e)
  {-# INLINE safeMod #-}
  safeQuot = binSomeBVSafeR1 (safeQuot @e)
  {-# INLINE safeQuot #-}
  safeRem = binSomeBVSafeR1 (safeRem @e)
  {-# INLINE safeRem #-}
  safeDivMod = binSomeBVSafeR2 (safeDivMod @e)
  {-# INLINE safeDivMod #-}
  safeQuotRem = binSomeBVSafeR2 (safeQuotRem @e)
  {-# INLINE safeQuotRem #-}

instance
  ( forall n.
    (KnownNat n, 1 <= n) =>
    SafeLinearArith e (bv n) (ExceptT e m),
    MonadError (Either BitwidthMismatch e) m,
    TryMerge m,
    Mergeable e
  ) =>
  SafeLinearArith (Either BitwidthMismatch e) (SomeBV bv) m
  where
  safeAdd = binSomeBVSafeR1 (safeAdd @e)
  {-# INLINE safeAdd #-}
  safeSub = binSomeBVSafeR1 (safeSub @e)
  {-# INLINE safeSub #-}
  safeNeg =
    unarySomeBV
      ( \v ->
          mrgFmap SomeBV $
            runExceptT (safeNeg @e v) >>= either (throwError . Right) pure
      )
  {-# INLINE safeNeg #-}

instance
  (forall n. (KnownNat n, 1 <= n) => SymShift (bv n)) =>
  SymShift (SomeBV bv)
  where
  symShift = binSomeBVR1 symShift
  {-# INLINE symShift #-}
  symShiftNegated = binSomeBVR1 symShiftNegated
  {-# INLINE symShiftNegated #-}

instance
  (forall n. (KnownNat n, 1 <= n) => SymRotate (bv n)) =>
  SymRotate (SomeBV bv)
  where
  symRotate = binSomeBVR1 symRotate
  {-# INLINE symRotate #-}
  symRotateNegated = binSomeBVR1 symRotateNegated
  {-# INLINE symRotateNegated #-}

instance
  ( forall n.
    (KnownNat n, 1 <= n) =>
    SafeSymShift e (bv n) (ExceptT e m),
    MonadError (Either BitwidthMismatch e) m,
    TryMerge m,
    Mergeable e
  ) =>
  SafeSymShift (Either BitwidthMismatch e) (SomeBV bv) m
  where
  safeSymShiftL = binSomeBVSafeR1 (safeSymShiftL @e)
  {-# INLINE safeSymShiftL #-}
  safeSymShiftR = binSomeBVSafeR1 (safeSymShiftR @e)
  {-# INLINE safeSymShiftR #-}
  safeSymStrictShiftL = binSomeBVSafeR1 (safeSymStrictShiftL @e)
  {-# INLINE safeSymStrictShiftL #-}
  safeSymStrictShiftR = binSomeBVSafeR1 (safeSymStrictShiftR @e)
  {-# INLINE safeSymStrictShiftR #-}

instance
  ( forall n.
    (KnownNat n, 1 <= n) =>
    SafeSymRotate e (bv n) (ExceptT e m),
    MonadError (Either BitwidthMismatch e) m,
    TryMerge m,
    Mergeable e
  ) =>
  SafeSymRotate (Either BitwidthMismatch e) (SomeBV bv) m
  where
  safeSymRotateL = binSomeBVSafeR1 (safeSymRotateL @e)
  {-# INLINE safeSymRotateL #-}
  safeSymRotateR = binSomeBVSafeR1 (safeSymRotateR @e)
  {-# INLINE safeSymRotateR #-}

instance
  (forall n. (KnownNat n, 1 <= n) => ITEOp (bv n)) =>
  ITEOp (SomeBV bv)
  where
  symIte cond = binSomeBVR1 (symIte cond)

instance
  (forall n. (KnownNat n, 1 <= n) => AllSyms (bv n)) =>
  AllSyms (SomeBV bv)
  where
  allSyms = unarySomeBV allSyms
  {-# INLINE allSyms #-}
  allSymsS = unarySomeBV allSymsS
  {-# INLINE allSymsS #-}

-- Synonyms

-- | Type synonym for t'SomeBV' for concrete signed bitvectors.
type SomeIntN = SomeBV IntN

-- | Pattern synonym for t'SomeBV' for concrete signed bitvectors.
pattern SomeIntN :: () => (KnownNat n, 1 <= n) => IntN n -> SomeIntN
pattern SomeIntN a = SomeBV a

-- | Type synonym for t'SomeBV' for concrete unsigned bitvectors.
type SomeWordN = SomeBV WordN

-- | Pattern synonym for t'SomeBV' for concrete unsigned bitvectors.
pattern SomeWordN :: () => (KnownNat n, 1 <= n) => WordN n -> SomeWordN
pattern SomeWordN a = SomeBV a

-- | Type synonym for t'SomeBV' for symbolic signed bitvectors.
type SomeSymIntN = SomeBV SymIntN

-- | Pattern synonym for t'SomeBV' for symbolic signed bitvectors.
pattern SomeSymIntN :: () => (KnownNat n, 1 <= n) => SymIntN n -> SomeSymIntN
pattern SomeSymIntN a = SomeBV a

-- | Type synonym for t'SomeBV' for symbolic unsigned bitvectors.
type SomeSymWordN = SomeBV SymWordN

-- | Pattern synonym for t'SomeBV' for symbolic unsigned bitvectors.
pattern SomeSymWordN :: () => (KnownNat n, 1 <= n) => SymWordN n -> SomeSymWordN
pattern SomeSymWordN a = SomeBV a

-- Construction

-- | Construct a t'SomeBV' with a given run-time bitwidth and a polymorphic
-- value for the underlying bitvector.
unsafeSomeBV ::
  forall bv.
  Int ->
  (forall proxy n. (KnownNat n, 1 <= n) => proxy n -> bv n) ->
  SomeBV bv
unsafeSomeBV n i
  | n <= 0 = error "unsafeBV: trying to create a bitvector of non-positive size"
  | otherwise = case mkPositiveNatRepr (fromIntegral n) of
      SomePositiveNatRepr (_ :: NatRepr x) -> SomeBV (i (Proxy @x))

-- | Construct a symbolic t'SomeBV' with a given concrete t'SomeBV'. Similar to
-- 'con' but for t'SomeBV'.
--
-- >>> a = bv 8 0x12 :: SomeIntN
-- >>> conBV a :: SomeSymIntN
-- 0x12
conBV ::
  forall cbv bv.
  ( forall n. (KnownNat n, 1 <= n) => Solvable (cbv n) (bv n),
    Solvable (cbv 1) (bv 1)
  ) =>
  SomeBV cbv ->
  SomeBV bv
conBV (SomeBV (v :: cbv n)) = SomeBV $ con @(cbv n) @(bv n) v

-- | View pattern for symbolic t'SomeBV' to see if it contains a concrete value
-- and extract it. Similar to 'conView' but for t'SomeBV'.
--
-- >>> conBVView (bv 8 0x12 :: SomeSymIntN)
-- Just 0x12
-- >>> conBVView (ssymBV 4 "a" :: SomeSymIntN)
-- Nothing
conBVView ::
  forall cbv bv.
  ( forall n. (KnownNat n, 1 <= n) => Solvable (cbv n) (bv n),
    Solvable (cbv 1) (bv 1)
  ) =>
  SomeBV bv ->
  Maybe (SomeBV cbv)
conBVView (SomeBV (bv :: bv n)) = case conView @(cbv n) bv of
  Just c -> Just $ SomeBV c
  Nothing -> Nothing

-- | Pattern synonym for symbolic t'SomeBV' to see if it contains a concrete
-- value and extract it. Similar to 'Grisette.Core.Con' but for t'SomeBV'.
--
-- >>> case (bv 8 0x12 :: SomeSymIntN) of { ConBV c -> c; _ -> error "impossible" }
-- 0x12
pattern ConBV ::
  forall cbv bv.
  ( forall n. (KnownNat n, 1 <= n) => Solvable (cbv n) (bv n),
    Solvable (cbv 1) (bv 1)
  ) =>
  SomeBV cbv ->
  SomeBV bv
pattern ConBV c <- (conBVView -> Just c)
  where
    ConBV c = conBV c

-- | Construct a symbolic t'SomeBV' with a given run-time bitwidth and a symbol.
-- Similar to 'sym' but for t'SomeBV'.
--
-- >>> symBV 8 "a" :: SomeSymIntN
-- a
symBV ::
  forall cbv bv.
  ( forall n. (KnownNat n, 1 <= n) => Solvable (cbv n) (bv n),
    Solvable (cbv 1) (bv 1)
  ) =>
  Int ->
  Symbol ->
  SomeBV bv
symBV n s = unsafeSomeBV n $ \(_ :: proxy n) -> sym @(cbv n) s

-- | Construct a symbolic t'SomeBV' with a given run-time bitwidth and an
-- identifier. Similar to 'ssym' but for t'SomeBV'.
--
-- >>> ssymBV 8 "a" :: SomeSymIntN
-- a
ssymBV ::
  forall cbv bv.
  ( forall n. (KnownNat n, 1 <= n) => Solvable (cbv n) (bv n),
    Solvable (cbv 1) (bv 1)
  ) =>
  Int ->
  Identifier ->
  SomeBV bv
ssymBV n s = unsafeSomeBV n $ \(_ :: proxy n) -> ssym @(cbv n) s

-- | Construct a symbolic t'SomeBV' with a given run-time bitwidth, an identifier
-- and an index. Similar to 'isym' but for t'SomeBV'.
--
-- >>> isymBV 8 "a" 1 :: SomeSymIntN
-- a@1
isymBV ::
  forall cbv bv.
  ( forall n. (KnownNat n, 1 <= n) => Solvable (cbv n) (bv n),
    Solvable (cbv 1) (bv 1)
  ) =>
  Int ->
  Identifier ->
  Int ->
  SomeBV bv
isymBV n s i = unsafeSomeBV n $ \(_ :: proxy n) -> isym @(cbv n) s i

-- | Generate an arbitrary t'SomeBV' with a given run-time bitwidth.
arbitraryBV ::
  forall bv.
  (forall n. (KnownNat n, 1 <= n) => Arbitrary (bv n)) =>
  Int ->
  Gen (SomeBV bv)
arbitraryBV n
  | n <= 0 =
      error "arbitraryBV: trying to create a bitvector of non-positive size"
  | otherwise = case mkPositiveNatRepr (fromIntegral n) of
      SomePositiveNatRepr (_ :: NatRepr x) -> do
        v <- arbitrary :: Gen (bv x)
        return $ SomeBV v

-- Helpers

-- | Lift a unary operation on sized bitvectors that returns anything to
-- t'SomeBV'.
unarySomeBV :: forall bv r. (forall n. (KnownNat n, 1 <= n) => bv n -> r) -> SomeBV bv -> r
unarySomeBV f (SomeBV bv) = f bv
{-# INLINE unarySomeBV #-}

-- | Lift a unary operation on sized bitvectors that returns a bitvector to
-- t'SomeBV'. The result will also be wrapped with t'SomeBV'.
unarySomeBVR1 ::
  (forall n. (KnownNat n, 1 <= n) => bv n -> bv n) -> SomeBV bv -> SomeBV bv
unarySomeBVR1 f = unarySomeBV (SomeBV . f)
{-# INLINE unarySomeBVR1 #-}

-- | Lift a binary operation on sized bitvectors that returns anything to
-- t'SomeBV'. Crash if the bitwidths do not match.
binSomeBV ::
  (forall n. (KnownNat n, 1 <= n) => bv n -> bv n -> r) ->
  SomeBV bv ->
  SomeBV bv ->
  r
binSomeBV f (SomeBV (l :: bv l)) (SomeBV (r :: bv r)) =
  case sameNat (Proxy @l) (Proxy @r) of
    Just Refl -> f l r
    Nothing -> throw BitwidthMismatch
{-# INLINE binSomeBV #-}

-- | Lift a ternary operation on sized bitvectors that returns anything to
-- t'SomeBV'. Crash if the bitwidths do not match.
ternSomeBV ::
  (forall n. (KnownNat n, 1 <= n) => bv n -> bv n -> bv n -> r) ->
  SomeBV bv ->
  SomeBV bv ->
  SomeBV bv ->
  r
ternSomeBV f (SomeBV (a :: bv a)) (SomeBV (b :: bv b)) (SomeBV (c :: bv c)) =
  case (sameNat (Proxy @a) (Proxy @b), sameNat (Proxy @a) (Proxy @c)) of
    (Just Refl, Just Refl) -> f a b c
    _ -> throw BitwidthMismatch
{-# INLINE ternSomeBV #-}

-- | Lift a binary operation on sized bitvectors that returns a bitvector to
-- t'SomeBV'. The result will also be wrapped with t'SomeBV'. Crash if the
-- bitwidths do not match.
binSomeBVR1 ::
  (forall n. (KnownNat n, 1 <= n) => bv n -> bv n -> bv n) ->
  SomeBV bv ->
  SomeBV bv ->
  SomeBV bv
binSomeBVR1 f = binSomeBV (\a b -> SomeBV $ f a b)
{-# INLINE binSomeBVR1 #-}

-- | Lift a binary operation on sized bitvectors that returns two bitvectors to
-- t'SomeBV'. The results will also be wrapped with t'SomeBV'. Crash if the
-- bitwidths do not match.
binSomeBVR2 ::
  (forall n. (KnownNat n, 1 <= n) => bv n -> bv n -> (bv n, bv n)) ->
  SomeBV bv ->
  SomeBV bv ->
  (SomeBV bv, SomeBV bv)
binSomeBVR2 f = binSomeBV (\a b -> let (x, y) = f a b in (SomeBV x, SomeBV y))
{-# INLINE binSomeBVR2 #-}

-- | Lift a ternary operation on sized bitvectors that returns a bitvector to
-- t'SomeBV'. The result will also be wrapped with t'SomeBV'. Crash if the
-- bitwidths do not match.
ternSomeBVR1 ::
  (forall n. (KnownNat n, 1 <= n) => bv n -> bv n -> bv n -> bv n) ->
  SomeBV bv ->
  SomeBV bv ->
  SomeBV bv ->
  SomeBV bv
ternSomeBVR1 f = ternSomeBV (\a b c -> SomeBV $ f a b c)
{-# INLINE ternSomeBVR1 #-}

-- | Lift a binary operation on sized bitvectors that returns anything wrapped
-- with 'ExceptT' to t'SomeBV'. If the bitwidths do not match, throw an
-- t`BitwidthMismatch` error to the monadic context.
binSomeBVSafe ::
  ( MonadError (Either BitwidthMismatch e) m,
    TryMerge m,
    Mergeable e,
    Mergeable r
  ) =>
  (forall n. (KnownNat n, 1 <= n) => bv n -> bv n -> ExceptT e m r) ->
  SomeBV bv ->
  SomeBV bv ->
  m r
binSomeBVSafe f (SomeBV (l :: bv l)) (SomeBV (r :: bv r)) =
  case sameNat (Proxy @l) (Proxy @r) of
    Just Refl ->
      tryMerge $ runExceptT (f l r) >>= either (throwError . Right) pure
    Nothing -> tryMerge $ throwError $ Left BitwidthMismatch
{-# INLINE binSomeBVSafe #-}

-- | Lift a binary operation on sized bitvectors that returns a bitvector
-- wrapped with 'ExceptT' to t'SomeBV'. The result will also be wrapped with
-- t'SomeBV'.
--
-- If the bitwidths do not match, throw an t`BitwidthMismatch` error to the
-- monadic context.
binSomeBVSafeR1 ::
  ( MonadError (Either BitwidthMismatch e) m,
    TryMerge m,
    Mergeable e,
    forall n. (KnownNat n, 1 <= n) => Mergeable (bv n)
  ) =>
  (forall n. (KnownNat n, 1 <= n) => bv n -> bv n -> ExceptT e m (bv n)) ->
  SomeBV bv ->
  SomeBV bv ->
  m (SomeBV bv)
binSomeBVSafeR1 f = binSomeBVSafe (\l r -> mrgFmap SomeBV $ f l r)
{-# INLINE binSomeBVSafeR1 #-}

-- | Lift a binary operation on sized bitvectors that returns two bitvectors
-- wrapped with 'ExceptT' to t'SomeBV'. The results will also be wrapped with
-- t'SomeBV'.
--
-- If the bitwidths do not match, throw an t`BitwidthMismatch` error to the
-- monadic context.
binSomeBVSafeR2 ::
  ( MonadError (Either BitwidthMismatch e) m,
    TryMerge m,
    Mergeable e,
    forall n. (KnownNat n, 1 <= n) => Mergeable (bv n)
  ) =>
  ( forall n.
    (KnownNat n, 1 <= n) =>
    bv n ->
    bv n ->
    ExceptT e m (bv n, bv n)
  ) ->
  SomeBV bv ->
  SomeBV bv ->
  m (SomeBV bv, SomeBV bv)
binSomeBVSafeR2 f =
  binSomeBVSafe (\l r -> mrgFmap (bimap SomeBV SomeBV) $ f l r)
{-# INLINE binSomeBVSafeR2 #-}
