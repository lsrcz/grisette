{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Pizza.IR.SymPrim.Data.SymPrim
  ( Sym (..),
    SymBool,
    SymInteger,
    type (=~>),
    type (-~>),
    SymWordN,
    SymIntN,
    symSize,
    symsSize,
  )
where

import Control.DeepSeq
import Control.Monad.Except
import Data.Bits
import Data.HashSet as S
import Data.Hashable
import Data.Int
import Data.Proxy
import Data.String
import Data.Word
import GHC.Generics
import GHC.TypeLits
import Language.Haskell.TH.Syntax
import Pizza.Core.Data.Class.BitVector
import Pizza.Core.Data.Class.Bool
import Pizza.Core.Data.Class.Error
import Pizza.Core.Data.Class.Evaluate
import Pizza.Core.Data.Class.ExtractSymbolics
import Pizza.Core.Data.Class.Function
import Pizza.Core.Data.Class.GenSym
import Pizza.Core.Data.Class.Integer
import Pizza.Core.Data.Class.Mergeable
import Pizza.Core.Data.Class.PrimWrapper
import Pizza.Core.Data.Class.SOrd
import Pizza.Core.Data.Class.SimpleMergeable
import Pizza.Core.Data.Class.ToCon
import Pizza.Core.Data.Class.ToSym
import Pizza.IR.SymPrim.Data.BV
import Pizza.IR.SymPrim.Data.IntBitwidth
import Pizza.IR.SymPrim.Data.Prim.InternedTerm.InternedCtors
import Pizza.IR.SymPrim.Data.Prim.InternedTerm.Term
import Pizza.IR.SymPrim.Data.Prim.InternedTerm.TermUtils
import Pizza.IR.SymPrim.Data.Prim.Model
import Pizza.IR.SymPrim.Data.Prim.PartialEval.BV
import Pizza.IR.SymPrim.Data.Prim.PartialEval.Bits
import Pizza.IR.SymPrim.Data.Prim.PartialEval.Bool
import Pizza.IR.SymPrim.Data.Prim.PartialEval.GeneralFunc
import Pizza.IR.SymPrim.Data.Prim.PartialEval.Integer
import Pizza.IR.SymPrim.Data.Prim.PartialEval.Num
import Pizza.IR.SymPrim.Data.Prim.PartialEval.TabularFunc
import Pizza.IR.SymPrim.Data.TabularFunc
import Pizza.Lib.Control.Monad

newtype Sym a = Sym {underlyingTerm :: Term a} deriving (Lift, Generic)

instance NFData (Sym a) where
  rnf (Sym t) = rnf t

{-
class SupportedPrim a => SymConcView a where
  symConcView :: Sym a -> Maybe a

pattern SymConc :: SymConcView a => a -> Sym a
pattern SymConc c <-
  (Sym (ConcTerm _ c))
  where
    SymConc c = conc c
    -}

instance (SupportedPrim a) => ITEOp (Sym Bool) (Sym a) where
  ites (Sym c) (Sym t) (Sym f) = Sym $ pevalITETerm c t f

instance (SupportedPrim a) => Mergeable (Sym Bool) (Sym a) where
  mergingStrategy = SimpleStrategy ites

instance (SupportedPrim a) => SimpleMergeable (Sym Bool) (Sym a) where
  mrgIte = ites

instance (SupportedPrim a) => PrimWrapper (Sym a) a where
  conc = Sym . concTerm
  ssymb = Sym . ssymbTerm
  isymb str i = Sym $ isymbTerm str i
  sinfosymb str info = Sym $ sinfosymbTerm str info
  iinfosymb str i info = Sym $ iinfosymbTerm str i info
  concView (Sym (ConcTerm _ t)) = Just t
  concView _ = Nothing

instance (SupportedPrim t) => IsString (Sym t) where
  fromString = ssymb

instance (SupportedPrim a) => ToSym (Sym a) (Sym a) where
  toSym = id

instance (SupportedPrim a) => ToSym a (Sym a) where
  toSym = conc

instance (SupportedPrim a) => ToCon (Sym a) (Sym a) where
  toCon = Just

instance (SupportedPrim a) => ToCon (Sym a) a where
  toCon = concView

instance (SupportedPrim a) => EvaluateSym Model (Sym a) where
  evaluateSym fillDefault model (Sym t) = Sym $ evaluateTerm fillDefault model t

instance (SupportedPrim a) => ExtractSymbolics (S.HashSet TermSymbol) (Sym a) where
  extractSymbolics (Sym t) = extractSymbolicsTerm t

instance (SymBoolOp (Sym Bool), SupportedPrim a) => GenSym (Sym Bool) () (Sym a) where
  genSymFresh _ = mrgReturn <$> genSymSimpleFresh ()

instance (SymBoolOp (Sym Bool), SupportedPrim a) => GenSymSimple () (Sym a) where
  genSymSimpleFresh _ = do
    ident <- getGenSymIdent
    GenSymIndex i <- nextGenSymIndex
    case ident of
      GenSymIdent s -> return $ isymb s i
      GenSymIdentWithInfo s info -> return $ iinfosymb s i info

instance (SymBoolOp (Sym Bool), SupportedPrim a) => GenSym (Sym Bool) (Sym a) (Sym a)

instance (SymBoolOp (Sym Bool), SupportedPrim a) => GenSymSimple (Sym a) (Sym a) where
  genSymSimpleFresh _ = genSymSimpleFresh ()

instance (SupportedPrim a) => Show (Sym a) where
  show (Sym t) = pformat t

instance (SupportedPrim a) => Hashable (Sym a) where
  hashWithSalt s (Sym v) = s `hashWithSalt` v

instance (SupportedPrim a) => Eq (Sym a) where
  (Sym l) == (Sym r) = l == r

#define SEQ_SYM(type) \
instance (SupportedPrim type) => SEq (Sym Bool) (Sym type) where \
  (Sym l) ==~ (Sym r) = Sym $ pevalEqvTerm l r

#define SORD_SYM(type) \
instance (SupportedPrim type) => SOrd (Sym Bool) (Sym type) where \
  (Sym a) <=~ (Sym b) = Sym $ withPrim (Proxy @type) $ pevalLeNumTerm a b; \
  (Sym a) <~ (Sym b) = Sym $ withPrim (Proxy @type) $ pevalLtNumTerm a b; \
  (Sym a) >=~ (Sym b) = Sym $ withPrim (Proxy @type) $ pevalGeNumTerm a b; \
  (Sym a) >~ (Sym b) = Sym $ withPrim (Proxy @type) $ pevalGtNumTerm a b; \
  a `symCompare` b = \
    withPrim (Proxy @type) $ mrgIf \
      (a <~ b) \
      (mrgReturn LT) \
      (mrgIf (a ==~ b) (mrgReturn EQ) (mrgReturn GT))

SEQ_SYM (Bool)
SEQ_SYM (Integer)
SEQ_SYM ((IntN n))
SEQ_SYM ((WordN n))
SORD_SYM (Integer)
SORD_SYM ((IntN n))
SORD_SYM ((WordN n))

-- bool
type SymBool = Sym Bool

instance SOrd (Sym Bool) (Sym Bool) where
  l <=~ r = nots l ||~ r
  l <~ r = nots l &&~ r
  l >=~ r = l ||~ nots r
  l >~ r = l &&~ nots r
  symCompare l r =
    mrgIf
      (nots l &&~ r)
      (mrgReturn LT)
      (mrgIf (l ==~ r) (mrgReturn EQ) (mrgReturn GT))

instance LogicalOp (Sym Bool) where
  (Sym l) ||~ (Sym r) = Sym $ pevalOrTerm l r
  (Sym l) &&~ (Sym r) = Sym $ pevalAndTerm l r
  nots (Sym v) = Sym $ pevalNotTerm v
  (Sym l) `xors` (Sym r) = Sym $ pevalXorTerm l r
  (Sym l) `implies` (Sym r) = Sym $ pevalImplyTerm l r

instance SymBoolOp (Sym Bool)

-- integer
type SymInteger = Sym Integer

instance Num (Sym Integer) where
  (Sym l) + (Sym r) = Sym $ pevalAddNumTerm l r
  (Sym l) - (Sym r) = Sym $ pevalMinusNumTerm l r
  (Sym l) * (Sym r) = Sym $ pevalTimesNumTerm l r
  negate (Sym v) = Sym $ pevalUMinusNumTerm v
  abs (Sym v) = Sym $ pevalAbsNumTerm v
  signum (Sym v) = Sym $ pevalSignumNumTerm v
  fromInteger = conc

instance SignedDivMod (Sym Bool) (Sym Integer) where
  divs (Sym l) rs@(Sym r) =
    mrgIf @(Sym Bool)
      (rs ==~ conc 0)
      (throwError $ transformError DivideByZero)
      (mrgReturn $ Sym $ pevalDivIntegerTerm l r)
  mods (Sym l) rs@(Sym r) =
    mrgIf @(Sym Bool)
      (rs ==~ conc 0)
      (throwError $ transformError DivideByZero)
      (mrgReturn $ Sym $ pevalModIntegerTerm l r)

instance SymIntegerOp (Sym Bool) (Sym Integer)

-- signed bv
type SymIntN n = Sym (IntN n)

instance (SupportedPrim (IntN n)) => Num (Sym (IntN n)) where
  (Sym l) + (Sym r) = Sym $ withPrim (Proxy @(IntN n)) $ pevalAddNumTerm l r
  (Sym l) - (Sym r) = Sym $ withPrim (Proxy @(IntN n)) $ pevalMinusNumTerm l r
  (Sym l) * (Sym r) = Sym $ withPrim (Proxy @(IntN n)) $ pevalTimesNumTerm l r
  negate (Sym v) = Sym $ withPrim (Proxy @(IntN n)) $ pevalUMinusNumTerm v
  abs (Sym v) = Sym $ withPrim (Proxy @(IntN n)) $ pevalAbsNumTerm v
  signum (Sym v) = Sym $ withPrim (Proxy @(IntN n)) $ pevalSignumNumTerm v
  fromInteger i = withPrim (Proxy @(IntN n)) $ conc $ fromInteger i

instance (SupportedPrim (IntN n)) => Bits (Sym (IntN n)) where
  Sym l .&. Sym r = Sym $ withPrim (Proxy @(IntN n)) $ pevalAndBitsTerm l r
  Sym l .|. Sym r = Sym $ withPrim (Proxy @(IntN n)) $ pevalOrBitsTerm l r
  Sym l `xor` Sym r = Sym $ withPrim (Proxy @(IntN n)) $ pevalXorBitsTerm l r
  complement (Sym n) = Sym $ withPrim (Proxy @(IntN n)) $ pevalComplementBitsTerm n
  shift (Sym n) i = Sym $ withPrim (Proxy @(IntN n)) $ pevalShiftBitsTerm n i
  rotate (Sym n) i = Sym $ withPrim (Proxy @(IntN n)) $ pevalRotateBitsTerm n i
  bitSize _ = fromInteger $ withPrim (Proxy @(IntN n)) $ natVal (Proxy @n)
  bitSizeMaybe _ = Just $ fromInteger $ withPrim (Proxy @(IntN n)) $ natVal (Proxy @n)
  isSigned _ = True
  testBit (Conc n) = withPrim (Proxy @(IntN n)) $ testBit n
  testBit _ = error "You cannot call testBit on symbolic variables"
  bit = withPrim (Proxy @(IntN n)) $ conc . bit
  popCount (Conc n) = withPrim (Proxy @(IntN n)) $ popCount n
  popCount _ = error "You cannot call popCount on symbolic variables"

instance
  (KnownNat w', KnownNat n, KnownNat w, w' ~ (n + w), 1 <= n, 1 <= w, 1 <= w') =>
  BVConcat (Sym (IntN n)) (Sym (IntN w)) (Sym (IntN w'))
  where
  bvconcat (Sym l) (Sym r) = Sym (pevalBVConcatTerm l r)

instance
  ( KnownNat w,
    KnownNat w',
    1 <= w,
    1 <= w',
    w <= w',
    w + 1 <= w',
    1 <= w' - w,
    KnownNat (w' - w)
  ) =>
  BVExtend (Sym (IntN w)) w' (Sym (IntN w'))
  where
  bvzeroExtend _ (Sym v) = Sym $ pevalBVExtendTerm False (Proxy @w') v
  bvsignExtend _ (Sym v) = Sym $ pevalBVExtendTerm True (Proxy @w') v
  bvextend = bvsignExtend

instance
  ( KnownNat ix,
    KnownNat w,
    KnownNat ow,
    ix + w <= ow,
    1 <= ow,
    1 <= w
  ) =>
  BVSelect (Sym (IntN ow)) ix w (Sym (IntN w))
  where
  bvselect pix pw (Sym v) = Sym $ pevalBVSelectTerm pix pw v

#define TOSYM_MACHINE_INTEGER(int, bv) \
instance ToSym int (Sym (bv)) where \
  toSym = fromIntegral

#define TOCON_MACHINE_INTEGER(bvw, n, int) \
instance ToCon (Sym (bvw n)) int where \
  toCon (Conc (bvw v :: bvw n)) = Just $ fromIntegral v; \
  toCon _ = Nothing

TOSYM_MACHINE_INTEGER (Int8, IntN 8)
TOSYM_MACHINE_INTEGER (Int16, IntN 16)
TOSYM_MACHINE_INTEGER (Int32, IntN 32)
TOSYM_MACHINE_INTEGER (Int64, IntN 64)
TOSYM_MACHINE_INTEGER (Word8, WordN 8)
TOSYM_MACHINE_INTEGER (Word16, WordN 16)
TOSYM_MACHINE_INTEGER (Word32, WordN 32)
TOSYM_MACHINE_INTEGER (Word64, WordN 64)
TOSYM_MACHINE_INTEGER (Int, IntN $intBitwidthQ)
TOSYM_MACHINE_INTEGER (Word, WordN $intBitwidthQ)

TOCON_MACHINE_INTEGER (IntN, 8, Int8)
TOCON_MACHINE_INTEGER (IntN, 16, Int16)
TOCON_MACHINE_INTEGER (IntN, 32, Int32)
TOCON_MACHINE_INTEGER (IntN, 64, Int64)
TOCON_MACHINE_INTEGER (WordN, 8, Word8)
TOCON_MACHINE_INTEGER (WordN, 16, Word16)
TOCON_MACHINE_INTEGER (WordN, 32, Word32)
TOCON_MACHINE_INTEGER (WordN, 64, Word64)
TOCON_MACHINE_INTEGER (IntN, $intBitwidthQ, Int)
TOCON_MACHINE_INTEGER (WordN, $intBitwidthQ, Word)

-- unsigned bv
type SymWordN n = Sym (WordN n)

instance (SupportedPrim (WordN n)) => Num (Sym (WordN n)) where
  (Sym l) + (Sym r) = Sym $ withPrim (Proxy @(WordN n)) $ pevalAddNumTerm l r
  (Sym l) - (Sym r) = Sym $ withPrim (Proxy @(WordN n)) $ pevalMinusNumTerm l r
  (Sym l) * (Sym r) = Sym $ withPrim (Proxy @(WordN n)) $ pevalTimesNumTerm l r
  negate (Sym v) = Sym $ withPrim (Proxy @(WordN n)) $ pevalUMinusNumTerm v
  abs (Sym v) = Sym $ withPrim (Proxy @(WordN n)) $ pevalAbsNumTerm v
  signum (Sym v) = Sym $ withPrim (Proxy @(WordN n)) $ pevalSignumNumTerm v
  fromInteger i = withPrim (Proxy @(WordN n)) $ conc $ fromInteger i

instance
  (KnownNat w', KnownNat n, KnownNat w, w' ~ (n + w), 1 <= n, 1 <= w, 1 <= w') =>
  BVConcat (Sym (WordN n)) (Sym (WordN w)) (Sym (WordN w'))
  where
  bvconcat (Sym l) (Sym r) = Sym (pevalBVConcatTerm l r)

instance
  ( KnownNat w,
    KnownNat w',
    1 <= w,
    1 <= w',
    w + 1 <= w',
    w <= w',
    1 <= w' - w,
    KnownNat (w' - w)
  ) =>
  BVExtend (Sym (WordN w)) w' (Sym (WordN w'))
  where
  bvzeroExtend _ (Sym v) = Sym $ pevalBVExtendTerm False (Proxy @w') v
  bvsignExtend _ (Sym v) = Sym $ pevalBVExtendTerm True (Proxy @w') v
  bvextend = bvzeroExtend

instance
  ( KnownNat ix,
    KnownNat w,
    KnownNat ow,
    ix + w <= ow,
    1 <= ow,
    1 <= w
  ) =>
  BVSelect (Sym (WordN ow)) ix w (Sym (WordN w))
  where
  bvselect pix pw (Sym v) = Sym $ pevalBVSelectTerm pix pw v

instance (SupportedPrim (WordN n)) => Bits (Sym (WordN n)) where
  Sym l .&. Sym r = Sym $ withPrim (Proxy @(WordN n)) $ pevalAndBitsTerm l r
  Sym l .|. Sym r = Sym $ withPrim (Proxy @(WordN n)) $ pevalOrBitsTerm l r
  Sym l `xor` Sym r = Sym $ withPrim (Proxy @(WordN n)) $ pevalXorBitsTerm l r
  complement (Sym n) = Sym $ withPrim (Proxy @(WordN n)) $ pevalComplementBitsTerm n
  shift (Sym n) i = Sym $ withPrim (Proxy @(WordN n)) $ pevalShiftBitsTerm n i
  rotate (Sym n) i = Sym $ withPrim (Proxy @(WordN n)) $ pevalRotateBitsTerm n i
  bitSize _ = fromInteger $ withPrim (Proxy @(WordN n)) $ natVal (Proxy @n)
  bitSizeMaybe _ = Just $ fromInteger $ withPrim (Proxy @(WordN n)) $ natVal (Proxy @n)
  isSigned _ = False
  testBit (Conc n) = withPrim (Proxy @(WordN n)) $ testBit n
  testBit _ = error "You cannot call testBit on symbolic variables"
  bit = withPrim (Proxy @(WordN n)) $ conc . bit
  popCount (Conc n) = withPrim (Proxy @(WordN n)) $ popCount n
  popCount _ = error "You cannot call popCount on symbolic variables"

-- tabular func
type a =~> b = Sym (a =-> b)

infixr 0 =~>

instance (SupportedPrim a, SupportedPrim b) => Function (a =~> b) where
  type Arg (a =~> b) = Sym a
  type Ret (a =~> b) = Sym b
  (Sym f) # (Sym t) = Sym $ pevalTabularFuncApplyTerm f t

-- general func
type a -~> b = Sym (a --> b)

infixr 0 -~>

instance (SupportedPrim a, SupportedPrim b) => Function (a -~> b) where
  type Arg (a -~> b) = Sym a
  type Ret (a -~> b) = Sym b
  (Sym f) # (Sym t) = Sym $ pevalGeneralFuncApplyTerm f t

symsSize :: [Sym a] -> Int
symsSize = termsSize . fmap underlyingTerm

symSize :: Sym a -> Int
symSize = termSize . underlyingTerm
