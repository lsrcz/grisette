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

module Grisette.IR.SymPrim.Data.SymPrim
  ( Sym (..),
    SymBool,
    SymInteger,
    type (=~>),
    type (-~>),
    SymWordN,
    SymIntN,
    symSize,
    symsSize,
    ModelSymPair (..),
  )
where

import Control.DeepSeq
import Control.Monad.Except
import Data.Bits
import Data.Hashable
import Data.Int
import Data.Proxy
import Data.String
import Data.Word
import GHC.Generics
import GHC.TypeLits
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
import Grisette.Core.Data.Class.PrimWrapper
import Grisette.Core.Data.Class.SOrd
import Grisette.Core.Data.Class.SimpleMergeable
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
import Grisette.IR.SymPrim.Data.Prim.PartialEval.GeneralFunc
import Grisette.IR.SymPrim.Data.Prim.PartialEval.Integer
import Grisette.IR.SymPrim.Data.Prim.PartialEval.Num
import Grisette.IR.SymPrim.Data.Prim.PartialEval.TabularFunc
import Grisette.IR.SymPrim.Data.TabularFunc
import Grisette.Lib.Control.Monad
import Language.Haskell.TH.Syntax

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

instance (SupportedPrim a) => GMergeable (Sym Bool) (Sym a) where
  gmergingStrategy = SimpleStrategy ites

instance (SupportedPrim a) => GSimpleMergeable (Sym Bool) (Sym a) where
  gmrgIte = ites

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

instance (SupportedPrim a) => GEvaluateSym Model (Sym a) where
  gevaluateSym fillDefault model (Sym t) = Sym $ evaluateTerm fillDefault model t

instance (SupportedPrim a) => GExtractSymbolics SymbolSet (Sym a) where
  gextractSymbolics (Sym t) = SymbolSet $ extractSymbolicsTerm t

instance (SymBoolOp (Sym Bool), SupportedPrim a) => GGenSym (Sym Bool) () (Sym a) where
  ggenSymFresh _ = mrgReturn <$> genSymSimpleFresh ()

instance (SymBoolOp (Sym Bool), SupportedPrim a) => GenSymSimple () (Sym a) where
  genSymSimpleFresh _ = do
    ident <- getGenSymIdent
    GenSymIndex i <- nextGenSymIndex
    case ident of
      GenSymIdent s -> return $ isymb s i
      GenSymIdentWithInfo s info -> return $ iinfosymb s i info

instance (SymBoolOp (Sym Bool), SupportedPrim a) => GGenSym (Sym Bool) (Sym a) (Sym a)

instance (SymBoolOp (Sym Bool), SupportedPrim a) => GenSymSimple (Sym a) (Sym a) where
  genSymSimpleFresh _ = genSymSimpleFresh ()

instance (SupportedPrim a) => Show (Sym a) where
  show (Sym t) = pformat t

instance (SupportedPrim a) => Hashable (Sym a) where
  hashWithSalt s (Sym v) = s `hashWithSalt` v

instance (SupportedPrim a) => Eq (Sym a) where
  (Sym l) == (Sym r) = l == r

#define SEQ_SYM(type) \
instance (SupportedPrim type) => GSEq (Sym Bool) (Sym type) where \
  (Sym l) `gsymeq` (Sym r) = Sym $ pevalEqvTerm l r

#define SORD_SYM(type) \
instance (SupportedPrim type) => GSOrd (Sym Bool) (Sym type) where \
  (Sym a) `gsymle` (Sym b) = Sym $ withPrim (Proxy @type) $ pevalLeNumTerm a b; \
  (Sym a) `gsymlt` (Sym b) = Sym $ withPrim (Proxy @type) $ pevalLtNumTerm a b; \
  (Sym a) `gsymge` (Sym b) = Sym $ withPrim (Proxy @type) $ pevalGeNumTerm a b; \
  (Sym a) `gsymgt` (Sym b) = Sym $ withPrim (Proxy @type) $ pevalGtNumTerm a b; \
  a `gsymCompare` b = \
    withPrim (Proxy @type) $ mrgIf \
      (a `gsymlt` b) \
      (mrgReturn LT) \
      (mrgIf (a `gsymeq` b) (mrgReturn EQ) (mrgReturn GT))

SEQ_SYM (Bool)
SEQ_SYM (Integer)
SEQ_SYM ((IntN n))
SEQ_SYM ((WordN n))
SORD_SYM (Integer)
SORD_SYM ((IntN n))
SORD_SYM ((WordN n))

-- bool
type SymBool = Sym Bool

instance GSOrd (Sym Bool) (Sym Bool) where
  l `gsymle` r = nots l ||~ r
  l `gsymlt` r = nots l &&~ r
  l `gsymge` r = l ||~ nots r
  l `gsymgt` r = l &&~ nots r
  gsymCompare l r =
    mrgIf
      (nots l &&~ r)
      (mrgReturn LT)
      (mrgIf (l `gsymeq` r) (mrgReturn EQ) (mrgReturn GT))

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
      (rs `gsymeq` conc 0)
      (throwError $ transformError DivideByZero)
      (mrgReturn $ Sym $ pevalDivIntegerTerm l r)
  mods (Sym l) rs@(Sym r) =
    mrgIf @(Sym Bool)
      (rs `gsymeq` conc 0)
      (throwError $ transformError DivideByZero)
      (mrgReturn $ Sym $ pevalModIntegerTerm l r)

instance GSymIntegerOp (Sym Bool) (Sym Integer)

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

data ModelSymPair t = (Sym t) := t deriving (Show)

instance ModelRep (ModelSymPair t) Model SymbolSet TypedSymbol where
  buildModel (Sym (SymbTerm _ sym) := val) = insertValue sym val emptyModel
  buildModel _ = error "buildModel: should only use symbolic constants"

instance
  ModelRep
    ( ModelSymPair a,
      ModelSymPair b
    )
    Model
    SymbolSet
    TypedSymbol
  where
  buildModel
    ( Sym (SymbTerm _ sym1) := val1,
      Sym (SymbTerm _ sym2) := val2
      ) =
      insertValue sym1 val1
        . insertValue sym2 val2
        $ emptyModel
  buildModel _ = error "buildModel: should only use symbolic constants"

instance
  ModelRep
    ( ModelSymPair a,
      ModelSymPair b,
      ModelSymPair c
    )
    Model
    SymbolSet
    TypedSymbol
  where
  buildModel
    ( Sym (SymbTerm _ sym1) := val1,
      Sym (SymbTerm _ sym2) := val2,
      Sym (SymbTerm _ sym3) := val3
      ) =
      insertValue sym1 val1
        . insertValue sym2 val2
        . insertValue sym3 val3
        $ emptyModel
  buildModel _ = error "buildModel: should only use symbolic constants"

instance
  ModelRep
    ( ModelSymPair a,
      ModelSymPair b,
      ModelSymPair c,
      ModelSymPair d
    )
    Model
    SymbolSet
    TypedSymbol
  where
  buildModel
    ( Sym (SymbTerm _ sym1) := val1,
      Sym (SymbTerm _ sym2) := val2,
      Sym (SymbTerm _ sym3) := val3,
      Sym (SymbTerm _ sym4) := val4
      ) =
      insertValue sym1 val1
        . insertValue sym2 val2
        . insertValue sym3 val3
        . insertValue sym4 val4
        $ emptyModel
  buildModel _ = error "buildModel: should only use symbolic constants"

instance
  ModelRep
    ( ModelSymPair a,
      ModelSymPair b,
      ModelSymPair c,
      ModelSymPair d,
      ModelSymPair e
    )
    Model
    SymbolSet
    TypedSymbol
  where
  buildModel
    ( Sym (SymbTerm _ sym1) := val1,
      Sym (SymbTerm _ sym2) := val2,
      Sym (SymbTerm _ sym3) := val3,
      Sym (SymbTerm _ sym4) := val4,
      Sym (SymbTerm _ sym5) := val5
      ) =
      insertValue sym1 val1
        . insertValue sym2 val2
        . insertValue sym3 val3
        . insertValue sym4 val4
        . insertValue sym5 val5
        $ emptyModel
  buildModel _ = error "buildModel: should only use symbolic constants"

instance
  ModelRep
    ( ModelSymPair a,
      ModelSymPair b,
      ModelSymPair c,
      ModelSymPair d,
      ModelSymPair e,
      ModelSymPair f
    )
    Model
    SymbolSet
    TypedSymbol
  where
  buildModel
    ( Sym (SymbTerm _ sym1) := val1,
      Sym (SymbTerm _ sym2) := val2,
      Sym (SymbTerm _ sym3) := val3,
      Sym (SymbTerm _ sym4) := val4,
      Sym (SymbTerm _ sym5) := val5,
      Sym (SymbTerm _ sym6) := val6
      ) =
      insertValue sym1 val1
        . insertValue sym2 val2
        . insertValue sym3 val3
        . insertValue sym4 val4
        . insertValue sym5 val5
        . insertValue sym6 val6
        $ emptyModel
  buildModel _ = error "buildModel: should only use symbolic constants"

instance
  ModelRep
    ( ModelSymPair a,
      ModelSymPair b,
      ModelSymPair c,
      ModelSymPair d,
      ModelSymPair e,
      ModelSymPair f,
      ModelSymPair g
    )
    Model
    SymbolSet
    TypedSymbol
  where
  buildModel
    ( Sym (SymbTerm _ sym1) := val1,
      Sym (SymbTerm _ sym2) := val2,
      Sym (SymbTerm _ sym3) := val3,
      Sym (SymbTerm _ sym4) := val4,
      Sym (SymbTerm _ sym5) := val5,
      Sym (SymbTerm _ sym6) := val6,
      Sym (SymbTerm _ sym7) := val7
      ) =
      insertValue sym1 val1
        . insertValue sym2 val2
        . insertValue sym3 val3
        . insertValue sym4 val4
        . insertValue sym5 val5
        . insertValue sym6 val6
        . insertValue sym7 val7
        $ emptyModel
  buildModel _ = error "buildModel: should only use symbolic constants"

instance
  ModelRep
    ( ModelSymPair a,
      ModelSymPair b,
      ModelSymPair c,
      ModelSymPair d,
      ModelSymPair e,
      ModelSymPair f,
      ModelSymPair g,
      ModelSymPair h
    )
    Model
    SymbolSet
    TypedSymbol
  where
  buildModel
    ( Sym (SymbTerm _ sym1) := val1,
      Sym (SymbTerm _ sym2) := val2,
      Sym (SymbTerm _ sym3) := val3,
      Sym (SymbTerm _ sym4) := val4,
      Sym (SymbTerm _ sym5) := val5,
      Sym (SymbTerm _ sym6) := val6,
      Sym (SymbTerm _ sym7) := val7,
      Sym (SymbTerm _ sym8) := val8
      ) =
      insertValue sym1 val1
        . insertValue sym2 val2
        . insertValue sym3 val3
        . insertValue sym4 val4
        . insertValue sym5 val5
        . insertValue sym6 val6
        . insertValue sym7 val7
        . insertValue sym8 val8
        $ emptyModel
  buildModel _ = error "buildModel: should only use symbolic constants"

instance GSubstituteSym TypedSymbol Sym (Sym a) where
  gsubstituteSym sym (Sym val) (Sym x) =
    introSupportedPrimConstraint val $
      introSupportedPrimConstraint x $
        Sym $
          substTerm sym val x

instance GSubstituteSymSymbol TypedSymbol Sym
