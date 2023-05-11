{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# HLINT ignore "Use ==" #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Grisette.IR.SymPrim.Data.DynSymBVTests {-(dynBVTests)-} where

import Control.Monad
import Data.Bits
import Data.Int
import Data.Proxy
import Data.Word
import GHC.TypeNats
import Grisette.Core.Data.BV
import Grisette.Core.Data.Class.BitVector
import Grisette.Core.Data.DynBV
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck hiding ((.&.))
import Control.DeepSeq
import Control.Exception
import Grisette.IR.SymPrim.Data.SymPrim
import Grisette.Core.Data.Class.GenSym

class
  ( SizedBV bv,
    Num (bv 8),
    Num (bv 32),
    Num (bv 64),
    Num dbv,
    Show dbv,
    Bits dbv,
    Ord dbv,
    Real dbv,
    Integral dbv,
    NFData dbv,
    DynBV dbv,
    DynBVLink dbv bv sbv
  ) =>
  DynBVAssociate dbv bv sbv
    | dbv -> bv sbv

instance DynBVAssociate DynIntN IntN SomeIntN

instance DynBVAssociate DynWordN WordN SomeWordN

bv136UnaryIntProp ::
  (Int -> DynSymIntN -> SymIntN 136 -> p) ->
  p
bv136UnaryIntProp f = undefined
  where
    d :: DynSymIntN = genSymSimple (136 :: Int) "a"

{-
bv136UnaryIntProp ::
  forall dbv bv sbv bv8 bv32 bv64 p.
  (Testable p, DynBVAssociate dbv bv sbv bv8 bv32 bv64) =>
  (Int -> dbv -> bv 136 -> p) ->
  (Int, bv8, bv64, bv64) ->
  p
bv136UnaryIntProp f (v, x11, x12, x13) =
  let x1 =
        sizedBVConcat
          (sizedBVConcat (fromIntegral x11 :: bv 8) (fromIntegral x12 :: bv 64))
          (fromIntegral x13 :: bv 64)
   in f v (sizedBVToDynBV x1) x1
   -}

{-
bv192UnaryIntProp ::
  forall dbv bv sbv bv8 bv32 bv64 p.
  (Testable p, DynBVAssociate dbv bv sbv bv8 bv32 bv64) =>
  (Int -> dbv -> bv 192 -> p) ->
  (Int, bv64, bv64, bv64) ->
  p
bv192UnaryIntProp f (v, x11, x12, x13) =
  let x1 =
        sizedBVConcat
          (sizedBVConcat (fromIntegral x11 :: bv 64) (fromIntegral x12 :: bv 64))
          (fromIntegral x13 :: bv 64)
   in f v (sizedBVToDynBV x1) x1

bv136UnaryProp ::
  forall dbv bv sbv bv8 bv32 bv64 p.
  (Testable p, DynBVAssociate dbv bv sbv bv8 bv32 bv64) =>
  (dbv -> bv 136 -> p) ->
  (bv8, bv64, bv64) ->
  p
bv136UnaryProp f (x11, x12, x13) =
  let x1 =
        sizedBVConcat
          (sizedBVConcat (fromIntegral x11 :: bv 8) (fromIntegral x12 :: bv 64))
          (fromIntegral x13 :: bv 64)
   in f (sizedBVToDynBV x1) x1

bv192UnaryProp ::
  forall dbv bv sbv bv8 bv32 bv64 p.
  (Testable p, DynBVAssociate dbv bv sbv bv8 bv32 bv64) =>
  (dbv -> bv 192 -> p) ->
  (bv64, bv64, bv64) ->
  p
bv192UnaryProp f (x11, x12, x13) =
  let x1 =
        sizedBVConcat
          (sizedBVConcat (fromIntegral x11 :: bv 64) (fromIntegral x12 :: bv 64))
          (fromIntegral x13 :: bv 64)
   in f (sizedBVToDynBV x1) x1

bv136BinProp ::
  forall dbv bv sbv bv8 bv32 bv64 p.
  (Testable p, DynBVAssociate dbv bv sbv bv8 bv32 bv64) =>
  (dbv -> dbv -> bv 136 -> bv 136 -> p) ->
  (bv8, bv64, bv64) ->
  (bv8, bv64, bv64) ->
  p
bv136BinProp f (x11, x12, x13) (x21, x22, x23) =
  let x1 =
        sizedBVConcat
          (sizedBVConcat (fromIntegral x11 :: bv 8) (fromIntegral x12 :: bv 64))
          (fromIntegral x13 :: bv 64)
      x2 =
        sizedBVConcat
          (sizedBVConcat (fromIntegral x21 :: bv 8) (fromIntegral x22 :: bv 64))
          (fromIntegral x23 :: bv 64)
   in f (sizedBVToDynBV x1) (sizedBVToDynBV x2) x1 x2

bv192BinProp ::
  forall dbv bv sbv bv8 bv32 bv64 p.
  (Testable p, DynBVAssociate dbv bv sbv bv8 bv32 bv64) =>
  (dbv -> dbv -> bv 192 -> bv 192 -> p) ->
  (bv64, bv64, bv64) ->
  (bv64, bv64, bv64) ->
  p
bv192BinProp f (x11, x12, x13) (x21, x22, x23) =
  let x1 =
        sizedBVConcat
          (sizedBVConcat (fromIntegral x11 :: bv 64) (fromIntegral x12 :: bv 64))
          (fromIntegral x13 :: bv 64)
      x2 =
        sizedBVConcat
          (sizedBVConcat (fromIntegral x21 :: bv 64) (fromIntegral x22 :: bv 64))
          (fromIntegral x23 :: bv 64)
   in f (sizedBVToDynBV x1) (sizedBVToDynBV x2) x1 x2

cmpOpTest ::
  forall p dbv bv sbv bv8 bv32 bv64.
  (DynBVAssociate dbv bv sbv bv8 bv32 bv64, Ord (bv 136), Ord (bv 192)) =>
  p dbv ->
  String ->
  Bool ->
  (forall x. (Ord x) => x -> x -> Bool) ->
  TestTree
cmpOpTest _ name reflexive op =
  testGroup
    name
    [ testGroup
        "136"
        [ testProperty "arbitrary" $
            bv136BinProp @dbv $
              \x1 x2 x1n x2n -> ioProperty $ op x1 x2 @=? op x1n x2n,
          testProperty "same" $
            bv136UnaryProp @dbv $
              \x1 _ -> ioProperty $ op x1 x1 @=? reflexive
        ],
      testGroup
        "192"
        [ testProperty "arbitrary" $
            bv192BinProp @dbv $
              \x1 x2 x1n x2n -> ioProperty $ op x1 x2 @=? op x1n x2n,
          testProperty "same" $
            bv192UnaryProp @dbv $
              \x1 _ -> ioProperty $ op x1 x1 @=? reflexive
        ]
    ]

binOpTest ::
  forall p dbv bv sbv bv8 bv32 bv64.
  (DynBVAssociate dbv bv sbv bv8 bv32 bv64, Bits (bv 136), Bits (bv 192), Num (bv 136), Num (bv 192),
  Integral (bv 136), Integral (bv 192)) =>
  p dbv ->
  String ->
  (forall x. (Bits x, Num x, Integral x) => x -> x -> x) ->
  TestTree
binOpTest _ name op =
  testGroup
    name
    [ testGroup
        "136"
        [ testProperty "arbitrary" $
            bv136BinProp @dbv $
              \x1 x2 x1n x2n -> ioProperty $ op x1 x2 @=? sizedBVToDynBV (op x1n x2n)
        ],
      testGroup
        "192"
        [ testProperty "arbitrary" $
            bv192BinProp @dbv $
              \x1 x2 x1n x2n -> ioProperty $ do
                op x1 x2 @=? sizedBVToDynBV (op x1n x2n)
        ]
    ]

newtype AEWrapper = AEWrapper ArithException deriving (Eq)

instance Show AEWrapper where
  show (AEWrapper x) = show x

instance NFData AEWrapper where
  rnf (AEWrapper x) = x `seq` ()

throwableBinOpTest ::
  forall p e dbv bv sbv bv8 bv32 bv64.
  (DynBVAssociate dbv bv sbv bv8 bv32 bv64, Bits (bv 136), Bits (bv 192), Num (bv 136), Num (bv 192),
  Integral (bv 136), Integral (bv 192)) =>
  p dbv ->
  String ->
  (forall x. (Bits x, Num x, Integral x) => x -> x -> x) ->
  TestTree
throwableBinOpTest _ name op =
  testGroup
    name
    [ testGroup
        "136"
        [ testProperty "arbitrary" $
            bv136BinProp @dbv $
              \x1 x2 x1n x2n -> ioProperty $ do
                xa <- evaluate (force $ Right $ op x1 x2) `catch` \(e :: ArithException) -> return $ Left $ AEWrapper e
                xb <- evaluate (force $ Right $ sizedBVToDynBV (op x1n x2n)) `catch` \(e :: ArithException) -> return $ Left $ AEWrapper e
                xa @=? xb
        ],
      testGroup
        "192"
        [ testProperty "arbitrary" $
            bv192BinProp @dbv $
              \x1 x2 x1n x2n -> ioProperty $ do
                xa <- evaluate (force $ Right $ op x1 x2) `catch` \(e :: ArithException) -> return $ Left $ AEWrapper e
                xb <- evaluate (force $ Right $ sizedBVToDynBV (op x1n x2n)) `catch` \(e :: ArithException) -> return $ Left $ AEWrapper e
                xa @=? xb
        ]
    ]

unaryOpToDirectlyComparableTest ::
  forall p dbv bv sbv bv8 bv32 bv64 r.
  ( DynBVAssociate dbv bv sbv bv8 bv32 bv64,
    Bits (bv 136),
    Bits (bv 192),
    Num (bv 136),
    Num (bv 192),
    Real (bv 136),
    Real (bv 192),
    Integral (bv 136),
    Integral (bv 192),
    Eq r,
    Show r
  ) =>
  p dbv ->
  String ->
  (forall x. (Bits x, Num x, Real x, Integral x) => x -> r) ->
  TestTree
unaryOpToDirectlyComparableTest _ name op =
  testGroup
    name
    [ testGroup
        "136"
        [ testProperty "arbitrary" $
            bv136UnaryProp @dbv $
              \x1 x1n -> ioProperty $ op x1 @=? op x1n
        ],
      testGroup
        "192"
        [ testProperty "arbitrary" $
            bv192UnaryProp @dbv $
              \x1 x1n -> ioProperty $ op x1 @=? op x1n
        ]
    ]

unaryOpTest ::
  forall p dbv bv sbv bv8 bv32 bv64.
  ( DynBVAssociate dbv bv sbv bv8 bv32 bv64,
    Bits (bv 136),
    Bits (bv 192),
    Num (bv 136),
    Num (bv 192)
  ) =>
  p dbv ->
  String ->
  (forall x. (Bits x, Num x) => x -> x) ->
  TestTree
unaryOpTest _ name op =
  testGroup
    name
    [ testGroup
        "136"
        [ testProperty "arbitrary" $
            bv136UnaryProp @dbv $
              \x1 x1n -> ioProperty $ op x1 @=? sizedBVToDynBV (op x1n)
        ],
      testGroup
        "192"
        [ testProperty "arbitrary" $
            bv192UnaryProp @dbv $
              \x1 x1n -> ioProperty $ op x1 @=? sizedBVToDynBV (op x1n)
        ]
    ]

unaryOpIntTest ::
  forall p dbv bv sbv bv8 bv32 bv64.
  (DynBVAssociate dbv bv sbv bv8 bv32 bv64, Bits (bv 136), Bits (bv 192)) =>
  p dbv ->
  String ->
  (forall x. (Bits x) => x -> Int -> x) ->
  (Int -> Bool) ->
  TestTree
unaryOpIntTest _ name op pred =
  testGroup
    name
    [ testGroup
        "136"
        [ testProperty "arbitrary" $
            bv136UnaryIntProp @dbv $
              \v x1 x1n -> ioProperty $ when (pred v) $ op x1 v @=? sizedBVToDynBV (op x1n v)
        ],
      testGroup
        "192"
        [ testProperty "arbitrary" $
            bv192UnaryIntProp @dbv $
              \v x1 x1n -> ioProperty $ when (pred v) $ op x1 v @=? sizedBVToDynBV (op x1n v)
        ]
    ]

unaryOpIntToDirectlyComparableTest ::
  forall p dbv bv sbv bv8 bv32 bv64 r.
  (DynBVAssociate dbv bv sbv bv8 bv32 bv64, Bits (bv 136), Bits (bv 192), Eq r, Show r) =>
  p dbv ->
  String ->
  (forall x. (Bits x) => x -> Int -> r) ->
  (Int -> Bool) ->
  TestTree
unaryOpIntToDirectlyComparableTest _ name op pred =
  testGroup
    name
    [ testGroup
        "136"
        [ testProperty "arbitrary" $
            bv136UnaryIntProp @dbv $
              \v x1 x1n -> ioProperty $ when (pred v) $ op x1 v @=? op x1n v
        ],
      testGroup
        "192"
        [ testProperty "arbitrary" $
            bv192UnaryIntProp @dbv $
              \v x1 x1n -> ioProperty $ when (pred v) $ op x1 v @=? op x1n v
        ]
    ]

dynIntNProxy :: Proxy DynIntN
dynIntNProxy = Proxy

dynWordNProxy :: Proxy DynWordN
dynWordNProxy = Proxy

eqTests ::
  forall p dbv bv sbv bv8 bv32 bv64.
  (DynBVAssociate dbv bv sbv bv8 bv32 bv64, Ord (bv 136), Ord (bv 192)) =>
  p dbv ->
  TestTree
eqTests p =
  testGroup
    "Eq"
    [ cmpOpTest p "==" True (==),
      cmpOpTest p "/=" False (/=)
    ]

ordTests ::
  forall p dbv bv sbv bv8 bv32 bv64.
  (DynBVAssociate dbv bv sbv bv8 bv32 bv64, Ord (bv 136), Ord (bv 192)) =>
  p dbv ->
  TestTree
ordTests p =
  testGroup
    "Ord"
    [ cmpOpTest p "<=" True (<=),
      cmpOpTest p "<" False (<),
      cmpOpTest p ">=" True (>=),
      cmpOpTest p ">" False (>)
    ]

bitsTests ::
  forall p dbv bv sbv bv8 bv32 bv64.
  ( DynBVAssociate dbv bv sbv bv8 bv32 bv64,
    Bits (bv 136),
    Bits (bv 192),
    Integral (bv 136),
    Integral (bv 192),
    Num (bv 136),
    Num (bv 192)
  ) =>
  p dbv ->
  Bool ->
  TestTree
bitsTests p s =
  testGroup
    "Bits"
    [ binOpTest p ".&." (.&.),
      binOpTest p ".|." (.|.),
      binOpTest p "xor" xor,
      binOpTest p "+" (+),
      unaryOpTest p "complement" complement,
      unaryOpIntTest p "setBit" setBit (>= 0),
      unaryOpIntTest p "clearBit" clearBit (>= 0),
      unaryOpIntTest p "complementBit" complementBit (>= 0),
      unaryOpIntToDirectlyComparableTest p "testBit" testBit (>= 0),
      testCase "bitSizeMaybe" $ do
        bitSizeMaybe (sizedBVToDynBV (0 :: bv 136)) @=? Just 136
        bitSizeMaybe (sizedBVToDynBV (0 :: bv 192)) @=? Just 192,
      testCase "isSigned" $ do
        isSigned (sizedBVToDynBV (0 :: bv 136)) @=? s,
      unaryOpIntTest p "shiftL" shiftL (>= 0),
      unaryOpIntTest p "shiftR" shiftR (>= 0),
      unaryOpIntTest p "shift" shift (const True),
      unaryOpIntTest p "rotateL" rotateL (>= 0),
      unaryOpIntTest p "rotateR" rotateR (>= 0),
      unaryOpIntTest p "rotate" rotate (const True),
      unaryOpIntToDirectlyComparableTest p "popCount" (\x _ -> popCount x) (const True)
    ]

numInstanceTests ::
  forall p dbv bv sbv bv8 bv32 bv64.
  ( DynBVAssociate dbv bv sbv bv8 bv32 bv64,
    Bits (bv 136),
    Bits (bv 192),
    Integral (bv 136),
    Integral (bv 192),
    Num (bv 136),
    Num (bv 192)
  ) =>
  p dbv ->
  TestTree
numInstanceTests p =
  testGroup
    "Num"
    [ binOpTest p "+" (+),
      binOpTest p "-" (-),
      binOpTest p "*" (*),
      unaryOpTest p "negate" negate,
      unaryOpTest p "signum" signum,
      unaryOpTest p "abs" abs
    ]

dynBVInstanceTests ::
  forall p dbv bv sbv bv8 bv32 bv64.
  ( DynBVAssociate dbv bv sbv bv8 bv32 bv64,
    Bits (bv 136),
    Bits (bv 192),
    Num (bv 136),
    Num (bv 192)
  ) =>
  p dbv ->
  TestTree
dynBVInstanceTests p =
  testGroup
    "DynBV"
    [ testProperty "concat" $
        \(a1 :: bv8, a2 :: bv32, a3 :: bv64) (b1 :: bv8, b2 :: bv32, b3 :: bv64) ->
          ioProperty $ do
            let a :: bv 104 =
                  sizedBVConcat
                    (fromIntegral a1 :: bv 8)
                    ( sizedBVConcat
                        (fromIntegral a2 :: bv 32)
                        (fromIntegral a3 :: bv 64)
                    )
            let b :: bv 104 =
                  sizedBVConcat
                    (fromIntegral b1 :: bv 8)
                    ( sizedBVConcat
                        (fromIntegral b2 :: bv 32)
                        (fromIntegral b3 :: bv 64)
                    )
            let ad :: dbv = sizedBVToDynBV a
            let bd :: dbv = sizedBVToDynBV b
            dynBVConcat ad bd @=? sizedBVToDynBV (sizedBVConcat a b),
      testProperty "zext" $
        \(a1 :: bv32, a2 :: bv64) ->
          ioProperty $ do
            let a = sizedBVConcat (fromIntegral a1 :: bv 32) (fromIntegral a2 :: bv 64)
            let ad :: dbv = sizedBVToDynBV a
            dynBVZext 192 ad @=? sizedBVToDynBV (sizedBVZext (Proxy @192) a),
      testProperty "zext" $
        \(a1 :: bv32, a2 :: bv64) ->
          ioProperty $ do
            let a = sizedBVConcat (fromIntegral a1 :: bv 32) (fromIntegral a2 :: bv 64)
            let ad :: dbv = sizedBVToDynBV a
            dynBVSext 192 ad @=? sizedBVToDynBV (sizedBVSext (Proxy @192) a),
      testProperty "ext" $
        \(a1 :: bv32, a2 :: bv64) ->
          ioProperty $ do
            let a = sizedBVConcat (fromIntegral a1 :: bv 32) (fromIntegral a2 :: bv 64)
            let ad :: dbv = sizedBVToDynBV a
            dynBVExt 192 ad @=? sizedBVToDynBV (sizedBVExt (Proxy @192) a),
      testProperty "select" $
        \(a1 :: bv32, a2 :: bv64, a3 :: bv64) ->
          ioProperty $ do
            let a =
                  sizedBVConcat
                    (sizedBVConcat (fromIntegral a1 :: bv 32) (fromIntegral a2 :: bv 64))
                    (fromIntegral a3 :: bv 64)
            let ad :: dbv = sizedBVToDynBV a
            dynBVSelect 0 1 ad @=? sizedBVToDynBV (sizedBVSelect (Proxy @0) (Proxy @1) a)
            dynBVSelect 0 8 ad @=? sizedBVToDynBV (sizedBVSelect (Proxy @0) (Proxy @8) a)
            dynBVSelect 0 64 ad @=? sizedBVToDynBV (sizedBVSelect (Proxy @0) (Proxy @64) a)
            dynBVSelect 0 72 ad @=? sizedBVToDynBV (sizedBVSelect (Proxy @0) (Proxy @72) a)
            dynBVSelect 0 128 ad @=? sizedBVToDynBV (sizedBVSelect (Proxy @0) (Proxy @128) a)
            dynBVSelect 0 129 ad @=? sizedBVToDynBV (sizedBVSelect (Proxy @0) (Proxy @129) a)
            dynBVSelect 0 159 ad @=? sizedBVToDynBV (sizedBVSelect (Proxy @0) (Proxy @159) a)
            dynBVSelect 0 160 ad @=? sizedBVToDynBV (sizedBVSelect (Proxy @0) (Proxy @160) a)
            dynBVSelect 8 1 ad @=? sizedBVToDynBV (sizedBVSelect (Proxy @8) (Proxy @1) a)
            dynBVSelect 8 8 ad @=? sizedBVToDynBV (sizedBVSelect (Proxy @8) (Proxy @8) a)
            dynBVSelect 8 56 ad @=? sizedBVToDynBV (sizedBVSelect (Proxy @8) (Proxy @56) a)
            dynBVSelect 8 72 ad @=? sizedBVToDynBV (sizedBVSelect (Proxy @8) (Proxy @72) a)
            dynBVSelect 8 120 ad @=? sizedBVToDynBV (sizedBVSelect (Proxy @8) (Proxy @120) a)
            dynBVSelect 8 129 ad @=? sizedBVToDynBV (sizedBVSelect (Proxy @8) (Proxy @129) a)
            dynBVSelect 8 152 ad @=? sizedBVToDynBV (sizedBVSelect (Proxy @8) (Proxy @152) a)
            dynBVSelect 56 1 ad @=? sizedBVToDynBV (sizedBVSelect (Proxy @56) (Proxy @1) a)
            dynBVSelect 56 8 ad @=? sizedBVToDynBV (sizedBVSelect (Proxy @56) (Proxy @8) a)
            dynBVSelect 56 64 ad @=? sizedBVToDynBV (sizedBVSelect (Proxy @56) (Proxy @64) a)
            dynBVSelect 56 72 ad @=? sizedBVToDynBV (sizedBVSelect (Proxy @56) (Proxy @72) a)
            dynBVSelect 56 80 ad @=? sizedBVToDynBV (sizedBVSelect (Proxy @56) (Proxy @80) a)
            dynBVSelect 56 104 ad @=? sizedBVToDynBV (sizedBVSelect (Proxy @56) (Proxy @104) a)
            dynBVSelect 64 1 ad @=? sizedBVToDynBV (sizedBVSelect (Proxy @64) (Proxy @1) a)
            dynBVSelect 64 8 ad @=? sizedBVToDynBV (sizedBVSelect (Proxy @64) (Proxy @8) a)
            dynBVSelect 64 56 ad @=? sizedBVToDynBV (sizedBVSelect (Proxy @64) (Proxy @56) a)
            dynBVSelect 64 64 ad @=? sizedBVToDynBV (sizedBVSelect (Proxy @64) (Proxy @64) a)
            dynBVSelect 64 72 ad @=? sizedBVToDynBV (sizedBVSelect (Proxy @64) (Proxy @72) a)
            dynBVSelect 64 80 ad @=? sizedBVToDynBV (sizedBVSelect (Proxy @64) (Proxy @80) a)
            dynBVSelect 64 96 ad @=? sizedBVToDynBV (sizedBVSelect (Proxy @64) (Proxy @96) a)
            dynBVSelect 72 1 ad @=? sizedBVToDynBV (sizedBVSelect (Proxy @72) (Proxy @1) a)
            dynBVSelect 72 8 ad @=? sizedBVToDynBV (sizedBVSelect (Proxy @72) (Proxy @8) a)
            dynBVSelect 72 48 ad @=? sizedBVToDynBV (sizedBVSelect (Proxy @72) (Proxy @48) a)
            dynBVSelect 72 56 ad @=? sizedBVToDynBV (sizedBVSelect (Proxy @72) (Proxy @56) a)
            dynBVSelect 72 64 ad @=? sizedBVToDynBV (sizedBVSelect (Proxy @72) (Proxy @64) a)
            dynBVSelect 72 80 ad @=? sizedBVToDynBV (sizedBVSelect (Proxy @72) (Proxy @80) a)
            dynBVSelect 72 88 ad @=? sizedBVToDynBV (sizedBVSelect (Proxy @72) (Proxy @88) a)
            dynBVSelect 120 1 ad @=? sizedBVToDynBV (sizedBVSelect (Proxy @120) (Proxy @1) a)
            dynBVSelect 120 8 ad @=? sizedBVToDynBV (sizedBVSelect (Proxy @120) (Proxy @8) a)
            dynBVSelect 120 16 ad @=? sizedBVToDynBV (sizedBVSelect (Proxy @120) (Proxy @16) a)
            dynBVSelect 120 40 ad @=? sizedBVToDynBV (sizedBVSelect (Proxy @120) (Proxy @40) a)
            dynBVSelect 128 1 ad @=? sizedBVToDynBV (sizedBVSelect (Proxy @128) (Proxy @1) a)
            dynBVSelect 128 8 ad @=? sizedBVToDynBV (sizedBVSelect (Proxy @128) (Proxy @8) a)
            dynBVSelect 128 16 ad @=? sizedBVToDynBV (sizedBVSelect (Proxy @128) (Proxy @16) a)
            dynBVSelect 128 32 ad @=? sizedBVToDynBV (sizedBVSelect (Proxy @128) (Proxy @32) a)
            dynBVSelect 136 1 ad @=? sizedBVToDynBV (sizedBVSelect (Proxy @136) (Proxy @1) a)
            dynBVSelect 136 8 ad @=? sizedBVToDynBV (sizedBVSelect (Proxy @136) (Proxy @8) a)
            dynBVSelect 136 16 ad @=? sizedBVToDynBV (sizedBVSelect (Proxy @136) (Proxy @16) a)
            dynBVSelect 136 24 ad @=? sizedBVToDynBV (sizedBVSelect (Proxy @136) (Proxy @24) a)
    ]

realTests ::
  forall p dbv bv sbv bv8 bv32 bv64.
  ( DynBVAssociate dbv bv sbv bv8 bv32 bv64,
    Bits (bv 136),
    Bits (bv 192),
    Num (bv 136),
    Num (bv 192),
    Real (bv 136),
    Real (bv 192),
    Integral (bv 136),
    Integral (bv 192)
  ) =>
  p dbv ->
  TestTree
realTests p =
  testGroup
    "Real"
    [ unaryOpToDirectlyComparableTest p "toRational" toRational
    ]

integralTests ::
  forall p dbv bv sbv bv8 bv32 bv64.
  ( DynBVAssociate dbv bv sbv bv8 bv32 bv64,
    Bits (bv 136),
    Bits (bv 192),
    Num (bv 136),
    Num (bv 192),
    Real (bv 136),
    Real (bv 192),
    Integral (bv 136),
    Integral (bv 192)
  ) =>
  p dbv ->
  TestTree
integralTests p =
  testGroup
    "Integral"
    [ throwableBinOpTest p "quot" quot,
      throwableBinOpTest p "rem" rem,
      throwableBinOpTest p "div" div,
      throwableBinOpTest p "mod" mod,
      unaryOpToDirectlyComparableTest p "toInteger" toInteger
    ]

dynBVTests :: TestTree
dynBVTests =
  testGroup
    "DynBVTests"
    [ testGroup
        "DynIntN"
        [ testGroup
            "Show"
            [ testProperty "show" $ bv136UnaryProp @DynIntN $ \x1 x1n ->
                ioProperty $ show x1 @=? show x1n
            ],
          eqTests dynIntNProxy,
          ordTests dynIntNProxy,
          bitsTests dynIntNProxy True,
          numInstanceTests dynIntNProxy,
          dynBVInstanceTests dynIntNProxy,
          realTests dynIntNProxy,
          integralTests dynIntNProxy
        ],
      testGroup
        "DynWordN"
        [ testGroup
            "Show"
            [ testProperty "show" $ bv136UnaryProp @DynWordN $ \x1 x1n ->
                ioProperty $ show x1 @=? show x1n
            ],
          eqTests dynWordNProxy,
          ordTests dynWordNProxy,
          bitsTests dynWordNProxy False,
          numInstanceTests dynWordNProxy,
          dynBVInstanceTests dynWordNProxy,
          realTests dynWordNProxy,
          integralTests dynWordNProxy
        ]
    ]
-}