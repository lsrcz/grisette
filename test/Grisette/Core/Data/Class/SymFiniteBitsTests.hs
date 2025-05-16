{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Grisette.Core.Data.Class.SymFiniteBitsTests (symFiniteBitsTests) where

import Data.Proxy (Proxy (Proxy))
import Grisette
  ( BV (bv),
    EvalSym,
    ITEOp (symIte),
    LogicalOp (false, true),
    SomeSymIntN,
    SomeSymWordN,
    SymEq,
    SymIntN,
    SymWordN,
  )
import Grisette.Internal.Core.Data.Class.SymFiniteBits
  ( SymFiniteBits (symFromBits, symSetBitTo, symTestBit),
    symBitBlast,
    symCountLeadingZeros,
    symCountTrailingZeros,
    symLsb,
    symMsb,
    symPopCount,
  )
import Grisette.TestUtil.SymbolicAssertion ((.@?=))
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Type.Reflection (Typeable, typeRep)

someBVSymFiniteBitsTest ::
  forall p bv.
  ( Typeable bv,
    BV bv,
    SymFiniteBits bv,
    Show bv,
    SymEq bv,
    Num bv,
    EvalSym bv
  ) =>
  p bv ->
  Test
someBVSymFiniteBitsTest _ =
  testGroup
    (show $ typeRep @bv)
    [ testCase "symTestBit" $ do
        let a = bv 4 0b0101 :: bv
        symTestBit a 0 .@?= true
        symTestBit a 1 .@?= false
        symTestBit a 2 .@?= true
        symTestBit a 3 .@?= false,
      testCase "symSetBitTo" $ do
        let a = bv 4 0b0101 :: bv
        symSetBitTo a 0 "b" .@?= symIte "b" (bv 4 0b0101) (bv 4 0b0100)
        symSetBitTo a 1 "b" .@?= symIte "b" (bv 4 0b0111) (bv 4 0b0101),
      testCase "symFromBits" $ do
        let actual = symFromBits ["a", "b", true, false] :: bv
        let expected =
              symIte
                "a"
                (symIte "b" (bv 4 0b0111) (bv 4 0b0101))
                (symIte "b" (bv 4 0b0110) (bv 4 0b0100))
        actual .@?= expected,
      testCase "symBitBlast" $ do
        symBitBlast (bv 4 0b0101 :: bv) .@?= [true, false, true, false],
      testCase "symLsb" $ do
        symLsb (bv 4 0b0101 :: bv) .@?= true
        symLsb (bv 4 0b0100 :: bv) .@?= false,
      testCase "symMsb" $ do
        symMsb (bv 4 0b0101 :: bv) .@?= false
        symMsb (bv 4 0b1101 :: bv) .@?= true,
      testCase "symPopCount" $ do
        symPopCount (bv 4 0 :: bv) .@?= 0
        symPopCount (bv 4 0b0101 :: bv) .@?= 2
        symPopCount (bv 4 0b1101 :: bv) .@?= 3
        symPopCount (bv 4 0b1111 :: bv) .@?= 4,
      testCase "symCountLeadingZeros" $ do
        symCountLeadingZeros (bv 4 0 :: bv) .@?= 4
        symCountLeadingZeros (bv 4 0b0101 :: bv) .@?= 1
        symCountLeadingZeros (bv 4 0b1101 :: bv) .@?= 0
        symCountLeadingZeros (bv 4 0b0011 :: bv) .@?= 2,
      testCase "symCountTrailingZeros" $ do
        symCountTrailingZeros (bv 4 0 :: bv) .@?= 4
        symCountTrailingZeros (bv 4 0b1010 :: bv) .@?= 1
        symCountTrailingZeros (bv 4 0b1011 :: bv) .@?= 0
        symCountTrailingZeros (bv 4 0b1100 :: bv) .@?= 2
    ]

bvSymFiniteBitsTest ::
  forall p bv.
  ( Num (bv 4),
    Typeable bv,
    SymFiniteBits (bv 4),
    Show (bv 4),
    SymEq (bv 4),
    EvalSym (bv 4)
  ) =>
  p bv ->
  Test
bvSymFiniteBitsTest _ =
  testGroup
    (show $ typeRep @bv)
    [ testCase "symTestBit" $ do
        let a = 5 :: bv 4
        symTestBit a 0 .@?= true
        symTestBit a 1 .@?= false
        symTestBit a 2 .@?= true
        symTestBit a 3 .@?= false,
      testCase "symSetBitTo" $ do
        let a = 5 :: bv 4
        symSetBitTo a 0 "b" .@?= symIte "b" 0b0101 0b0100
        symSetBitTo a 1 "b" .@?= symIte "b" 0b0111 0b0101,
      testCase "symFromBits" $ do
        let actual = symFromBits ["a", "b", true, false] :: bv 4
        let expected =
              symIte
                "a"
                (symIte "b" 0b0111 0b0101)
                (symIte "b" 0b0110 0b0100)
        actual .@?= expected,
      testCase "symBitBlast" $ do
        symBitBlast (0b0101 :: bv 4) .@?= [true, false, true, false],
      testCase "symLsb" $ do
        symLsb (0b0101 :: bv 4) .@?= true
        symLsb (0b0100 :: bv 4) .@?= false,
      testCase "symMsb" $ do
        symMsb (0b0101 :: bv 4) .@?= false
        symMsb (0b1101 :: bv 4) .@?= true,
      testCase "symPopCount" $ do
        symPopCount (0 :: bv 4) .@?= 0
        symPopCount (0b0101 :: bv 4) .@?= 2
        symPopCount (0b1101 :: bv 4) .@?= 3
        symPopCount (0b1111 :: bv 4) .@?= 4,
      testCase "symCountLeadingZeros" $ do
        symCountLeadingZeros (0 :: bv 4) .@?= 4
        symCountLeadingZeros (0b0101 :: bv 4) .@?= 1
        symCountLeadingZeros (0b1101 :: bv 4) .@?= 0
        symCountLeadingZeros (0b0011 :: bv 4) .@?= 2,
      testCase "symCountTrailingZeros" $ do
        symCountTrailingZeros (0 :: bv 4) .@?= 4
        symCountTrailingZeros (0b1010 :: bv 4) .@?= 1
        symCountTrailingZeros (0b1011 :: bv 4) .@?= 0
        symCountTrailingZeros (0b1100 :: bv 4) .@?= 2
    ]

symFiniteBitsTests :: Test
symFiniteBitsTests =
  testGroup
    "SymFiniteBits"
    [ testGroup
        "SymFiniteBits"
        [ someBVSymFiniteBitsTest (Proxy @SomeSymWordN),
          someBVSymFiniteBitsTest (Proxy @SomeSymIntN),
          bvSymFiniteBitsTest (Proxy @SymWordN),
          bvSymFiniteBitsTest (Proxy @SymIntN)
        ]
    ]
