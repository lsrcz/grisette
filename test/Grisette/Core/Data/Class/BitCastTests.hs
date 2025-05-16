{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Grisette.Core.Data.Class.BitCastTests (bitCastTests) where

import Data.Typeable (Proxy (Proxy), Typeable, typeRep)
import Grisette
  ( AsKey,
    BitCast (bitCast),
    FP32,
    IntN,
    IntN32,
    LogicalOp (false, true),
    SymBool,
    SymFP32,
    SymIntN,
    SymIntN32,
    SymWordN,
    SymWordN32,
    WordN,
    WordN32,
    bitCastOrCanonical,
    fpNaN,
  )
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit ((@?=))

bitCastBit1Tests ::
  forall b r.
  ( BitCast b r,
    BitCast r b,
    LogicalOp b,
    Num r,
    Typeable b,
    Typeable r,
    Eq b,
    Eq r,
    Show b,
    Show r
  ) =>
  [Test]
bitCastBit1Tests =
  [ testCase (bname <> " to " <> rname) $ do
      bitCast (true :: b) @?= (1 :: r)
      bitCast (false :: b) @?= (0 :: r),
    testCase (rname <> " to " <> bname) $ do
      bitCast (1 :: r) @?= (true :: b)
      bitCast (0 :: r) @?= (false :: b)
  ]
  where
    bname = show $ typeRep (Proxy :: Proxy b)
    rname = show $ typeRep (Proxy :: Proxy r)

bitCastTests :: Test
bitCastTests =
  testGroup
    "BitCast"
    [ testGroup "1 bit" $
        concat
          [ bitCastBit1Tests @Bool @(IntN 1),
            bitCastBit1Tests @Bool @(WordN 1),
            bitCastBit1Tests @(AsKey SymBool) @(AsKey (SymIntN 1)),
            bitCastBit1Tests @(AsKey SymBool) @(AsKey (SymWordN 1))
          ],
      testGroup
        "FP"
        [ testCase "FP32" $ do
            bitCastOrCanonical (-512.625 :: FP32) @?= (0xc4002800 :: WordN32)
            bitCastOrCanonical (fpNaN :: FP32) @?= (0x7fc00000 :: WordN32)
            bitCast (0xc4002800 :: WordN32) @?= (-512.625 :: FP32)
            bitCastOrCanonical (-512.625 :: FP32) @?= (0xc4002800 :: IntN32)
            bitCastOrCanonical (fpNaN :: FP32) @?= (0x7fc00000 :: IntN32)
            bitCast (0xc4002800 :: IntN32) @?= (-512.625 :: FP32),
          testCase "SymFP32" $ do
            bitCastOrCanonical (-512.625 :: SymFP32)
              @?= (0xc4002800 :: AsKey SymWordN32)
            bitCastOrCanonical (fpNaN :: SymFP32) @?= (0x7fc00000 :: AsKey SymWordN32)
            bitCast (0xc4002800 :: SymWordN32) @?= (-512.625 :: AsKey SymFP32)
            bitCastOrCanonical (-512.625 :: SymFP32)
              @?= (0xc4002800 :: AsKey SymIntN32)
            bitCastOrCanonical (fpNaN :: SymFP32) @?= (0x7fc00000 :: AsKey SymIntN32)
            bitCast (0xc4002800 :: SymIntN32) @?= (-512.625 :: AsKey SymFP32)
        ],
      testCase "Nested" $ do
        let int32 = "x" :: AsKey SymIntN32
        let word32 = bitCast int32 :: AsKey SymWordN32
        let final = bitCast word32 :: AsKey SymIntN32
        final @?= int32
    ]
