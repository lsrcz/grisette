{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Grisette.Core.Data.Class.BitCastTests (bitCastTests) where

import Data.Typeable (Proxy (Proxy), Typeable, typeRep)
import Grisette
  ( BitCast (bitCast),
    IntN,
    LogicalOp (false, true),
    SymBool,
    SymIntN,
    SymWordN,
    WordN,
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
            bitCastBit1Tests @SymBool @(SymIntN 1),
            bitCastBit1Tests @SymBool @(SymWordN 1)
          ]
    ]
