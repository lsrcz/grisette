{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use /=" #-}
{-# HLINT ignore "Use ==" #-}

module Grisette.SymPrim.SomeBVTests (someBVTests) where

import Control.DeepSeq (NFData, force)
import Control.Exception (ArithException (Overflow), catch, evaluate)
import Control.Monad.Except (ExceptT)
import Data.Bits
  ( Bits
      ( clearBit,
        complement,
        complementBit,
        setBit,
        shiftL,
        unsafeShiftL,
        xor,
        (.&.),
        (.|.)
      ),
    FiniteBits (finiteBitSize),
  )
import Data.Proxy (Proxy (Proxy))
import Data.Serialize (decode, encode)
import Grisette
  ( BV (bv, bvConcat, bvExt, bvSelect, bvSext, bvZext),
    ITEOp (symIte),
    LogicalOp (symNot),
    Mergeable (rootStrategy),
    SafeLinearArith (safeAdd, safeSub),
    SignConversion (toSigned, toUnsigned),
    Solvable (con, isym, ssym),
    SomeBV (SomeBVLit),
    SomeWordN,
    SymEq ((./=), (.==)),
    genSym,
    genSymSimple,
    mrgIf,
    mrgReturn,
    mrgSingle,
  )
import Grisette.Internal.Core.Control.Monad.Union (Union (UMrg))
import Grisette.Internal.Core.Data.UnionBase
  ( UnionBase (UnionSingle),
    ifWithLeftMost,
  )
import Grisette.Internal.SymPrim.BV (IntN)
import Grisette.Internal.SymPrim.SomeBV
  ( SomeBV (SomeBV),
    SomeBVException (BitwidthMismatch),
    SomeIntN,
    SomeSymIntN,
    arbitraryBV,
    binSomeBV,
    binSomeBVR1,
    binSomeBVR2,
    binSomeBVSafe,
    binSomeBVSafeR1,
    conBV,
    conBVView,
    isymBV,
    ssymBV,
    unarySomeBV,
    unarySomeBVR1,
    pattern ConBV,
    pattern SomeIntN,
  )
import Grisette.Internal.SymPrim.SymBV (SymIntN)
import Grisette.Lib.Control.Monad.Except (mrgThrowError)
import Grisette.Lib.Data.Functor (mrgFmap)
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.HUnit (assertBool, (@?=))
import Test.QuickCheck
  ( Arbitrary (arbitrary),
    Gen,
    NonNegative (getNonNegative),
    forAll,
    ioProperty,
  )

testFuncMatch ::
  (Eq r, Show r) =>
  String ->
  (SomeIntN -> SomeIntN -> r) ->
  SomeIntN ->
  SomeIntN ->
  r ->
  Test
testFuncMatch name f a b r = testCase name $ do
  let actual = f a b
  let expected = r
  actual @?= expected

testFuncMatchLit ::
  String ->
  (SomeIntN -> SomeIntN -> SomeIntN) ->
  SomeIntN ->
  SomeIntN ->
  SomeIntN ->
  Test
testFuncMatchLit name f a b r = testCase name $ do
  let SomeBVLit actual = f a b
  let SomeBVLit expected = r
  actual @?= expected

testSymFuncMatch ::
  String ->
  (SomeSymIntN -> SomeSymIntN -> SomeSymIntN) ->
  SomeSymIntN ->
  SomeSymIntN ->
  SomeSymIntN ->
  Test
testSymFuncMatch name f a b r = testCase name $ do
  let actual = f a b
  let expected = r
  actual @?= expected

testSymFuncMatchLit ::
  String ->
  (SomeSymIntN -> SomeSymIntN -> SomeSymIntN) ->
  SomeSymIntN ->
  SomeSymIntN ->
  SomeSymIntN ->
  Test
testSymFuncMatchLit name f a b r = testCase name $ do
  let SomeBVLit actual = f a b
  let SomeBVLit expected = r
  actual @?= expected

testFuncMisMatch ::
  (NFData r, Show r, Eq r) =>
  (SomeIntN -> SomeIntN -> r) ->
  SomeIntN ->
  SomeIntN ->
  r ->
  Test
testFuncMisMatch f a b r = testCase "bit width mismatch" $ do
  actual <-
    evaluate (force $ f a b)
      `catch` \(_ :: SomeBVException) -> return r
  let expected = r
  actual @?= expected

testSafeFuncMatchException ::
  (Eq r, Show r, Mergeable r) =>
  ( SomeIntN ->
    SomeIntN ->
    ExceptT (Either SomeBVException ArithException) Union r
  ) ->
  SomeIntN ->
  SomeIntN ->
  ArithException ->
  Test
testSafeFuncMatchException f a b e = testCase "bit width match" $ do
  let actual = f a b
  let expected = mrgThrowError (Right e)
  actual @?= expected

testSafeFuncMatch ::
  (Eq r, Show r, Mergeable r) =>
  String ->
  ( SomeIntN ->
    SomeIntN ->
    ExceptT (Either SomeBVException ArithException) Union r
  ) ->
  SomeIntN ->
  SomeIntN ->
  r ->
  Test
testSafeFuncMatch name f a b r = testCase name $ do
  let actual = f a b
  let expected = mrgSingle r
  actual @?= expected

testSafeFuncMatchLit ::
  String ->
  ( SomeIntN ->
    SomeIntN ->
    Either (Either SomeBVException ArithException) SomeIntN
  ) ->
  SomeIntN ->
  SomeIntN ->
  SomeIntN ->
  Test
testSafeFuncMatchLit name f a b r = testCase name $ do
  let Right (SomeBVLit actual) = f a b
  let Right (SomeBVLit expected) = mrgSingle r
  actual @?= expected

testSafeFuncMisMatch ::
  (Eq r, Show r, Mergeable r) =>
  ( SomeIntN ->
    SomeIntN ->
    ExceptT (Either SomeBVException ArithException) Union r
  ) ->
  SomeIntN ->
  SomeIntN ->
  Test
testSafeFuncMisMatch f a b = testCase "bit width mismatch" $ do
  let actual = f a b
  let expected = mrgThrowError (Left BitwidthMismatch)
  actual @?= expected

someBVTests :: Test
someBVTests =
  testGroup
    "SomeBV"
    [ testGroup
        "Helpers"
        [ testCase "conBV" $ do
            let actual = conBV (bv 4 5)
            let expected = bv 4 5 :: SomeSymIntN
            actual @?= expected,
          testGroup
            "conBVView"
            [ testCase "is concrete" $ do
                let value = bv 4 5 :: SomeSymIntN
                let actual = conBVView value
                let expected = Just (bv 4 5)
                actual @?= expected
                case value of
                  ConBV v -> v @?= bv 4 5
                  _ -> fail "is concrete",
              testCase "is not concrete" $ do
                let value = ssymBV 4 "a" :: SomeSymIntN
                let actual = conBVView value
                let expected = Nothing
                actual @?= expected
                case value of
                  ConBV _ -> fail "is not concrete"
                  _ -> return ()
            ],
          testCase "ssymBV" $ ssymBV 4 "a" @?= SomeBV (ssym "a" :: SymIntN 4),
          testCase "isymBV" $
            isymBV 4 "a" 1 @?= SomeBV (isym "a" 1 :: SymIntN 4),
          testGroup
            "unarySomeBV"
            [ testCase "SomeBV" $ do
                let actual =
                      unarySomeBV @IntN @SomeIntN
                        (SomeIntN . negate)
                        undefined
                        (bv 4 5 :: SomeIntN)
                let expected = bv 4 (-5)
                actual @?= expected,
              testCase "SomeBVLit" $ do
                let SomeBVLit actual =
                      unarySomeBV @IntN @SomeIntN
                        undefined
                        (SomeBVLit . negate)
                        (5 :: SomeIntN)
                let SomeBVLit expected = SomeBVLit $ -5
                actual @?= expected
            ],
          testGroup
            "unarySomeBVR1"
            [ testCase "SomeBV" $ do
                let actual = unarySomeBVR1 negate undefined (bv 4 5 :: SomeIntN)
                let expected = bv 4 (-5)
                actual @?= expected,
              testCase "SomeBVLit" $ do
                let SomeBVLit actual =
                      unarySomeBVR1 undefined negate (SomeBVLit 5 :: SomeIntN)
                let SomeBVLit expected = SomeBVLit $ -5
                actual @?= expected
            ],
          testGroup
            "binSomeBV"
            [ testFuncMatch @SomeIntN
                "SomeBV/SomeBV"
                (binSomeBV (\l r -> SomeIntN $ l + r) undefined)
                (bv 4 5)
                (bv 4 2)
                (bv 4 7),
              testFuncMatch @SomeIntN
                "SomeBV/SomeBVLit"
                (binSomeBV (\l r -> SomeIntN $ l + r) undefined)
                (bv 4 5)
                2
                (bv 4 7),
              testFuncMatch @SomeIntN
                "SomeBVLit/SomeBV"
                (binSomeBV (\l r -> SomeIntN $ l + r) undefined)
                5
                (bv 4 2)
                (bv 4 7),
              testFuncMatchLit
                "SomeBVLit/SomeBVLit"
                (binSomeBV undefined (\l r -> SomeBVLit $ l + r))
                5
                2
                7,
              testSymFuncMatch
                "SomeBV/SomeBV"
                (binSomeBV (\l r -> SomeBV $ l + r) undefined)
                (ssymBV 4 "a")
                (ssymBV 4 "b")
                ((ssymBV 4 "a") + (ssymBV 4 "b")),
              testSymFuncMatch
                "SomeBV/SomeBVCondLit"
                (binSomeBV (\l r -> SomeBV $ l + r) undefined)
                (ssymBV 4 "a")
                (symIte "b" 5 6)
                ((ssymBV 4 "a") + symIte "b" (bv 4 5) (bv 4 6)),
              testSymFuncMatchLit
                "SomeBVCondLit/SomeBVCondLit"
                (binSomeBV undefined (\l r -> SomeBVLit $ l + r))
                (symIte "a" 5 6)
                (symIte "b" 5 6)
                (symIte "a" (symIte "b" 10 11) (symIte "b" 11 12)),
              testSymFuncMatchLit
                "SomeBVLit/SomeBVCondLit"
                (binSomeBV undefined (\l r -> SomeBVLit $ l + r))
                5
                (symIte "b" 5 6)
                (symIte "b" 10 11),
              testFuncMisMatch @SomeIntN
                (binSomeBV (\l r -> SomeIntN $ l + r) undefined)
                (bv 4 5)
                (bv 5 4)
                (bv 3 0)
            ],
          testGroup
            "binSomeBVR1"
            [ testFuncMatch
                "SomeBV/SomeBV"
                (binSomeBVR1 (+) undefined)
                (bv 4 5)
                (bv 4 2)
                (bv 4 7),
              testFuncMisMatch
                (binSomeBVR1 (+) undefined)
                (bv 4 5)
                (bv 5 4)
                (bv 3 0)
            ],
          testGroup
            "binSomeBVR2"
            [ testFuncMatch
                "SomeBV/SomeBV"
                (binSomeBVR2 (\l r -> (l + r, l - r)) undefined)
                (bv 4 5)
                (bv 4 2)
                (bv 4 7, bv 4 3),
              testFuncMisMatch
                (binSomeBVR2 (\l r -> (l + r, l - r)) undefined)
                (bv 4 5)
                (bv 5 4)
                (bv 3 0, bv 6 1)
            ],
          testGroup "binSomeBVSafe" $ do
            let func l r = mrgFmap SomeIntN $ safeAdd l r
            [ testSafeFuncMatch @SomeIntN
                "SomeBV/SomeBV"
                (binSomeBVSafe func undefined)
                (bv 4 5)
                (bv 4 2)
                (bv 4 7),
              testSafeFuncMatch @SomeIntN
                "SomeBV/SomeBVInt"
                (binSomeBVSafe func undefined)
                (bv 4 5)
                2
                (bv 4 7),
              testSafeFuncMatchLit
                "SomeBVInt/SomeBVInt"
                ( binSomeBVSafe
                    undefined
                    (\l r -> mrgReturn $ SomeBVLit $ l + r)
                )
                5
                2
                7,
              testSafeFuncMatchException @SomeIntN
                (binSomeBVSafe func undefined)
                (bv 4 5)
                (bv 4 5)
                Overflow,
              testSafeFuncMisMatch @SomeIntN
                (binSomeBVSafe func undefined)
                (bv 4 5)
                (bv 5 4)
              ],
          testGroup
            "binSomeBVSafeR1"
            [ testSafeFuncMatch
                "SomeBV/SomeBV"
                (binSomeBVSafeR1 safeAdd undefined)
                (bv 4 5)
                (bv 4 2)
                (bv 4 7),
              testSafeFuncMatchException
                (binSomeBVSafeR1 safeAdd undefined)
                (bv 4 5)
                (bv 4 5)
                Overflow,
              testSafeFuncMisMatch
                (binSomeBVSafeR1 safeAdd undefined)
                (bv 4 5)
                (bv 5 4)
            ],
          testGroup "binSomeBVSafeR2" $ do
            let func l r = do
                  a <- safeAdd l r
                  b <- safeSub l r
                  mrgSingle (a, b)
            [ testSafeFuncMatch
                "SomeBV/SomeBV"
                func
                (bv 4 5)
                (bv 4 2)
                (bv 4 7, bv 4 3),
              testSafeFuncMatchException
                func
                (bv 4 5)
                (bv 4 5)
                Overflow,
              testSafeFuncMisMatch func (bv 4 5) (bv 5 4)
              ]
        ],
      testGroup
        "BV"
        [ testCase "bvConcat" $ do
            bvConcat (bv 8 0x14 :: SomeIntN) (bv 4 2) @?= bv 12 0x142,
          testCase "bvZext" $ do
            bvZext 8 (bv 4 0x8 :: SomeIntN) @?= bv 8 0x08,
          testCase "bvSext" $ do
            bvSext 8 (bv 4 0x8 :: SomeIntN) @?= bv 8 0xF8,
          testCase "bvExt" $ do
            bvExt 8 (bv 4 0x8 :: SomeIntN) @?= bv 8 0xF8
            bvExt 8 (bv 4 0x8 :: SomeWordN) @?= bv 8 0x08,
          testCase "bvSelect" $ do
            bvSelect 1 4 (bv 8 0x17 :: SomeIntN) @?= bv 4 0xB,
          testCase "bv" $ bv 8 0x14 @?= (SomeIntN (0x14 :: IntN 8))
        ],
      testGroup
        "Mergeable"
        [ testGroup "SomeIntN" $ do
            (name, l, r, merged) <-
              [ ( "same bitwidth",
                  bv 4 3,
                  bv 4 5,
                  ifWithLeftMost
                    True
                    "cond"
                    (UnionSingle $ bv 4 3)
                    (UnionSingle $ bv 4 5)
                ),
                ( "same bitwidth, should invert",
                  bv 4 5,
                  bv 4 2,
                  ifWithLeftMost
                    True
                    (symNot "cond")
                    (UnionSingle $ bv 4 2)
                    (UnionSingle $ bv 4 5)
                ),
                ( "different bitwidth",
                  bv 4 5,
                  bv 5 4,
                  ifWithLeftMost
                    True
                    "cond"
                    (UnionSingle $ bv 4 5)
                    (UnionSingle $ bv 5 4)
                ),
                ( "different bitwidth, should invert",
                  bv 5 4,
                  bv 4 5,
                  ifWithLeftMost
                    True
                    (symNot "cond")
                    (UnionSingle $ bv 4 5)
                    (UnionSingle $ bv 5 4)
                )
                ]
            return $ testCase name $ do
              let actual =
                    mrgIf "cond" (return l) (return r) :: Union SomeIntN
              let expected = UMrg rootStrategy merged
              actual @?= expected,
          testGroup "SomeSymIntN" $ do
            (name, l, r, merged) <-
              [ ( "same bitwidth",
                  ssymBV 4 "a",
                  ssymBV 4 "b",
                  (UnionSingle $ symIte "cond" (ssymBV 4 "a") (ssymBV 4 "b"))
                ),
                ( "different bitwidth",
                  ssymBV 4 "a",
                  ssymBV 5 "b",
                  ifWithLeftMost
                    True
                    "cond"
                    (UnionSingle $ ssymBV 4 "a")
                    (UnionSingle $ ssymBV 5 "b")
                ),
                ( "different bitwidth, should invert",
                  ssymBV 5 "b",
                  ssymBV 4 "a",
                  ifWithLeftMost
                    True
                    (symNot "cond")
                    (UnionSingle $ ssymBV 4 "a")
                    (UnionSingle $ ssymBV 5 "b")
                )
                ]
            return $ testCase name $ do
              let actual =
                    mrgIf "cond" (return l) (return r) :: Union SomeSymIntN
              let expected = UMrg rootStrategy merged
              actual @?= expected
        ],
      testGroup
        "GenSym"
        [ testCase "Proxy n" $ do
            let actual = genSym (Proxy :: Proxy 4) "a" :: Union SomeSymIntN
            let expected = mrgSingle $ isymBV 4 "a" 0
            actual @?= expected,
          testCase "SomeBV" $ do
            let actual =
                  genSym (bv 4 1 :: SomeSymIntN) "a" :: Union SomeSymIntN
            let expected = mrgSingle $ isymBV 4 "a" 0
            actual @?= expected,
          testCase "Int" $ do
            let actual =
                  genSym (4 :: Int) "a" :: Union SomeSymIntN
            let expected = mrgSingle $ isymBV 4 "a" 0
            actual @?= expected
        ],
      testGroup
        "GenSymSimple"
        [ testCase "Proxy n" $ do
            let actual = genSymSimple (Proxy :: Proxy 4) "a" :: SomeSymIntN
            let expected = isymBV 4 "a" 0
            actual @?= expected,
          testCase "SomeBV" $ do
            let actual =
                  genSymSimple (bv 4 1 :: SomeSymIntN) "a" :: SomeSymIntN
            let expected = isymBV 4 "a" 0
            actual @?= expected,
          testCase "Int" $ do
            let actual = genSymSimple (4 :: Int) "a" :: SomeSymIntN
            let expected = isymBV 4 "a" 0
            actual @?= expected
        ],
      testProperty "arbitraryBV" $
        forAll (arbitraryBV 4) $
          \(bv :: SomeIntN) -> ioProperty $ finiteBitSize bv @?= 4,
      testGroup
        "Eq"
        [ testCase "same bitwidth equal" $ do
            let a = bv 4 5 :: SomeIntN
            let b = bv 4 5 :: SomeIntN
            assertBool "SomeBV with same bitwidth should compare the value" $
              a == b
            assertBool "SomeBV with same bitwidth should compare the value" $
              not $
                a /= b,
          testProperty "==/SomeBV/SomeBVLit" $ \(a :: Integer) (b :: Integer) ->
            let ai = fromIntegral a :: SomeWordN
                bi = fromIntegral b :: SomeWordN
                ab = bv 4 a :: SomeWordN
                bb = bv 4 b :: SomeWordN
             in (ai == bb) == (ab == bb)
                  && (ab == bi) == (ab == bb)
                  && ai == ab,
          testProperty "/=/SomeBV/SomeBVLit" $ \(a :: Integer) (b :: Integer) ->
            let ai = fromIntegral a :: SomeWordN
                bi = fromIntegral b :: SomeWordN
                ab = bv 4 a :: SomeWordN
                bb = bv 4 b :: SomeWordN
             in (ai /= bb) == (ab /= bb)
                  && (ab /= bi) == (ab /= bb)
                  && not (ai /= ab),
          testCase "same bitwidth not equal" $ do
            let a = bv 4 4 :: SomeIntN
            let b = bv 4 5 :: SomeIntN
            assertBool "SomeBV with same bitwidth should compare the value" $
              not $
                a == b
            assertBool "SomeBV with same bitwidth should compare the value" $
              a /= b,
          testCase "different bitwidth" $ do
            let a = bv 3 5 :: SomeIntN
            let b = bv 4 5 :: SomeIntN
            assertBool "SomeBV with different bit width are not equal" $
              not $
                a == b
            assertBool "SomeBV with different bit width are not equal" $ a /= b
        ],
      testGroup
        "SymEq"
        [ testCase "same bitwidth" $ do
            let a = ssymBV 4 "a" :: SomeSymIntN
            let b = ssymBV 4 "b" :: SomeSymIntN
            a .== b @?= ("a" :: SymIntN 4) .== "b"
            a ./= b @?= ("a" :: SymIntN 4) ./= "b",
          testCase "different bitwidth" $ do
            let a = ssymBV 4 "a" :: SomeSymIntN
            let b = ssymBV 3 "b" :: SomeSymIntN
            a .== b @?= con False
            a ./= b @?= con True
        ],
      testGroup
        "Num"
        [ testGroup
            "SomeIntN"
            [ binOpLitTest @SomeIntN (+) "+",
              binOpLitTest @SomeIntN (-) "-",
              unaryOpLitTest @SomeIntN negate "negate"
            ],
          testGroup
            "SomeWordN"
            [ binOpLitTest @SomeWordN (+) "+",
              binOpLitTest @SomeWordN (-) "-",
              unaryOpLitTest @SomeWordN negate "negate"
            ]
        ],
      testGroup
        "SignConversion"
        [ testGroup
            "SomeIntN"
            [unaryOpLitTest @SomeIntN toUnsigned "toUnsigned"],
          testGroup
            "SomeWordN"
            [unaryOpLitTest @SomeWordN toSigned "toSigned"]
        ],
      testGroup
        "Bits"
        [ testGroup
            "SomeIntN"
            [ binOpLitTest @SomeIntN (.&.) ".&.",
              binOpLitTest @SomeIntN (.|.) ".|.",
              binOpLitTest @SomeIntN xor "xor",
              unaryOpLitTest @SomeIntN complement "complement",
              binIntOpLitTest @SomeIntN
                (getNonNegative <$> arbitrary)
                setBit
                "setBit",
              binIntOpLitTest @SomeIntN
                (getNonNegative <$> arbitrary)
                clearBit
                "clearBit",
              binIntOpLitTest @SomeIntN
                (getNonNegative <$> arbitrary)
                complementBit
                "complementBit",
              binIntOpLitTest @SomeIntN
                (getNonNegative <$> arbitrary)
                shiftL
                "shiftL",
              binIntOpLitTest @SomeIntN
                (getNonNegative <$> arbitrary)
                unsafeShiftL
                "unsafeShiftL"
            ],
          testGroup
            "SomeWordN"
            [ binOpLitTest @SomeWordN (.&.) ".&.",
              binOpLitTest @SomeWordN (.|.) ".|.",
              binOpLitTest @SomeWordN xor "xor",
              unaryOpLitTest @SomeWordN complement "complement",
              binIntOpLitTest @SomeWordN
                (getNonNegative <$> arbitrary)
                setBit
                "setBit",
              binIntOpLitTest @SomeWordN
                (getNonNegative <$> arbitrary)
                clearBit
                "clearBit",
              binIntOpLitTest @SomeWordN
                (getNonNegative <$> arbitrary)
                complementBit
                "complementBit",
              binIntOpLitTest @SomeWordN
                (getNonNegative <$> arbitrary)
                shiftL
                "shiftL",
              binIntOpLitTest @SomeWordN
                (getNonNegative <$> arbitrary)
                unsafeShiftL
                "unsafeShiftL"
            ]
        ],
      testProperty "Serialize" $ forAll (arbitraryBV 8) $ \(v :: SomeWordN) ->
        Right v == decode (encode v)
    ]

binOpLitTest ::
  forall bv r. (Num bv, Eq r, BV bv) => (bv -> bv -> r) -> String -> Test
binOpLitTest f name =
  testProperty (name ++ "/SomeBV/SomeBVLit") $ \(a :: Integer) (b :: Integer) ->
    let ai = fromIntegral a :: bv
        bi = fromIntegral b :: bv
        ab = bv 4 a :: bv
        bb = bv 4 b :: bv
     in (f ai bb) == (f ab bb)
          && (f ab bi) == (f ab bb)
          && (f ai bi) == (f ab bb)

binIntOpLitTest ::
  forall bv.
  (Num bv, Eq bv, BV bv) =>
  Gen Int ->
  (bv -> Int -> bv) ->
  String ->
  Test
binIntOpLitTest gen f name =
  testProperty (name ++ "/SomeBV/SomeBVLit") $ \(a :: Integer) -> forAll gen $
    \(b :: Int) ->
      let ai = fromIntegral a :: bv
          ab = bv 4 a :: bv
       in (f ai b) == (f ab b)
            && (f ab b) == (f ab b)
            && (f ai b) == (f ab b)

unaryOpLitTest ::
  forall bv r. (Num bv, Eq r, BV bv) => (bv -> r) -> String -> Test
unaryOpLitTest f name =
  testProperty (name ++ "/SomeBV/SomeBVLit") $ \(a :: Integer) ->
    let ai = fromIntegral a :: bv
        ab = bv 4 a :: bv
     in f ai == f ab
