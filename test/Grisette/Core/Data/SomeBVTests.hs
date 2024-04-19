{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Grisette.Core.Data.SomeBVTests (someBVTests) where

import Control.DeepSeq (NFData, force)
import Control.Exception (ArithException (Overflow), catch, evaluate)
import Control.Monad.Except (ExceptT)
import Data.Bits (FiniteBits (finiteBitSize))
import Data.Proxy (Proxy (Proxy))
import Grisette (ITEOp (symIte))
import Grisette.Core.Control.Monad.UnionM (UnionM (UMrg))
import Grisette.Core.Data.BV (BitwidthMismatch (BitwidthMismatch), IntN)
import Grisette.Core.Data.Class.BitVector
  ( BV (bv, bvConcat, bvExt, bvSelect, bvSext, bvZext),
  )
import Grisette.Core.Data.Class.GenSym (genSym, genSymSimple)
import Grisette.Core.Data.Class.LogicalOp (LogicalOp (symNot))
import Grisette.Core.Data.Class.Mergeable (Mergeable (rootStrategy))
import Grisette.Core.Data.Class.SafeLinearArith
  ( SafeLinearArith (safeAdd, safeSub),
  )
import Grisette.Core.Data.Class.SimpleMergeable (mrgIf)
import Grisette.Core.Data.Class.Solvable (Solvable (isym, ssym))
import Grisette.Core.Data.Class.TryMerge (mrgSingle)
import Grisette.Core.Data.SomeBV
  ( SomeBV (SomeBV),
    SomeIntN,
    SomeSymIntN,
    SomeWordN,
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
import Grisette.Core.Data.Union (Union (UnionSingle), ifWithLeftMost)
import Grisette.Lib.Control.Monad.Except (mrgThrowError)
import Grisette.Lib.Data.Functor (mrgFmap)
import Grisette.SymPrim.SymBV (SymIntN)
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.HUnit ((@?=))
import Test.QuickCheck (forAll, ioProperty)

testFuncMatch ::
  (Eq r, Show r) =>
  (SomeIntN -> SomeIntN -> r) ->
  SomeIntN ->
  SomeIntN ->
  r ->
  Test
testFuncMatch f a b r = testCase "bit width match" $ do
  let actual = f a b
  let expected = r
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
      `catch` \(_ :: BitwidthMismatch) -> return r
  let expected = r
  actual @?= expected

testSafeFuncMatchException ::
  (Eq r, Show r, Mergeable r) =>
  ( SomeIntN ->
    SomeIntN ->
    ExceptT (Either BitwidthMismatch ArithException) UnionM r
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
  ( SomeIntN ->
    SomeIntN ->
    ExceptT (Either BitwidthMismatch ArithException) UnionM r
  ) ->
  SomeIntN ->
  SomeIntN ->
  r ->
  Test
testSafeFuncMatch f a b r = testCase "bit width match" $ do
  let actual = f a b
  let expected = mrgSingle r
  actual @?= expected

testSafeFuncMisMatch ::
  (Eq r, Show r, Mergeable r) =>
  ( SomeIntN ->
    SomeIntN ->
    ExceptT (Either BitwidthMismatch ArithException) UnionM r
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
          testCase "unarySomeBV" $ do
            let actual =
                  unarySomeBV @IntN @SomeIntN
                    (SomeIntN . negate)
                    (bv 4 5 :: SomeIntN)
            let expected = bv 4 (-5)
            actual @?= expected,
          testCase "unarySomeBVR1" $ do
            let actual = unarySomeBVR1 (negate) (bv 4 5 :: SomeIntN)
            let expected = bv 4 (-5)
            actual @?= expected,
          testGroup
            "binSomeBV"
            [ testFuncMatch @SomeIntN
                (binSomeBV (\l r -> SomeIntN $ l + r))
                (bv 4 5)
                (bv 4 2)
                (bv 4 7),
              testFuncMisMatch @SomeIntN
                (binSomeBV (\l r -> SomeIntN $ l + r))
                (bv 4 5)
                (bv 5 4)
                (bv 3 0)
            ],
          testGroup
            "binSomeBVR1"
            [ testFuncMatch (binSomeBVR1 (+)) (bv 4 5) (bv 4 2) (bv 4 7),
              testFuncMisMatch (binSomeBVR1 (+)) (bv 4 5) (bv 5 4) (bv 3 0)
            ],
          testGroup
            "binSomeBVR2"
            [ testFuncMatch
                (binSomeBVR2 (\l r -> (l + r, l - r)))
                (bv 4 5)
                (bv 4 2)
                (bv 4 7, bv 4 3),
              testFuncMisMatch
                (binSomeBVR2 (\l r -> (l + r, l - r)))
                (bv 4 5)
                (bv 5 4)
                (bv 3 0, bv 6 1)
            ],
          testGroup "binSomeBVSafe" $ do
            let func l r = mrgFmap SomeIntN $ safeAdd l r
            [ testSafeFuncMatch @SomeIntN
                (binSomeBVSafe func)
                (bv 4 5)
                (bv 4 2)
                (bv 4 7),
              testSafeFuncMatchException @SomeIntN
                (binSomeBVSafe func)
                (bv 4 5)
                (bv 4 5)
                Overflow,
              testSafeFuncMisMatch @SomeIntN
                (binSomeBVSafe func)
                (bv 4 5)
                (bv 5 4)
              ],
          testGroup
            "binSomeBVSafeR1"
            [ testSafeFuncMatch
                (binSomeBVSafeR1 safeAdd)
                (bv 4 5)
                (bv 4 2)
                (bv 4 7),
              testSafeFuncMatchException
                (binSomeBVSafeR1 safeAdd)
                (bv 4 5)
                (bv 4 5)
                Overflow,
              testSafeFuncMisMatch (binSomeBVSafeR1 safeAdd) (bv 4 5) (bv 5 4)
            ],
          testGroup "binSomeBVSafeR2" $ do
            let func l r = do
                  a <- safeAdd l r
                  b <- safeSub l r
                  mrgSingle (a, b)
            [ testSafeFuncMatch
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
                    mrgIf "cond" (return l) (return r) :: UnionM SomeIntN
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
                    mrgIf "cond" (return l) (return r) :: UnionM SomeSymIntN
              let expected = UMrg rootStrategy merged
              actual @?= expected
        ],
      testGroup
        "GenSym"
        [ testCase "Proxy n" $ do
            let actual = genSym (Proxy :: Proxy 4) "a" :: UnionM SomeSymIntN
            let expected = mrgSingle $ isymBV 4 "a" 0
            actual @?= expected,
          testCase "SomeBV" $ do
            let actual =
                  genSym (bv 4 1 :: SomeSymIntN) "a" :: UnionM SomeSymIntN
            let expected = mrgSingle $ isymBV 4 "a" 0
            actual @?= expected,
          testCase "Int" $ do
            let actual =
                  genSym (4 :: Int) "a" :: UnionM SomeSymIntN
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
          \(bv :: SomeIntN) -> ioProperty $ finiteBitSize bv @?= 4
    ]
