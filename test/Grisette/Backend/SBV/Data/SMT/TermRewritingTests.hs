{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Grisette.Backend.SBV.Data.SMT.TermRewritingTests
  ( termRewritingTests,
    validateSpec,
  )
where

import Data.Foldable (traverse_)
import qualified Data.SBV as SBV
import Grisette.Backend.SBV.Data.SMT.Solving
  ( GrisetteSMTConfig,
    precise,
  )
import Grisette.Backend.SBV.Data.SMT.TermRewritingGen
  ( BoolOnlySpec,
    BoolWithLIASpec,
    DifferentSizeBVSpec,
    FixedSizedBVWithBoolSpec,
    GeneralSpec,
    LIAWithBoolSpec,
    TermRewritingSpec
      ( conSpec,
        counterExample,
        norewriteVer,
        rewriteVer,
        same,
        symSpec
      ),
    absNumSpec,
    addNumSpec,
    andSpec,
    divBoundedIntegralSpec,
    divIntegralSpec,
    eqvSpec,
    iteSpec,
    modBoundedIntegralSpec,
    modIntegralSpec,
    notSpec,
    orSpec,
    quotBoundedIntegralSpec,
    quotIntegralSpec,
    remBoundedIntegralSpec,
    remIntegralSpec,
    shiftRightSpec,
    timesNumSpec,
    uminusNumSpec,
  )
import Grisette.Core.Data.BV (IntN, WordN)
import Grisette.Core.Data.Class.Solver (solve)
import Grisette.IR.SymPrim.Data.Prim.InternedTerm.Term
  ( SupportedPrim,
  )
import Grisette.IR.SymPrim.Data.Prim.InternedTerm.TermUtils
  ( pformat,
  )
import Grisette.IR.SymPrim.Data.SymPrim (SymBool (SymBool))
import Test.Framework (Test, TestName, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.HUnit (Assertion, assertFailure)
import Test.QuickCheck (ioProperty, mapSize, withMaxSuccess)

validateSpec :: (TermRewritingSpec a av, Show a, SupportedPrim av) => GrisetteSMTConfig n -> a -> Assertion
validateSpec config a = do
  r <- solve config (SymBool $ counterExample a)
  rs <- solve config (SymBool $ same a)
  case (r, rs) of
    (Left _, Right _) -> do
      return ()
    (Left _, Left _) -> do
      assertFailure $ "Bad rewriting with unsolvable formula: " ++ pformat (norewriteVer a) ++ " was rewritten to " ++ pformat (rewriteVer a)
    (Right m, _) -> do
      assertFailure $ "With model" ++ show m ++ "Bad rewriting: " ++ pformat (norewriteVer a) ++ " was rewritten to " ++ pformat (rewriteVer a)

unboundedConfig = precise SBV.z3

divisionTest ::
  forall a b.
  (TermRewritingSpec a b, Show a, Enum b, Num b, SupportedPrim b) =>
  TestName ->
  (a -> a -> a) ->
  Test
divisionTest name f =
  testGroup
    name
    [ testCase "on concrete" $ do
        traverse_
          ( \(x :: b, y :: b) -> do
              validateSpec @a unboundedConfig $ f (conSpec x) (conSpec y)
          )
          [(i, j) | i <- [-3 .. 3], j <- [-3 .. 3]],
      testCase "on single concrete" $ do
        traverse_
          ( \x -> do
              validateSpec @a unboundedConfig $ f (conSpec x) (symSpec "a")
              validateSpec @a unboundedConfig $ f (symSpec "a") (conSpec x)
          )
          [-3 .. 3]
    ]

termRewritingTests :: Test
termRewritingTests =
  testGroup
    "TermRewriting"
    [ testGroup
        "Bool only"
        [ testProperty "Bool only random test" $
            mapSize (`min` 10) $
              ioProperty . \(x :: BoolOnlySpec) -> do
                validateSpec unboundedConfig x,
          testCase "Regression nested ite with (ite a (ite b c d) e) with b is true" $ do
            validateSpec @BoolOnlySpec
              unboundedConfig
              ( iteSpec
                  (symSpec "a" :: BoolOnlySpec)
                  ( iteSpec
                      (orSpec (notSpec (andSpec (symSpec "b1") (symSpec "b2"))) (symSpec "b2") :: BoolOnlySpec)
                      (symSpec "c")
                      (symSpec "d")
                  )
                  (symSpec "e")
              ),
          testCase "Regression for pevalImpliesTerm _ false should be false" $ do
            validateSpec @BoolOnlySpec
              unboundedConfig
              ( iteSpec
                  (symSpec "fbool" :: BoolOnlySpec)
                  ( notSpec
                      ( orSpec
                          (orSpec (notSpec (andSpec (symSpec "gbool" :: BoolOnlySpec) (symSpec "fbool" :: BoolOnlySpec))) (symSpec "gbool" :: BoolOnlySpec))
                          (orSpec (symSpec "abool" :: BoolOnlySpec) (notSpec (andSpec (symSpec "gbool" :: BoolOnlySpec) (symSpec "bbool" :: BoolOnlySpec))))
                      )
                  )
                  (symSpec "xxx" :: BoolOnlySpec)
              )
        ],
      testGroup
        "LIA"
        [ testProperty "LIA random test" $
            mapSize (`min` 10) $
              ioProperty . \(x :: LIAWithBoolSpec) -> do
                validateSpec unboundedConfig x,
          testCase "Regression nested ite with (ite a b (ite c d e)) with c implies a" $ do
            validateSpec @LIAWithBoolSpec
              unboundedConfig
              ( iteSpec
                  (notSpec (eqvSpec (symSpec "v" :: LIAWithBoolSpec) (conSpec 1 :: LIAWithBoolSpec) :: BoolWithLIASpec))
                  (symSpec "b")
                  ( iteSpec
                      (eqvSpec (symSpec "v" :: LIAWithBoolSpec) (conSpec 2 :: LIAWithBoolSpec) :: BoolWithLIASpec)
                      (symSpec "d")
                      (symSpec "d")
                  )
              )
        ],
      testGroup
        "Different sized signed BV"
        [ testProperty "Random test" $
            withMaxSuccess 1000 . mapSize (`min` 5) $
              ioProperty . \(x :: (DifferentSizeBVSpec IntN 4)) -> do
                validateSpec unboundedConfig x
        ],
      testGroup
        "Fixed sized signed BV"
        [ testProperty "Random test on IntN 1" $
            withMaxSuccess 200 . mapSize (`min` 5) $
              ioProperty . \(x :: (FixedSizedBVWithBoolSpec IntN 1)) -> do
                validateSpec unboundedConfig x,
          testProperty "Random test on IntN 2" $
            withMaxSuccess 200 . mapSize (`min` 5) $
              ioProperty . \(x :: (FixedSizedBVWithBoolSpec IntN 2)) -> do
                validateSpec unboundedConfig x,
          testProperty "Random test on IntN 4" $
            withMaxSuccess 200 . mapSize (`min` 5) $
              ioProperty . \(x :: (FixedSizedBVWithBoolSpec IntN 4)) -> do
                validateSpec unboundedConfig x,
          testProperty "Random test on IntN 63" $
            withMaxSuccess 200 . mapSize (`min` 1) $
              ioProperty . \(x :: (FixedSizedBVWithBoolSpec IntN 63)) -> do
                validateSpec unboundedConfig x,
          testProperty "Random test on IntN 64" $
            withMaxSuccess 200 . mapSize (`min` 1) $
              ioProperty . \(x :: (FixedSizedBVWithBoolSpec IntN 64)) -> do
                validateSpec unboundedConfig x,
          testProperty "Random test on IntN 65" $
            withMaxSuccess 200 . mapSize (`min` 1) $
              ioProperty . \(x :: (FixedSizedBVWithBoolSpec IntN 65)) -> do
                validateSpec unboundedConfig x,
          testProperty "Random test on IntN 128" $
            withMaxSuccess 200 . mapSize (`min` 1) $
              ioProperty . \(x :: (FixedSizedBVWithBoolSpec IntN 128)) -> do
                validateSpec unboundedConfig x
        ],
      testGroup
        "Different sized unsigned BV"
        [ testProperty "random test" $
            withMaxSuccess 1000 . mapSize (`min` 5) $
              ioProperty . \(x :: (DifferentSizeBVSpec WordN 4)) -> do
                validateSpec unboundedConfig x
        ],
      testGroup
        "Fixed sized unsigned BV"
        [ testProperty "Random test on WordN 1" $
            withMaxSuccess 200 . mapSize (`min` 5) $
              ioProperty . \(x :: (FixedSizedBVWithBoolSpec WordN 1)) -> do
                validateSpec unboundedConfig x,
          testProperty "Random test on WordN 2" $
            withMaxSuccess 200 . mapSize (`min` 5) $
              ioProperty . \(x :: (FixedSizedBVWithBoolSpec WordN 2)) -> do
                validateSpec unboundedConfig x,
          testProperty "Random test on WordN 4" $
            withMaxSuccess 200 . mapSize (`min` 5) $
              ioProperty . \(x :: (FixedSizedBVWithBoolSpec WordN 4)) -> do
                validateSpec unboundedConfig x,
          testProperty "Random test on WordN 63" $
            withMaxSuccess 200 . mapSize (`min` 1) $
              ioProperty . \(x :: (FixedSizedBVWithBoolSpec WordN 63)) -> do
                validateSpec unboundedConfig x,
          testProperty "Random test on WordN 64" $
            withMaxSuccess 200 . mapSize (`min` 1) $
              ioProperty . \(x :: (FixedSizedBVWithBoolSpec WordN 64)) -> do
                validateSpec unboundedConfig x,
          testProperty "Random test on WordN 65" $
            withMaxSuccess 200 . mapSize (`min` 1) $
              ioProperty . \(x :: (FixedSizedBVWithBoolSpec WordN 65)) -> do
                validateSpec unboundedConfig x,
          testProperty "Random test on WordN 128" $
            withMaxSuccess 200 . mapSize (`min` 1) $
              ioProperty . \(x :: (FixedSizedBVWithBoolSpec WordN 128)) -> do
                validateSpec unboundedConfig x
        ],
      testCase "Regression: shift twice and the sum of shift amount overflows" $ do
        validateSpec @(FixedSizedBVWithBoolSpec IntN 4)
          unboundedConfig
          ( shiftRightSpec
              (shiftRightSpec (symSpec "fint") (conSpec 0x5))
              (conSpec 0x5)
          ),
      testGroup
        "Regression for abs on unsigned BV"
        [ testCase "abs on negate" $
            validateSpec @(FixedSizedBVWithBoolSpec WordN 4)
              unboundedConfig
              (absNumSpec (uminusNumSpec (symSpec "a"))),
          testCase "abs on times negate" $
            validateSpec @(FixedSizedBVWithBoolSpec WordN 4)
              unboundedConfig
              (absNumSpec (timesNumSpec (symSpec "a") (uminusNumSpec (symSpec "b"))))
        ],
      testGroup
        "timesNumSpec on integer"
        [ testCase "times on both concrete" $ do
            traverse_
              (\(x, y) -> validateSpec @(GeneralSpec Integer) unboundedConfig $ timesNumSpec (conSpec x) (conSpec y))
              [(i, j) | i <- [-3 .. 3], j <- [-3 .. 3]],
          testCase "times on single concrete" $ do
            traverse_
              ( \x -> do
                  validateSpec @(GeneralSpec Integer) unboundedConfig $ timesNumSpec (conSpec x) (symSpec "a")
                  validateSpec @(GeneralSpec Integer) unboundedConfig $ timesNumSpec (symSpec "a") (conSpec x)
              )
              [-3 .. 3],
          testCase "Two times with two concrete combined" $ do
            traverse_
              ( \(x, y) -> do
                  validateSpec @(GeneralSpec Integer) unboundedConfig $ timesNumSpec (conSpec x) $ timesNumSpec (conSpec y) (symSpec "a")
                  validateSpec @(GeneralSpec Integer) unboundedConfig $ timesNumSpec (conSpec x) $ timesNumSpec (symSpec "a") (conSpec y)
                  validateSpec @(GeneralSpec Integer) unboundedConfig $ timesNumSpec (timesNumSpec (conSpec x) (symSpec "a")) (conSpec y)
                  validateSpec @(GeneralSpec Integer) unboundedConfig $ timesNumSpec (timesNumSpec (symSpec "a") (conSpec x)) (conSpec y)
              )
              [(i, j) | i <- [-3 .. 3], j <- [-3 .. 3]],
          testCase "Two times with one concrete" $ do
            traverse_
              ( \x -> do
                  validateSpec @(GeneralSpec Integer) unboundedConfig $ timesNumSpec (conSpec x) $ timesNumSpec (symSpec "b") (symSpec "a")
                  validateSpec @(GeneralSpec Integer) unboundedConfig $ timesNumSpec (symSpec "b") $ timesNumSpec (symSpec "a") (conSpec x)
                  validateSpec @(GeneralSpec Integer) unboundedConfig $ timesNumSpec (symSpec "b") $ timesNumSpec (conSpec x) (symSpec "a")
                  validateSpec @(GeneralSpec Integer) unboundedConfig $ timesNumSpec (timesNumSpec (conSpec x) (symSpec "a")) (symSpec "b")
                  validateSpec @(GeneralSpec Integer) unboundedConfig $ timesNumSpec (timesNumSpec (symSpec "a") (conSpec x)) (symSpec "b")
                  validateSpec @(GeneralSpec Integer) unboundedConfig $ timesNumSpec (timesNumSpec (symSpec "a") (symSpec "b")) (conSpec x)
              )
              [-3 .. 3],
          testCase "times and add with two concretes combined" $ do
            traverse_
              ( \(x, y) -> do
                  validateSpec @(GeneralSpec Integer) unboundedConfig $ timesNumSpec (conSpec x) $ addNumSpec (conSpec y) (symSpec "a")
                  validateSpec @(GeneralSpec Integer) unboundedConfig $ timesNumSpec (conSpec x) $ addNumSpec (symSpec "a") (conSpec y)
                  validateSpec @(GeneralSpec Integer) unboundedConfig $ timesNumSpec (addNumSpec (conSpec x) (symSpec "a")) (conSpec y)
                  validateSpec @(GeneralSpec Integer) unboundedConfig $ timesNumSpec (addNumSpec (symSpec "a") (conSpec x)) (conSpec y)
                  validateSpec @(GeneralSpec Integer) unboundedConfig $ addNumSpec (conSpec x) $ timesNumSpec (conSpec y) (symSpec "a")
                  validateSpec @(GeneralSpec Integer) unboundedConfig $ addNumSpec (conSpec x) $ timesNumSpec (symSpec "a") (conSpec y)
                  validateSpec @(GeneralSpec Integer) unboundedConfig $ addNumSpec (timesNumSpec (conSpec x) (symSpec "a")) (conSpec y)
                  validateSpec @(GeneralSpec Integer) unboundedConfig $ addNumSpec (timesNumSpec (symSpec "a") (conSpec x)) (conSpec y)
              )
              [(i, j) | i <- [-3 .. 3], j <- [-3 .. 3]],
          testCase "times concrete with uminusNumSpec symbolic" $ do
            traverse_
              ( \x -> do
                  validateSpec @(GeneralSpec Integer) unboundedConfig $ timesNumSpec (conSpec x) (uminusNumSpec $ symSpec "a")
                  validateSpec @(GeneralSpec Integer) unboundedConfig $ timesNumSpec (uminusNumSpec $ symSpec "a") (conSpec x)
              )
              [-3 .. 3]
        ],
      testGroup
        "divisions on integer"
        [ divisionTest @(GeneralSpec Integer) "div" divIntegralSpec,
          divisionTest @(GeneralSpec Integer) "mod" modIntegralSpec,
          divisionTest @(GeneralSpec Integer) "quot" quotIntegralSpec,
          divisionTest @(GeneralSpec Integer) "rem" remIntegralSpec
        ],
      testGroup
        "divisions on signed bv"
        [ divisionTest @(GeneralSpec (IntN 4)) "div" divBoundedIntegralSpec,
          divisionTest @(GeneralSpec (IntN 4)) "mod" modBoundedIntegralSpec,
          divisionTest @(GeneralSpec (IntN 4)) "quot" quotBoundedIntegralSpec,
          divisionTest @(GeneralSpec (IntN 4)) "rem" remBoundedIntegralSpec
        ],
      testGroup
        "divisions on unsigned bv"
        [ divisionTest @(GeneralSpec (WordN 4)) "div" divIntegralSpec,
          divisionTest @(GeneralSpec (WordN 4)) "mod" modIntegralSpec,
          divisionTest @(GeneralSpec (WordN 4)) "quot" quotIntegralSpec,
          divisionTest @(GeneralSpec (WordN 4)) "rem" remIntegralSpec
        ]
    ]
