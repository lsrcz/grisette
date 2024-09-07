{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Grisette.Backend.TermRewritingTests
  ( termRewritingTests,
    validateSpec,
    bitwuzlaConfig,
  )
where

import Data.Foldable (traverse_)
import GHC.TypeLits (KnownNat, type (<=))
import Grisette
  ( AlgReal,
    BitCast (bitCast),
    GrisetteSMTConfig,
    IEEEFPConstants
      ( fpMaxNormalized,
        fpMaxSubnormal,
        fpMinNormalized,
        fpMinSubnormal,
        fpNaN,
        fpNegativeInfinite,
        fpNegativeZero,
        fpPositiveInfinite,
        fpPositiveZero
      ),
    IEEEFPRoundingMode (rna, rne, rtn, rtp, rtz),
    ITEOp (symIte),
    IntN,
    LinkedRep,
    LogicalOp (symNot, true),
    Solvable (con),
    SymBool (SymBool),
    SymFP,
    SymIEEEFPTraits (symFpIsNaN),
    SymRep (SymType),
    WordN,
    bitwuzla,
    fpIsNaN,
    solve,
    z3,
  )
import Grisette.Backend.TermRewritingGen
  ( BoolOnlySpec,
    BoolWithLIASpec,
    DifferentSizeBVSpec,
    FPRoundingModeBoolOpSpec,
    FPRoundingModeSpec,
    FixedSizedBVWithBoolSpec,
    GeneralSpec,
    IEEEFPBoolOpSpec (IEEEFPBoolOpSpec),
    IEEEFPSpec,
    LIAWithBoolSpec,
    NRAWithBoolSpec,
    TermRewritingSpec
      ( conSpec,
        counterExample,
        norewriteVer,
        rewriteVer,
        same,
        symSpec,
        wrap
      ),
    absNumSpec,
    addNumSpec,
    andSpec,
    bitCastOrSpec,
    bitCastSpec,
    divIntegralSpec,
    eqvSpec,
    fpBinaryOpSpec,
    fpFMASpec,
    fpRoundingBinarySpec,
    fpRoundingUnaryOpSpec,
    fpTraitSpec,
    fromFPOrSpec,
    iteSpec,
    leOrdSpec,
    modIntegralSpec,
    mulNumSpec,
    negNumSpec,
    notSpec,
    orSpec,
    quotIntegralSpec,
    remIntegralSpec,
    shiftRightSpec,
    signumNumSpec,
    toFPSpec,
  )
import Grisette.Internal.Core.Data.Class.LogicalOp (LogicalOp ((.&&)))
import Grisette.Internal.Core.Data.Class.SymEq (SymEq ((./=), (.==)))
import Grisette.Internal.Core.Data.Class.SymIEEEFP
  ( SymIEEEFPTraits (symFpIsPositiveInfinite),
  )
import Grisette.Internal.SymPrim.FP
  ( ConvertibleBound (convertibleLowerBound, convertibleUpperBound),
    FP,
    FP32,
    FPRoundingMode,
    ValidFP,
    nextFP,
    prevFP,
  )
import Grisette.Internal.SymPrim.Prim.Term
  ( FPBinaryOp (FPMaximum, FPMaximumNumber, FPMinimum, FPMinimumNumber, FPRem),
    FPRoundingBinaryOp (FPAdd, FPDiv, FPMul, FPSub),
    FPRoundingUnaryOp (FPRoundToIntegral, FPSqrt),
    FPTrait (FPIsPositive),
    PEvalBitCastOrTerm,
    PEvalBitCastTerm,
    PEvalIEEEFPConvertibleTerm,
    SupportedNonFuncPrim,
    Term,
    conTerm,
    fpTraitTerm,
    iteTerm,
    notTerm,
    pformatTerm,
    ssymTerm,
  )
import Grisette.Internal.SymPrim.SymFP (SymFP32)
import Test.Framework (Test, TestName, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.HUnit (Assertion, assertFailure)
import Test.QuickCheck
  ( Arbitrary,
    elements,
    forAll,
    ioProperty,
    mapSize,
    vectorOf,
    withMaxSuccess,
    (==>),
  )
import Type.Reflection (Typeable, typeRep)

validateSpec' ::
  (TermRewritingSpec a av) =>
  GrisetteSMTConfig ->
  SymBool ->
  a ->
  Assertion
validateSpec' config precond a = do
  r <- solve config (precond .&& SymBool (counterExample a))
  rs <- solve config (precond .&& SymBool (same a))
  case (r, rs) of
    (Left _, Right _) -> do
      return ()
    (Left _, Left err) -> do
      print err
      assertFailure $
        "Bad rewriting with unsolvable formula: "
          ++ pformatTerm (norewriteVer a)
          ++ " was rewritten to "
          ++ pformatTerm (rewriteVer a)
          ++ " under precondition"
          ++ show precond
          ++ " corresponding same formula:"
          ++ pformatTerm (same a)
    (Right m, _) -> do
      assertFailure $
        "With model"
          ++ show m
          ++ "Bad rewriting: "
          ++ pformatTerm (norewriteVer a)
          ++ " was rewritten to "
          ++ pformatTerm (rewriteVer a)
          ++ " under precondition"
          ++ show precond
          ++ " corresponding cex formula:"
          ++ pformatTerm (counterExample a)
          ++ "\n"
          ++ show (norewriteVer a)
          ++ "\n"
          ++ show (rewriteVer a)
          ++ "\n"
          ++ show (counterExample a)

validateSpec ::
  (TermRewritingSpec a av) =>
  GrisetteSMTConfig ->
  a ->
  Assertion
validateSpec config = validateSpec' config true

bitwuzlaConfig :: IO (Maybe GrisetteSMTConfig)
bitwuzlaConfig = do
  v <-
    solve bitwuzla $
      (("x" :: SymFP32) ./= "x")
        .&& symNot (symFpIsPositiveInfinite (con $ -4.7e-38 :: SymFP32))
        .&& ( symIte
                "bool"
                (con fpPositiveInfinite :: SymFP32)
                (con fpNegativeInfinite)
                .== "m"
            )
  case v of
    Left _ -> return Nothing
    Right _ -> return $ Just bitwuzla

onlyWhenBitwuzlaIsAvailable :: (GrisetteSMTConfig -> IO ()) -> IO ()
onlyWhenBitwuzlaIsAvailable action = do
  config <- bitwuzlaConfig
  case config of
    Just config -> action config
    Nothing ->
      putStrLn $
        "bitwuzla isn't available in the system, or the dependent sbv"
          <> " library does not work well with it. This test is marked as "
          <> " success."

unboundedConfig = z3

divisionTest ::
  forall a b.
  (TermRewritingSpec a b, Enum b, Num b) =>
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
        "NRA"
        [ testProperty "NRA random test" $
            mapSize (`min` 5) $
              ioProperty . \(x :: NRAWithBoolSpec) ->
                validateSpec unboundedConfig x
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
              (absNumSpec (negNumSpec (symSpec "a"))),
          testCase "abs on times negate" $
            validateSpec @(FixedSizedBVWithBoolSpec WordN 4)
              unboundedConfig
              (absNumSpec (mulNumSpec (symSpec "a") (negNumSpec (symSpec "b"))))
        ],
      testGroup
        "mulNumSpec on integer"
        [ testCase "times on both concrete" $ do
            traverse_
              (\(x, y) -> validateSpec @(GeneralSpec Integer) unboundedConfig $ mulNumSpec (conSpec x) (conSpec y))
              [(i, j) | i <- [-3 .. 3], j <- [-3 .. 3]],
          testCase "times on single concrete" $ do
            traverse_
              ( \x -> do
                  validateSpec @(GeneralSpec Integer) unboundedConfig $ mulNumSpec (conSpec x) (symSpec "a")
                  validateSpec @(GeneralSpec Integer) unboundedConfig $ mulNumSpec (symSpec "a") (conSpec x)
              )
              [-3 .. 3],
          testCase "Two times with two concrete combined" $ do
            traverse_
              ( \(x, y) -> do
                  validateSpec @(GeneralSpec Integer) unboundedConfig $ mulNumSpec (conSpec x) $ mulNumSpec (conSpec y) (symSpec "a")
                  validateSpec @(GeneralSpec Integer) unboundedConfig $ mulNumSpec (conSpec x) $ mulNumSpec (symSpec "a") (conSpec y)
                  validateSpec @(GeneralSpec Integer) unboundedConfig $ mulNumSpec (mulNumSpec (conSpec x) (symSpec "a")) (conSpec y)
                  validateSpec @(GeneralSpec Integer) unboundedConfig $ mulNumSpec (mulNumSpec (symSpec "a") (conSpec x)) (conSpec y)
              )
              [(i, j) | i <- [-3 .. 3], j <- [-3 .. 3]],
          testCase "Two times with one concrete" $ do
            traverse_
              ( \x -> do
                  validateSpec @(GeneralSpec Integer) unboundedConfig $ mulNumSpec (conSpec x) $ mulNumSpec (symSpec "b") (symSpec "a")
                  validateSpec @(GeneralSpec Integer) unboundedConfig $ mulNumSpec (symSpec "b") $ mulNumSpec (symSpec "a") (conSpec x)
                  validateSpec @(GeneralSpec Integer) unboundedConfig $ mulNumSpec (symSpec "b") $ mulNumSpec (conSpec x) (symSpec "a")
                  validateSpec @(GeneralSpec Integer) unboundedConfig $ mulNumSpec (mulNumSpec (conSpec x) (symSpec "a")) (symSpec "b")
                  validateSpec @(GeneralSpec Integer) unboundedConfig $ mulNumSpec (mulNumSpec (symSpec "a") (conSpec x)) (symSpec "b")
                  validateSpec @(GeneralSpec Integer) unboundedConfig $ mulNumSpec (mulNumSpec (symSpec "a") (symSpec "b")) (conSpec x)
              )
              [-3 .. 3],
          testCase "times and add with two concretes combined" $ do
            traverse_
              ( \(x, y) -> do
                  validateSpec @(GeneralSpec Integer) unboundedConfig $ mulNumSpec (conSpec x) $ addNumSpec (conSpec y) (symSpec "a")
                  validateSpec @(GeneralSpec Integer) unboundedConfig $ mulNumSpec (conSpec x) $ addNumSpec (symSpec "a") (conSpec y)
                  validateSpec @(GeneralSpec Integer) unboundedConfig $ mulNumSpec (addNumSpec (conSpec x) (symSpec "a")) (conSpec y)
                  validateSpec @(GeneralSpec Integer) unboundedConfig $ mulNumSpec (addNumSpec (symSpec "a") (conSpec x)) (conSpec y)
                  validateSpec @(GeneralSpec Integer) unboundedConfig $ addNumSpec (conSpec x) $ mulNumSpec (conSpec y) (symSpec "a")
                  validateSpec @(GeneralSpec Integer) unboundedConfig $ addNumSpec (conSpec x) $ mulNumSpec (symSpec "a") (conSpec y)
                  validateSpec @(GeneralSpec Integer) unboundedConfig $ addNumSpec (mulNumSpec (conSpec x) (symSpec "a")) (conSpec y)
                  validateSpec @(GeneralSpec Integer) unboundedConfig $ addNumSpec (mulNumSpec (symSpec "a") (conSpec x)) (conSpec y)
              )
              [(i, j) | i <- [-3 .. 3], j <- [-3 .. 3]],
          testCase "times concrete with negNumSpec symbolic" $ do
            traverse_
              ( \x -> do
                  validateSpec @(GeneralSpec Integer) unboundedConfig $ mulNumSpec (conSpec x) (negNumSpec $ symSpec "a")
                  validateSpec @(GeneralSpec Integer) unboundedConfig $ mulNumSpec (negNumSpec $ symSpec "a") (conSpec x)
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
        [ divisionTest @(GeneralSpec (IntN 4)) "div" divIntegralSpec,
          divisionTest @(GeneralSpec (IntN 4)) "mod" modIntegralSpec,
          divisionTest @(GeneralSpec (IntN 4)) "quot" quotIntegralSpec,
          divisionTest @(GeneralSpec (IntN 4)) "rem" remIntegralSpec
        ],
      testGroup
        "divisions on unsigned bv"
        [ divisionTest @(GeneralSpec (WordN 4)) "div" divIntegralSpec,
          divisionTest @(GeneralSpec (WordN 4)) "mod" modIntegralSpec,
          divisionTest @(GeneralSpec (WordN 4)) "quot" quotIntegralSpec,
          divisionTest @(GeneralSpec (WordN 4)) "rem" remIntegralSpec
        ],
      testGroup
        "FP"
        [ testCase "0.0 == -0.0" $
            onlyWhenBitwuzlaIsAvailable
              ( `validateSpec`
                  ( eqvSpec
                      (conSpec 0.0 :: IEEEFPSpec 5 11)
                      (conSpec $ -0.0) ::
                      IEEEFPBoolOpSpec 5 11
                  )
              ),
          testCase "-0.0 <= -0.0" $
            onlyWhenBitwuzlaIsAvailable
              ( `validateSpec`
                  ( leOrdSpec
                      (conSpec 0.0 :: IEEEFPSpec 5 11)
                      (conSpec $ -0.0) ::
                      IEEEFPBoolOpSpec 5 11
                  )
              ),
          testCase "is_pos(nan)" $
            onlyWhenBitwuzlaIsAvailable
              ( `validateSpec`
                  ( fpTraitSpec
                      FPIsPositive
                      (conSpec fpNaN :: IEEEFPSpec 5 11) ::
                      IEEEFPBoolOpSpec 5 11
                  )
              ),
          testCase "is_pos(+inf)" $
            onlyWhenBitwuzlaIsAvailable
              ( `validateSpec`
                  ( fpTraitSpec
                      FPIsPositive
                      ( iteSpec
                          (symSpec "bool" :: BoolOnlySpec)
                          (conSpec fpNegativeInfinite)
                          (conSpec fpPositiveInfinite) ::
                          IEEEFPSpec 5 11
                      ) ::
                      IEEEFPBoolOpSpec 5 11
                  )
              ),
          testCase "regression 2" $
            onlyWhenBitwuzlaIsAvailable
              ( `validateSpec`
                  ( eqvSpec
                      (signumNumSpec (conSpec (1.175e-38) :: IEEEFPSpec 5 11))
                      (symSpec "b") ::
                      IEEEFPBoolOpSpec 5 11
                  )
              ),
          testCase "test sbv bug mitigation sbv#702" $
            onlyWhenBitwuzlaIsAvailable
              ( flip validateSpec $
                  IEEEFPBoolOpSpec
                    ( fpTraitTerm
                        FPIsPositive
                        ( iteTerm
                            (ssymTerm "bool")
                            (conTerm fpNegativeInfinite :: Term FP32)
                            (conTerm fpPositiveInfinite :: Term FP32)
                        )
                    )
                    (notTerm $ ssymTerm "bool")
              ),
          testProperty "FP32BoolOp" $
            withMaxSuccess 1000 . mapSize (`min` 10) $
              ioProperty . \(x :: IEEEFPBoolOpSpec 5 11) ->
                onlyWhenBitwuzlaIsAvailable (`validateSpec` x),
          testProperty "FPRoundingModeBoolOpSpec" $
            mapSize (`min` 10) $
              ioProperty . \(x :: FPRoundingModeBoolOpSpec) ->
                onlyWhenBitwuzlaIsAvailable (`validateSpec` x),
          testGroup "fpBinaryOp" $ do
            op <-
              [FPMaximum, FPMinimum, FPMaximumNumber, FPMinimumNumber, FPRem]
            return $ testCase (show op) $ do
              let lst =
                    [ conSpec fpNegativeInfinite,
                      conSpec fpPositiveInfinite,
                      conSpec fpNaN,
                      conSpec fpPositiveZero,
                      conSpec fpNegativeZero,
                      conSpec 1,
                      conSpec (-1),
                      symSpec "a",
                      symSpec "b"
                    ]
              let ps =
                    [ fpBinaryOpSpec op l r :: IEEEFPSpec 4 4
                      | l <- lst,
                        r <- lst
                    ]
              traverse_ (validateSpec z3) ps,
          testGroup "RoundingOp" $ do
            let rdgen =
                  elements
                    [ conSpec rna :: FPRoundingModeSpec,
                      conSpec rne,
                      conSpec rtz,
                      conSpec rtp,
                      conSpec rtn
                    ]
            let vgen =
                  elements
                    [ conSpec fpNegativeInfinite :: IEEEFPSpec 4 4,
                      conSpec fpPositiveInfinite,
                      conSpec fpNaN,
                      conSpec fpPositiveZero,
                      conSpec fpNegativeZero,
                      conSpec 120,
                      conSpec 60,
                      conSpec 1,
                      conSpec 2,
                      conSpec 3,
                      conSpec 4,
                      conSpec (-1),
                      conSpec (-2),
                      conSpec (-3),
                      conSpec (-4),
                      conSpec 1.5625e-2,
                      conSpec 2.4e2,
                      conSpec 1.953125e-3,
                      conSpec 1.3671875e-2,
                      symSpec "a",
                      symSpec "b"
                    ]
            [ testGroup "fpRoundingUnaryOp" $ do
                op <- [FPSqrt, FPRoundToIntegral]
                return $
                  testProperty (show op) $
                    forAll rdgen $ \rd ->
                      forAll vgen $ \v ->
                        ioProperty $
                          validateSpec z3 $
                            fpRoundingUnaryOpSpec op rd v,
              testGroup "fpRoundingBinaryOp" $ do
                op <- [FPAdd, FPSub, FPMul, FPDiv]
                return $
                  testProperty (show op) $
                    forAll rdgen $ \rd ->
                      forAll (vectorOf 2 vgen) $ \[l, r] ->
                        ioProperty $
                          validateSpec z3 $
                            fpRoundingBinarySpec op rd l r,
              testProperty "fma" $
                forAll rdgen $ \rd ->
                  forAll (vectorOf 3 vgen) $ \[x, y, z] ->
                    ioProperty $
                      validateSpec z3 $
                        fpFMASpec rd x y z
              ]
        ],
      testGroup "bitCast" $ do
        let bitCastCase ::
              forall a b.
              ( Arbitrary a,
                PEvalBitCastTerm a b,
                SupportedNonFuncPrim a,
                Show a
              ) =>
              Test
            bitCastCase = testProperty
              (show (typeRep @a) <> " -> " <> show (typeRep @b))
              $ \x ->
                withMaxSuccess 10 . ioProperty $
                  validateSpec
                    z3
                    ( bitCastSpec (conSpec x :: GeneralSpec a) ::
                        GeneralSpec b
                    )
        let fromFPCase ::
              forall a b.
              ( Arbitrary a,
                Arbitrary b,
                PEvalBitCastOrTerm a b,
                RealFloat a,
                SupportedNonFuncPrim a,
                SupportedNonFuncPrim b,
                Show b,
                Show a
              ) =>
              Test
            fromFPCase = testProperty
              (show (typeRep @a) <> " -> " <> show (typeRep @b))
              $ \d x ->
                withMaxSuccess 10 . (not (isNaN x) ==>) . ioProperty $
                  validateSpec
                    z3
                    ( bitCastOrSpec
                        (conSpec d :: GeneralSpec b)
                        (conSpec x :: GeneralSpec a)
                    )
        let toFPCase ::
              forall a b.
              ( Arbitrary a,
                PEvalBitCastTerm a b,
                RealFloat b,
                SupportedNonFuncPrim a,
                Show a
              ) =>
              Test
            toFPCase = testProperty
              (show (typeRep @a) <> " -> " <> show (typeRep @b))
              $ \x ->
                withMaxSuccess 10
                  . (not (isNaN (bitCast x :: b)) ==>)
                  . ioProperty
                  $ validateSpec
                    z3
                    ( bitCastSpec (conSpec x :: GeneralSpec a) ::
                        GeneralSpec b
                    )
        [ bitCastCase @(IntN 4) @(WordN 4),
          bitCastCase @(WordN 4) @(IntN 4),
          bitCastCase @(IntN 1) @Bool,
          bitCastCase @(WordN 1) @Bool,
          bitCastCase @Bool @(IntN 1),
          bitCastCase @Bool @(WordN 1),
          fromFPCase @(FP 3 5) @(IntN 8),
          fromFPCase @(FP 3 5) @(WordN 8),
          toFPCase @(IntN 8) @(FP 3 5),
          toFPCase @(WordN 8) @(FP 3 5)
          ],
      testGroup "FPConvertible" $ do
        let fromFPAssertion ::
              forall eb sb spec b.
              ( ValidFP eb sb,
                PEvalIEEEFPConvertibleTerm b,
                TermRewritingSpec spec b
              ) =>
              b ->
              FPRoundingMode ->
              FP eb sb ->
              IO ()
            fromFPAssertion d rd x
              | fpIsNaN x = return ()
              | otherwise =
                  validateSpec'
                    z3
                    ( con x
                        .== p
                        .&& symNot (symFpIsNaN p)
                    )
                    ( fromFPOrSpec
                        (conSpec d :: spec)
                        (conSpec rd :: GeneralSpec FPRoundingMode)
                        ( wrap (ssymTerm "p") (conTerm x) ::
                            GeneralSpec (FP eb sb)
                        ) ::
                        spec
                    )
              where
                p = "p" :: SymFP eb sb
            fromFPAssertionDirect ::
              forall eb sb spec b.
              ( ValidFP eb sb,
                PEvalIEEEFPConvertibleTerm b,
                TermRewritingSpec spec b
              ) =>
              b ->
              FPRoundingMode ->
              FP eb sb ->
              IO ()
            fromFPAssertionDirect d rd x =
              validateSpec
                z3
                ( fromFPOrSpec
                    (conSpec d :: spec)
                    (conSpec rd :: GeneralSpec FPRoundingMode)
                    (conSpec x :: GeneralSpec (FP eb sb))
                )
            fromFPCase ::
              forall eb sb spec b.
              ( ValidFP eb sb,
                Arbitrary b,
                PEvalIEEEFPConvertibleTerm b,
                TermRewritingSpec spec b,
                Show b
              ) =>
              Bool ->
              Test
            fromFPCase direct = testProperty
              (show (typeRep @(FP eb sb)) <> " -> " <> show (typeRep @b))
              $ \(d :: b) rd (x :: FP eb sb) ->
                withMaxSuccess 10 . ioProperty $
                  ( if direct
                      then fromFPAssertionDirect @eb @sb @spec
                      else fromFPAssertion @eb @sb @spec
                  )
                    d
                    rd
                    x
            toFPAssertion ::
              forall eb sb b bs.
              ( ValidFP eb sb,
                PEvalIEEEFPConvertibleTerm b,
                LinkedRep b bs,
                Solvable b bs,
                SupportedNonFuncPrim b,
                SymEq bs
              ) =>
              FPRoundingMode ->
              b ->
              IO ()
            toFPAssertion rd x =
              validateSpec'
                z3
                ((con x :: SymType b) .== "p")
                ( toFPSpec
                    (conSpec rd :: GeneralSpec FPRoundingMode)
                    (wrap (ssymTerm "p") (conTerm x) :: GeneralSpec b) ::
                    IEEEFPSpec eb sb
                )
            toFPAssertionFP ::
              forall eb sb eb0 sb0.
              ( ValidFP eb sb,
                ValidFP eb0 sb0
              ) =>
              FPRoundingMode ->
              FP eb0 sb0 ->
              IO ()
            toFPAssertionFP _ x | fpIsNaN x = return ()
            toFPAssertionFP rd x =
              validateSpec'
                z3
                ( con x
                    .== p
                    .&& symNot (symFpIsNaN p)
                )
                ( toFPSpec
                    (conSpec rd :: GeneralSpec FPRoundingMode)
                    ( wrap (ssymTerm "p") (conTerm x) ::
                        GeneralSpec (FP eb0 sb0)
                    ) ::
                    IEEEFPSpec eb sb
                )
              where
                p = "p" :: SymFP eb0 sb0
            toFPCase ::
              forall eb sb b bs.
              ( ValidFP eb sb,
                Arbitrary b,
                PEvalIEEEFPConvertibleTerm b,
                SupportedNonFuncPrim b,
                LinkedRep b bs,
                Solvable b bs,
                SymEq bs,
                Show b
              ) =>
              Test
            toFPCase = testProperty
              (show (typeRep @b) <> " -> " <> show (typeRep @(FP eb sb)))
              $ \rd (x :: b) ->
                withMaxSuccess 10 . ioProperty $
                  toFPAssertion @eb @sb rd x
            toFPCaseFP ::
              forall eb sb eb0 sb0.
              ( ValidFP eb sb,
                ValidFP eb0 sb0
              ) =>
              Test
            toFPCaseFP = testProperty
              ( show (typeRep @(FP eb0 sb0))
                  <> " -> "
                  <> show (typeRep @(FP eb sb))
              )
              $ \rd (x :: b) ->
                withMaxSuccess 10 . ioProperty $
                  toFPAssertionFP @eb @sb @eb0 @sb0 rd x
            specialFps :: (ValidFP eb sb) => [FP eb sb]
            specialFps =
              [ fpPositiveZero,
                fpNegativeZero,
                fpPositiveInfinite,
                fpNegativeInfinite,
                fpNaN,
                fpMaxNormalized,
                fpMinNormalized,
                fpMaxSubnormal,
                fpMinSubnormal
              ]
            boundFps ::
              (ConvertibleBound bv, KnownNat n, 1 <= n, ValidFP eb sb) =>
              bv n ->
              FPRoundingMode ->
              [FP eb sb]
            boundFps n mode =
              [ convertibleLowerBound n mode,
                convertibleUpperBound n mode
              ]
            fps ::
              (ConvertibleBound bv, KnownNat n, 1 <= n, ValidFP eb sb) =>
              bv n ->
              FPRoundingMode ->
              [FP eb sb]
            fps n mode =
              specialFps
                ++ boundFps n mode
                ++ (nextFP <$> boundFps n mode)
                ++ (prevFP <$> boundFps n mode)
            boundedFromFPCase ::
              forall bv n eb sb.
              ( ConvertibleBound bv,
                KnownNat n,
                1 <= n,
                ValidFP eb sb,
                PEvalIEEEFPConvertibleTerm (bv n),
                SupportedNonFuncPrim (bv n),
                Num (bv n),
                Typeable bv
              ) =>
              FPRoundingMode ->
              Test
            boundedFromFPCase mode =
              testCase (show (typeRep @bv) ++ "/" ++ show mode) $
                mapM_
                  ( fromFPAssertion @eb @sb @(GeneralSpec (bv n))
                      123
                      mode
                  )
                  (fps (undefined :: (bv n)) mode)
            boundedFromFPTestGroup ::
              forall n eb sb.
              ( KnownNat n,
                1 <= n,
                ValidFP eb sb
              ) =>
              Test
            boundedFromFPTestGroup =
              testGroup
                ( show (typeRep @(FP eb sb))
                    ++ " -> "
                    ++ show (typeRep @(IntN n))
                    ++ "/"
                    ++ show (typeRep @(WordN n))
                )
                $ do
                  mode <- [rna, rne, rtz, rtp, rtn]
                  [ boundedFromFPCase @IntN @n @eb @sb mode,
                    boundedFromFPCase @WordN @n @eb @sb mode
                    ]
        [ -- z3 is buggy with the indirect encoding
          -- https://github.com/Z3Prover/z3/issues/7321
          fromFPCase @4 @4 @(GeneralSpec AlgReal) True,
          toFPCase @4 @4 @AlgReal,
          testCase "FP 4 4 -> Integer" $ do
            sequence_ $
              (fromFPAssertionDirect @4 @4 @(GeneralSpec Integer) 0)
                <$> [rna, rne, rtz, rtp, rtn]
                <*> (specialFps ++ ((/ 4) . fromIntegral <$> [-7 .. 7])),
          toFPCase @4 @4 @Integer,
          fromFPCase @4 @4 @(IEEEFPSpec 3 3) False,
          toFPCaseFP @4 @4 @3 @3,
          fromFPCase @4 @4 @(IEEEFPSpec 5 5) False,
          toFPCaseFP @4 @4 @5 @5,
          toFPCase @4 @4 @(WordN 8),
          toFPCase @4 @4 @(IntN 8),
          boundedFromFPTestGroup @32 @4 @4,
          boundedFromFPTestGroup @12 @4 @16,
          boundedFromFPTestGroup @12 @4 @12,
          boundedFromFPTestGroup @12 @4 @11,
          boundedFromFPTestGroup @12 @4 @10,
          boundedFromFPTestGroup @12 @4 @9,
          boundedFromFPTestGroup @12 @4 @2,
          boundedFromFPTestGroup @10 @4 @16,
          boundedFromFPTestGroup @10 @4 @10,
          boundedFromFPTestGroup @10 @4 @9,
          boundedFromFPTestGroup @10 @4 @8,
          boundedFromFPTestGroup @10 @4 @7,
          boundedFromFPTestGroup @10 @4 @2,
          boundedFromFPTestGroup @9 @4 @16,
          boundedFromFPTestGroup @9 @4 @9,
          boundedFromFPTestGroup @9 @4 @8,
          boundedFromFPTestGroup @9 @4 @7,
          boundedFromFPTestGroup @9 @4 @6,
          boundedFromFPTestGroup @9 @4 @2,
          boundedFromFPTestGroup @8 @4 @16,
          boundedFromFPTestGroup @8 @4 @8,
          boundedFromFPTestGroup @8 @4 @7,
          boundedFromFPTestGroup @8 @4 @6,
          boundedFromFPTestGroup @8 @4 @5,
          boundedFromFPTestGroup @8 @4 @2,
          boundedFromFPTestGroup @7 @4 @16,
          boundedFromFPTestGroup @7 @4 @7,
          boundedFromFPTestGroup @7 @4 @6,
          boundedFromFPTestGroup @7 @4 @5,
          boundedFromFPTestGroup @7 @4 @4,
          boundedFromFPTestGroup @7 @4 @2,
          boundedFromFPTestGroup @5 @4 @16,
          boundedFromFPTestGroup @5 @4 @5,
          boundedFromFPTestGroup @5 @4 @4,
          boundedFromFPTestGroup @5 @4 @3,
          boundedFromFPTestGroup @5 @4 @2
          ]
    ]
