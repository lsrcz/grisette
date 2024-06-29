{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Grisette.Core.Control.ExceptionTests (exceptionTests) where

import Control.Exception
  ( ArrayException (IndexOutOfBounds, UndefinedElement),
  )
import Control.Monad.Except (ExceptT (ExceptT))
import Grisette
  ( AssertionError (AssertionError),
    EvalSym (evalSym),
    ExtractSym (extractSym),
    LogicalOp (symNot),
    Mergeable (rootStrategy),
    MergingStrategy (SimpleStrategy),
    ModelOps (emptyModel),
    SimpleMergeable (mrgIte),
    Solvable (con),
    SymEq ((.==)),
    SymOrd (symCompare, (.<), (.<=), (.>), (.>=)),
    SymbolSetOps (emptySet),
    ToCon (toCon),
    ToSym (toSym),
    TransformError (transformError),
    Union,
    VerificationConditions (AssertionViolation, AssumptionViolation),
    mrgIf,
    mrgSingle,
    symAssert,
  )
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit ((@?=))

exceptionTests :: Test
exceptionTests =
  testGroup
    "Exception"
    [ testGroup
        "AssertionError"
        [ testCase "ToCon" $ do
            toCon AssertionError @?= Just AssertionError,
          testCase "ToSym" $ do
            toSym AssertionError @?= AssertionError,
          testCase "SymEq" $ do
            AssertionError .== AssertionError @?= con True,
          testCase "SymOrd" $ do
            AssertionError .<= AssertionError @?= con True
            AssertionError .< AssertionError @?= con False
            AssertionError .>= AssertionError @?= con True
            AssertionError .> AssertionError @?= con False
            AssertionError
              `symCompare` AssertionError
              @?= (mrgSingle EQ :: Union Ordering),
          testCase "GEvalSym" $ do
            evalSym False emptyModel AssertionError @?= AssertionError,
          testCase "GExtractSym" $ do
            extractSym AssertionError @?= emptySet,
          testCase "SimpleMergeable" $ do
            mrgIte "a" AssertionError AssertionError @?= AssertionError,
          testCase "Mergeable" $ do
            let SimpleStrategy s =
                  rootStrategy ::
                    MergingStrategy AssertionError
            s "a" AssertionError AssertionError @?= AssertionError,
          testCase "Transform AssertionError to VerificationConditions" $ do
            transformError AssertionError @?= AssertionViolation,
          testCase "Transform AssertionError to AssertionError" $ do
            transformError AssertionError @?= AssertionError,
          testCase "Transform ArrayException to AssertionError" $ do
            transformError (IndexOutOfBounds "") @?= AssertionError,
          testCase "Transform ArrayException to AssertionError" $ do
            transformError (UndefinedElement "") @?= AssertionError
        ],
      testGroup
        "VerificationConditions"
        [ testCase "ToCon" $ do
            toCon AssertionViolation @?= Just AssertionViolation
            toCon AssumptionViolation @?= Just AssumptionViolation,
          testCase "ToSym" $ do
            toSym AssertionViolation @?= AssertionViolation
            toSym AssumptionViolation @?= AssumptionViolation,
          testCase "SymEq" $ do
            AssertionViolation .== AssertionViolation @?= con True
            AssertionViolation .== AssumptionViolation @?= con False
            AssumptionViolation .== AssertionViolation @?= con False
            AssumptionViolation .== AssumptionViolation @?= con True,
          testCase "SymOrd" $ do
            AssertionViolation .<= AssertionViolation @?= con True
            AssertionViolation .< AssertionViolation @?= con False
            AssertionViolation .>= AssertionViolation @?= con True
            AssertionViolation .> AssertionViolation @?= con False
            AssertionViolation
              `symCompare` AssertionViolation
              @?= (mrgSingle EQ :: Union Ordering)

            AssertionViolation .<= AssumptionViolation @?= con True
            AssertionViolation .< AssumptionViolation @?= con True
            AssertionViolation .>= AssumptionViolation @?= con False
            AssertionViolation .> AssumptionViolation @?= con False
            AssertionViolation
              `symCompare` AssumptionViolation
              @?= (mrgSingle LT :: Union Ordering)

            AssumptionViolation .<= AssertionViolation @?= con False
            AssumptionViolation .< AssertionViolation @?= con False
            AssumptionViolation .>= AssertionViolation @?= con True
            AssumptionViolation .> AssertionViolation @?= con True
            AssumptionViolation
              `symCompare` AssertionViolation
              @?= (mrgSingle GT :: Union Ordering)

            AssumptionViolation .<= AssumptionViolation @?= con True
            AssumptionViolation .< AssumptionViolation @?= con False
            AssumptionViolation .>= AssumptionViolation @?= con True
            AssumptionViolation .> AssumptionViolation @?= con False
            AssumptionViolation
              `symCompare` AssumptionViolation
              @?= (mrgSingle EQ :: Union Ordering),
          testCase "GEvalSym" $ do
            evalSym False emptyModel AssertionViolation
              @?= AssertionViolation
            evalSym False emptyModel AssumptionViolation
              @?= AssumptionViolation,
          testCase "GExtractSym" $ do
            extractSym AssertionViolation @?= emptySet
            extractSym AssumptionViolation @?= emptySet,
          testCase "Mergeable" $ do
            mrgIf
              "a"
              (mrgSingle AssumptionViolation)
              (mrgSingle AssertionViolation)
              @?= ( mrgIf
                      (symNot "a")
                      (mrgSingle AssertionViolation)
                      (mrgSingle AssumptionViolation) ::
                      Union VerificationConditions
                  ),
          testCase
            "Transform VerificationConditions to VerificationConditions"
            $ do
              transformError AssertionViolation @?= AssertionViolation
              transformError AssumptionViolation @?= AssumptionViolation
        ],
      testCase "symAssert" $ do
        (symAssert "a" :: ExceptT VerificationConditions Union ())
          @?= ExceptT
            ( mrgIf
                (symNot "a")
                (mrgSingle $ Left AssertionViolation)
                (mrgSingle $ Right ())
            )
    ]
