{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Grisette.Core.Control.ExceptionTests (exceptionTests) where

import Control.Exception
  ( ArrayException (IndexOutOfBounds, UndefinedElement),
  )
import Control.Monad.Except (ExceptT (ExceptT))
import Grisette
  ( AsKey (AsKey),
    AssertionError (AssertionError),
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
import Grisette.Internal.Core.Data.Class.AsKey (AsKey1 (AsKey1))
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
            AsKey (AssertionError .== AssertionError) @?= con True,
          testCase "SymOrd" $ do
            AsKey (AssertionError .<= AssertionError) @?= con True
            AsKey (AssertionError .< AssertionError) @?= con False
            AsKey (AssertionError .>= AssertionError) @?= con True
            AsKey (AssertionError .> AssertionError) @?= con False
            AsKey1 (AssertionError `symCompare` AssertionError)
              @?= (mrgSingle EQ :: AsKey1 Union Ordering),
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
            AsKey (AssertionViolation .== AssertionViolation) @?= con True
            AsKey (AssertionViolation .== AssumptionViolation) @?= con False
            AsKey (AssumptionViolation .== AssertionViolation) @?= con False
            AsKey (AssumptionViolation .== AssumptionViolation) @?= con True,
          testCase "SymOrd" $ do
            AsKey (AssertionViolation .<= AssertionViolation) @?= con True
            AsKey (AssertionViolation .< AssertionViolation) @?= con False
            AsKey (AssertionViolation .>= AssertionViolation) @?= con True
            AsKey (AssertionViolation .> AssertionViolation) @?= con False
            AsKey1 (AssertionViolation `symCompare` AssertionViolation)
              @?= (mrgSingle EQ :: AsKey1 Union Ordering)

            AsKey (AssertionViolation .<= AssumptionViolation) @?= con True
            AsKey (AssertionViolation .< AssumptionViolation) @?= con True
            AsKey (AssertionViolation .>= AssumptionViolation) @?= con False
            AsKey (AssertionViolation .> AssumptionViolation) @?= con False
            AsKey1 (AssertionViolation `symCompare` AssumptionViolation)
              @?= (mrgSingle LT :: AsKey1 Union Ordering)

            AsKey (AssumptionViolation .<= AssertionViolation) @?= con False
            AsKey (AssumptionViolation .< AssertionViolation) @?= con False
            AsKey (AssumptionViolation .>= AssertionViolation) @?= con True
            AsKey (AssumptionViolation .> AssertionViolation) @?= con True
            AsKey1 (AssumptionViolation `symCompare` AssertionViolation)
              @?= (mrgSingle GT :: AsKey1 Union Ordering)

            AsKey (AssumptionViolation .<= AssumptionViolation) @?= con True
            AsKey (AssumptionViolation .< AssumptionViolation) @?= con False
            AsKey (AssumptionViolation .>= AssumptionViolation) @?= con True
            AsKey (AssumptionViolation .> AssumptionViolation) @?= con False
            AsKey1 (AssumptionViolation `symCompare` AssumptionViolation)
              @?= (mrgSingle EQ :: AsKey1 Union Ordering),
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
                      AsKey1 Union VerificationConditions
                  ),
          testCase
            "Transform VerificationConditions to VerificationConditions"
            $ do
              transformError AssertionViolation @?= AssertionViolation
              transformError AssumptionViolation @?= AssumptionViolation
        ],
      testCase "symAssert" $ do
        (symAssert "a" :: ExceptT VerificationConditions (AsKey1 Union) ())
          @?= ExceptT
            ( mrgIf
                (symNot "a")
                (mrgSingle $ Left AssertionViolation)
                (mrgSingle $ Right ())
            )
    ]
