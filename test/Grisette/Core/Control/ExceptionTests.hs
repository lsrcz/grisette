{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Grisette.Core.Control.ExceptionTests (exceptionTests) where

import Control.Exception
  ( ArrayException (IndexOutOfBounds, UndefinedElement),
  )
import Control.Monad.Except (ExceptT (ExceptT))
import Grisette.Core.Control.Exception
  ( AssertionError (AssertionError),
    VerificationConditions (AssertionViolation, AssumptionViolation),
  )
import Grisette.Core.Control.Monad.UnionM (UnionM)
import Grisette.Core.Data.Class.Error
  ( TransformError (transformError),
    symAssert,
  )
import Grisette.Core.Data.Class.EvaluateSym
  ( EvaluateSym (evaluateSym),
  )
import Grisette.Core.Data.Class.ExtractSymbolics
  ( ExtractSymbolics (extractSymbolics),
  )
import Grisette.Core.Data.Class.LogicalOp (LogicalOp (symNot))
import Grisette.Core.Data.Class.Mergeable
  ( Mergeable (rootStrategy),
    MergingStrategy (SimpleStrategy),
  )
import Grisette.Core.Data.Class.ModelOps
  ( ModelOps (emptyModel),
    SymbolSetOps (emptySet),
  )
import Grisette.Core.Data.Class.SEq (SEq ((.==)))
import Grisette.Core.Data.Class.SOrd
  ( SOrd (symCompare, (.<), (.<=), (.>), (.>=)),
  )
import Grisette.Core.Data.Class.SimpleMergeable
  ( SimpleMergeable (mrgIte),
    mrgIf,
  )
import Grisette.Core.Data.Class.Solvable (Solvable (con))
import Grisette.Core.Data.Class.ToCon (ToCon (toCon))
import Grisette.Core.Data.Class.ToSym (ToSym (toSym))
import Grisette.Core.Data.Class.TryMerge
  ( mrgPure,
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
          testCase "SEq" $ do
            AssertionError .== AssertionError @?= con True,
          testCase "SOrd" $ do
            AssertionError .<= AssertionError @?= con True
            AssertionError .< AssertionError @?= con False
            AssertionError .>= AssertionError @?= con True
            AssertionError .> AssertionError @?= con False
            AssertionError
              `symCompare` AssertionError
              @?= (mrgPure EQ :: UnionM Ordering),
          testCase "GEvaluateSym" $ do
            evaluateSym False emptyModel AssertionError @?= AssertionError,
          testCase "GExtractSymbolics" $ do
            extractSymbolics AssertionError @?= emptySet,
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
          testCase "SEq" $ do
            AssertionViolation .== AssertionViolation @?= con True
            AssertionViolation .== AssumptionViolation @?= con False
            AssumptionViolation .== AssertionViolation @?= con False
            AssumptionViolation .== AssumptionViolation @?= con True,
          testCase "SOrd" $ do
            AssertionViolation .<= AssertionViolation @?= con True
            AssertionViolation .< AssertionViolation @?= con False
            AssertionViolation .>= AssertionViolation @?= con True
            AssertionViolation .> AssertionViolation @?= con False
            AssertionViolation
              `symCompare` AssertionViolation
              @?= (mrgPure EQ :: UnionM Ordering)

            AssertionViolation .<= AssumptionViolation @?= con True
            AssertionViolation .< AssumptionViolation @?= con True
            AssertionViolation .>= AssumptionViolation @?= con False
            AssertionViolation .> AssumptionViolation @?= con False
            AssertionViolation
              `symCompare` AssumptionViolation
              @?= (mrgPure LT :: UnionM Ordering)

            AssumptionViolation .<= AssertionViolation @?= con False
            AssumptionViolation .< AssertionViolation @?= con False
            AssumptionViolation .>= AssertionViolation @?= con True
            AssumptionViolation .> AssertionViolation @?= con True
            AssumptionViolation
              `symCompare` AssertionViolation
              @?= (mrgPure GT :: UnionM Ordering)

            AssumptionViolation .<= AssumptionViolation @?= con True
            AssumptionViolation .< AssumptionViolation @?= con False
            AssumptionViolation .>= AssumptionViolation @?= con True
            AssumptionViolation .> AssumptionViolation @?= con False
            AssumptionViolation
              `symCompare` AssumptionViolation
              @?= (mrgPure EQ :: UnionM Ordering),
          testCase "GEvaluateSym" $ do
            evaluateSym False emptyModel AssertionViolation
              @?= AssertionViolation
            evaluateSym False emptyModel AssumptionViolation
              @?= AssumptionViolation,
          testCase "GExtractSymbolics" $ do
            extractSymbolics AssertionViolation @?= emptySet
            extractSymbolics AssumptionViolation @?= emptySet,
          testCase "Mergeable" $ do
            mrgIf
              "a"
              (mrgPure AssumptionViolation)
              (mrgPure AssertionViolation)
              @?= ( mrgIf
                      (symNot "a")
                      (mrgPure AssertionViolation)
                      (mrgPure AssumptionViolation) ::
                      UnionM VerificationConditions
                  ),
          testCase
            "Transform VerificationConditions to VerificationConditions"
            $ do
              transformError AssertionViolation @?= AssertionViolation
              transformError AssumptionViolation @?= AssumptionViolation
        ],
      testCase "symAssert" $ do
        (symAssert "a" :: ExceptT VerificationConditions UnionM ())
          @?= ExceptT
            ( mrgIf
                (symNot "a")
                (mrgPure $ Left AssertionViolation)
                (mrgPure $ Right ())
            )
    ]
