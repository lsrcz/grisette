{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Grisette.Core.Control.ExceptionTests where

import Control.Exception hiding (evaluate)
import Control.Monad.Except
import qualified Data.HashSet as S
import Grisette.Core.Control.Exception
import Grisette.Core.Control.Monad.UnionMBase
import Grisette.Core.Data.Class.Bool
import Grisette.Core.Data.Class.Error
import Grisette.Core.Data.Class.Evaluate
import Grisette.Core.Data.Class.ExtractSymbolics
import Grisette.Core.Data.Class.Mergeable
import Grisette.Core.Data.Class.SOrd
import Grisette.Core.Data.Class.SimpleMergeable
import Grisette.Core.Data.Class.ToCon
import Grisette.Core.Data.Class.ToSym
import Grisette.TestUtils.SBool
import Test.Tasty
import Test.Tasty.HUnit

exceptionTests :: TestTree
exceptionTests =
  testGroup
    "ExceptionTests"
    [ testGroup
        "AssertionError"
        [ testCase "ToCon" $ do
            toCon AssertionError @=? Just AssertionError,
          testCase "ToSym" $ do
            toSym AssertionError @=? AssertionError,
          testCase "SEq" $ do
            AssertionError `gsymeq` AssertionError @=? CBool True,
          testCase "SOrd" $ do
            AssertionError `gsymle` AssertionError @=? CBool True
            AssertionError `gsymlt` AssertionError @=? CBool False
            AssertionError `gsymge` AssertionError @=? CBool True
            AssertionError `gsymgt` AssertionError @=? CBool False
            AssertionError `gsymCompare` AssertionError @=? (mrgSingle EQ :: UnionMBase SBool Ordering),
          testCase "GEvaluateSym" $ do
            gevaluateSym False () AssertionError @=? AssertionError,
          testCase "GExtractSymbolics" $ do
            gextractSymbolics AssertionError @=? (S.empty :: S.HashSet Symbol),
          testCase "SimpleMergeable" $ do
            mrgIte (SSBool "a") AssertionError AssertionError @=? AssertionError,
          testCase "Mergeable" $ do
            let SimpleStrategy s = gmergingStrategy :: GMergingStrategy SBool AssertionError
            s (SSBool "a") AssertionError AssertionError @=? AssertionError,
          testCase "Transform AssertionError to VerificationConditions" $ do
            transformError AssertionError @=? AssertionViolation,
          testCase "Transform AssertionError to AssertionError" $ do
            transformError AssertionError @=? AssertionError,
          testCase "Transform ArrayException to AssertionError" $ do
            transformError (IndexOutOfBounds "") @=? AssertionError,
          testCase "Transform ArrayException to AssertionError" $ do
            transformError (UndefinedElement "") @=? AssertionError
        ],
      testGroup
        "VerificationConditions"
        [ testCase "ToCon" $ do
            toCon AssertionViolation @=? Just AssertionViolation
            toCon AssumptionViolation @=? Just AssumptionViolation,
          testCase "ToSym" $ do
            toSym AssertionViolation @=? AssertionViolation
            toSym AssumptionViolation @=? AssumptionViolation,
          testCase "SEq" $ do
            AssertionViolation `gsymeq` AssertionViolation @=? CBool True
            AssertionViolation `gsymeq` AssumptionViolation @=? CBool False
            AssumptionViolation `gsymeq` AssertionViolation @=? CBool False
            AssumptionViolation `gsymeq` AssumptionViolation @=? CBool True,
          testCase "SOrd" $ do
            AssertionViolation `gsymle` AssertionViolation @=? CBool True
            AssertionViolation `gsymlt` AssertionViolation @=? CBool False
            AssertionViolation `gsymge` AssertionViolation @=? CBool True
            AssertionViolation `gsymgt` AssertionViolation @=? CBool False
            AssertionViolation `gsymCompare` AssertionViolation @=? (mrgSingle EQ :: UnionMBase SBool Ordering)

            AssertionViolation `gsymle` AssumptionViolation @=? CBool True
            AssertionViolation `gsymlt` AssumptionViolation @=? CBool True
            AssertionViolation `gsymge` AssumptionViolation @=? CBool False
            AssertionViolation `gsymgt` AssumptionViolation @=? CBool False
            AssertionViolation `gsymCompare` AssumptionViolation @=? (mrgSingle LT :: UnionMBase SBool Ordering)

            AssumptionViolation `gsymle` AssertionViolation @=? CBool False
            AssumptionViolation `gsymlt` AssertionViolation @=? CBool False
            AssumptionViolation `gsymge` AssertionViolation @=? CBool True
            AssumptionViolation `gsymgt` AssertionViolation @=? CBool True
            AssumptionViolation `gsymCompare` AssertionViolation @=? (mrgSingle GT :: UnionMBase SBool Ordering)

            AssumptionViolation `gsymle` AssumptionViolation @=? CBool True
            AssumptionViolation `gsymlt` AssumptionViolation @=? CBool False
            AssumptionViolation `gsymge` AssumptionViolation @=? CBool True
            AssumptionViolation `gsymgt` AssumptionViolation @=? CBool False
            AssumptionViolation `gsymCompare` AssumptionViolation @=? (mrgSingle EQ :: UnionMBase SBool Ordering),
          testCase "GEvaluateSym" $ do
            gevaluateSym False () AssertionViolation @=? AssertionViolation
            gevaluateSym False () AssumptionViolation @=? AssumptionViolation,
          testCase "GExtractSymbolics" $ do
            gextractSymbolics AssertionViolation @=? (S.empty :: S.HashSet Symbol)
            gextractSymbolics AssumptionViolation @=? (S.empty :: S.HashSet Symbol),
          testCase "Mergeable" $ do
            mrgIf (SSBool "a") (mrgSingle AssumptionViolation) (mrgSingle AssertionViolation)
              @=? ( mrgIf (Not $ SSBool "a") (mrgSingle AssertionViolation) (mrgSingle AssumptionViolation) ::
                      UnionMBase SBool VerificationConditions
                  ),
          testCase "Transform VerificationConditions to VerificationConditions" $ do
            transformError AssertionViolation @=? AssertionViolation
            transformError AssumptionViolation @=? AssumptionViolation
        ],
      testCase "symAssert" $ do
        (symAssert (SSBool "a") :: ExceptT VerificationConditions (UnionMBase SBool) ())
          @=? ExceptT (mrgIf (Not $ SSBool "a") (mrgSingle $ Left AssertionViolation) (mrgSingle $ Right ()))
    ]
