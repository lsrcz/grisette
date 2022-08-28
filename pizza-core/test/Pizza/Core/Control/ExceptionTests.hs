{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Pizza.Core.Control.ExceptionTests where

import Control.Exception hiding (evaluate)
import Control.Monad.Except
import qualified Data.HashSet as S
import Pizza.Core.Control.Exception
import Pizza.Core.Control.Monad.UnionMBase
import Pizza.Core.Data.Class.Bool
import Pizza.Core.Data.Class.Error
import Pizza.Core.Data.Class.Evaluate
import Pizza.Core.Data.Class.ExtractSymbolics
import Pizza.Core.Data.Class.Mergeable
import Pizza.Core.Data.Class.SOrd
import Pizza.Core.Data.Class.SimpleMergeable
import Pizza.Core.Data.Class.ToCon
import Pizza.Core.Data.Class.ToSym
import Pizza.TestUtils.SBool
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
            AssertionError ==~ AssertionError @=? CBool True,
          testCase "SOrd" $ do
            AssertionError <=~ AssertionError @=? CBool True
            AssertionError <~ AssertionError @=? CBool False
            AssertionError >=~ AssertionError @=? CBool True
            AssertionError >~ AssertionError @=? CBool False
            AssertionError `symCompare` AssertionError @=? (mrgSingle EQ :: UnionMBase SBool Ordering),
          testCase "EvaluateSym" $ do
            evaluateSym False () AssertionError @=? AssertionError,
          testCase "ExtractSymbolics" $ do
            extractSymbolics AssertionError @=? (S.empty :: S.HashSet Symbol),
          testCase "SimpleMergeable" $ do
            mrgIte (SSBool "a") AssertionError AssertionError @=? AssertionError,
          testCase "Mergeable" $ do
            let SimpleStrategy s = mergingStrategy :: MergingStrategy SBool AssertionError
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
            AssertionViolation ==~ AssertionViolation @=? CBool True
            AssertionViolation ==~ AssumptionViolation @=? CBool False
            AssumptionViolation ==~ AssertionViolation @=? CBool False
            AssumptionViolation ==~ AssumptionViolation @=? CBool True,
          testCase "SOrd" $ do
            AssertionViolation <=~ AssertionViolation @=? CBool True
            AssertionViolation <~ AssertionViolation @=? CBool False
            AssertionViolation >=~ AssertionViolation @=? CBool True
            AssertionViolation >~ AssertionViolation @=? CBool False
            AssertionViolation `symCompare` AssertionViolation @=? (mrgSingle EQ :: UnionMBase SBool Ordering)

            AssertionViolation <=~ AssumptionViolation @=? CBool True
            AssertionViolation <~ AssumptionViolation @=? CBool True
            AssertionViolation >=~ AssumptionViolation @=? CBool False
            AssertionViolation >~ AssumptionViolation @=? CBool False
            AssertionViolation `symCompare` AssumptionViolation @=? (mrgSingle LT :: UnionMBase SBool Ordering)

            AssumptionViolation <=~ AssertionViolation @=? CBool False
            AssumptionViolation <~ AssertionViolation @=? CBool False
            AssumptionViolation >=~ AssertionViolation @=? CBool True
            AssumptionViolation >~ AssertionViolation @=? CBool True
            AssumptionViolation `symCompare` AssertionViolation @=? (mrgSingle GT :: UnionMBase SBool Ordering)

            AssumptionViolation <=~ AssumptionViolation @=? CBool True
            AssumptionViolation <~ AssumptionViolation @=? CBool False
            AssumptionViolation >=~ AssumptionViolation @=? CBool True
            AssumptionViolation >~ AssumptionViolation @=? CBool False
            AssumptionViolation `symCompare` AssumptionViolation @=? (mrgSingle EQ :: UnionMBase SBool Ordering),
          testCase "EvaluateSym" $ do
            evaluateSym False () AssertionViolation @=? AssertionViolation
            evaluateSym False () AssumptionViolation @=? AssumptionViolation,
          testCase "ExtractSymbolics" $ do
            extractSymbolics AssertionViolation @=? (S.empty :: S.HashSet Symbol)
            extractSymbolics AssumptionViolation @=? (S.empty :: S.HashSet Symbol),
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
