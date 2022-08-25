{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Pizza.Core.Control.ExceptionSpec where

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
import Test.Hspec

spec :: Spec
spec = do
  describe "AssertionError" $ do
    it "ToCon for AssertionError" $ do
      toCon AssertionError `shouldBe` Just AssertionError
    it "ToSym for AssertionError" $ do
      toSym AssertionError `shouldBe` AssertionError
    it "SEq for AssertionError" $ do
      AssertionError ==~ AssertionError `shouldBe` CBool True
    it "SOrd for AssertionError" $ do
      AssertionError <=~ AssertionError `shouldBe` CBool True
      AssertionError <~ AssertionError `shouldBe` CBool False
      AssertionError >=~ AssertionError `shouldBe` CBool True
      AssertionError >~ AssertionError `shouldBe` CBool False
      AssertionError `symCompare` AssertionError `shouldBe` (mrgSingle EQ :: UnionMBase SBool Ordering)
    it "Evaluate for AssertionError" $ do
      evaluateSym False () AssertionError `shouldBe` AssertionError
    it "ExtractSymbolics for AssertionError" $ do
      extractSymbolics AssertionError `shouldBe` (S.empty :: S.HashSet Symbol)
    it "SimpleMergeable for AssertionError" $ do
      mrgIte (SSBool "a") AssertionError AssertionError `shouldBe` AssertionError
    it "Mergeable for AssertionError" $ do
      let SimpleStrategy s = mergingStrategy :: MergingStrategy SBool AssertionError
      s (SSBool "a") AssertionError AssertionError `shouldBe` AssertionError
    it "Transform AssertionError to VerificationConditions" $ do
      transformError AssertionError `shouldBe` AssertionViolation
    it "Transform AssertionError to AssertionError" $ do
      transformError AssertionError `shouldBe` AssertionError
    it "Transform ArrayException to AssertionError" $ do
      transformError (IndexOutOfBounds "") `shouldBe` AssertionError
    it "Transform ArrayException to AssertionError" $ do
      transformError (UndefinedElement "") `shouldBe` AssertionError
  describe "VerificationConditions" $ do
    it "ToCon for VerificationConditions" $ do
      toCon AssertionViolation `shouldBe` Just AssertionViolation
      toCon AssumptionViolation `shouldBe` Just AssumptionViolation
    it "ToSym for AssertionError" $ do
      toSym AssertionViolation `shouldBe` AssertionViolation
      toSym AssumptionViolation `shouldBe` AssumptionViolation
    it "SEq for AssertionError" $ do
      AssertionViolation ==~ AssertionViolation `shouldBe` CBool True
      AssertionViolation ==~ AssumptionViolation `shouldBe` CBool False
      AssumptionViolation ==~ AssertionViolation `shouldBe` CBool False
      AssumptionViolation ==~ AssumptionViolation `shouldBe` CBool True
    it "SOrd for AssertionError" $ do
      AssertionViolation <=~ AssertionViolation `shouldBe` CBool True
      AssertionViolation <~ AssertionViolation `shouldBe` CBool False
      AssertionViolation >=~ AssertionViolation `shouldBe` CBool True
      AssertionViolation >~ AssertionViolation `shouldBe` CBool False
      AssertionViolation `symCompare` AssertionViolation `shouldBe` (mrgSingle EQ :: UnionMBase SBool Ordering)

      AssertionViolation <=~ AssumptionViolation `shouldBe` CBool True
      AssertionViolation <~ AssumptionViolation `shouldBe` CBool True
      AssertionViolation >=~ AssumptionViolation `shouldBe` CBool False
      AssertionViolation >~ AssumptionViolation `shouldBe` CBool False
      AssertionViolation `symCompare` AssumptionViolation `shouldBe` (mrgSingle LT :: UnionMBase SBool Ordering)

      AssumptionViolation <=~ AssertionViolation `shouldBe` CBool False
      AssumptionViolation <~ AssertionViolation `shouldBe` CBool False
      AssumptionViolation >=~ AssertionViolation `shouldBe` CBool True
      AssumptionViolation >~ AssertionViolation `shouldBe` CBool True
      AssumptionViolation `symCompare` AssertionViolation `shouldBe` (mrgSingle GT :: UnionMBase SBool Ordering)

      AssumptionViolation <=~ AssumptionViolation `shouldBe` CBool True
      AssumptionViolation <~ AssumptionViolation `shouldBe` CBool False
      AssumptionViolation >=~ AssumptionViolation `shouldBe` CBool True
      AssumptionViolation >~ AssumptionViolation `shouldBe` CBool False
      AssumptionViolation `symCompare` AssumptionViolation `shouldBe` (mrgSingle EQ :: UnionMBase SBool Ordering)
    it "Evaluate for VerificationConditions" $ do
      evaluateSym False () AssertionViolation `shouldBe` AssertionViolation
      evaluateSym False () AssumptionViolation `shouldBe` AssumptionViolation
    it "ExtractSymbolics for AssertionError" $ do
      extractSymbolics AssertionViolation `shouldBe` (S.empty :: S.HashSet Symbol)
      extractSymbolics AssumptionViolation `shouldBe` (S.empty :: S.HashSet Symbol)
    it "Mergeable for AssertionError" $ do
      mrgIf (SSBool "a") (mrgSingle AssumptionViolation) (mrgSingle AssertionViolation)
        `shouldBe` ( mrgIf (Not $ SSBool "a") (mrgSingle AssertionViolation) (mrgSingle AssumptionViolation) ::
                       UnionMBase SBool VerificationConditions
                   )
    it "Transform VerificationConditions to VerificationConditions" $ do
      transformError AssertionViolation `shouldBe` AssertionViolation
      transformError AssumptionViolation `shouldBe` AssumptionViolation
  describe "symAssert" $ do
    it "symAssert should work" $ do
      (symAssert (SSBool "a") :: ExceptT VerificationConditions (UnionMBase SBool) ())
        `shouldBe` ExceptT (mrgIf (Not $ SSBool "a") (mrgSingle $ Left AssertionViolation) (mrgSingle $ Right ()))
