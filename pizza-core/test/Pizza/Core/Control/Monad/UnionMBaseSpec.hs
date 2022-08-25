{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Pizza.Core.Control.Monad.UnionMBaseSpec where

import Control.Monad.Identity
import qualified Data.ByteString as B
import qualified Data.HashMap.Lazy as ML
import qualified Data.HashMap.Strict as M
import qualified Data.HashSet as S
import Pizza.Core.Control.Monad.UnionMBase
import Pizza.Core.Data.Class.Bool
import Pizza.Core.Data.Class.Evaluate
import Pizza.Core.Data.Class.ExtractSymbolics
import Pizza.Core.Data.Class.Function
import Pizza.Core.Data.Class.GenSym
import Pizza.Core.Data.Class.PrimWrapper
import Pizza.Core.Data.Class.SOrd
import Pizza.Core.Data.Class.SimpleMergeable
import Pizza.Core.Data.Class.ToCon
import Pizza.Core.Data.Class.ToSym
import Pizza.Core.Data.UnionBase
import Pizza.TestUtils.SBool
import Test.Hspec

spec :: Spec
spec = do
  describe "Mergeable for UnionMBase" $ do
    it "Mergeable for UnionMBase should work" $ do
      let r =
            ( mrgIf
                (SSBool "a")
                (mrgSingle (mrgIf (SSBool "b") (mrgSingle $ Left $ SSBool "c") (mrgSingle $ Right $ SSBool "d")))
                (mrgSingle (mrgIf (SSBool "e") (mrgSingle $ Left $ SSBool "f") (mrgSingle $ Right $ SSBool "g"))) ::
                UnionMBase SBool (UnionMBase SBool (Either SBool SBool))
            )
      isMerged r `shouldBe` True
      underlyingUnion (underlyingUnion <$> r)
        `shouldBe` Single
          ( If
              (Left $ ITE (SSBool "a") (SSBool "c") (SSBool "f"))
              True
              (ITE (SSBool "a") (SSBool "b") (SSBool "e"))
              (Single $ Left $ ITE (SSBool "a") (SSBool "c") (SSBool "f"))
              (Single $ Right $ ITE (SSBool "a") (SSBool "d") (SSBool "g"))
          )
  describe "SimpleMergeable for UnionMBase" $ do
    it "SimpleMergeable for UnionMBase should work" $ do
      let l :: UnionMBase SBool (Either SBool SBool) = mrgIf (SSBool "b") (mrgSingle $ Left $ SSBool "c") (mrgSingle $ Right $ SSBool "d")
      let r = mrgIf (SSBool "e") (mrgSingle $ Left $ SSBool "f") (mrgSingle $ Right $ SSBool "g")
      let res = mrgIte (SSBool "a") l r
      let ref =
            If
              (Left $ ITE (SSBool "a") (SSBool "c") (SSBool "f"))
              True
              (ITE (SSBool "a") (SSBool "b") (SSBool "e"))
              (Single $ Left $ ITE (SSBool "a") (SSBool "c") (SSBool "f"))
              (Single $ Right $ ITE (SSBool "a") (SSBool "d") (SSBool "g"))
      isMerged res `shouldBe` True
      underlyingUnion res `shouldBe` ref
    it "SimpleMergeable1 for UnionMBase should work" $ do
      let l :: UnionMBase SBool SBool = mrgIf (SSBool "b") (mrgSingle $ SSBool "c") (mrgSingle $ SSBool "d")
      let r :: UnionMBase SBool SBool = mrgIf (SSBool "e") (mrgSingle $ SSBool "f") (mrgSingle $ SSBool "g")
      let res = mrgIte1 (SSBool "a") l r
      isMerged res `shouldBe` True
      underlyingUnion res
        `shouldBe` Single
          ( ITE
              (SSBool "a")
              (ITE (SSBool "b") (SSBool "c") (SSBool "d"))
              (ITE (SSBool "e") (SSBool "f") (SSBool "g"))
          )
  describe "Functor for UnionMBase" $ do
    it "fmap should work but would strip mergeable knowledge" $ do
      let x :: UnionMBase SBool Integer = (+ 1) <$> mrgIf (SSBool "a") (mrgSingle 1) (mrgSingle 2)
      x `shouldBe` unionIf (SSBool "a") (return 2) (return 3)
  describe "Applicative for UnionMBase" $ do
    it "pure should work but won't give us mergeable knowledge" $ do
      (pure 1 :: UnionMBase SBool Integer) `shouldBe` single 1
    it "<*> should work but won't give us mergeable knowledge" $ do
      let f :: UnionMBase SBool (Integer -> Integer) = mrgIf (SSBool "a") (mrgSingle id) (mrgSingle (+ 1))
      let v :: UnionMBase SBool Integer = mrgIf (SSBool "b") (mrgSingle 1) (mrgSingle 3)
      f <*> v
        `shouldBe` unionIf
          (SSBool "a")
          (unionIf (SSBool "b") (single 1) (single 3))
          (unionIf (SSBool "b") (single 2) (single 4))
  describe "Monad for UnionMBase" $ do
    it "return should work but won't give us mergeable knowledge" $ do
      (pure 1 :: UnionMBase SBool Integer) `shouldBe` single 1
    it ">>= should work and keeps mergeable knowledge" $ do
      let v :: UnionMBase SBool Integer = mrgIf (SSBool "a") (mrgSingle 0) (mrgSingle 1)
      let f :: Integer -> UnionMBase SBool Integer = \i -> mrgIf (SSBool "b") (mrgSingle $ i + 1) (mrgSingle $ i + 3)
      (v >>= f)
        `shouldBe` mrgIf
          (SSBool "a")
          (mrgIf (SSBool "b") (mrgSingle 1) (mrgSingle 3))
          (mrgIf (SSBool "b") (mrgSingle 2) (mrgSingle 4))
  describe "UnionOp for UnionMBase" $ do
    it "single for UnionMBase should work" $ do
      let r1 :: UnionMBase SBool SBool = single (SSBool "a")
      isMerged r1 `shouldBe` False
      underlyingUnion r1 `shouldBe` Single (SSBool "a")
    it "unionIf for UnionMBase should work when no merged" $ do
      let r1 :: UnionMBase SBool SBool = unionIf (SSBool "a") (single $ SSBool "b") (single $ SSBool "c")
      isMerged r1 `shouldBe` False
      underlyingUnion r1 `shouldBe` If (SSBool "b") False (SSBool "a") (Single $ SSBool "b") (Single $ SSBool "c")
    it "unionIf for UnionMBase should propagate and merge the results when some branch merged" $ do
      let r1 :: UnionMBase SBool SBool = unionIf (SSBool "a") (mrgSingle $ SSBool "b") (single $ SSBool "c")
      isMerged r1 `shouldBe` True
      underlyingUnion r1 `shouldBe` Single (ITE (SSBool "a") (SSBool "b") (SSBool "c"))
      let r2 :: UnionMBase SBool SBool = unionIf (SSBool "a") (single $ SSBool "b") (mrgSingle $ SSBool "c")
      isMerged r2 `shouldBe` True
      underlyingUnion r2 `shouldBe` Single (ITE (SSBool "a") (SSBool "b") (SSBool "c"))
      let r3 :: UnionMBase SBool SBool = unionIf (SSBool "a") (mrgSingle $ SSBool "b") (mrgSingle $ SSBool "c")
      isMerged r3 `shouldBe` True
      underlyingUnion r3 `shouldBe` Single (ITE (SSBool "a") (SSBool "b") (SSBool "c"))
    it "singleView for UnionMBase should work" $ do
      singleView (single $ SSBool "a" :: UnionMBase SBool SBool) `shouldBe` Just (SSBool "a")
      singleView (mrgSingle $ SSBool "a" :: UnionMBase SBool SBool) `shouldBe` Just (SSBool "a")
      singleView
        ( unionIf (SSBool "a") (single $ Left $ SSBool "b") (single $ Right $ SSBool "c") ::
            UnionMBase SBool (Either SBool SBool)
        )
        `shouldBe` Nothing
      case (single $ SSBool "a" :: UnionMBase SBool SBool) of
        SingleU r -> r `shouldBe` SSBool "a"
        _ -> expectationFailure "SingleU match failed"
      case (mrgSingle $ SSBool "a" :: UnionMBase SBool SBool) of
        SingleU r -> r `shouldBe` SSBool "a"
        _ -> expectationFailure "SingleU match failed"
      case ( unionIf (SSBool "a") (single $ Left $ SSBool "b") (single $ Right $ SSBool "c") ::
               UnionMBase SBool (Either SBool SBool)
           ) of
        SingleU _ -> expectationFailure "SingleU match failed"
        _ -> return ()
    it "ifView for UnionMBase should work" $ do
      let r1 :: UnionMBase SBool (Either SBool SBool) =
            unionIf (SSBool "a") (single $ Left $ SSBool "b") (single $ Right $ SSBool "c")
      let r2 :: UnionMBase SBool (Either SBool SBool) =
            mrgIf (SSBool "a") (mrgSingle $ Left $ SSBool "b") (mrgSingle $ Right $ SSBool "c")
      ifView r1 `shouldBe` Just (SSBool "a", single $ Left $ SSBool "b", single $ Right $ SSBool "c")
      ifView r2 `shouldBe` Just (SSBool "a", mrgSingle $ Left $ SSBool "b", mrgSingle $ Right $ SSBool "c")
      ifView (single $ SSBool "a" :: UnionMBase SBool SBool) `shouldBe` Nothing
      case r1 of
        IfU c l r -> do
          c `shouldBe` SSBool "a"
          l `shouldBe` single (Left $ SSBool "b")
          r `shouldBe` single (Right $ SSBool "c")
        _ -> expectationFailure "SingleU match failed"
      case r2 of
        IfU c l r -> do
          c `shouldBe` SSBool "a"
          l `shouldBe` mrgSingle (Left $ SSBool "b")
          r `shouldBe` mrgSingle (Right $ SSBool "c")
        _ -> expectationFailure "SingleU match failed"
      case single $ SSBool "a" :: UnionMBase SBool SBool of
        IfU {} -> expectationFailure "SingleU match failed"
        _ -> return ()
    it "leftMost for UnionMBase should work" $ do
      leftMost (single $ SSBool "a" :: UnionMBase SBool SBool) `shouldBe` SSBool "a"
      leftMost (mrgSingle $ SSBool "a" :: UnionMBase SBool SBool) `shouldBe` SSBool "a"
      let r1 :: UnionMBase SBool (Either SBool SBool) =
            unionIf (SSBool "a") (single $ Left $ SSBool "b") (single $ Right $ SSBool "c")
      let r2 :: UnionMBase SBool (Either SBool SBool) =
            mrgIf (SSBool "a") (mrgSingle $ Left $ SSBool "b") (mrgSingle $ Right $ SSBool "c")
      leftMost r1 `shouldBe` Left (SSBool "b")
      leftMost r2 `shouldBe` Left (SSBool "b")
  describe "MonadUnion for UnionMBase" $ do
    it "merge for UnionMBase should work" $ do
      let r1 :: UnionMBase SBool SBool = merge (unionIf (SSBool "a") (single $ SSBool "b") (single $ SSBool "c"))
      isMerged r1 `shouldBe` True
      underlyingUnion r1 `shouldBe` Single (ITE (SSBool "a") (SSBool "b") (SSBool "c"))
    it "mrgSingle for UnionMBase should work" $ do
      let r1 :: UnionMBase SBool SBool = mrgSingle (SSBool "a")
      isMerged r1 `shouldBe` True
      underlyingUnion r1 `shouldBe` Single (SSBool "a")
    describe "mrgIf for UnionMBase should work" $ do
      it "mrgIf should perform lazy evaluation" $ do
        (mrgIf (CBool True) (mrgSingle $ SSBool "a") undefined :: UnionMBase SBool SBool)
          `shouldBe` mrgSingle (SSBool "a")
        (mrgIf (CBool False) undefined (mrgSingle $ SSBool "a") :: UnionMBase SBool SBool)
          `shouldBe` mrgSingle (SSBool "a")
      it "mrgIf should work" $ do
        (mrgIf (SSBool "a") (single $ SSBool "b") (single $ SSBool "c") :: UnionMBase SBool SBool)
          `shouldBe` merge (unionIf (SSBool "a") (single $ SSBool "b") (single $ SSBool "c"))
  describe "SEq for UnionMBase" $ do
    it "SEq with Single/Single" $ do
      (mrgSingle $ SSBool "a" :: UnionMBase SBool SBool) ==~ mrgSingle (SSBool "b")
        `shouldBe` Equal (SSBool "a") (SSBool "b")
    let g1 :: UnionMBase SBool (Either SBool SBool) =
          mrgIf (SSBool "a") (mrgSingle $ Left $ SSBool "b") (mrgSingle $ Right $ SSBool "c")
    let g2 :: UnionMBase SBool (Either SBool SBool) =
          mrgIf (SSBool "d") (mrgSingle $ Left $ SSBool "e") (mrgSingle $ Right $ SSBool "f")
    it "SEq with If/Single" $ do
      g1 ==~ mrgSingle (Left $ SSBool "d")
        `shouldBe` ITE (SSBool "a") (Equal (SSBool "b") (SSBool "d")) (CBool False)
      g1 ==~ mrgSingle (Right $ SSBool "d")
        `shouldBe` ITE (SSBool "a") (CBool False) (Equal (SSBool "c") (SSBool "d"))
    it "SEq with Single/If" $ do
      mrgSingle (Left $ SSBool "d") ==~ g1
        `shouldBe` ITE (SSBool "a") (Equal (SSBool "d") (SSBool "b")) (CBool False)
      mrgSingle (Right $ SSBool "d") ==~ g1
        `shouldBe` ITE (SSBool "a") (CBool False) (Equal (SSBool "d") (SSBool "c"))
    it "SEq with If/If" $ do
      g1 ==~ g2
        `shouldBe` ITE
          (SSBool "a")
          (ITE (SSBool "d") (Equal (SSBool "b") (SSBool "e")) (CBool False))
          (ITE (SSBool "d") (CBool False) (Equal (SSBool "c") (SSBool "f")))
  describe "SOrd for UnionMBase" $ do
    it "SOrd with Single/Single" $ do
      (mrgSingle $ SSBool "a" :: UnionMBase SBool SBool) <=~ mrgSingle (SSBool "b")
        `shouldBe` (SSBool "a" <=~ SSBool "b" :: SBool)
      (mrgSingle $ SSBool "a" :: UnionMBase SBool SBool)
        <~ mrgSingle (SSBool "b")
        `shouldBe` (SSBool "a" <~ SSBool "b" :: SBool)
      (mrgSingle $ SSBool "a" :: UnionMBase SBool SBool) >=~ mrgSingle (SSBool "b")
        `shouldBe` (SSBool "a" >=~ SSBool "b" :: SBool)
      (mrgSingle $ SSBool "a" :: UnionMBase SBool SBool) >~ mrgSingle (SSBool "b")
        `shouldBe` (SSBool "a" >~ SSBool "b" :: SBool)
      (mrgSingle $ SSBool "a" :: UnionMBase SBool SBool)
        `symCompare` mrgSingle (SSBool "b")
        `shouldBe` (SSBool "a" `symCompare` SSBool "b" :: UnionMBase SBool Ordering)
    let g1 :: UnionMBase SBool (Either SBool SBool) =
          mrgIf (SSBool "a") (mrgSingle $ Left $ SSBool "b") (mrgSingle $ Right $ SSBool "c")
    let g2 :: UnionMBase SBool (Either SBool SBool) =
          mrgIf (SSBool "d") (mrgSingle $ Left $ SSBool "e") (mrgSingle $ Right $ SSBool "f")
    it "SOrd with If/Single" $ do
      g1 <=~ mrgSingle (Left $ SSBool "d")
        `shouldBe` ITE (SSBool "a") (SSBool "b" <=~ SSBool "d") (CBool False)
      g1
        <~ mrgSingle (Left $ SSBool "d")
        `shouldBe` ITE (SSBool "a") (SSBool "b" <~ SSBool "d") (CBool False)
      g1 >=~ mrgSingle (Left $ SSBool "d")
        `shouldBe` ITE (SSBool "a") (SSBool "b" >=~ SSBool "d") (CBool True)
      g1 >~ mrgSingle (Left $ SSBool "d")
        `shouldBe` ITE (SSBool "a") (SSBool "b" >~ SSBool "d") (CBool True)

      g1
        `symCompare` mrgSingle (Left $ SSBool "d")
        `shouldBe` ( mrgIf (SSBool "a") (SSBool "b" `symCompare` SSBool "d") (mrgSingle GT) ::
                       UnionMBase SBool Ordering
                   )

      g1 <=~ mrgSingle (Right $ SSBool "d")
        `shouldBe` ITE (SSBool "a") (CBool True) (SSBool "c" <=~ SSBool "d")
      g1
        <~ mrgSingle (Right $ SSBool "d")
        `shouldBe` ITE (SSBool "a") (CBool True) (SSBool "c" <~ SSBool "d")
      g1 >=~ mrgSingle (Right $ SSBool "d")
        `shouldBe` ITE (SSBool "a") (CBool False) (SSBool "c" >=~ SSBool "d")
      g1 >~ mrgSingle (Right $ SSBool "d")
        `shouldBe` ITE (SSBool "a") (CBool False) (SSBool "c" >~ SSBool "d")

      g1
        `symCompare` mrgSingle (Right $ SSBool "d")
        `shouldBe` ( mrgIf (SSBool "a") (mrgSingle LT) (SSBool "c" `symCompare` SSBool "d") ::
                       UnionMBase SBool Ordering
                   )
    it "SOrd with Single/If" $ do
      mrgSingle (Left $ SSBool "d") <=~ g1
        `shouldBe` ITE (SSBool "a") (SSBool "d" <=~ SSBool "b") (CBool True)
      mrgSingle (Left $ SSBool "d")
        <~ g1
        `shouldBe` ITE (SSBool "a") (SSBool "d" <~ SSBool "b") (CBool True)
      mrgSingle (Left $ SSBool "d") >=~ g1
        `shouldBe` ITE (SSBool "a") (SSBool "d" >=~ SSBool "b") (CBool False)
      mrgSingle (Left $ SSBool "d") >~ g1
        `shouldBe` ITE (SSBool "a") (SSBool "d" >~ SSBool "b") (CBool False)

      mrgSingle (Left $ SSBool "d")
        `symCompare` g1
        `shouldBe` ( mrgIf (SSBool "a") (SSBool "d" `symCompare` SSBool "b") (mrgSingle LT) ::
                       UnionMBase SBool Ordering
                   )

      mrgSingle (Right $ SSBool "d") <=~ g1
        `shouldBe` ITE (SSBool "a") (CBool False) (SSBool "d" <=~ SSBool "c")
      mrgSingle (Right $ SSBool "d")
        <~ g1
        `shouldBe` ITE (SSBool "a") (CBool False) (SSBool "d" <~ SSBool "c")
      mrgSingle (Right $ SSBool "d") >=~ g1
        `shouldBe` ITE (SSBool "a") (CBool True) (SSBool "d" >=~ SSBool "c")
      mrgSingle (Right $ SSBool "d") >~ g1
        `shouldBe` ITE (SSBool "a") (CBool True) (SSBool "d" >~ SSBool "c")

      mrgSingle (Right $ SSBool "d")
        `symCompare` g1
        `shouldBe` ( mrgIf (SSBool "a") (mrgSingle GT) (SSBool "d" `symCompare` SSBool "c") ::
                       UnionMBase SBool Ordering
                   )
    it "SOrd with If/If" $ do
      g1 <=~ g2
        `shouldBe` ITE
          (SSBool "a")
          (ITE (SSBool "d") (SSBool "b" <=~ SSBool "e") (CBool True))
          (ITE (SSBool "d") (CBool False) (SSBool "c" <=~ SSBool "f"))
      g1
        <~ g2
        `shouldBe` ITE
          (SSBool "a")
          (ITE (SSBool "d") (SSBool "b" <~ SSBool "e") (CBool True))
          (ITE (SSBool "d") (CBool False) (SSBool "c" <~ SSBool "f"))
      g1 >=~ g2
        `shouldBe` ITE
          (SSBool "a")
          (ITE (SSBool "d") (SSBool "b" >=~ SSBool "e") (CBool False))
          (ITE (SSBool "d") (CBool True) (SSBool "c" >=~ SSBool "f"))
      g1 >~ g2
        `shouldBe` ITE
          (SSBool "a")
          (ITE (SSBool "d") (SSBool "b" >~ SSBool "e") (CBool False))
          (ITE (SSBool "d") (CBool True) (SSBool "c" >~ SSBool "f"))
      g1
        `symCompare` g2
        `shouldBe` ( mrgIf
                       (SSBool "a")
                       (mrgIf (SSBool "d") (SSBool "b" `symCompare` SSBool "e") (mrgSingle LT))
                       (mrgIf (SSBool "d") (mrgSingle GT) (SSBool "c" `symCompare` SSBool "f")) ::
                       UnionMBase SBool Ordering
                   )
  describe "ToSym for UnionMBase" $ do
    it "ToSym from single" $ do
      (toSym True :: UnionMBase SBool SBool) `shouldBe` mrgSingle (CBool True)
    it "ToSym from UnionMBase" $ do
      (toSym (mrgSingle True :: UnionMBase SBool Bool) :: UnionMBase SBool SBool) `shouldBe` mrgSingle (CBool True)
    it "ToSym from Identity" $ do
      (toSym (Identity True :: Identity Bool) :: UnionMBase SBool SBool) `shouldBe` mrgSingle (CBool True)
  describe "ToCon for UnionMBase" $ do
    it "ToCon for UnionMBase should work" $ do
      (toCon (mrgSingle (CBool True) :: UnionMBase SBool SBool) :: Maybe Bool) `shouldBe` Just True
      (toCon (mrgSingle (SSBool "a") :: UnionMBase SBool SBool) :: Maybe Bool) `shouldBe` Nothing
      ( toCon (mrgIf (SSBool "a") (mrgSingle (1 :: Integer)) (mrgSingle (2 :: Integer)) :: UnionMBase SBool Integer) ::
          Maybe Integer
        )
        `shouldBe` Nothing
  describe "Evaluate for UnionMBase" $ do
    it "Evaluate for UnionMBase should work" $ do
      let model = M.empty :: M.HashMap Symbol Bool
      let model1 =
            M.fromList
              [ (SSymbol "a", True),
                (SSymbol "b", False),
                (SSymbol "c", True)
              ] ::
              M.HashMap Symbol Bool
      evaluateSym False model (mrgSingle $ SSBool "a") `shouldBe` (mrgSingle $ SSBool "a" :: UnionMBase SBool SBool)
      evaluateSym True model (mrgSingle $ SSBool "a") `shouldBe` (mrgSingle $ CBool False :: UnionMBase SBool SBool)
      evaluateSym False model1 (mrgSingle $ SSBool "a") `shouldBe` (mrgSingle $ CBool True :: UnionMBase SBool SBool)
      evaluateSym True model1 (mrgSingle $ SSBool "a") `shouldBe` (mrgSingle $ CBool True :: UnionMBase SBool SBool)
      evaluateSym False model1 (mrgIf (SSBool "a") (mrgSingle $ Left (SSBool "d")) (mrgSingle $ Right (SSBool "e")))
        `shouldBe` (mrgSingle $ Left $ SSBool "d" :: UnionMBase SBool (Either SBool SBool))
      evaluateSym True model1 (mrgIf (SSBool "a") (mrgSingle $ Left (SSBool "d")) (mrgSingle $ Right (SSBool "e")))
        `shouldBe` (mrgSingle $ Left $ CBool False :: UnionMBase SBool (Either SBool SBool))
      evaluateSym False model1 (mrgIf (SSBool "d") (mrgSingle $ Left (SSBool "a")) (mrgSingle $ Right (SSBool "b")))
        `shouldBe` ( mrgIf (SSBool "d") (mrgSingle $ Left $ CBool True) (mrgSingle $ Right $ CBool False) ::
                       UnionMBase SBool (Either SBool SBool)
                   )
      evaluateSym True model1 (mrgIf (SSBool "d") (mrgSingle $ Left (SSBool "a")) (mrgSingle $ Right (SSBool "b")))
        `shouldBe` (mrgSingle $ Right $ CBool False :: UnionMBase SBool (Either SBool SBool))
      evaluateSym False model1 (mrgIf (SSBool "a") (mrgSingle $ Left (SSBool "b")) (mrgSingle $ Right (SSBool "c")))
        `shouldBe` (mrgSingle $ Left $ CBool False :: UnionMBase SBool (Either SBool SBool))
  describe "ExtractSymbolics for UnionMBase" $ do
    it "ExtractSymbolic for UnionMBase should work" $ do
      extractSymbolics (mrgSingle $ SSBool "a" :: UnionMBase SBool SBool)
        `shouldBe` S.singleton (SSymbol "a")
      extractSymbolics
        ( mrgIf (SSBool "a") (mrgSingle $ Left $ SSBool "b") (mrgSingle $ Right $ SSBool "c") ::
            UnionMBase SBool (Either SBool SBool)
        )
        `shouldBe` S.fromList [SSymbol "a", SSymbol "b", SSymbol "c"]
  describe "Num for UnionMBase" $ do
    describe "Num for UnionMBase should work" $ do
      it "fromInteger for UnionMBase should work" $ do
        (1 :: UnionMBase SBool Integer) `shouldBe` mrgSingle 1
      it "negate for UnionMBase should work" $ do
        negate (mrgIf (SSBool "a") (mrgSingle 1) (mrgSingle 2) :: UnionMBase SBool Integer)
          `shouldBe` mrgIf (SSBool "a") (mrgSingle $ -1) (mrgSingle $ -2)
      it "plus for UnionMBase should work" $ do
        (mrgIf (SSBool "a") (mrgSingle 0) (mrgSingle 1) :: UnionMBase SBool Integer)
          + mrgIf (SSBool "b") (mrgSingle 1) (mrgSingle 3)
          `shouldBe` mrgIf
            (SSBool "a")
            (mrgIf (SSBool "b") (mrgSingle 1) (mrgSingle 3))
            (mrgIf (SSBool "b") (mrgSingle 2) (mrgSingle 4))
      it "minus for UnionMBase should work" $ do
        (mrgIf (SSBool "a") (mrgSingle 0) (mrgSingle 1) :: UnionMBase SBool Integer)
          - mrgIf (SSBool "b") (mrgSingle $ -3) (mrgSingle $ -1)
          `shouldBe` mrgIf
            (SSBool "a")
            (mrgIf (Not $ SSBool "b") (mrgSingle 1) (mrgSingle 3))
            (mrgIf (Not $ SSBool "b") (mrgSingle 2) (mrgSingle 4))
      it "times for UnionMBase should work" $ do
        (mrgIf (SSBool "a") (mrgSingle 1) (mrgSingle 2) :: UnionMBase SBool Integer)
          * mrgIf (SSBool "b") (mrgSingle 3) (mrgSingle 4)
          `shouldBe` mrgIf
            (SSBool "a")
            (mrgIf (SSBool "b") (mrgSingle 3) (mrgSingle 4))
            (mrgIf (SSBool "b") (mrgSingle 6) (mrgSingle 8))
      it "abs for UnionMBase should work" $ do
        abs (mrgIf (SSBool "a") (mrgSingle $ -1) (mrgSingle 2) :: UnionMBase SBool Integer)
          `shouldBe` mrgIf (SSBool "a") (mrgSingle 1) (mrgSingle 2)
      it "signum for UnionMBase should work" $ do
        signum (mrgIf (SSBool "a") (mrgSingle $ -1) (mrgSingle 2) :: UnionMBase SBool Integer)
          `shouldBe` mrgIf (SSBool "a") (mrgSingle $ -1) (mrgSingle 1)
  describe "ITEOp for UnionMBase" $ do
    it "ites for UnionMBase should work" $ do
      ites (SSBool "a") (mrgSingle $ SSBool "b") (mrgSingle $ SSBool "c")
        `shouldBe` (mrgSingle (ITE (SSBool "a") (SSBool "b") (SSBool "c")) :: UnionMBase SBool SBool)
  describe "LogicalOp for UnionMBase" $ do
    let l = mrgIf (SSBool "a") (mrgSingle False) (mrgSingle True)
    let r = mrgIf (SSBool "b") (mrgSingle False) (mrgSingle True)
    it "||~ for UnionMBase should work" $ do
      l ||~ r `shouldBe` (mrgIf (And (SSBool "a") (SSBool "b")) (mrgSingle False) (mrgSingle True) :: UnionMBase SBool Bool)
    it "&&~ for UnionMBase should work" $ do
      l &&~ r `shouldBe` (mrgIf (Or (SSBool "a") (SSBool "b")) (mrgSingle False) (mrgSingle True) :: UnionMBase SBool Bool)
    it "nots for UnionMBase should work" $ do
      nots l `shouldBe` mrgIf (Not $ SSBool "a") (mrgSingle False) (mrgSingle True)
    it "xors for UnionMBase should work" $ do
      l
        `xors` r
        `shouldBe` ( mrgIf
                       (ITE (SSBool "a") (SSBool "b") (Not $ SSBool "b"))
                       (mrgSingle False)
                       (mrgSingle True) ::
                       UnionMBase SBool Bool
                   )
    it "implies for UnionMBase should work" $ do
      l
        `implies` r
        `shouldBe` ( mrgIf
                       (And (Not $ SSBool "a") (SSBool "b"))
                       (mrgSingle False)
                       (mrgSingle True) ::
                       UnionMBase SBool Bool
                   )
  describe "PrimWrapper for UnionMBase" $ do
    it "PrimWrapper should work for UnionMBase" $ do
      conc True `shouldBe` (mrgSingle $ CBool True :: UnionMBase SBool SBool)
      ssymb "a" `shouldBe` (mrgSingle $ SSBool "a" :: UnionMBase SBool SBool)
      isymb "a" 0 `shouldBe` (mrgSingle $ ISBool "a" 0 :: UnionMBase SBool SBool)
      concView (mrgSingle $ CBool True :: UnionMBase SBool SBool) `shouldBe` Just True
      concView (mrgSingle $ SSBool "a" :: UnionMBase SBool SBool) `shouldBe` Nothing
      concView
        ( mrgIf
            (SSBool "a")
            (mrgSingle $ CBool False)
            (mrgSingle $ CBool True) ::
            UnionMBase SBool SBool
        )
        `shouldBe` Nothing
  describe "Function class for UnionMBase" $ do
    it "Applying function in UnionMBase" $ do
      let func = mrgIf (SSBool "a") (mrgSingle (+ 1)) (mrgSingle (+ 2)) :: UnionMBase SBool (Integer -> Integer)
      func # (1 :: Integer) `shouldBe` mrgIf (SSBool "a") (mrgSingle 2) (mrgSingle 3)
    it "Helper for applying on UnionMBase" $ do
      let func (x :: Integer) = mrgIf (SSBool "a") (mrgSingle $ x + 1) (mrgSingle $ x + 3)
      (func #~ (mrgIf (SSBool "b") (mrgSingle 0) (mrgSingle 1) :: UnionMBase SBool Integer))
        `shouldBe` ( mrgIf
                       (SSBool "b")
                       (mrgIf (SSBool "a") (mrgSingle 1) (mrgSingle 3))
                       (mrgIf (SSBool "a") (mrgSingle 2) (mrgSingle 4)) ::
                       UnionMBase SBool Integer
                   )
  describe "IsString for UnionMBase" $ do
    it "IsString for UnionMBase should work" $ do
      ("x" :: UnionMBase SBool B.ByteString) `shouldBe` mrgSingle "x"
  describe "GenSym for UnionMBase" $ do
    it "GenSym for UnionMBase with spec" $ do
      (genSym (ListSpec 1 3 ()) "a" :: UnionMBase SBool (UnionMBase SBool [SBool]))
        `shouldBe` mrgSingle
          ( mrgIf
              (ISBool "a" 3)
              (mrgSingle [ISBool "a" 2])
              ( mrgIf
                  (ISBool "a" 4)
                  (mrgSingle [ISBool "a" 1, ISBool "a" 2])
                  (mrgSingle [ISBool "a" 0, ISBool "a" 1, ISBool "a" 2])
              )
          )
      (genSymSimple (ListSpec 1 3 ()) "a" :: UnionMBase SBool [SBool])
        `shouldBe` mrgIf
          (ISBool "a" 3)
          (mrgSingle [ISBool "a" 2])
          ( mrgIf
              (ISBool "a" 4)
              (mrgSingle [ISBool "a" 1, ISBool "a" 2])
              (mrgSingle [ISBool "a" 0, ISBool "a" 1, ISBool "a" 2])
          )
    it "GenSym for UnionMBase with same shape" $ do
      ( genSym
          ( mrgIf
              (SSBool "a")
              (mrgSingle [SSBool "x"])
              (mrgSingle [SSBool "y", SSBool "z"]) ::
              UnionMBase SBool [SBool]
          )
          "a" ::
          UnionMBase SBool [SBool]
        )
        `shouldBe` mrgIf (ISBool "a" 0) (mrgSingle [ISBool "a" 1]) (mrgSingle [ISBool "a" 2, ISBool "a" 3])
  describe "Concrete Key HashMaps" $ do
    it "Concrete Key HashMap should work" $ do
      mrgIte
        (SSBool "a")
        ( ML.fromList [(1, mrgSingle $ Just 1), (2, mrgSingle $ Just 2)] ::
            ML.HashMap Integer (UnionMBase SBool (Maybe Integer))
        )
        (ML.fromList [(1, mrgSingle $ Just 2), (3, mrgSingle $ Just 3)])
        `shouldBe` ML.fromList
          [ (1, mrgIf (SSBool "a") (mrgSingle $ Just 1) (mrgSingle $ Just 2)),
            (2, mrgIf (Not $ SSBool "a") (mrgSingle Nothing) (mrgSingle $ Just 2)),
            (3, mrgIf (SSBool "a") (mrgSingle Nothing) (mrgSingle $ Just 3))
          ]
      mrgIf
        (SSBool "a")
        ( mrgSingle $ ML.fromList [(1, mrgSingle $ Just 1), (2, mrgSingle $ Just 2)] ::
            UnionMBase SBool (ML.HashMap Integer (UnionMBase SBool (Maybe Integer)))
        )
        (mrgSingle (ML.fromList [(1, mrgSingle $ Just 2), (3, mrgSingle $ Just 3)]))
        `shouldBe` mrgSingle
          ( ML.fromList
              [ (1, mrgIf (SSBool "a") (mrgSingle $ Just 1) (mrgSingle $ Just 2)),
                (2, mrgIf (Not $ SSBool "a") (mrgSingle Nothing) (mrgSingle $ Just 2)),
                (3, mrgIf (SSBool "a") (mrgSingle Nothing) (mrgSingle $ Just 3))
              ]
          )
