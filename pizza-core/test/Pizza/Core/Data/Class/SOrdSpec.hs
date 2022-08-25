{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Pizza.Core.Data.Class.SOrdSpec where

import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.Trans.Maybe
import qualified Control.Monad.Writer.Lazy as WriterLazy
import qualified Control.Monad.Writer.Strict as WriterStrict
import Data.Bifunctor
import qualified Data.ByteString as B
import Data.Foldable
import Data.Functor.Sum
import Data.Int
import Data.Word
import Pizza.Core.Control.Monad.UnionMBase
import Pizza.Core.Data.Class.Bool
import Pizza.Core.Data.Class.SOrd
import Pizza.Core.Data.Class.SimpleMergeable
import Pizza.TestUtils.SBool
import Pizza.TestUtils.SOrd
import Test.Hspec
import Test.Hspec.QuickCheck

spec :: Spec
spec = do
  describe "SOrd for common types" $ do
    describe "SOrd for SBool" $ do
      it "SOrd for concrete SBool should work" $ do
        CBool False <=~ CBool False `shouldBe` CBool True
        CBool False <~ CBool False `shouldBe` CBool False
        CBool False >=~ CBool False `shouldBe` CBool True
        CBool False >~ CBool False `shouldBe` CBool False
        CBool False <=~ CBool True `shouldBe` CBool True
        CBool False <~ CBool True `shouldBe` CBool True
        CBool False >=~ CBool True `shouldBe` CBool False
        CBool False >~ CBool True `shouldBe` CBool False
        CBool True <=~ CBool False `shouldBe` CBool False
        CBool True <~ CBool False `shouldBe` CBool False
        CBool True >=~ CBool False `shouldBe` CBool True
        CBool True >~ CBool False `shouldBe` CBool True
        CBool True <=~ CBool True `shouldBe` CBool True
        CBool True <~ CBool True `shouldBe` CBool False
        CBool True >=~ CBool True `shouldBe` CBool True
        CBool True >~ CBool True `shouldBe` CBool False
      it "SOrd for symbolic SBool should work" $ do
        SSBool "a" <=~ SSBool "b" `shouldBe` Or (Not (SSBool "a")) (SSBool "b")
        SSBool "a" <~ SSBool "b" `shouldBe` And (Not (SSBool "a")) (SSBool "b")
        SSBool "a" >=~ SSBool "b" `shouldBe` Or (SSBool "a") (Not (SSBool "b"))
        SSBool "a" >~ SSBool "b" `shouldBe` And (SSBool "a") (Not (SSBool "b"))
        symCompare (SSBool "a") (SSBool "b")
          `shouldBe` ( mrgIf
                         (And (Not (SSBool "a")) (SSBool "b"))
                         (mrgSingle LT)
                         ( mrgIf
                             (Equal (SSBool "a") (SSBool "b"))
                             (mrgSingle EQ)
                             (mrgSingle GT)
                         ) ::
                         UnionMBase SBool Ordering
                     )
    describe "SOrd for Bool" $ do
      prop "SOrd for Bool should work" (concreteOrdOkProp @Bool)
    describe "SOrd for Integer" $ do
      prop "SOrd for Integer should work" (concreteOrdOkProp @Integer)
    describe "SOrd for Char" $ do
      prop "SOrd for Char should work" (concreteOrdOkProp @Char)
    describe "SOrd for Int" $ do
      prop "SOrd for Int should work" (concreteOrdOkProp @Int)
    describe "SOrd for Int8" $ do
      prop "SOrd for Int8 should work" (concreteOrdOkProp @Int8)
    describe "SOrd for Int16" $ do
      prop "SOrd for Int16 should work" (concreteOrdOkProp @Int16)
    describe "SOrd for Int32" $ do
      prop "SOrd for Int32 should work" (concreteOrdOkProp @Int32)
    describe "SOrd for Int64" $ do
      prop "SOrd for Int64 should work" (concreteOrdOkProp @Int64)
    describe "SOrd for Word" $ do
      prop "SOrd for Word should work" (concreteOrdOkProp @Word)
    describe "SOrd for Word8" $ do
      prop "SOrd for Word8 should work" (concreteOrdOkProp @Word8)
    describe "SOrd for Word16" $ do
      prop "SOrd for Word16 should work" (concreteOrdOkProp @Word16)
    describe "SOrd for Word32" $ do
      prop "SOrd for Word32 should work" (concreteOrdOkProp @Word32)
    describe "SOrd for Word64" $ do
      prop "SOrd for Word64 should work" (concreteOrdOkProp @Word64)
    describe "SOrd for List" $ do
      prop "SOrd for concrete List should work" (concreteOrdOkProp @[Integer])
      prop "SOrd for concrete String should work" (concreteOrdOkProp @String)
      it "SOrd for general List should work" $ do
        ([] :: [SBool]) <=~ [] `shouldBe` CBool True
        ([] :: [SBool]) <~ [] `shouldBe` CBool False
        ([] :: [SBool]) >=~ [] `shouldBe` CBool True
        ([] :: [SBool]) >~ [] `shouldBe` CBool False
        ([] :: [SBool]) `symCompare` [] `shouldBe` (mrgSingle EQ :: UnionMBase SBool Ordering)
        [] <=~ [SSBool "a"] `shouldBe` CBool True
        [] <~ [SSBool "a"] `shouldBe` CBool True
        [] >=~ [SSBool "a"] `shouldBe` CBool False
        [] >~ [SSBool "a"] `shouldBe` CBool False
        [] `symCompare` [SSBool "a"] `shouldBe` (mrgSingle LT :: UnionMBase SBool Ordering)
        [SSBool "a"] <=~ [] `shouldBe` CBool False
        [SSBool "a"] <~ [] `shouldBe` CBool False
        [SSBool "a"] >=~ [] `shouldBe` CBool True
        [SSBool "a"] >~ [] `shouldBe` CBool True
        [SSBool "a"] `symCompare` [] `shouldBe` (mrgSingle GT :: UnionMBase SBool Ordering)

        [SSBool "a", SSBool "b"] <=~ [SSBool "c"]
          `shouldBe` (SSBool "a" <~ SSBool "c" :: SBool)
        [SSBool "a", SSBool "b"]
          <~ [SSBool "c"]
          `shouldBe` (SSBool "a" <~ SSBool "c" :: SBool)
        [SSBool "a", SSBool "b"] >=~ [SSBool "c"]
          `shouldBe` ((SSBool "a" >~ SSBool "c") ||~ (SSBool "a" ==~ SSBool "c") :: SBool)
        [SSBool "a", SSBool "b"] >~ [SSBool "c"]
          `shouldBe` ((SSBool "a" >~ SSBool "c") ||~ (SSBool "a" ==~ SSBool "c") :: SBool)
        [SSBool "a"]
          `symCompare` [SSBool "b"]
          `shouldBe` (SSBool "a" `symCompare` SSBool "b" :: UnionMBase SBool Ordering)

        [SSBool "a"] <=~ [SSBool "b", SSBool "c"]
          `shouldBe` ((SSBool "a" <~ SSBool "b") ||~ (SSBool "a" ==~ SSBool "b") :: SBool)
        [SSBool "a"]
          <~ [SSBool "b", SSBool "c"]
          `shouldBe` ((SSBool "a" <~ SSBool "b") ||~ (SSBool "a" ==~ SSBool "b") :: SBool)
        [SSBool "a"] >=~ [SSBool "b", SSBool "c"]
          `shouldBe` (SSBool "a" >~ SSBool "b" :: SBool)
        [SSBool "a"] >~ [SSBool "b", SSBool "c"]
          `shouldBe` (SSBool "a" >~ SSBool "b" :: SBool)
        [SSBool "a"]
          `symCompare` [SSBool "b", SSBool "c"]
          `shouldBe` ( mrgIf
                         (SSBool "a" <~ SSBool "b")
                         (mrgSingle LT)
                         (mrgIf (SSBool "a" ==~ SSBool "b") (mrgSingle LT) (mrgSingle GT)) ::
                         UnionMBase SBool Ordering
                     )

        [SSBool "a", SSBool "b"] <=~ [SSBool "c", SSBool "d"]
          `shouldBe` ( (SSBool "a" <~ SSBool "c")
                         ||~ ( SSBool "a"
                                 ==~ SSBool "c"
                                 &&~ ((SSBool "b" <~ SSBool "d") ||~ (SSBool "b" ==~ SSBool "d"))
                             ) ::
                         SBool
                     )
        [SSBool "a", SSBool "b"]
          <~ [SSBool "c", SSBool "d"]
          `shouldBe` ( (SSBool "a" <~ SSBool "c")
                         ||~ ( SSBool "a"
                                 ==~ SSBool "c"
                                 &&~ (SSBool "b" <~ SSBool "d")
                             ) ::
                         SBool
                     )
        [SSBool "a", SSBool "b"] >=~ [SSBool "c", SSBool "d"]
          `shouldBe` ( (SSBool "a" >~ SSBool "c")
                         ||~ ( SSBool "a"
                                 ==~ SSBool "c"
                                 &&~ ((SSBool "b" >~ SSBool "d") ||~ (SSBool "b" ==~ SSBool "d"))
                             ) ::
                         SBool
                     )
        [SSBool "a", SSBool "b"] >~ [SSBool "c", SSBool "d"]
          `shouldBe` ( (SSBool "a" >~ SSBool "c")
                         ||~ ( SSBool "a"
                                 ==~ SSBool "c"
                                 &&~ (SSBool "b" >~ SSBool "d")
                             ) ::
                         SBool
                     )
        [SSBool "a", SSBool "b"]
          `symCompare` [SSBool "c", SSBool "d"]
          `shouldBe` ( mrgIf
                         (SSBool "a" <~ SSBool "c")
                         (mrgSingle LT)
                         ( mrgIf
                             (SSBool "a" ==~ SSBool "c")
                             (SSBool "b" `symCompare` SSBool "d")
                             (mrgSingle GT)
                         ) ::
                         UnionMBase SBool Ordering
                     )
    describe "SOrd for Maybe" $ do
      prop "SOrd for concrete Maybe should work" (concreteOrdOkProp @(Maybe Integer))
      it "SOrd for general Maybe should work" $ do
        (Nothing :: Maybe SBool) <=~ Nothing `shouldBe` CBool True
        (Nothing :: Maybe SBool) <~ Nothing `shouldBe` CBool False
        (Nothing :: Maybe SBool) >=~ Nothing `shouldBe` CBool True
        (Nothing :: Maybe SBool) >~ Nothing `shouldBe` CBool False
        (Nothing :: Maybe SBool) `symCompare` Nothing `shouldBe` (mrgSingle EQ :: UnionMBase SBool Ordering)
        Nothing <=~ Just (SSBool "a") `shouldBe` CBool True
        Nothing <~ Just (SSBool "a") `shouldBe` CBool True
        Nothing >=~ Just (SSBool "a") `shouldBe` CBool False
        Nothing >~ Just (SSBool "a") `shouldBe` CBool False
        Nothing `symCompare` Just (SSBool "a") `shouldBe` (mrgSingle LT :: UnionMBase SBool Ordering)
        Just (SSBool "a") <=~ Nothing `shouldBe` CBool False
        Just (SSBool "a") <~ Nothing `shouldBe` CBool False
        Just (SSBool "a") >=~ Nothing `shouldBe` CBool True
        Just (SSBool "a") >~ Nothing `shouldBe` CBool True
        Just (SSBool "a") `symCompare` Nothing `shouldBe` (mrgSingle GT :: UnionMBase SBool Ordering)
        Just (SSBool "a") <=~ Just (SSBool "b") `shouldBe` (SSBool "a" <=~ SSBool "b" :: SBool)
        Just (SSBool "a") <~ Just (SSBool "b") `shouldBe` (SSBool "a" <~ SSBool "b" :: SBool)
        Just (SSBool "a") >=~ Just (SSBool "b") `shouldBe` (SSBool "a" >=~ SSBool "b" :: SBool)
        Just (SSBool "a") >~ Just (SSBool "b") `shouldBe` (SSBool "a" >~ SSBool "b" :: SBool)
        Just (SSBool "a")
          `symCompare` Just (SSBool "b")
          `shouldBe` (SSBool "a" `symCompare` SSBool "b" :: UnionMBase SBool Ordering)
    describe "SOrd for MaybeT" $ do
      prop "SOrd for concrete MaybeT should work" (concreteOrdOkProp @(MaybeT Maybe Integer) . bimap MaybeT MaybeT)
      it "SOrd for general MaybeT should work" $ do
        (MaybeT Nothing :: MaybeT Maybe SBool) <=~ MaybeT Nothing `shouldBe` CBool True
        (MaybeT Nothing :: MaybeT Maybe SBool) <=~ MaybeT (Just (Just (SSBool "a"))) `shouldBe` CBool True
        MaybeT (Just (Just (SSBool "a"))) <=~ (MaybeT Nothing :: MaybeT Maybe SBool) `shouldBe` CBool False
        MaybeT (Just (Just (SSBool "a"))) <=~ (MaybeT (Just (Just (SSBool "b"))) :: MaybeT Maybe SBool)
          `shouldBe` (SSBool "a" <=~ SSBool "b" :: SBool)

        (MaybeT Nothing :: MaybeT Maybe SBool) <~ MaybeT Nothing `shouldBe` CBool False
        (MaybeT Nothing :: MaybeT Maybe SBool) <~ MaybeT (Just (Just (SSBool "a"))) `shouldBe` CBool True
        MaybeT (Just (Just (SSBool "a"))) <~ (MaybeT Nothing :: MaybeT Maybe SBool) `shouldBe` CBool False
        MaybeT (Just (Just (SSBool "a")))
          <~ (MaybeT (Just (Just (SSBool "b"))) :: MaybeT Maybe SBool)
          `shouldBe` (SSBool "a" <~ SSBool "b" :: SBool)

        (MaybeT Nothing :: MaybeT Maybe SBool) >=~ MaybeT Nothing `shouldBe` CBool True
        (MaybeT Nothing :: MaybeT Maybe SBool) >=~ MaybeT (Just (Just (SSBool "a"))) `shouldBe` CBool False
        MaybeT (Just (Just (SSBool "a"))) >=~ (MaybeT Nothing :: MaybeT Maybe SBool) `shouldBe` CBool True
        MaybeT (Just (Just (SSBool "a"))) >=~ (MaybeT (Just (Just (SSBool "b"))) :: MaybeT Maybe SBool)
          `shouldBe` (SSBool "a" >=~ SSBool "b" :: SBool)

        (MaybeT Nothing :: MaybeT Maybe SBool) >~ MaybeT Nothing `shouldBe` CBool False
        (MaybeT Nothing :: MaybeT Maybe SBool) >~ MaybeT (Just (Just (SSBool "a"))) `shouldBe` CBool False
        MaybeT (Just (Just (SSBool "a"))) >~ (MaybeT Nothing :: MaybeT Maybe SBool) `shouldBe` CBool True
        MaybeT (Just (Just (SSBool "a"))) >~ (MaybeT (Just (Just (SSBool "b"))) :: MaybeT Maybe SBool)
          `shouldBe` (SSBool "a" >~ SSBool "b" :: SBool)

        (MaybeT Nothing :: MaybeT Maybe SBool)
          `symCompare` MaybeT Nothing
          `shouldBe` (mrgSingle EQ :: UnionMBase SBool Ordering)
        (MaybeT Nothing :: MaybeT Maybe SBool)
          `symCompare` MaybeT (Just (Just (SSBool "a")))
          `shouldBe` (mrgSingle LT :: UnionMBase SBool Ordering)
        MaybeT (Just (Just (SSBool "a")))
          `symCompare` (MaybeT Nothing :: MaybeT Maybe SBool)
          `shouldBe` (mrgSingle GT :: UnionMBase SBool Ordering)
        MaybeT (Just (Just (SSBool "a")))
          `symCompare` (MaybeT (Just (Just (SSBool "b"))) :: MaybeT Maybe SBool)
          `shouldBe` (SSBool "a" `symCompare` SSBool "b" :: UnionMBase SBool Ordering)
    describe "SOrd for Either" $ do
      prop "SOrd for concrete Either should work" (concreteOrdOkProp @(Either Integer Integer))
      it "SOrd for general Either should work" $ do
        (Left (SSBool "a") :: Either SBool SBool) <=~ Left (SSBool "b") `shouldBe` (SSBool "a" <=~ SSBool "b" :: SBool)
        (Left (SSBool "a") :: Either SBool SBool) <~ Left (SSBool "b") `shouldBe` (SSBool "a" <~ SSBool "b" :: SBool)
        (Left (SSBool "a") :: Either SBool SBool) >=~ Left (SSBool "b") `shouldBe` (SSBool "a" >=~ SSBool "b" :: SBool)
        (Left (SSBool "a") :: Either SBool SBool) >~ Left (SSBool "b") `shouldBe` (SSBool "a" >~ SSBool "b" :: SBool)
        (Left (SSBool "a") :: Either SBool SBool)
          `symCompare` Left (SSBool "b")
          `shouldBe` (SSBool "a" `symCompare` SSBool "b" :: UnionMBase SBool Ordering)
        (Left (SSBool "a") :: Either SBool SBool) <=~ Right (SSBool "b") `shouldBe` CBool True
        (Left (SSBool "a") :: Either SBool SBool) <~ Right (SSBool "b") `shouldBe` CBool True
        (Left (SSBool "a") :: Either SBool SBool) >=~ Right (SSBool "b") `shouldBe` CBool False
        (Left (SSBool "a") :: Either SBool SBool) >~ Right (SSBool "b") `shouldBe` CBool False
        (Left (SSBool "a") :: Either SBool SBool)
          `symCompare` Right (SSBool "b")
          `shouldBe` (mrgSingle LT :: UnionMBase SBool Ordering)
        (Right (SSBool "a") :: Either SBool SBool) <=~ Left (SSBool "b") `shouldBe` CBool False
        (Right (SSBool "a") :: Either SBool SBool) <~ Left (SSBool "b") `shouldBe` CBool False
        (Right (SSBool "a") :: Either SBool SBool) >=~ Left (SSBool "b") `shouldBe` CBool True
        (Right (SSBool "a") :: Either SBool SBool) >~ Left (SSBool "b") `shouldBe` CBool True
        (Right (SSBool "a") :: Either SBool SBool)
          `symCompare` Left (SSBool "b")
          `shouldBe` (mrgSingle GT :: UnionMBase SBool Ordering)
        (Right (SSBool "a") :: Either SBool SBool) <=~ Right (SSBool "b") `shouldBe` (SSBool "a" <=~ SSBool "b" :: SBool)
        (Right (SSBool "a") :: Either SBool SBool) <~ Right (SSBool "b") `shouldBe` (SSBool "a" <~ SSBool "b" :: SBool)
        (Right (SSBool "a") :: Either SBool SBool) >=~ Right (SSBool "b") `shouldBe` (SSBool "a" >=~ SSBool "b" :: SBool)
        (Right (SSBool "a") :: Either SBool SBool) >~ Right (SSBool "b") `shouldBe` (SSBool "a" >~ SSBool "b" :: SBool)
        (Right (SSBool "a") :: Either SBool SBool)
          `symCompare` Right (SSBool "b")
          `shouldBe` (SSBool "a" `symCompare` SSBool "b" :: UnionMBase SBool Ordering)
    describe "SOrd for ExceptT" $ do
      prop "SOrd for concrete ExceptT should work" (concreteOrdOkProp @(ExceptT Integer Maybe Integer) . bimap ExceptT ExceptT)
      it "SOrd for general ExceptT should work" $ do
        (ExceptT Nothing :: ExceptT SBool Maybe SBool) <=~ ExceptT Nothing `shouldBe` CBool True
        (ExceptT Nothing :: ExceptT SBool Maybe SBool) <=~ ExceptT (Just (Left (SSBool "a"))) `shouldBe` CBool True
        (ExceptT Nothing :: ExceptT SBool Maybe SBool) <=~ ExceptT (Just (Right (SSBool "a"))) `shouldBe` CBool True
        ExceptT (Just (Left (SSBool "a"))) <=~ (ExceptT Nothing :: ExceptT SBool Maybe SBool) `shouldBe` CBool False
        ExceptT (Just (Right (SSBool "a"))) <=~ (ExceptT Nothing :: ExceptT SBool Maybe SBool) `shouldBe` CBool False
        ExceptT (Just (Left (SSBool "a"))) <=~ (ExceptT (Just (Left (SSBool "b"))) :: ExceptT SBool Maybe SBool)
          `shouldBe` (SSBool "a" <=~ SSBool "b" :: SBool)
        ExceptT (Just (Right (SSBool "a"))) <=~ (ExceptT (Just (Left (SSBool "b"))) :: ExceptT SBool Maybe SBool)
          `shouldBe` CBool False
        ExceptT (Just (Left (SSBool "a"))) <=~ (ExceptT (Just (Right (SSBool "b"))) :: ExceptT SBool Maybe SBool)
          `shouldBe` CBool True
        ExceptT (Just (Right (SSBool "a"))) <=~ (ExceptT (Just (Right (SSBool "b"))) :: ExceptT SBool Maybe SBool)
          `shouldBe` (SSBool "a" <=~ SSBool "b" :: SBool)

        (ExceptT Nothing :: ExceptT SBool Maybe SBool) <~ ExceptT Nothing `shouldBe` CBool False
        (ExceptT Nothing :: ExceptT SBool Maybe SBool) <~ ExceptT (Just (Left (SSBool "a"))) `shouldBe` CBool True
        (ExceptT Nothing :: ExceptT SBool Maybe SBool) <~ ExceptT (Just (Right (SSBool "a"))) `shouldBe` CBool True
        ExceptT (Just (Left (SSBool "a"))) <~ (ExceptT Nothing :: ExceptT SBool Maybe SBool) `shouldBe` CBool False
        ExceptT (Just (Right (SSBool "a"))) <~ (ExceptT Nothing :: ExceptT SBool Maybe SBool) `shouldBe` CBool False
        ExceptT (Just (Left (SSBool "a")))
          <~ (ExceptT (Just (Left (SSBool "b"))) :: ExceptT SBool Maybe SBool)
          `shouldBe` (SSBool "a" <~ SSBool "b" :: SBool)
        ExceptT (Just (Right (SSBool "a")))
          <~ (ExceptT (Just (Left (SSBool "b"))) :: ExceptT SBool Maybe SBool)
          `shouldBe` CBool False
        ExceptT (Just (Left (SSBool "a")))
          <~ (ExceptT (Just (Right (SSBool "b"))) :: ExceptT SBool Maybe SBool)
          `shouldBe` CBool True
        ExceptT (Just (Right (SSBool "a")))
          <~ (ExceptT (Just (Right (SSBool "b"))) :: ExceptT SBool Maybe SBool)
          `shouldBe` (SSBool "a" <~ SSBool "b" :: SBool)

        (ExceptT Nothing :: ExceptT SBool Maybe SBool) >=~ ExceptT Nothing `shouldBe` CBool True
        (ExceptT Nothing :: ExceptT SBool Maybe SBool) >=~ ExceptT (Just (Left (SSBool "a"))) `shouldBe` CBool False
        (ExceptT Nothing :: ExceptT SBool Maybe SBool) >=~ ExceptT (Just (Right (SSBool "a"))) `shouldBe` CBool False
        ExceptT (Just (Left (SSBool "a"))) >=~ (ExceptT Nothing :: ExceptT SBool Maybe SBool) `shouldBe` CBool True
        ExceptT (Just (Right (SSBool "a"))) >=~ (ExceptT Nothing :: ExceptT SBool Maybe SBool) `shouldBe` CBool True
        ExceptT (Just (Left (SSBool "a"))) >=~ (ExceptT (Just (Left (SSBool "b"))) :: ExceptT SBool Maybe SBool)
          `shouldBe` (SSBool "a" >=~ SSBool "b" :: SBool)
        ExceptT (Just (Right (SSBool "a"))) >=~ (ExceptT (Just (Left (SSBool "b"))) :: ExceptT SBool Maybe SBool)
          `shouldBe` CBool True
        ExceptT (Just (Left (SSBool "a"))) >=~ (ExceptT (Just (Right (SSBool "b"))) :: ExceptT SBool Maybe SBool)
          `shouldBe` CBool False
        ExceptT (Just (Right (SSBool "a"))) >=~ (ExceptT (Just (Right (SSBool "b"))) :: ExceptT SBool Maybe SBool)
          `shouldBe` (SSBool "a" >=~ SSBool "b" :: SBool)

        (ExceptT Nothing :: ExceptT SBool Maybe SBool) >~ ExceptT Nothing `shouldBe` CBool False
        (ExceptT Nothing :: ExceptT SBool Maybe SBool) >~ ExceptT (Just (Left (SSBool "a"))) `shouldBe` CBool False
        (ExceptT Nothing :: ExceptT SBool Maybe SBool) >~ ExceptT (Just (Right (SSBool "a"))) `shouldBe` CBool False
        ExceptT (Just (Left (SSBool "a"))) >~ (ExceptT Nothing :: ExceptT SBool Maybe SBool) `shouldBe` CBool True
        ExceptT (Just (Right (SSBool "a"))) >~ (ExceptT Nothing :: ExceptT SBool Maybe SBool) `shouldBe` CBool True
        ExceptT (Just (Left (SSBool "a"))) >~ (ExceptT (Just (Left (SSBool "b"))) :: ExceptT SBool Maybe SBool)
          `shouldBe` (SSBool "a" >~ SSBool "b" :: SBool)
        ExceptT (Just (Right (SSBool "a"))) >~ (ExceptT (Just (Left (SSBool "b"))) :: ExceptT SBool Maybe SBool)
          `shouldBe` CBool True
        ExceptT (Just (Left (SSBool "a"))) >~ (ExceptT (Just (Right (SSBool "b"))) :: ExceptT SBool Maybe SBool)
          `shouldBe` CBool False
        ExceptT (Just (Right (SSBool "a"))) >~ (ExceptT (Just (Right (SSBool "b"))) :: ExceptT SBool Maybe SBool)
          `shouldBe` (SSBool "a" >~ SSBool "b" :: SBool)

        (ExceptT Nothing :: ExceptT SBool Maybe SBool)
          `symCompare` ExceptT Nothing
          `shouldBe` (mrgSingle EQ :: UnionMBase SBool Ordering)
        (ExceptT Nothing :: ExceptT SBool Maybe SBool)
          `symCompare` ExceptT (Just (Left (SSBool "a")))
          `shouldBe` (mrgSingle LT :: UnionMBase SBool Ordering)
        (ExceptT Nothing :: ExceptT SBool Maybe SBool)
          `symCompare` ExceptT (Just (Right (SSBool "a")))
          `shouldBe` (mrgSingle LT :: UnionMBase SBool Ordering)
        ExceptT (Just (Left (SSBool "a")))
          `symCompare` (ExceptT Nothing :: ExceptT SBool Maybe SBool)
          `shouldBe` (mrgSingle GT :: UnionMBase SBool Ordering)
        ExceptT (Just (Right (SSBool "a")))
          `symCompare` (ExceptT Nothing :: ExceptT SBool Maybe SBool)
          `shouldBe` (mrgSingle GT :: UnionMBase SBool Ordering)
        ExceptT (Just (Left (SSBool "a")))
          `symCompare` (ExceptT (Just (Left (SSBool "b"))) :: ExceptT SBool Maybe SBool)
          `shouldBe` (SSBool "a" `symCompare` SSBool "b" :: UnionMBase SBool Ordering)
        ExceptT (Just (Right (SSBool "a")))
          `symCompare` (ExceptT (Just (Left (SSBool "b"))) :: ExceptT SBool Maybe SBool)
          `shouldBe` (mrgSingle GT :: UnionMBase SBool Ordering)
        ExceptT (Just (Left (SSBool "a")))
          `symCompare` (ExceptT (Just (Right (SSBool "b"))) :: ExceptT SBool Maybe SBool)
          `shouldBe` (mrgSingle LT :: UnionMBase SBool Ordering)
        ExceptT (Just (Right (SSBool "a")))
          `symCompare` (ExceptT (Just (Right (SSBool "b"))) :: ExceptT SBool Maybe SBool)
          `shouldBe` (SSBool "a" `symCompare` SSBool "b" :: UnionMBase SBool Ordering)

    describe "SOrd for ()" $ do
      prop "SOrd for () should work" (concreteOrdOkProp @())
    describe "SOrd for (,)" $ do
      prop "SOrd for concrete (,) should work" (concreteOrdOkProp @(Integer, Integer))
      it "SOrd for general (,) should work" $ do
        let l = (SSBool "a", SSBool "c")
        let r = (SSBool "b", SSBool "d")
        let ll = SSBool "a"
        let lr = SSBool "c"
        let rl = SSBool "b"
        let rr = SSBool "d"
        symbolicProdOrdOkProp l r ll lr rl rr
    describe "SOrd for (,,)" $ do
      prop "SOrd for concrete (,,) should work" (concreteOrdOkProp @(Integer, Integer, Integer))
      it "SOrd for general (,,) should work" $ do
        let l = (SSBool "a", SSBool "c", SSBool "e")
        let r = (SSBool "b", SSBool "d", SSBool "f")
        let ll = SSBool "a"
        let lr = (SSBool "c", SSBool "e")
        let rl = SSBool "b"
        let rr = (SSBool "d", SSBool "f")
        symbolicProdOrdOkProp l r ll lr rl rr
    describe "SOrd for (,,,)" $ do
      prop "SOrd for concrete (,,,) should work" (concreteOrdOkProp @(Integer, Integer, Integer, Integer))
      it "SOrd for general (,,,) should work" $ do
        let l = (SSBool "a", SSBool "c", SSBool "e", SSBool "g")
        let r = (SSBool "b", SSBool "d", SSBool "f", SSBool "h")
        let ll = (SSBool "a", SSBool "c")
        let lr = (SSBool "e", SSBool "g")
        let rl = (SSBool "b", SSBool "d")
        let rr = (SSBool "f", SSBool "h")
        symbolicProdOrdOkProp l r ll lr rl rr
    describe "SOrd for (,,,,)" $ do
      prop "SOrd for concrete (,,,,) should work" (concreteOrdOkProp @(Integer, Integer, Integer, Integer, Integer))
      it "SOrd for general (,,,,) should work" $ do
        let l = (SSBool "a", SSBool "c", SSBool "e", SSBool "g", SSBool "i")
        let r = (SSBool "b", SSBool "d", SSBool "f", SSBool "h", SSBool "j")
        let ll = (SSBool "a", SSBool "c")
        let lr = (SSBool "e", SSBool "g", SSBool "i")
        let rl = (SSBool "b", SSBool "d")
        let rr = (SSBool "f", SSBool "h", SSBool "j")
        symbolicProdOrdOkProp l r ll lr rl rr
    describe "SOrd for (,,,,,)" $ do
      prop
        "SOrd for concrete (,,,,,) should work"
        (concreteOrdOkProp @(Integer, Integer, Integer, Integer, Integer, Integer))
      it "SOrd for general (,,,,,) should work" $ do
        let l = (SSBool "a", SSBool "c", SSBool "e", SSBool "g", SSBool "i", SSBool "k")
        let r = (SSBool "b", SSBool "d", SSBool "f", SSBool "h", SSBool "j", SSBool "l")
        let ll = (SSBool "a", SSBool "c", SSBool "e")
        let lr = (SSBool "g", SSBool "i", SSBool "k")
        let rl = (SSBool "b", SSBool "d", SSBool "f")
        let rr = (SSBool "h", SSBool "j", SSBool "l")
        symbolicProdOrdOkProp l r ll lr rl rr
    describe "SOrd for (,,,,,,)" $ do
      prop
        "SOrd for concrete (,,,,,,) should work"
        (concreteOrdOkProp @(Integer, Integer, Integer, Integer, Integer, Integer, Integer))
      it "SOrd for general (,,,,,,) should work" $ do
        let l = (SSBool "a", SSBool "c", SSBool "e", SSBool "g", SSBool "i", SSBool "k", SSBool "m")
        let r = (SSBool "b", SSBool "d", SSBool "f", SSBool "h", SSBool "j", SSBool "l", SSBool "n")
        let ll = (SSBool "a", SSBool "c", SSBool "e")
        let lr = (SSBool "g", SSBool "i", SSBool "k", SSBool "m")
        let rl = (SSBool "b", SSBool "d", SSBool "f")
        let rr = (SSBool "h", SSBool "j", SSBool "l", SSBool "n")
        symbolicProdOrdOkProp l r ll lr rl rr
    describe "SOrd for (,,,,,,,)" $ do
      prop
        "SOrd for concrete (,,,,,,,) should work"
        (concreteOrdOkProp @(Integer, Integer, Integer, Integer, Integer, Integer, Integer, Integer))
      it "SOrd for general (,,,,,,,) should work" $ do
        let l = (SSBool "a", SSBool "c", SSBool "e", SSBool "g", SSBool "i", SSBool "k", SSBool "m", SSBool "o")
        let r = (SSBool "b", SSBool "d", SSBool "f", SSBool "h", SSBool "j", SSBool "l", SSBool "n", SSBool "p")
        let ll = (SSBool "a", SSBool "c", SSBool "e", SSBool "g")
        let lr = (SSBool "i", SSBool "k", SSBool "m", SSBool "o")
        let rl = (SSBool "b", SSBool "d", SSBool "f", SSBool "h")
        let rr = (SSBool "j", SSBool "l", SSBool "n", SSBool "p")
        symbolicProdOrdOkProp l r ll lr rl rr
    describe "SOrd for Sum" $ do
      prop
        "SOrd for concrete Sum should work"
        ( \v ->
            let eitherToSum :: Either (Maybe Integer) (Maybe Integer) -> Sum Maybe Maybe Integer
                eitherToSum (Left x) = InL x
                eitherToSum (Right x) = InR x
             in concreteOrdOkProp (bimap eitherToSum eitherToSum v)
        )
      it "SOrd for general Sum should work" $ do
        (InL $ Just $ SSBool "a" :: Sum Maybe Maybe SBool) <=~ InL (Just $ SSBool "b")
          `shouldBe` (SSBool "a" <=~ SSBool "b" :: SBool)
        (InL $ Just $ SSBool "a" :: Sum Maybe Maybe SBool)
          <~ InL (Just $ SSBool "b")
          `shouldBe` (SSBool "a" <~ SSBool "b" :: SBool)
        (InL $ Just $ SSBool "a" :: Sum Maybe Maybe SBool) >=~ InL (Just $ SSBool "b")
          `shouldBe` (SSBool "a" >=~ SSBool "b" :: SBool)
        (InL $ Just $ SSBool "a" :: Sum Maybe Maybe SBool) >~ InL (Just $ SSBool "b")
          `shouldBe` (SSBool "a" >~ SSBool "b" :: SBool)
        (InL $ Just $ SSBool "a" :: Sum Maybe Maybe SBool) <=~ InR (Just $ SSBool "b") `shouldBe` CBool True
        (InL $ Just $ SSBool "a" :: Sum Maybe Maybe SBool) <~ InR (Just $ SSBool "b") `shouldBe` CBool True
        (InL $ Just $ SSBool "a" :: Sum Maybe Maybe SBool) >=~ InR (Just $ SSBool "b") `shouldBe` CBool False
        (InL $ Just $ SSBool "a" :: Sum Maybe Maybe SBool) >~ InR (Just $ SSBool "b") `shouldBe` CBool False
        (InR $ Just $ SSBool "a" :: Sum Maybe Maybe SBool) <=~ InR (Just $ SSBool "b")
          `shouldBe` (SSBool "a" <=~ SSBool "b" :: SBool)
        (InR $ Just $ SSBool "a" :: Sum Maybe Maybe SBool)
          <~ InR (Just $ SSBool "b")
          `shouldBe` (SSBool "a" <~ SSBool "b" :: SBool)
        (InR $ Just $ SSBool "a" :: Sum Maybe Maybe SBool) >=~ InR (Just $ SSBool "b")
          `shouldBe` (SSBool "a" >=~ SSBool "b" :: SBool)
        (InR $ Just $ SSBool "a" :: Sum Maybe Maybe SBool) >~ InR (Just $ SSBool "b")
          `shouldBe` (SSBool "a" >~ SSBool "b" :: SBool)
        (InR $ Just $ SSBool "a" :: Sum Maybe Maybe SBool) <=~ InL (Just $ SSBool "b") `shouldBe` CBool False
        (InR $ Just $ SSBool "a" :: Sum Maybe Maybe SBool) <~ InL (Just $ SSBool "b") `shouldBe` CBool False
        (InR $ Just $ SSBool "a" :: Sum Maybe Maybe SBool) >=~ InL (Just $ SSBool "b") `shouldBe` CBool True
        (InR $ Just $ SSBool "a" :: Sum Maybe Maybe SBool) >~ InL (Just $ SSBool "b") `shouldBe` CBool True
    describe "SOrd for WriterT" $ do
      prop
        "SOrd for concrete Lazy WriterT should work"
        ( \(v1 :: Either Integer (Integer, Integer), v2 :: Either Integer (Integer, Integer)) ->
            concreteOrdOkProp (WriterLazy.WriterT v1, WriterLazy.WriterT v2)
        )
      prop
        "SOrd for concrete Strict WriterT should work"
        ( \(v1 :: Either Integer (Integer, Integer), v2 :: Either Integer (Integer, Integer)) ->
            concreteOrdOkProp (WriterStrict.WriterT v1, WriterStrict.WriterT v2)
        )
      it "SOrd for general Lazy WriterT should work" $ do
        (WriterLazy.WriterT $ Left $ SSBool "a" :: WriterLazy.WriterT SBool (Either SBool) SBool)
          <=~ WriterLazy.WriterT (Left $ SSBool "b")
          `shouldBe` (SSBool "a" <=~ SSBool "b" :: SBool)
        (WriterLazy.WriterT $ Left $ SSBool "a" :: WriterLazy.WriterT SBool (Either SBool) SBool)
          <~ WriterLazy.WriterT (Left $ SSBool "b")
          `shouldBe` (SSBool "a" <~ SSBool "b" :: SBool)
        (WriterLazy.WriterT $ Left $ SSBool "a" :: WriterLazy.WriterT SBool (Either SBool) SBool)
          >=~ WriterLazy.WriterT (Left $ SSBool "b")
          `shouldBe` (SSBool "a" >=~ SSBool "b" :: SBool)
        (WriterLazy.WriterT $ Left $ SSBool "a" :: WriterLazy.WriterT SBool (Either SBool) SBool)
          >~ WriterLazy.WriterT (Left $ SSBool "b")
          `shouldBe` (SSBool "a" >~ SSBool "b" :: SBool)
        (WriterLazy.WriterT $ Left $ SSBool "a" :: WriterLazy.WriterT SBool (Either SBool) SBool)
          `symCompare` WriterLazy.WriterT (Left $ SSBool "b")
          `shouldBe` (SSBool "a" `symCompare` SSBool "b" :: UnionMBase SBool Ordering)

        (WriterLazy.WriterT $ Left $ SSBool "a" :: WriterLazy.WriterT SBool (Either SBool) SBool)
          <=~ WriterLazy.WriterT (Right (SSBool "b", SSBool "d"))
          `shouldBe` CBool True
        (WriterLazy.WriterT $ Left $ SSBool "a" :: WriterLazy.WriterT SBool (Either SBool) SBool)
          <~ WriterLazy.WriterT (Right (SSBool "b", SSBool "d"))
          `shouldBe` CBool True
        (WriterLazy.WriterT $ Left $ SSBool "a" :: WriterLazy.WriterT SBool (Either SBool) SBool)
          >=~ WriterLazy.WriterT (Right (SSBool "b", SSBool "d"))
          `shouldBe` CBool False
        (WriterLazy.WriterT $ Left $ SSBool "a" :: WriterLazy.WriterT SBool (Either SBool) SBool)
          >~ WriterLazy.WriterT (Right (SSBool "b", SSBool "d"))
          `shouldBe` CBool False
        (WriterLazy.WriterT $ Left $ SSBool "a" :: WriterLazy.WriterT SBool (Either SBool) SBool)
          `symCompare` WriterLazy.WriterT (Right (SSBool "b", SSBool "d"))
          `shouldBe` (mrgSingle LT :: UnionMBase SBool Ordering)

        (WriterLazy.WriterT $ Right (SSBool "a", SSBool "c") :: WriterLazy.WriterT SBool (Either SBool) SBool)
          <=~ WriterLazy.WriterT (Left $ SSBool "b")
          `shouldBe` CBool False
        (WriterLazy.WriterT $ Right (SSBool "a", SSBool "c") :: WriterLazy.WriterT SBool (Either SBool) SBool)
          <~ WriterLazy.WriterT (Left $ SSBool "b")
          `shouldBe` CBool False
        (WriterLazy.WriterT $ Right (SSBool "a", SSBool "c") :: WriterLazy.WriterT SBool (Either SBool) SBool)
          >=~ WriterLazy.WriterT (Left $ SSBool "b")
          `shouldBe` CBool True
        (WriterLazy.WriterT $ Right (SSBool "a", SSBool "c") :: WriterLazy.WriterT SBool (Either SBool) SBool)
          >~ WriterLazy.WriterT (Left $ SSBool "b")
          `shouldBe` CBool True
        (WriterLazy.WriterT $ Right (SSBool "a", SSBool "c") :: WriterLazy.WriterT SBool (Either SBool) SBool)
          `symCompare` WriterLazy.WriterT (Left $ SSBool "b")
          `shouldBe` (mrgSingle GT :: UnionMBase SBool Ordering)

        (WriterLazy.WriterT $ Right (SSBool "a", SSBool "c") :: WriterLazy.WriterT SBool (Either SBool) SBool)
          <=~ WriterLazy.WriterT (Right (SSBool "b", SSBool "d"))
          `shouldBe` ((SSBool "a", SSBool "c") <=~ (SSBool "b", SSBool "d") :: SBool)
        (WriterLazy.WriterT $ Right (SSBool "a", SSBool "c") :: WriterLazy.WriterT SBool (Either SBool) SBool)
          <~ WriterLazy.WriterT (Right (SSBool "b", SSBool "d"))
          `shouldBe` ((SSBool "a", SSBool "c") <~ (SSBool "b", SSBool "d") :: SBool)
        (WriterLazy.WriterT $ Right (SSBool "a", SSBool "c") :: WriterLazy.WriterT SBool (Either SBool) SBool)
          >=~ WriterLazy.WriterT (Right (SSBool "b", SSBool "d"))
          `shouldBe` ((SSBool "a", SSBool "c") >=~ (SSBool "b", SSBool "d") :: SBool)
        (WriterLazy.WriterT $ Right (SSBool "a", SSBool "c") :: WriterLazy.WriterT SBool (Either SBool) SBool)
          >~ WriterLazy.WriterT (Right (SSBool "b", SSBool "d"))
          `shouldBe` ((SSBool "a", SSBool "c") >~ (SSBool "b", SSBool "d") :: SBool)
        (WriterLazy.WriterT $ Right (SSBool "a", SSBool "c") :: WriterLazy.WriterT SBool (Either SBool) SBool)
          `symCompare` WriterLazy.WriterT (Right (SSBool "b", SSBool "d"))
          `shouldBe` ((SSBool "a", SSBool "c") `symCompare` (SSBool "b", SSBool "d") :: UnionMBase SBool Ordering)

      it "SOrd for general Strict WriterT should work" $ do
        (WriterStrict.WriterT $ Left $ SSBool "a" :: WriterStrict.WriterT SBool (Either SBool) SBool)
          <=~ WriterStrict.WriterT (Left $ SSBool "b")
          `shouldBe` (SSBool "a" <=~ SSBool "b" :: SBool)
        (WriterStrict.WriterT $ Left $ SSBool "a" :: WriterStrict.WriterT SBool (Either SBool) SBool)
          <~ WriterStrict.WriterT (Left $ SSBool "b")
          `shouldBe` (SSBool "a" <~ SSBool "b" :: SBool)
        (WriterStrict.WriterT $ Left $ SSBool "a" :: WriterStrict.WriterT SBool (Either SBool) SBool)
          >=~ WriterStrict.WriterT (Left $ SSBool "b")
          `shouldBe` (SSBool "a" >=~ SSBool "b" :: SBool)
        (WriterStrict.WriterT $ Left $ SSBool "a" :: WriterStrict.WriterT SBool (Either SBool) SBool)
          >~ WriterStrict.WriterT (Left $ SSBool "b")
          `shouldBe` (SSBool "a" >~ SSBool "b" :: SBool)
        (WriterStrict.WriterT $ Left $ SSBool "a" :: WriterStrict.WriterT SBool (Either SBool) SBool)
          `symCompare` WriterStrict.WriterT (Left $ SSBool "b")
          `shouldBe` (SSBool "a" `symCompare` SSBool "b" :: UnionMBase SBool Ordering)

        (WriterStrict.WriterT $ Left $ SSBool "a" :: WriterStrict.WriterT SBool (Either SBool) SBool)
          <=~ WriterStrict.WriterT (Right (SSBool "b", SSBool "d"))
          `shouldBe` CBool True
        (WriterStrict.WriterT $ Left $ SSBool "a" :: WriterStrict.WriterT SBool (Either SBool) SBool)
          <~ WriterStrict.WriterT (Right (SSBool "b", SSBool "d"))
          `shouldBe` CBool True
        (WriterStrict.WriterT $ Left $ SSBool "a" :: WriterStrict.WriterT SBool (Either SBool) SBool)
          >=~ WriterStrict.WriterT (Right (SSBool "b", SSBool "d"))
          `shouldBe` CBool False
        (WriterStrict.WriterT $ Left $ SSBool "a" :: WriterStrict.WriterT SBool (Either SBool) SBool)
          >~ WriterStrict.WriterT (Right (SSBool "b", SSBool "d"))
          `shouldBe` CBool False
        (WriterStrict.WriterT $ Left $ SSBool "a" :: WriterStrict.WriterT SBool (Either SBool) SBool)
          `symCompare` WriterStrict.WriterT (Right (SSBool "b", SSBool "d"))
          `shouldBe` (mrgSingle LT :: UnionMBase SBool Ordering)

        (WriterStrict.WriterT $ Right (SSBool "a", SSBool "c") :: WriterStrict.WriterT SBool (Either SBool) SBool)
          <=~ WriterStrict.WriterT (Left $ SSBool "b")
          `shouldBe` CBool False
        (WriterStrict.WriterT $ Right (SSBool "a", SSBool "c") :: WriterStrict.WriterT SBool (Either SBool) SBool)
          <~ WriterStrict.WriterT (Left $ SSBool "b")
          `shouldBe` CBool False
        (WriterStrict.WriterT $ Right (SSBool "a", SSBool "c") :: WriterStrict.WriterT SBool (Either SBool) SBool)
          >=~ WriterStrict.WriterT (Left $ SSBool "b")
          `shouldBe` CBool True
        (WriterStrict.WriterT $ Right (SSBool "a", SSBool "c") :: WriterStrict.WriterT SBool (Either SBool) SBool)
          >~ WriterStrict.WriterT (Left $ SSBool "b")
          `shouldBe` CBool True
        (WriterStrict.WriterT $ Right (SSBool "a", SSBool "c") :: WriterStrict.WriterT SBool (Either SBool) SBool)
          `symCompare` WriterStrict.WriterT (Left $ SSBool "b")
          `shouldBe` (mrgSingle GT :: UnionMBase SBool Ordering)

        (WriterStrict.WriterT $ Right (SSBool "a", SSBool "c") :: WriterStrict.WriterT SBool (Either SBool) SBool)
          <=~ WriterStrict.WriterT (Right (SSBool "b", SSBool "d"))
          `shouldBe` ((SSBool "a", SSBool "c") <=~ (SSBool "b", SSBool "d") :: SBool)
        (WriterStrict.WriterT $ Right (SSBool "a", SSBool "c") :: WriterStrict.WriterT SBool (Either SBool) SBool)
          <~ WriterStrict.WriterT (Right (SSBool "b", SSBool "d"))
          `shouldBe` ((SSBool "a", SSBool "c") <~ (SSBool "b", SSBool "d") :: SBool)
        (WriterStrict.WriterT $ Right (SSBool "a", SSBool "c") :: WriterStrict.WriterT SBool (Either SBool) SBool)
          >=~ WriterStrict.WriterT (Right (SSBool "b", SSBool "d"))
          `shouldBe` ((SSBool "a", SSBool "c") >=~ (SSBool "b", SSBool "d") :: SBool)
        (WriterStrict.WriterT $ Right (SSBool "a", SSBool "c") :: WriterStrict.WriterT SBool (Either SBool) SBool)
          >~ WriterStrict.WriterT (Right (SSBool "b", SSBool "d"))
          `shouldBe` ((SSBool "a", SSBool "c") >~ (SSBool "b", SSBool "d") :: SBool)
        (WriterStrict.WriterT $ Right (SSBool "a", SSBool "c") :: WriterStrict.WriterT SBool (Either SBool) SBool)
          `symCompare` WriterStrict.WriterT (Right (SSBool "b", SSBool "d"))
          `shouldBe` ((SSBool "a", SSBool "c") `symCompare` (SSBool "b", SSBool "d") :: UnionMBase SBool Ordering)
    describe "SOrd for Identity" $ do
      prop
        "SOrd for concrete Identity should work"
        ( \(v1 :: Integer, v2) ->
            concreteOrdOkProp (Identity v1, Identity v2)
        )
      it "SOrd for general Identity should work" $ do
        (Identity $ SSBool "a" :: Identity SBool) <=~ Identity (SSBool "b")
          `shouldBe` (SSBool "a" <=~ SSBool "b" :: SBool)
        (Identity $ SSBool "a" :: Identity SBool)
          <~ Identity (SSBool "b")
          `shouldBe` (SSBool "a" <~ SSBool "b" :: SBool)
        (Identity $ SSBool "a" :: Identity SBool) >=~ Identity (SSBool "b")
          `shouldBe` (SSBool "a" >=~ SSBool "b" :: SBool)
        (Identity $ SSBool "a" :: Identity SBool) >~ Identity (SSBool "b")
          `shouldBe` (SSBool "a" >~ SSBool "b" :: SBool)
    describe "SOrd for IdentityT" $ do
      prop
        "SOrd for concrete IdentityT should work"
        ( \(v1 :: Either Integer Integer, v2) ->
            concreteOrdOkProp (IdentityT v1, IdentityT v2)
        )
      it "SOrd for general IdentityT should work" $ do
        (IdentityT $ Left $ SSBool "a" :: IdentityT (Either SBool) SBool) <=~ IdentityT (Left $ SSBool "b")
          `shouldBe` (SSBool "a" <=~ SSBool "b" :: SBool)
        (IdentityT $ Left $ SSBool "a" :: IdentityT (Either SBool) SBool)
          <~ IdentityT (Left $ SSBool "b")
          `shouldBe` (SSBool "a" <~ SSBool "b" :: SBool)
        (IdentityT $ Left $ SSBool "a" :: IdentityT (Either SBool) SBool) >=~ IdentityT (Left $ SSBool "b")
          `shouldBe` (SSBool "a" >=~ SSBool "b" :: SBool)
        (IdentityT $ Left $ SSBool "a" :: IdentityT (Either SBool) SBool) >~ IdentityT (Left $ SSBool "b")
          `shouldBe` (SSBool "a" >~ SSBool "b" :: SBool)
        (IdentityT $ Left $ SSBool "a" :: IdentityT (Either SBool) SBool)
          `symCompare` IdentityT (Left $ SSBool "b")
          `shouldBe` (SSBool "a" `symCompare` SSBool "b" :: UnionMBase SBool Ordering)

        (IdentityT $ Left $ SSBool "a" :: IdentityT (Either SBool) SBool) <=~ IdentityT (Right $ SSBool "b")
          `shouldBe` CBool True
        (IdentityT $ Left $ SSBool "a" :: IdentityT (Either SBool) SBool)
          <~ IdentityT (Right $ SSBool "b")
          `shouldBe` CBool True
        (IdentityT $ Left $ SSBool "a" :: IdentityT (Either SBool) SBool) >=~ IdentityT (Right $ SSBool "b")
          `shouldBe` CBool False
        (IdentityT $ Left $ SSBool "a" :: IdentityT (Either SBool) SBool) >~ IdentityT (Right $ SSBool "b")
          `shouldBe` CBool False
        (IdentityT $ Left $ SSBool "a" :: IdentityT (Either SBool) SBool)
          `symCompare` IdentityT (Right $ SSBool "b")
          `shouldBe` (mrgSingle LT :: UnionMBase SBool Ordering)

        (IdentityT $ Right $ SSBool "a" :: IdentityT (Either SBool) SBool) <=~ IdentityT (Left $ SSBool "b")
          `shouldBe` CBool False
        (IdentityT $ Right $ SSBool "a" :: IdentityT (Either SBool) SBool)
          <~ IdentityT (Left $ SSBool "b")
          `shouldBe` CBool False
        (IdentityT $ Right $ SSBool "a" :: IdentityT (Either SBool) SBool) >=~ IdentityT (Left $ SSBool "b")
          `shouldBe` CBool True
        (IdentityT $ Right $ SSBool "a" :: IdentityT (Either SBool) SBool) >~ IdentityT (Left $ SSBool "b")
          `shouldBe` CBool True
        (IdentityT $ Right $ SSBool "a" :: IdentityT (Either SBool) SBool)
          `symCompare` IdentityT (Left $ SSBool "b")
          `shouldBe` (mrgSingle GT :: UnionMBase SBool Ordering)

        (IdentityT $ Right $ SSBool "a" :: IdentityT (Either SBool) SBool) <=~ IdentityT (Right $ SSBool "b")
          `shouldBe` (SSBool "a" <=~ SSBool "b" :: SBool)
        (IdentityT $ Right $ SSBool "a" :: IdentityT (Either SBool) SBool)
          <~ IdentityT (Right $ SSBool "b")
          `shouldBe` (SSBool "a" <~ SSBool "b" :: SBool)
        (IdentityT $ Right $ SSBool "a" :: IdentityT (Either SBool) SBool) >=~ IdentityT (Right $ SSBool "b")
          `shouldBe` (SSBool "a" >=~ SSBool "b" :: SBool)
        (IdentityT $ Right $ SSBool "a" :: IdentityT (Either SBool) SBool) >~ IdentityT (Right $ SSBool "b")
          `shouldBe` (SSBool "a" >~ SSBool "b" :: SBool)
        (IdentityT $ Right $ SSBool "a" :: IdentityT (Either SBool) SBool)
          `symCompare` IdentityT (Right $ SSBool "b")
          `shouldBe` (SSBool "a" `symCompare` SSBool "b" :: UnionMBase SBool Ordering)

    describe "SOrd for ByteString" $ do
      it "SOrd for ByteString should work" $ do
        let bytestrings :: [B.ByteString] = ["", "a", "b", "ab", "ba", "aa", "bb"]
        traverse_ concreteOrdOkProp [(x, y) | x <- bytestrings, y <- bytestrings]
