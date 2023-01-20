{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Grisette.Core.Data.Class.SOrdTests where

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
import Grisette.Core.Control.Monad.UnionMBase
import Grisette.Core.Data.Class.Bool
import Grisette.Core.Data.Class.SOrd
import Grisette.Core.Data.Class.SimpleMergeable
import Grisette.TestUtils.SBool
import Grisette.TestUtils.SOrd
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

sordTests :: TestTree
sordTests =
  testGroup
    "SOrdTests"
    [ testGroup
        "SOrd for common types"
        [ testGroup
            "SBool"
            [ testCase "Concrete SBool" $ do
                CBool False `gsymle` CBool False @=? CBool True
                CBool False `gsymlt` CBool False @=? CBool False
                CBool False `gsymge` CBool False @=? CBool True
                CBool False `gsymgt` CBool False @=? CBool False
                CBool False `gsymle` CBool True @=? CBool True
                CBool False `gsymlt` CBool True @=? CBool True
                CBool False `gsymge` CBool True @=? CBool False
                CBool False `gsymgt` CBool True @=? CBool False
                CBool True `gsymle` CBool False @=? CBool False
                CBool True `gsymlt` CBool False @=? CBool False
                CBool True `gsymge` CBool False @=? CBool True
                CBool True `gsymgt` CBool False @=? CBool True
                CBool True `gsymle` CBool True @=? CBool True
                CBool True `gsymlt` CBool True @=? CBool False
                CBool True `gsymge` CBool True @=? CBool True
                CBool True `gsymgt` CBool True @=? CBool False,
              testCase "Symbolic SBool" $ do
                SSBool "a" `gsymle` SSBool "b" @=? Or (Not (SSBool "a")) (SSBool "b")
                SSBool "a" `gsymlt` SSBool "b" @=? And (Not (SSBool "a")) (SSBool "b")
                SSBool "a" `gsymge` SSBool "b" @=? Or (SSBool "a") (Not (SSBool "b"))
                SSBool "a" `gsymgt` SSBool "b" @=? And (SSBool "a") (Not (SSBool "b"))
                gsymCompare (SSBool "a") (SSBool "b")
                  @=? ( mrgIf
                          (And (Not (SSBool "a")) (SSBool "b"))
                          (mrgSingle LT)
                          ( mrgIf
                              (Equal (SSBool "a") (SSBool "b"))
                              (mrgSingle EQ)
                              (mrgSingle GT)
                          ) ::
                          UnionMBase SBool Ordering
                      )
            ],
          testProperty "Bool" (ioProperty . concreteOrdOkProp @Bool),
          testProperty "Integer" (ioProperty . concreteOrdOkProp @Integer),
          testProperty "Char" (ioProperty . concreteOrdOkProp @Char),
          testProperty "Int" (ioProperty . concreteOrdOkProp @Int),
          testProperty "Int8" (ioProperty . concreteOrdOkProp @Int8),
          testProperty "Int16" (ioProperty . concreteOrdOkProp @Int16),
          testProperty "Int32" (ioProperty . concreteOrdOkProp @Int32),
          testProperty "Int64" (ioProperty . concreteOrdOkProp @Int64),
          testProperty "Word" (ioProperty . concreteOrdOkProp @Word),
          testProperty "Word8" (ioProperty . concreteOrdOkProp @Word8),
          testProperty "Word16" (ioProperty . concreteOrdOkProp @Word16),
          testProperty "Word32" (ioProperty . concreteOrdOkProp @Word32),
          testProperty "Word64" (ioProperty . concreteOrdOkProp @Word64),
          testGroup
            "List"
            [ testProperty "[Integer]" (ioProperty . concreteOrdOkProp @[Integer]),
              testProperty "[String]" (ioProperty . concreteOrdOkProp @[String]),
              testCase "[SBool]" $ do
                ([] :: [SBool]) `gsymle` [] @=? CBool True
                ([] :: [SBool]) `gsymlt` [] @=? CBool False
                ([] :: [SBool]) `gsymge` [] @=? CBool True
                ([] :: [SBool]) `gsymgt` [] @=? CBool False
                ([] :: [SBool]) `gsymCompare` [] @=? (mrgSingle EQ :: UnionMBase SBool Ordering)
                [] `gsymle` [SSBool "a"] @=? CBool True
                [] `gsymlt` [SSBool "a"] @=? CBool True
                [] `gsymge` [SSBool "a"] @=? CBool False
                [] `gsymgt` [SSBool "a"] @=? CBool False
                [] `gsymCompare` [SSBool "a"] @=? (mrgSingle LT :: UnionMBase SBool Ordering)
                [SSBool "a"] `gsymle` [] @=? CBool False
                [SSBool "a"] `gsymlt` [] @=? CBool False
                [SSBool "a"] `gsymge` [] @=? CBool True
                [SSBool "a"] `gsymgt` [] @=? CBool True
                [SSBool "a"] `gsymCompare` [] @=? (mrgSingle GT :: UnionMBase SBool Ordering)

                [SSBool "a", SSBool "b"]
                  `gsymle` [SSBool "c"]
                  @=? (SSBool "a" `gsymlt` SSBool "c" :: SBool)
                [SSBool "a", SSBool "b"]
                  `gsymlt` [SSBool "c"]
                  @=? (SSBool "a" `gsymlt` SSBool "c" :: SBool)
                [SSBool "a", SSBool "b"]
                  `gsymge` [SSBool "c"]
                  @=? ((SSBool "a" `gsymgt` SSBool "c") ||~ (SSBool "a" `gsymeq` SSBool "c") :: SBool)
                [SSBool "a", SSBool "b"]
                  `gsymgt` [SSBool "c"]
                  @=? ((SSBool "a" `gsymgt` SSBool "c") ||~ (SSBool "a" `gsymeq` SSBool "c") :: SBool)
                [SSBool "a"]
                  `gsymCompare` [SSBool "b"]
                  @=? (SSBool "a" `gsymCompare` SSBool "b" :: UnionMBase SBool Ordering)

                [SSBool "a"]
                  `gsymle` [SSBool "b", SSBool "c"]
                  @=? ((SSBool "a" `gsymlt` SSBool "b") ||~ (SSBool "a" `gsymeq` SSBool "b") :: SBool)
                [SSBool "a"]
                  `gsymlt` [SSBool "b", SSBool "c"]
                  @=? ((SSBool "a" `gsymlt` SSBool "b") ||~ (SSBool "a" `gsymeq` SSBool "b") :: SBool)
                [SSBool "a"]
                  `gsymge` [SSBool "b", SSBool "c"]
                  @=? (SSBool "a" `gsymgt` SSBool "b" :: SBool)
                [SSBool "a"]
                  `gsymgt` [SSBool "b", SSBool "c"]
                  @=? (SSBool "a" `gsymgt` SSBool "b" :: SBool)
                [SSBool "a"]
                  `gsymCompare` [SSBool "b", SSBool "c"]
                  @=? ( mrgIf
                          (SSBool "a" `gsymlt` SSBool "b")
                          (mrgSingle LT)
                          (mrgIf (SSBool "a" `gsymeq` SSBool "b") (mrgSingle LT) (mrgSingle GT)) ::
                          UnionMBase SBool Ordering
                      )

                [SSBool "a", SSBool "b"]
                  `gsymle` [SSBool "c", SSBool "d"]
                  @=? ( (SSBool "a" `gsymlt` SSBool "c")
                          ||~ ( SSBool "a"
                                  `gsymeq` SSBool "c"
                                  &&~ ((SSBool "b" `gsymlt` SSBool "d") ||~ (SSBool "b" `gsymeq` SSBool "d"))
                              ) ::
                          SBool
                      )
                [SSBool "a", SSBool "b"]
                  `gsymlt` [SSBool "c", SSBool "d"]
                  @=? ( (SSBool "a" `gsymlt` SSBool "c")
                          ||~ ( SSBool "a"
                                  `gsymeq` SSBool "c"
                                  &&~ (SSBool "b" `gsymlt` SSBool "d")
                              ) ::
                          SBool
                      )
                [SSBool "a", SSBool "b"]
                  `gsymge` [SSBool "c", SSBool "d"]
                  @=? ( (SSBool "a" `gsymgt` SSBool "c")
                          ||~ ( SSBool "a"
                                  `gsymeq` SSBool "c"
                                  &&~ ((SSBool "b" `gsymgt` SSBool "d") ||~ (SSBool "b" `gsymeq` SSBool "d"))
                              ) ::
                          SBool
                      )
                [SSBool "a", SSBool "b"]
                  `gsymgt` [SSBool "c", SSBool "d"]
                  @=? ( (SSBool "a" `gsymgt` SSBool "c")
                          ||~ ( SSBool "a"
                                  `gsymeq` SSBool "c"
                                  &&~ (SSBool "b" `gsymgt` SSBool "d")
                              ) ::
                          SBool
                      )
                [SSBool "a", SSBool "b"]
                  `gsymCompare` [SSBool "c", SSBool "d"]
                  @=? ( mrgIf
                          (SSBool "a" `gsymlt` SSBool "c")
                          (mrgSingle LT)
                          ( mrgIf
                              (SSBool "a" `gsymeq` SSBool "c")
                              (SSBool "b" `gsymCompare` SSBool "d")
                              (mrgSingle GT)
                          ) ::
                          UnionMBase SBool Ordering
                      )
            ],
          testGroup
            "Maybe"
            [ testProperty "Maybe Integer" (ioProperty . concreteOrdOkProp @(Maybe Integer)),
              testCase "Maybe SBool" $ do
                (Nothing :: Maybe SBool) `gsymle` Nothing @=? CBool True
                (Nothing :: Maybe SBool) `gsymlt` Nothing @=? CBool False
                (Nothing :: Maybe SBool) `gsymge` Nothing @=? CBool True
                (Nothing :: Maybe SBool) `gsymgt` Nothing @=? CBool False
                (Nothing :: Maybe SBool) `gsymCompare` Nothing @=? (mrgSingle EQ :: UnionMBase SBool Ordering)
                Nothing `gsymle` Just (SSBool "a") @=? CBool True
                Nothing `gsymlt` Just (SSBool "a") @=? CBool True
                Nothing `gsymge` Just (SSBool "a") @=? CBool False
                Nothing `gsymgt` Just (SSBool "a") @=? CBool False
                Nothing `gsymCompare` Just (SSBool "a") @=? (mrgSingle LT :: UnionMBase SBool Ordering)
                Just (SSBool "a") `gsymle` Nothing @=? CBool False
                Just (SSBool "a") `gsymlt` Nothing @=? CBool False
                Just (SSBool "a") `gsymge` Nothing @=? CBool True
                Just (SSBool "a") `gsymgt` Nothing @=? CBool True
                Just (SSBool "a") `gsymCompare` Nothing @=? (mrgSingle GT :: UnionMBase SBool Ordering)
                Just (SSBool "a") `gsymle` Just (SSBool "b") @=? (SSBool "a" `gsymle` SSBool "b" :: SBool)
                Just (SSBool "a") `gsymlt` Just (SSBool "b") @=? (SSBool "a" `gsymlt` SSBool "b" :: SBool)
                Just (SSBool "a") `gsymge` Just (SSBool "b") @=? (SSBool "a" `gsymge` SSBool "b" :: SBool)
                Just (SSBool "a") `gsymgt` Just (SSBool "b") @=? (SSBool "a" `gsymgt` SSBool "b" :: SBool)
                Just (SSBool "a")
                  `gsymCompare` Just (SSBool "b")
                  @=? (SSBool "a" `gsymCompare` SSBool "b" :: UnionMBase SBool Ordering)
            ],
          testGroup
            "MaybeT"
            [ testProperty "MaybeT Maybe Integer" (ioProperty . concreteOrdOkProp @(MaybeT Maybe Integer) . bimap MaybeT MaybeT),
              testCase "MaybeT Maybe SBool" $ do
                (MaybeT Nothing :: MaybeT Maybe SBool) `gsymle` MaybeT Nothing @=? CBool True
                (MaybeT Nothing :: MaybeT Maybe SBool) `gsymle` MaybeT (Just (Just (SSBool "a"))) @=? CBool True
                MaybeT (Just (Just (SSBool "a"))) `gsymle` (MaybeT Nothing :: MaybeT Maybe SBool) @=? CBool False
                MaybeT (Just (Just (SSBool "a")))
                  `gsymle` (MaybeT (Just (Just (SSBool "b"))) :: MaybeT Maybe SBool)
                  @=? (SSBool "a" `gsymle` SSBool "b" :: SBool)

                (MaybeT Nothing :: MaybeT Maybe SBool) `gsymlt` MaybeT Nothing @=? CBool False
                (MaybeT Nothing :: MaybeT Maybe SBool) `gsymlt` MaybeT (Just (Just (SSBool "a"))) @=? CBool True
                MaybeT (Just (Just (SSBool "a"))) `gsymlt` (MaybeT Nothing :: MaybeT Maybe SBool) @=? CBool False
                MaybeT (Just (Just (SSBool "a")))
                  `gsymlt` (MaybeT (Just (Just (SSBool "b"))) :: MaybeT Maybe SBool)
                  @=? (SSBool "a" `gsymlt` SSBool "b" :: SBool)

                (MaybeT Nothing :: MaybeT Maybe SBool) `gsymge` MaybeT Nothing @=? CBool True
                (MaybeT Nothing :: MaybeT Maybe SBool) `gsymge` MaybeT (Just (Just (SSBool "a"))) @=? CBool False
                MaybeT (Just (Just (SSBool "a"))) `gsymge` (MaybeT Nothing :: MaybeT Maybe SBool) @=? CBool True
                MaybeT (Just (Just (SSBool "a")))
                  `gsymge` (MaybeT (Just (Just (SSBool "b"))) :: MaybeT Maybe SBool)
                  @=? (SSBool "a" `gsymge` SSBool "b" :: SBool)

                (MaybeT Nothing :: MaybeT Maybe SBool) `gsymgt` MaybeT Nothing @=? CBool False
                (MaybeT Nothing :: MaybeT Maybe SBool) `gsymgt` MaybeT (Just (Just (SSBool "a"))) @=? CBool False
                MaybeT (Just (Just (SSBool "a"))) `gsymgt` (MaybeT Nothing :: MaybeT Maybe SBool) @=? CBool True
                MaybeT (Just (Just (SSBool "a")))
                  `gsymgt` (MaybeT (Just (Just (SSBool "b"))) :: MaybeT Maybe SBool)
                  @=? (SSBool "a" `gsymgt` SSBool "b" :: SBool)

                (MaybeT Nothing :: MaybeT Maybe SBool)
                  `gsymCompare` MaybeT Nothing
                  @=? (mrgSingle EQ :: UnionMBase SBool Ordering)
                (MaybeT Nothing :: MaybeT Maybe SBool)
                  `gsymCompare` MaybeT (Just (Just (SSBool "a")))
                  @=? (mrgSingle LT :: UnionMBase SBool Ordering)
                MaybeT (Just (Just (SSBool "a")))
                  `gsymCompare` (MaybeT Nothing :: MaybeT Maybe SBool)
                  @=? (mrgSingle GT :: UnionMBase SBool Ordering)
                MaybeT (Just (Just (SSBool "a")))
                  `gsymCompare` (MaybeT (Just (Just (SSBool "b"))) :: MaybeT Maybe SBool)
                  @=? (SSBool "a" `gsymCompare` SSBool "b" :: UnionMBase SBool Ordering)
            ],
          testGroup
            "Either"
            [ testProperty "Either Integer Integer" (ioProperty . concreteOrdOkProp @(Either Integer Integer)),
              testCase "Either SBool SBool" $ do
                (Left (SSBool "a") :: Either SBool SBool) `gsymle` Left (SSBool "b") @=? (SSBool "a" `gsymle` SSBool "b" :: SBool)
                (Left (SSBool "a") :: Either SBool SBool) `gsymlt` Left (SSBool "b") @=? (SSBool "a" `gsymlt` SSBool "b" :: SBool)
                (Left (SSBool "a") :: Either SBool SBool) `gsymge` Left (SSBool "b") @=? (SSBool "a" `gsymge` SSBool "b" :: SBool)
                (Left (SSBool "a") :: Either SBool SBool) `gsymgt` Left (SSBool "b") @=? (SSBool "a" `gsymgt` SSBool "b" :: SBool)
                (Left (SSBool "a") :: Either SBool SBool)
                  `gsymCompare` Left (SSBool "b")
                  @=? (SSBool "a" `gsymCompare` SSBool "b" :: UnionMBase SBool Ordering)
                (Left (SSBool "a") :: Either SBool SBool) `gsymle` Right (SSBool "b") @=? CBool True
                (Left (SSBool "a") :: Either SBool SBool) `gsymlt` Right (SSBool "b") @=? CBool True
                (Left (SSBool "a") :: Either SBool SBool) `gsymge` Right (SSBool "b") @=? CBool False
                (Left (SSBool "a") :: Either SBool SBool) `gsymgt` Right (SSBool "b") @=? CBool False
                (Left (SSBool "a") :: Either SBool SBool)
                  `gsymCompare` Right (SSBool "b")
                  @=? (mrgSingle LT :: UnionMBase SBool Ordering)
                (Right (SSBool "a") :: Either SBool SBool) `gsymle` Left (SSBool "b") @=? CBool False
                (Right (SSBool "a") :: Either SBool SBool) `gsymlt` Left (SSBool "b") @=? CBool False
                (Right (SSBool "a") :: Either SBool SBool) `gsymge` Left (SSBool "b") @=? CBool True
                (Right (SSBool "a") :: Either SBool SBool) `gsymgt` Left (SSBool "b") @=? CBool True
                (Right (SSBool "a") :: Either SBool SBool)
                  `gsymCompare` Left (SSBool "b")
                  @=? (mrgSingle GT :: UnionMBase SBool Ordering)
                (Right (SSBool "a") :: Either SBool SBool) `gsymle` Right (SSBool "b") @=? (SSBool "a" `gsymle` SSBool "b" :: SBool)
                (Right (SSBool "a") :: Either SBool SBool) `gsymlt` Right (SSBool "b") @=? (SSBool "a" `gsymlt` SSBool "b" :: SBool)
                (Right (SSBool "a") :: Either SBool SBool) `gsymge` Right (SSBool "b") @=? (SSBool "a" `gsymge` SSBool "b" :: SBool)
                (Right (SSBool "a") :: Either SBool SBool) `gsymgt` Right (SSBool "b") @=? (SSBool "a" `gsymgt` SSBool "b" :: SBool)
                (Right (SSBool "a") :: Either SBool SBool)
                  `gsymCompare` Right (SSBool "b")
                  @=? (SSBool "a" `gsymCompare` SSBool "b" :: UnionMBase SBool Ordering)
            ],
          testGroup
            "ExceptT"
            [ testProperty
                "ExceptT Integer Maybe Integer"
                (ioProperty . concreteOrdOkProp @(ExceptT Integer Maybe Integer) . bimap ExceptT ExceptT),
              testCase "ExceptT SBool Maybe SBool" $ do
                (ExceptT Nothing :: ExceptT SBool Maybe SBool) `gsymle` ExceptT Nothing @=? CBool True
                (ExceptT Nothing :: ExceptT SBool Maybe SBool) `gsymle` ExceptT (Just (Left (SSBool "a"))) @=? CBool True
                (ExceptT Nothing :: ExceptT SBool Maybe SBool) `gsymle` ExceptT (Just (Right (SSBool "a"))) @=? CBool True
                ExceptT (Just (Left (SSBool "a"))) `gsymle` (ExceptT Nothing :: ExceptT SBool Maybe SBool) @=? CBool False
                ExceptT (Just (Right (SSBool "a"))) `gsymle` (ExceptT Nothing :: ExceptT SBool Maybe SBool) @=? CBool False
                ExceptT (Just (Left (SSBool "a")))
                  `gsymle` (ExceptT (Just (Left (SSBool "b"))) :: ExceptT SBool Maybe SBool)
                  @=? (SSBool "a" `gsymle` SSBool "b" :: SBool)
                ExceptT (Just (Right (SSBool "a")))
                  `gsymle` (ExceptT (Just (Left (SSBool "b"))) :: ExceptT SBool Maybe SBool)
                  @=? CBool False
                ExceptT (Just (Left (SSBool "a")))
                  `gsymle` (ExceptT (Just (Right (SSBool "b"))) :: ExceptT SBool Maybe SBool)
                  @=? CBool True
                ExceptT (Just (Right (SSBool "a")))
                  `gsymle` (ExceptT (Just (Right (SSBool "b"))) :: ExceptT SBool Maybe SBool)
                  @=? (SSBool "a" `gsymle` SSBool "b" :: SBool)

                (ExceptT Nothing :: ExceptT SBool Maybe SBool) `gsymlt` ExceptT Nothing @=? CBool False
                (ExceptT Nothing :: ExceptT SBool Maybe SBool) `gsymlt` ExceptT (Just (Left (SSBool "a"))) @=? CBool True
                (ExceptT Nothing :: ExceptT SBool Maybe SBool) `gsymlt` ExceptT (Just (Right (SSBool "a"))) @=? CBool True
                ExceptT (Just (Left (SSBool "a"))) `gsymlt` (ExceptT Nothing :: ExceptT SBool Maybe SBool) @=? CBool False
                ExceptT (Just (Right (SSBool "a"))) `gsymlt` (ExceptT Nothing :: ExceptT SBool Maybe SBool) @=? CBool False
                ExceptT (Just (Left (SSBool "a")))
                  `gsymlt` (ExceptT (Just (Left (SSBool "b"))) :: ExceptT SBool Maybe SBool)
                  @=? (SSBool "a" `gsymlt` SSBool "b" :: SBool)
                ExceptT (Just (Right (SSBool "a")))
                  `gsymlt` (ExceptT (Just (Left (SSBool "b"))) :: ExceptT SBool Maybe SBool)
                  @=? CBool False
                ExceptT (Just (Left (SSBool "a")))
                  `gsymlt` (ExceptT (Just (Right (SSBool "b"))) :: ExceptT SBool Maybe SBool)
                  @=? CBool True
                ExceptT (Just (Right (SSBool "a")))
                  `gsymlt` (ExceptT (Just (Right (SSBool "b"))) :: ExceptT SBool Maybe SBool)
                  @=? (SSBool "a" `gsymlt` SSBool "b" :: SBool)

                (ExceptT Nothing :: ExceptT SBool Maybe SBool) `gsymge` ExceptT Nothing @=? CBool True
                (ExceptT Nothing :: ExceptT SBool Maybe SBool) `gsymge` ExceptT (Just (Left (SSBool "a"))) @=? CBool False
                (ExceptT Nothing :: ExceptT SBool Maybe SBool) `gsymge` ExceptT (Just (Right (SSBool "a"))) @=? CBool False
                ExceptT (Just (Left (SSBool "a"))) `gsymge` (ExceptT Nothing :: ExceptT SBool Maybe SBool) @=? CBool True
                ExceptT (Just (Right (SSBool "a"))) `gsymge` (ExceptT Nothing :: ExceptT SBool Maybe SBool) @=? CBool True
                ExceptT (Just (Left (SSBool "a")))
                  `gsymge` (ExceptT (Just (Left (SSBool "b"))) :: ExceptT SBool Maybe SBool)
                  @=? (SSBool "a" `gsymge` SSBool "b" :: SBool)
                ExceptT (Just (Right (SSBool "a")))
                  `gsymge` (ExceptT (Just (Left (SSBool "b"))) :: ExceptT SBool Maybe SBool)
                  @=? CBool True
                ExceptT (Just (Left (SSBool "a")))
                  `gsymge` (ExceptT (Just (Right (SSBool "b"))) :: ExceptT SBool Maybe SBool)
                  @=? CBool False
                ExceptT (Just (Right (SSBool "a")))
                  `gsymge` (ExceptT (Just (Right (SSBool "b"))) :: ExceptT SBool Maybe SBool)
                  @=? (SSBool "a" `gsymge` SSBool "b" :: SBool)

                (ExceptT Nothing :: ExceptT SBool Maybe SBool) `gsymgt` ExceptT Nothing @=? CBool False
                (ExceptT Nothing :: ExceptT SBool Maybe SBool) `gsymgt` ExceptT (Just (Left (SSBool "a"))) @=? CBool False
                (ExceptT Nothing :: ExceptT SBool Maybe SBool) `gsymgt` ExceptT (Just (Right (SSBool "a"))) @=? CBool False
                ExceptT (Just (Left (SSBool "a"))) `gsymgt` (ExceptT Nothing :: ExceptT SBool Maybe SBool) @=? CBool True
                ExceptT (Just (Right (SSBool "a"))) `gsymgt` (ExceptT Nothing :: ExceptT SBool Maybe SBool) @=? CBool True
                ExceptT (Just (Left (SSBool "a")))
                  `gsymgt` (ExceptT (Just (Left (SSBool "b"))) :: ExceptT SBool Maybe SBool)
                  @=? (SSBool "a" `gsymgt` SSBool "b" :: SBool)
                ExceptT (Just (Right (SSBool "a")))
                  `gsymgt` (ExceptT (Just (Left (SSBool "b"))) :: ExceptT SBool Maybe SBool)
                  @=? CBool True
                ExceptT (Just (Left (SSBool "a")))
                  `gsymgt` (ExceptT (Just (Right (SSBool "b"))) :: ExceptT SBool Maybe SBool)
                  @=? CBool False
                ExceptT (Just (Right (SSBool "a")))
                  `gsymgt` (ExceptT (Just (Right (SSBool "b"))) :: ExceptT SBool Maybe SBool)
                  @=? (SSBool "a" `gsymgt` SSBool "b" :: SBool)

                (ExceptT Nothing :: ExceptT SBool Maybe SBool)
                  `gsymCompare` ExceptT Nothing
                  @=? (mrgSingle EQ :: UnionMBase SBool Ordering)
                (ExceptT Nothing :: ExceptT SBool Maybe SBool)
                  `gsymCompare` ExceptT (Just (Left (SSBool "a")))
                  @=? (mrgSingle LT :: UnionMBase SBool Ordering)
                (ExceptT Nothing :: ExceptT SBool Maybe SBool)
                  `gsymCompare` ExceptT (Just (Right (SSBool "a")))
                  @=? (mrgSingle LT :: UnionMBase SBool Ordering)
                ExceptT (Just (Left (SSBool "a")))
                  `gsymCompare` (ExceptT Nothing :: ExceptT SBool Maybe SBool)
                  @=? (mrgSingle GT :: UnionMBase SBool Ordering)
                ExceptT (Just (Right (SSBool "a")))
                  `gsymCompare` (ExceptT Nothing :: ExceptT SBool Maybe SBool)
                  @=? (mrgSingle GT :: UnionMBase SBool Ordering)
                ExceptT (Just (Left (SSBool "a")))
                  `gsymCompare` (ExceptT (Just (Left (SSBool "b"))) :: ExceptT SBool Maybe SBool)
                  @=? (SSBool "a" `gsymCompare` SSBool "b" :: UnionMBase SBool Ordering)
                ExceptT (Just (Right (SSBool "a")))
                  `gsymCompare` (ExceptT (Just (Left (SSBool "b"))) :: ExceptT SBool Maybe SBool)
                  @=? (mrgSingle GT :: UnionMBase SBool Ordering)
                ExceptT (Just (Left (SSBool "a")))
                  `gsymCompare` (ExceptT (Just (Right (SSBool "b"))) :: ExceptT SBool Maybe SBool)
                  @=? (mrgSingle LT :: UnionMBase SBool Ordering)
                ExceptT (Just (Right (SSBool "a")))
                  `gsymCompare` (ExceptT (Just (Right (SSBool "b"))) :: ExceptT SBool Maybe SBool)
                  @=? (SSBool "a" `gsymCompare` SSBool "b" :: UnionMBase SBool Ordering)
            ],
          testProperty "()" (ioProperty . concreteOrdOkProp @()),
          testGroup
            "(,)"
            [ testProperty "(Integer, Integer)" (ioProperty . concreteOrdOkProp @(Integer, Integer)),
              testCase "(SBool, SBool)" $ do
                let l = (SSBool "a", SSBool "c")
                let r = (SSBool "b", SSBool "d")
                let ll = SSBool "a"
                let lr = SSBool "c"
                let rl = SSBool "b"
                let rr = SSBool "d"
                symbolicProdOrdOkProp l r ll lr rl rr
            ],
          testGroup
            "(,,)"
            [ testProperty "(Integer, Integer, Integer)" (ioProperty . concreteOrdOkProp @(Integer, Integer, Integer)),
              testCase "(SBool, SBool, SBool)" $ do
                let l = (SSBool "a", SSBool "c", SSBool "e")
                let r = (SSBool "b", SSBool "d", SSBool "f")
                let ll = SSBool "a"
                let lr = (SSBool "c", SSBool "e")
                let rl = SSBool "b"
                let rr = (SSBool "d", SSBool "f")
                symbolicProdOrdOkProp l r ll lr rl rr
            ],
          testGroup
            "(,,,)"
            [ testProperty
                "(Integer, Integer, Integer, Integer)"
                (ioProperty . concreteOrdOkProp @(Integer, Integer, Integer, Integer)),
              testCase "(SBool, SBool, SBool, SBool)" $ do
                let l = (SSBool "a", SSBool "c", SSBool "e", SSBool "g")
                let r = (SSBool "b", SSBool "d", SSBool "f", SSBool "h")
                let ll = (SSBool "a", SSBool "c")
                let lr = (SSBool "e", SSBool "g")
                let rl = (SSBool "b", SSBool "d")
                let rr = (SSBool "f", SSBool "h")
                symbolicProdOrdOkProp l r ll lr rl rr
            ],
          testGroup
            "(,,,,)"
            [ testProperty
                "(Integer, Integer, Integer, Integer, Integer)"
                (ioProperty . concreteOrdOkProp @(Integer, Integer, Integer, Integer, Integer)),
              testCase "(SBool, SBool, SBool, SBool, SBool)" $ do
                let l = (SSBool "a", SSBool "c", SSBool "e", SSBool "g", SSBool "i")
                let r = (SSBool "b", SSBool "d", SSBool "f", SSBool "h", SSBool "j")
                let ll = (SSBool "a", SSBool "c")
                let lr = (SSBool "e", SSBool "g", SSBool "i")
                let rl = (SSBool "b", SSBool "d")
                let rr = (SSBool "f", SSBool "h", SSBool "j")
                symbolicProdOrdOkProp l r ll lr rl rr
            ],
          testGroup
            "(,,,,,)"
            [ testProperty
                "(Integer, Integer, Integer, Integer, Integer, Integer)"
                (ioProperty . concreteOrdOkProp @(Integer, Integer, Integer, Integer, Integer, Integer)),
              testCase
                "(SBool, SBool, SBool, SBool, SBool, SBool)"
                $ do
                  let l = (SSBool "a", SSBool "c", SSBool "e", SSBool "g", SSBool "i", SSBool "k")
                  let r = (SSBool "b", SSBool "d", SSBool "f", SSBool "h", SSBool "j", SSBool "l")
                  let ll = (SSBool "a", SSBool "c", SSBool "e")
                  let lr = (SSBool "g", SSBool "i", SSBool "k")
                  let rl = (SSBool "b", SSBool "d", SSBool "f")
                  let rr = (SSBool "h", SSBool "j", SSBool "l")
                  symbolicProdOrdOkProp l r ll lr rl rr
            ],
          testGroup
            "(,,,,,,)"
            [ testProperty
                "(Integer, Integer, Integer, Integer, Integer, Integer, Integer)"
                (ioProperty . concreteOrdOkProp @(Integer, Integer, Integer, Integer, Integer, Integer, Integer)),
              testCase "(SBool, SBool, SBool, SBool, SBool, SBool, SBool)" $ do
                let l = (SSBool "a", SSBool "c", SSBool "e", SSBool "g", SSBool "i", SSBool "k", SSBool "m")
                let r = (SSBool "b", SSBool "d", SSBool "f", SSBool "h", SSBool "j", SSBool "l", SSBool "n")
                let ll = (SSBool "a", SSBool "c", SSBool "e")
                let lr = (SSBool "g", SSBool "i", SSBool "k", SSBool "m")
                let rl = (SSBool "b", SSBool "d", SSBool "f")
                let rr = (SSBool "h", SSBool "j", SSBool "l", SSBool "n")
                symbolicProdOrdOkProp l r ll lr rl rr
            ],
          testGroup
            "(,,,,,,,)"
            [ testProperty
                "(Integer, Integer, Integer, Integer, Integer, Integer, Integer, Integer)"
                (ioProperty . concreteOrdOkProp @(Integer, Integer, Integer, Integer, Integer, Integer, Integer, Integer)),
              testCase "(SBool, SBool, SBool, SBool, SBool, SBool, SBool, SBool)" $ do
                let l = (SSBool "a", SSBool "c", SSBool "e", SSBool "g", SSBool "i", SSBool "k", SSBool "m", SSBool "o")
                let r = (SSBool "b", SSBool "d", SSBool "f", SSBool "h", SSBool "j", SSBool "l", SSBool "n", SSBool "p")
                let ll = (SSBool "a", SSBool "c", SSBool "e", SSBool "g")
                let lr = (SSBool "i", SSBool "k", SSBool "m", SSBool "o")
                let rl = (SSBool "b", SSBool "d", SSBool "f", SSBool "h")
                let rr = (SSBool "j", SSBool "l", SSBool "n", SSBool "p")
                symbolicProdOrdOkProp l r ll lr rl rr
            ],
          testGroup
            "Sum"
            [ testProperty
                "Sum Maybe Maybe Integer"
                ( ioProperty . \v ->
                    let eitherToSum :: Either (Maybe Integer) (Maybe Integer) -> Sum Maybe Maybe Integer
                        eitherToSum (Left x) = InL x
                        eitherToSum (Right x) = InR x
                     in concreteOrdOkProp (bimap eitherToSum eitherToSum v)
                ),
              testCase "Sum Maybe Maybe SBool" $ do
                (InL $ Just $ SSBool "a" :: Sum Maybe Maybe SBool)
                  `gsymle` InL (Just $ SSBool "b")
                  @=? (SSBool "a" `gsymle` SSBool "b" :: SBool)
                (InL $ Just $ SSBool "a" :: Sum Maybe Maybe SBool)
                  `gsymlt` InL (Just $ SSBool "b")
                  @=? (SSBool "a" `gsymlt` SSBool "b" :: SBool)
                (InL $ Just $ SSBool "a" :: Sum Maybe Maybe SBool)
                  `gsymge` InL (Just $ SSBool "b")
                  @=? (SSBool "a" `gsymge` SSBool "b" :: SBool)
                (InL $ Just $ SSBool "a" :: Sum Maybe Maybe SBool)
                  `gsymgt` InL (Just $ SSBool "b")
                  @=? (SSBool "a" `gsymgt` SSBool "b" :: SBool)
                (InL $ Just $ SSBool "a" :: Sum Maybe Maybe SBool) `gsymle` InR (Just $ SSBool "b") @=? CBool True
                (InL $ Just $ SSBool "a" :: Sum Maybe Maybe SBool) `gsymlt` InR (Just $ SSBool "b") @=? CBool True
                (InL $ Just $ SSBool "a" :: Sum Maybe Maybe SBool) `gsymge` InR (Just $ SSBool "b") @=? CBool False
                (InL $ Just $ SSBool "a" :: Sum Maybe Maybe SBool) `gsymgt` InR (Just $ SSBool "b") @=? CBool False
                (InR $ Just $ SSBool "a" :: Sum Maybe Maybe SBool)
                  `gsymle` InR (Just $ SSBool "b")
                  @=? (SSBool "a" `gsymle` SSBool "b" :: SBool)
                (InR $ Just $ SSBool "a" :: Sum Maybe Maybe SBool)
                  `gsymlt` InR (Just $ SSBool "b")
                  @=? (SSBool "a" `gsymlt` SSBool "b" :: SBool)
                (InR $ Just $ SSBool "a" :: Sum Maybe Maybe SBool)
                  `gsymge` InR (Just $ SSBool "b")
                  @=? (SSBool "a" `gsymge` SSBool "b" :: SBool)
                (InR $ Just $ SSBool "a" :: Sum Maybe Maybe SBool)
                  `gsymgt` InR (Just $ SSBool "b")
                  @=? (SSBool "a" `gsymgt` SSBool "b" :: SBool)
                (InR $ Just $ SSBool "a" :: Sum Maybe Maybe SBool) `gsymle` InL (Just $ SSBool "b") @=? CBool False
                (InR $ Just $ SSBool "a" :: Sum Maybe Maybe SBool) `gsymlt` InL (Just $ SSBool "b") @=? CBool False
                (InR $ Just $ SSBool "a" :: Sum Maybe Maybe SBool) `gsymge` InL (Just $ SSBool "b") @=? CBool True
                (InR $ Just $ SSBool "a" :: Sum Maybe Maybe SBool) `gsymgt` InL (Just $ SSBool "b") @=? CBool True
            ],
          testGroup
            "WriterT"
            [ testGroup
                "Lazy"
                [ testProperty
                    "WriterT Integer (Either Integer) Integer"
                    ( ioProperty . \(v1 :: Either Integer (Integer, Integer), v2 :: Either Integer (Integer, Integer)) ->
                        concreteOrdOkProp (WriterLazy.WriterT v1, WriterLazy.WriterT v2)
                    ),
                  testCase "WriterT SBool (Either SBool) SBool" $ do
                    (WriterLazy.WriterT $ Left $ SSBool "a" :: WriterLazy.WriterT SBool (Either SBool) SBool)
                      `gsymle` WriterLazy.WriterT (Left $ SSBool "b")
                      @=? (SSBool "a" `gsymle` SSBool "b" :: SBool)
                    (WriterLazy.WriterT $ Left $ SSBool "a" :: WriterLazy.WriterT SBool (Either SBool) SBool)
                      `gsymlt` WriterLazy.WriterT (Left $ SSBool "b")
                      @=? (SSBool "a" `gsymlt` SSBool "b" :: SBool)
                    (WriterLazy.WriterT $ Left $ SSBool "a" :: WriterLazy.WriterT SBool (Either SBool) SBool)
                      `gsymge` WriterLazy.WriterT (Left $ SSBool "b")
                      @=? (SSBool "a" `gsymge` SSBool "b" :: SBool)
                    (WriterLazy.WriterT $ Left $ SSBool "a" :: WriterLazy.WriterT SBool (Either SBool) SBool)
                      `gsymgt` WriterLazy.WriterT (Left $ SSBool "b")
                      @=? (SSBool "a" `gsymgt` SSBool "b" :: SBool)
                    (WriterLazy.WriterT $ Left $ SSBool "a" :: WriterLazy.WriterT SBool (Either SBool) SBool)
                      `gsymCompare` WriterLazy.WriterT (Left $ SSBool "b")
                      @=? (SSBool "a" `gsymCompare` SSBool "b" :: UnionMBase SBool Ordering)

                    (WriterLazy.WriterT $ Left $ SSBool "a" :: WriterLazy.WriterT SBool (Either SBool) SBool)
                      `gsymle` WriterLazy.WriterT (Right (SSBool "b", SSBool "d"))
                      @=? CBool True
                    (WriterLazy.WriterT $ Left $ SSBool "a" :: WriterLazy.WriterT SBool (Either SBool) SBool)
                      `gsymlt` WriterLazy.WriterT (Right (SSBool "b", SSBool "d"))
                      @=? CBool True
                    (WriterLazy.WriterT $ Left $ SSBool "a" :: WriterLazy.WriterT SBool (Either SBool) SBool)
                      `gsymge` WriterLazy.WriterT (Right (SSBool "b", SSBool "d"))
                      @=? CBool False
                    (WriterLazy.WriterT $ Left $ SSBool "a" :: WriterLazy.WriterT SBool (Either SBool) SBool)
                      `gsymgt` WriterLazy.WriterT (Right (SSBool "b", SSBool "d"))
                      @=? CBool False
                    (WriterLazy.WriterT $ Left $ SSBool "a" :: WriterLazy.WriterT SBool (Either SBool) SBool)
                      `gsymCompare` WriterLazy.WriterT (Right (SSBool "b", SSBool "d"))
                      @=? (mrgSingle LT :: UnionMBase SBool Ordering)

                    (WriterLazy.WriterT $ Right (SSBool "a", SSBool "c") :: WriterLazy.WriterT SBool (Either SBool) SBool)
                      `gsymle` WriterLazy.WriterT (Left $ SSBool "b")
                      @=? CBool False
                    (WriterLazy.WriterT $ Right (SSBool "a", SSBool "c") :: WriterLazy.WriterT SBool (Either SBool) SBool)
                      `gsymlt` WriterLazy.WriterT (Left $ SSBool "b")
                      @=? CBool False
                    (WriterLazy.WriterT $ Right (SSBool "a", SSBool "c") :: WriterLazy.WriterT SBool (Either SBool) SBool)
                      `gsymge` WriterLazy.WriterT (Left $ SSBool "b")
                      @=? CBool True
                    (WriterLazy.WriterT $ Right (SSBool "a", SSBool "c") :: WriterLazy.WriterT SBool (Either SBool) SBool)
                      `gsymgt` WriterLazy.WriterT (Left $ SSBool "b")
                      @=? CBool True
                    (WriterLazy.WriterT $ Right (SSBool "a", SSBool "c") :: WriterLazy.WriterT SBool (Either SBool) SBool)
                      `gsymCompare` WriterLazy.WriterT (Left $ SSBool "b")
                      @=? (mrgSingle GT :: UnionMBase SBool Ordering)

                    (WriterLazy.WriterT $ Right (SSBool "a", SSBool "c") :: WriterLazy.WriterT SBool (Either SBool) SBool)
                      `gsymle` WriterLazy.WriterT (Right (SSBool "b", SSBool "d"))
                      @=? ((SSBool "a", SSBool "c") `gsymle` (SSBool "b", SSBool "d") :: SBool)
                    (WriterLazy.WriterT $ Right (SSBool "a", SSBool "c") :: WriterLazy.WriterT SBool (Either SBool) SBool)
                      `gsymlt` WriterLazy.WriterT (Right (SSBool "b", SSBool "d"))
                      @=? ((SSBool "a", SSBool "c") `gsymlt` (SSBool "b", SSBool "d") :: SBool)
                    (WriterLazy.WriterT $ Right (SSBool "a", SSBool "c") :: WriterLazy.WriterT SBool (Either SBool) SBool)
                      `gsymge` WriterLazy.WriterT (Right (SSBool "b", SSBool "d"))
                      @=? ((SSBool "a", SSBool "c") `gsymge` (SSBool "b", SSBool "d") :: SBool)
                    (WriterLazy.WriterT $ Right (SSBool "a", SSBool "c") :: WriterLazy.WriterT SBool (Either SBool) SBool)
                      `gsymgt` WriterLazy.WriterT (Right (SSBool "b", SSBool "d"))
                      @=? ((SSBool "a", SSBool "c") `gsymgt` (SSBool "b", SSBool "d") :: SBool)
                    (WriterLazy.WriterT $ Right (SSBool "a", SSBool "c") :: WriterLazy.WriterT SBool (Either SBool) SBool)
                      `gsymCompare` WriterLazy.WriterT (Right (SSBool "b", SSBool "d"))
                      @=? ((SSBool "a", SSBool "c") `gsymCompare` (SSBool "b", SSBool "d") :: UnionMBase SBool Ordering)
                ],
              testGroup
                "Strict"
                [ testProperty
                    "WriterT Integer (Either Integer) Integer"
                    ( ioProperty . \(v1 :: Either Integer (Integer, Integer), v2 :: Either Integer (Integer, Integer)) ->
                        concreteOrdOkProp (WriterStrict.WriterT v1, WriterStrict.WriterT v2)
                    ),
                  testCase "WriterT Integer (Either Integer) Integer" $ do
                    (WriterStrict.WriterT $ Left $ SSBool "a" :: WriterStrict.WriterT SBool (Either SBool) SBool)
                      `gsymle` WriterStrict.WriterT (Left $ SSBool "b")
                      @=? (SSBool "a" `gsymle` SSBool "b" :: SBool)
                    (WriterStrict.WriterT $ Left $ SSBool "a" :: WriterStrict.WriterT SBool (Either SBool) SBool)
                      `gsymlt` WriterStrict.WriterT (Left $ SSBool "b")
                      @=? (SSBool "a" `gsymlt` SSBool "b" :: SBool)
                    (WriterStrict.WriterT $ Left $ SSBool "a" :: WriterStrict.WriterT SBool (Either SBool) SBool)
                      `gsymge` WriterStrict.WriterT (Left $ SSBool "b")
                      @=? (SSBool "a" `gsymge` SSBool "b" :: SBool)
                    (WriterStrict.WriterT $ Left $ SSBool "a" :: WriterStrict.WriterT SBool (Either SBool) SBool)
                      `gsymgt` WriterStrict.WriterT (Left $ SSBool "b")
                      @=? (SSBool "a" `gsymgt` SSBool "b" :: SBool)
                    (WriterStrict.WriterT $ Left $ SSBool "a" :: WriterStrict.WriterT SBool (Either SBool) SBool)
                      `gsymCompare` WriterStrict.WriterT (Left $ SSBool "b")
                      @=? (SSBool "a" `gsymCompare` SSBool "b" :: UnionMBase SBool Ordering)

                    (WriterStrict.WriterT $ Left $ SSBool "a" :: WriterStrict.WriterT SBool (Either SBool) SBool)
                      `gsymle` WriterStrict.WriterT (Right (SSBool "b", SSBool "d"))
                      @=? CBool True
                    (WriterStrict.WriterT $ Left $ SSBool "a" :: WriterStrict.WriterT SBool (Either SBool) SBool)
                      `gsymlt` WriterStrict.WriterT (Right (SSBool "b", SSBool "d"))
                      @=? CBool True
                    (WriterStrict.WriterT $ Left $ SSBool "a" :: WriterStrict.WriterT SBool (Either SBool) SBool)
                      `gsymge` WriterStrict.WriterT (Right (SSBool "b", SSBool "d"))
                      @=? CBool False
                    (WriterStrict.WriterT $ Left $ SSBool "a" :: WriterStrict.WriterT SBool (Either SBool) SBool)
                      `gsymgt` WriterStrict.WriterT (Right (SSBool "b", SSBool "d"))
                      @=? CBool False
                    (WriterStrict.WriterT $ Left $ SSBool "a" :: WriterStrict.WriterT SBool (Either SBool) SBool)
                      `gsymCompare` WriterStrict.WriterT (Right (SSBool "b", SSBool "d"))
                      @=? (mrgSingle LT :: UnionMBase SBool Ordering)

                    (WriterStrict.WriterT $ Right (SSBool "a", SSBool "c") :: WriterStrict.WriterT SBool (Either SBool) SBool)
                      `gsymle` WriterStrict.WriterT (Left $ SSBool "b")
                      @=? CBool False
                    (WriterStrict.WriterT $ Right (SSBool "a", SSBool "c") :: WriterStrict.WriterT SBool (Either SBool) SBool)
                      `gsymlt` WriterStrict.WriterT (Left $ SSBool "b")
                      @=? CBool False
                    (WriterStrict.WriterT $ Right (SSBool "a", SSBool "c") :: WriterStrict.WriterT SBool (Either SBool) SBool)
                      `gsymge` WriterStrict.WriterT (Left $ SSBool "b")
                      @=? CBool True
                    (WriterStrict.WriterT $ Right (SSBool "a", SSBool "c") :: WriterStrict.WriterT SBool (Either SBool) SBool)
                      `gsymgt` WriterStrict.WriterT (Left $ SSBool "b")
                      @=? CBool True
                    (WriterStrict.WriterT $ Right (SSBool "a", SSBool "c") :: WriterStrict.WriterT SBool (Either SBool) SBool)
                      `gsymCompare` WriterStrict.WriterT (Left $ SSBool "b")
                      @=? (mrgSingle GT :: UnionMBase SBool Ordering)

                    (WriterStrict.WriterT $ Right (SSBool "a", SSBool "c") :: WriterStrict.WriterT SBool (Either SBool) SBool)
                      `gsymle` WriterStrict.WriterT (Right (SSBool "b", SSBool "d"))
                      @=? ((SSBool "a", SSBool "c") `gsymle` (SSBool "b", SSBool "d") :: SBool)
                    (WriterStrict.WriterT $ Right (SSBool "a", SSBool "c") :: WriterStrict.WriterT SBool (Either SBool) SBool)
                      `gsymlt` WriterStrict.WriterT (Right (SSBool "b", SSBool "d"))
                      @=? ((SSBool "a", SSBool "c") `gsymlt` (SSBool "b", SSBool "d") :: SBool)
                    (WriterStrict.WriterT $ Right (SSBool "a", SSBool "c") :: WriterStrict.WriterT SBool (Either SBool) SBool)
                      `gsymge` WriterStrict.WriterT (Right (SSBool "b", SSBool "d"))
                      @=? ((SSBool "a", SSBool "c") `gsymge` (SSBool "b", SSBool "d") :: SBool)
                    (WriterStrict.WriterT $ Right (SSBool "a", SSBool "c") :: WriterStrict.WriterT SBool (Either SBool) SBool)
                      `gsymgt` WriterStrict.WriterT (Right (SSBool "b", SSBool "d"))
                      @=? ((SSBool "a", SSBool "c") `gsymgt` (SSBool "b", SSBool "d") :: SBool)
                    (WriterStrict.WriterT $ Right (SSBool "a", SSBool "c") :: WriterStrict.WriterT SBool (Either SBool) SBool)
                      `gsymCompare` WriterStrict.WriterT (Right (SSBool "b", SSBool "d"))
                      @=? ((SSBool "a", SSBool "c") `gsymCompare` (SSBool "b", SSBool "d") :: UnionMBase SBool Ordering)
                ]
            ],
          testGroup
            "Identity"
            [ testProperty
                "Identity Integer"
                ( ioProperty . \(v1 :: Integer, v2) ->
                    concreteOrdOkProp (Identity v1, Identity v2)
                ),
              testCase "Identity SBool" $ do
                (Identity $ SSBool "a" :: Identity SBool)
                  `gsymle` Identity (SSBool "b")
                  @=? (SSBool "a" `gsymle` SSBool "b" :: SBool)
                (Identity $ SSBool "a" :: Identity SBool)
                  `gsymlt` Identity (SSBool "b")
                  @=? (SSBool "a" `gsymlt` SSBool "b" :: SBool)
                (Identity $ SSBool "a" :: Identity SBool)
                  `gsymge` Identity (SSBool "b")
                  @=? (SSBool "a" `gsymge` SSBool "b" :: SBool)
                (Identity $ SSBool "a" :: Identity SBool)
                  `gsymgt` Identity (SSBool "b")
                  @=? (SSBool "a" `gsymgt` SSBool "b" :: SBool)
            ],
          testGroup
            "IdentityT"
            [ testProperty
                "IdentityT (Either Integer) Integer"
                ( ioProperty . \(v1 :: Either Integer Integer, v2) ->
                    concreteOrdOkProp (IdentityT v1, IdentityT v2)
                ),
              testCase "IdentityT (Either SBool) SBool" $ do
                (IdentityT $ Left $ SSBool "a" :: IdentityT (Either SBool) SBool)
                  `gsymle` IdentityT (Left $ SSBool "b")
                  @=? (SSBool "a" `gsymle` SSBool "b" :: SBool)
                (IdentityT $ Left $ SSBool "a" :: IdentityT (Either SBool) SBool)
                  `gsymlt` IdentityT (Left $ SSBool "b")
                  @=? (SSBool "a" `gsymlt` SSBool "b" :: SBool)
                (IdentityT $ Left $ SSBool "a" :: IdentityT (Either SBool) SBool)
                  `gsymge` IdentityT (Left $ SSBool "b")
                  @=? (SSBool "a" `gsymge` SSBool "b" :: SBool)
                (IdentityT $ Left $ SSBool "a" :: IdentityT (Either SBool) SBool)
                  `gsymgt` IdentityT (Left $ SSBool "b")
                  @=? (SSBool "a" `gsymgt` SSBool "b" :: SBool)
                (IdentityT $ Left $ SSBool "a" :: IdentityT (Either SBool) SBool)
                  `gsymCompare` IdentityT (Left $ SSBool "b")
                  @=? (SSBool "a" `gsymCompare` SSBool "b" :: UnionMBase SBool Ordering)

                (IdentityT $ Left $ SSBool "a" :: IdentityT (Either SBool) SBool)
                  `gsymle` IdentityT (Right $ SSBool "b")
                  @=? CBool True
                (IdentityT $ Left $ SSBool "a" :: IdentityT (Either SBool) SBool)
                  `gsymlt` IdentityT (Right $ SSBool "b")
                  @=? CBool True
                (IdentityT $ Left $ SSBool "a" :: IdentityT (Either SBool) SBool)
                  `gsymge` IdentityT (Right $ SSBool "b")
                  @=? CBool False
                (IdentityT $ Left $ SSBool "a" :: IdentityT (Either SBool) SBool)
                  `gsymgt` IdentityT (Right $ SSBool "b")
                  @=? CBool False
                (IdentityT $ Left $ SSBool "a" :: IdentityT (Either SBool) SBool)
                  `gsymCompare` IdentityT (Right $ SSBool "b")
                  @=? (mrgSingle LT :: UnionMBase SBool Ordering)

                (IdentityT $ Right $ SSBool "a" :: IdentityT (Either SBool) SBool)
                  `gsymle` IdentityT (Left $ SSBool "b")
                  @=? CBool False
                (IdentityT $ Right $ SSBool "a" :: IdentityT (Either SBool) SBool)
                  `gsymlt` IdentityT (Left $ SSBool "b")
                  @=? CBool False
                (IdentityT $ Right $ SSBool "a" :: IdentityT (Either SBool) SBool)
                  `gsymge` IdentityT (Left $ SSBool "b")
                  @=? CBool True
                (IdentityT $ Right $ SSBool "a" :: IdentityT (Either SBool) SBool)
                  `gsymgt` IdentityT (Left $ SSBool "b")
                  @=? CBool True
                (IdentityT $ Right $ SSBool "a" :: IdentityT (Either SBool) SBool)
                  `gsymCompare` IdentityT (Left $ SSBool "b")
                  @=? (mrgSingle GT :: UnionMBase SBool Ordering)

                (IdentityT $ Right $ SSBool "a" :: IdentityT (Either SBool) SBool)
                  `gsymle` IdentityT (Right $ SSBool "b")
                  @=? (SSBool "a" `gsymle` SSBool "b" :: SBool)
                (IdentityT $ Right $ SSBool "a" :: IdentityT (Either SBool) SBool)
                  `gsymlt` IdentityT (Right $ SSBool "b")
                  @=? (SSBool "a" `gsymlt` SSBool "b" :: SBool)
                (IdentityT $ Right $ SSBool "a" :: IdentityT (Either SBool) SBool)
                  `gsymge` IdentityT (Right $ SSBool "b")
                  @=? (SSBool "a" `gsymge` SSBool "b" :: SBool)
                (IdentityT $ Right $ SSBool "a" :: IdentityT (Either SBool) SBool)
                  `gsymgt` IdentityT (Right $ SSBool "b")
                  @=? (SSBool "a" `gsymgt` SSBool "b" :: SBool)
                (IdentityT $ Right $ SSBool "a" :: IdentityT (Either SBool) SBool)
                  `gsymCompare` IdentityT (Right $ SSBool "b")
                  @=? (SSBool "a" `gsymCompare` SSBool "b" :: UnionMBase SBool Ordering)
            ],
          testCase "ByteString" $ do
            let bytestrings :: [B.ByteString] = ["", "a", "b", "ab", "ba", "aa", "bb"]
            traverse_ concreteOrdOkProp [(x, y) | x <- bytestrings, y <- bytestrings]
        ]
    ]
