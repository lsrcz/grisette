{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Grisette.Core.Data.Class.SOrdTests (sordTests) where

import Control.Monad.Except (ExceptT (ExceptT))
import Control.Monad.Identity
  ( Identity (Identity),
    IdentityT (IdentityT),
  )
import Control.Monad.Trans.Maybe (MaybeT (MaybeT))
import qualified Control.Monad.Writer.Lazy as WriterLazy
import qualified Control.Monad.Writer.Strict as WriterStrict
import Data.Bifunctor (Bifunctor (bimap))
import qualified Data.ByteString as B
import Data.Foldable (traverse_)
import Data.Functor.Sum (Sum (InL, InR))
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.Stack (HasCallStack)
import Grisette (ITEOp (symIte))
import Grisette.Core.Control.Monad.UnionM (UnionM)
import Grisette.Core.Data.Class.LogicalOp (LogicalOp (symNot, (.&&), (.||)))
import Grisette.Core.Data.Class.SEq (SEq ((.==)))
import Grisette.Core.Data.Class.SOrd
  ( SOrd (symCompare, (.<), (.<=), (.>), (.>=)),
    mrgMax,
    mrgMin,
    symMax,
    symMin,
  )
import Grisette.Core.Data.Class.SimpleMergeable
  ( mrgIf,
  )
import Grisette.Core.Data.Class.TestValues (conBool, ssymBool)
import Grisette.Core.Data.Class.TryMerge
  ( mrgSingle,
  )
import Grisette.IR.SymPrim.Data.SymPrim (SymBool, SymInteger)
import Grisette.Lib.Control.Monad (mrgReturn)
import Grisette.TestUtil.SymbolicAssertion ((.@?=))
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.HUnit (Assertion, (@?=))
import Test.QuickCheck (ioProperty)

concreteOrdOkProp :: (HasCallStack, SOrd a, Ord a) => (a, a) -> Assertion
concreteOrdOkProp (i, j) = do
  i .<= j @?= conBool (i <= j)
  i .< j @?= conBool (i < j)
  i .>= j @?= conBool (i >= j)
  i .> j @?= conBool (i > j)
  symCompare i j @?= (mrgReturn $ compare i j :: UnionM Ordering)

symbolicProdOrdOkProp ::
  (HasCallStack, Show v, Show vl, Show vr, SOrd v, SOrd vl, SOrd vr) =>
  v ->
  v ->
  vl ->
  vr ->
  vl ->
  vr ->
  Assertion
symbolicProdOrdOkProp l r ll lr rl rr = do
  l .<= r @?= ((ll .< rl) .|| ((ll .== rl) .&& (lr .<= rr)))
  l .< r @?= ((ll .< rl) .|| ((ll .== rl) .&& (lr .< rr)))
  l .>= r @?= ((ll .> rl) .|| ((ll .== rl) .&& (lr .>= rr)))
  l .> r @?= ((ll .> rl) .|| ((ll .== rl) .&& (lr .> rr)))
  l
    `symCompare` r
    @?= ( ( do
              lc <- symCompare ll rl
              case lc of
                EQ -> symCompare lr rr
                _ -> mrgReturn lc
          ) ::
            UnionM Ordering
        )

sordTests :: Test
sordTests =
  testGroup
    "SOrd"
    [ testGroup
        "SOrd for common types"
        [ testGroup
            "SymBool"
            [ testCase "Concrete SymBool" $ do
                conBool False .<= conBool False @?= conBool True
                conBool False .< conBool False @?= conBool False
                conBool False .>= conBool False @?= conBool True
                conBool False .> conBool False @?= conBool False
                conBool False .<= conBool True @?= conBool True
                conBool False .< conBool True @?= conBool True
                conBool False .>= conBool True @?= conBool False
                conBool False .> conBool True @?= conBool False
                conBool True .<= conBool False @?= conBool False
                conBool True .< conBool False @?= conBool False
                conBool True .>= conBool False @?= conBool True
                conBool True .> conBool False @?= conBool True
                conBool True .<= conBool True @?= conBool True
                conBool True .< conBool True @?= conBool False
                conBool True .>= conBool True @?= conBool True
                conBool True .> conBool True @?= conBool False,
              testCase "Symbolic SymBool" $ do
                ssymBool "a"
                  .<= ssymBool "b"
                  @?= (symNot (ssymBool "a"))
                    .|| (ssymBool "b")
                ssymBool "a"
                  .< ssymBool "b"
                  @?= (symNot (ssymBool "a"))
                    .&& (ssymBool "b")
                ssymBool "a"
                  .>= ssymBool "b"
                  @?= (ssymBool "a")
                    .|| (symNot (ssymBool "b"))
                ssymBool "a"
                  .> ssymBool "b"
                  @?= (ssymBool "a")
                    .&& (symNot (ssymBool "b"))
                symCompare (ssymBool "a") (ssymBool "b")
                  @?= ( mrgIf
                          ((symNot (ssymBool "a")) .&& (ssymBool "b"))
                          (mrgSingle LT)
                          ( mrgIf
                              ((ssymBool "a") .== (ssymBool "b"))
                              (mrgSingle EQ)
                              (mrgSingle GT)
                          ) ::
                          UnionM Ordering
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
            [ testProperty "[Integer]" $
                ioProperty . concreteOrdOkProp @[Integer],
              testProperty "[String]" $
                ioProperty . concreteOrdOkProp @[String],
              testCase "[SymBool]" $ do
                ([] :: [SymBool]) .<= [] @?= conBool True
                ([] :: [SymBool]) .< [] @?= conBool False
                ([] :: [SymBool]) .>= [] @?= conBool True
                ([] :: [SymBool]) .> [] @?= conBool False
                ([] :: [SymBool])
                  `symCompare` []
                  @?= (mrgSingle EQ :: UnionM Ordering)
                [] .<= [ssymBool "a"] @?= conBool True
                [] .< [ssymBool "a"] @?= conBool True
                [] .>= [ssymBool "a"] @?= conBool False
                [] .> [ssymBool "a"] @?= conBool False
                []
                  `symCompare` [ssymBool "a"]
                  @?= (mrgSingle LT :: UnionM Ordering)
                [ssymBool "a"] .<= [] @?= conBool False
                [ssymBool "a"] .< [] @?= conBool False
                [ssymBool "a"] .>= [] @?= conBool True
                [ssymBool "a"] .> [] @?= conBool True
                [ssymBool "a"]
                  `symCompare` []
                  @?= (mrgSingle GT :: UnionM Ordering)

                [ssymBool "a", ssymBool "b"]
                  .<= [ssymBool "c"]
                  @?= (ssymBool "a" .< ssymBool "c" :: SymBool)
                [ssymBool "a", ssymBool "b"]
                  .< [ssymBool "c"]
                  @?= (ssymBool "a" .< ssymBool "c" :: SymBool)
                [ssymBool "a", ssymBool "b"]
                  .>= [ssymBool "c"]
                  @?= ( (ssymBool "a" .> ssymBool "c")
                          .|| (ssymBool "a" .== ssymBool "c") ::
                          SymBool
                      )
                [ssymBool "a", ssymBool "b"]
                  .> [ssymBool "c"]
                  @?= ( (ssymBool "a" .> ssymBool "c")
                          .|| (ssymBool "a" .== ssymBool "c") ::
                          SymBool
                      )
                [ssymBool "a"]
                  `symCompare` [ssymBool "b"]
                  @?= (ssymBool "a" `symCompare` ssymBool "b" :: UnionM Ordering)

                [ssymBool "a"]
                  .<= [ssymBool "b", ssymBool "c"]
                  @?= ( (ssymBool "a" .< ssymBool "b")
                          .|| (ssymBool "a" .== ssymBool "b") ::
                          SymBool
                      )
                [ssymBool "a"]
                  .< [ssymBool "b", ssymBool "c"]
                  @?= ( (ssymBool "a" .< ssymBool "b")
                          .|| (ssymBool "a" .== ssymBool "b") ::
                          SymBool
                      )
                [ssymBool "a"]
                  .>= [ssymBool "b", ssymBool "c"]
                  @?= (ssymBool "a" .> ssymBool "b" :: SymBool)
                [ssymBool "a"]
                  .> [ssymBool "b", ssymBool "c"]
                  @?= (ssymBool "a" .> ssymBool "b" :: SymBool)
                [ssymBool "a"]
                  `symCompare` [ssymBool "b", ssymBool "c"]
                  @?= ( mrgIf
                          (ssymBool "a" .< ssymBool "b")
                          (mrgSingle LT)
                          ( mrgIf
                              (ssymBool "a" .== ssymBool "b")
                              (mrgSingle LT)
                              (mrgSingle GT)
                          ) ::
                          UnionM Ordering
                      )

                [ssymBool "a", ssymBool "b"]
                  .<= [ssymBool "c", ssymBool "d"]
                  @?= ( (ssymBool "a" .< ssymBool "c")
                          .|| ( ssymBool "a"
                                  .== ssymBool "c"
                                  .&& ( (ssymBool "b" .< ssymBool "d")
                                          .|| (ssymBool "b" .== ssymBool "d")
                                      )
                              ) ::
                          SymBool
                      )
                [ssymBool "a", ssymBool "b"]
                  .< [ssymBool "c", ssymBool "d"]
                  @?= ( (ssymBool "a" .< ssymBool "c")
                          .|| ( ssymBool "a"
                                  .== ssymBool "c"
                                  .&& (ssymBool "b" .< ssymBool "d")
                              ) ::
                          SymBool
                      )
                [ssymBool "a", ssymBool "b"]
                  .>= [ssymBool "c", ssymBool "d"]
                  @?= ( (ssymBool "a" .> ssymBool "c")
                          .|| ( ssymBool "a"
                                  .== ssymBool "c"
                                  .&& ( (ssymBool "b" .> ssymBool "d")
                                          .|| (ssymBool "b" .== ssymBool "d")
                                      )
                              ) ::
                          SymBool
                      )
                [ssymBool "a", ssymBool "b"]
                  .> [ssymBool "c", ssymBool "d"]
                  @?= ( (ssymBool "a" .> ssymBool "c")
                          .|| ( ssymBool "a"
                                  .== ssymBool "c"
                                  .&& (ssymBool "b" .> ssymBool "d")
                              ) ::
                          SymBool
                      )
                [ssymBool "a", ssymBool "b"]
                  `symCompare` [ssymBool "c", ssymBool "d"]
                  @?= ( mrgIf
                          (ssymBool "a" .< ssymBool "c")
                          (mrgSingle LT)
                          ( mrgIf
                              (ssymBool "a" .== ssymBool "c")
                              (ssymBool "b" `symCompare` ssymBool "d")
                              (mrgSingle GT)
                          ) ::
                          UnionM Ordering
                      )
            ],
          testGroup
            "Maybe"
            [ testProperty "Maybe Integer" $
                ioProperty . concreteOrdOkProp @(Maybe Integer),
              testCase "Maybe SymBool" $ do
                (Nothing :: Maybe SymBool) .<= Nothing @?= conBool True
                (Nothing :: Maybe SymBool) .< Nothing @?= conBool False
                (Nothing :: Maybe SymBool) .>= Nothing @?= conBool True
                (Nothing :: Maybe SymBool) .> Nothing @?= conBool False
                (Nothing :: Maybe SymBool)
                  `symCompare` Nothing
                  @?= (mrgSingle EQ :: UnionM Ordering)
                Nothing .<= Just (ssymBool "a") @?= conBool True
                Nothing .< Just (ssymBool "a") @?= conBool True
                Nothing .>= Just (ssymBool "a") @?= conBool False
                Nothing .> Just (ssymBool "a") @?= conBool False
                Nothing
                  `symCompare` Just (ssymBool "a")
                  @?= (mrgSingle LT :: UnionM Ordering)
                Just (ssymBool "a") .<= Nothing @?= conBool False
                Just (ssymBool "a") .< Nothing @?= conBool False
                Just (ssymBool "a") .>= Nothing @?= conBool True
                Just (ssymBool "a") .> Nothing @?= conBool True
                Just (ssymBool "a")
                  `symCompare` Nothing
                  @?= (mrgSingle GT :: UnionM Ordering)
                Just (ssymBool "a")
                  .<= Just (ssymBool "b")
                  @?= (ssymBool "a" .<= ssymBool "b" :: SymBool)
                Just (ssymBool "a")
                  .< Just (ssymBool "b")
                  @?= (ssymBool "a" .< ssymBool "b" :: SymBool)
                Just (ssymBool "a")
                  .>= Just (ssymBool "b")
                  @?= (ssymBool "a" .>= ssymBool "b" :: SymBool)
                Just (ssymBool "a")
                  .> Just (ssymBool "b")
                  @?= (ssymBool "a" .> ssymBool "b" :: SymBool)
                Just (ssymBool "a")
                  `symCompare` Just (ssymBool "b")
                  @?= ( ssymBool "a" `symCompare` ssymBool "b" ::
                          UnionM Ordering
                      )
            ],
          testGroup
            "MaybeT"
            [ testProperty "MaybeT Maybe Integer" $
                ioProperty
                  . concreteOrdOkProp @(MaybeT Maybe Integer)
                  . bimap MaybeT MaybeT,
              testCase "MaybeT Maybe SymBool" $ do
                (MaybeT Nothing :: MaybeT Maybe SymBool)
                  .<= MaybeT Nothing
                  @?= conBool True
                (MaybeT Nothing :: MaybeT Maybe SymBool)
                  .<= MaybeT (Just (Just (ssymBool "a")))
                  @?= conBool True
                MaybeT (Just (Just (ssymBool "a")))
                  .<= (MaybeT Nothing :: MaybeT Maybe SymBool)
                  @?= conBool False
                MaybeT (Just (Just (ssymBool "a")))
                  .<= ( MaybeT (Just (Just (ssymBool "b"))) ::
                          MaybeT Maybe SymBool
                      )
                  @?= (ssymBool "a" .<= ssymBool "b" :: SymBool)

                (MaybeT Nothing :: MaybeT Maybe SymBool)
                  .< MaybeT Nothing
                  @?= conBool False
                (MaybeT Nothing :: MaybeT Maybe SymBool)
                  .< MaybeT (Just (Just (ssymBool "a")))
                  @?= conBool True
                MaybeT (Just (Just (ssymBool "a")))
                  .< (MaybeT Nothing :: MaybeT Maybe SymBool)
                  @?= conBool False
                MaybeT (Just (Just (ssymBool "a")))
                  .< ( MaybeT (Just (Just (ssymBool "b"))) ::
                         MaybeT Maybe SymBool
                     )
                  @?= (ssymBool "a" .< ssymBool "b" :: SymBool)

                (MaybeT Nothing :: MaybeT Maybe SymBool)
                  .>= MaybeT Nothing
                  @?= conBool True
                (MaybeT Nothing :: MaybeT Maybe SymBool)
                  .>= MaybeT (Just (Just (ssymBool "a")))
                  @?= conBool False
                MaybeT (Just (Just (ssymBool "a")))
                  .>= (MaybeT Nothing :: MaybeT Maybe SymBool)
                  @?= conBool True
                MaybeT (Just (Just (ssymBool "a")))
                  .>= ( MaybeT (Just (Just (ssymBool "b"))) ::
                          MaybeT Maybe SymBool
                      )
                  @?= (ssymBool "a" .>= ssymBool "b" :: SymBool)

                (MaybeT Nothing :: MaybeT Maybe SymBool)
                  .> MaybeT Nothing
                  @?= conBool False
                (MaybeT Nothing :: MaybeT Maybe SymBool)
                  .> MaybeT (Just (Just (ssymBool "a")))
                  @?= conBool False
                MaybeT (Just (Just (ssymBool "a")))
                  .> (MaybeT Nothing :: MaybeT Maybe SymBool)
                  @?= conBool True
                MaybeT (Just (Just (ssymBool "a")))
                  .> ( MaybeT (Just (Just (ssymBool "b"))) ::
                         MaybeT Maybe SymBool
                     )
                  @?= (ssymBool "a" .> ssymBool "b" :: SymBool)

                (MaybeT Nothing :: MaybeT Maybe SymBool)
                  `symCompare` MaybeT Nothing
                  @?= (mrgSingle EQ :: UnionM Ordering)
                (MaybeT Nothing :: MaybeT Maybe SymBool)
                  `symCompare` MaybeT (Just (Just (ssymBool "a")))
                  @?= (mrgSingle LT :: UnionM Ordering)
                MaybeT (Just (Just (ssymBool "a")))
                  `symCompare` (MaybeT Nothing :: MaybeT Maybe SymBool)
                  @?= (mrgSingle GT :: UnionM Ordering)
                MaybeT (Just (Just (ssymBool "a")))
                  `symCompare` ( MaybeT (Just (Just (ssymBool "b"))) ::
                                   MaybeT Maybe SymBool
                               )
                  @?= ( ssymBool "a" `symCompare` ssymBool "b" ::
                          UnionM Ordering
                      )
            ],
          testGroup
            "Either"
            [ testProperty "Either Integer Integer" $
                ioProperty . concreteOrdOkProp @(Either Integer Integer),
              testCase "Either SymBool SymBool" $ do
                (Left (ssymBool "a") :: Either SymBool SymBool)
                  .<= Left (ssymBool "b")
                  @?= (ssymBool "a" .<= ssymBool "b" :: SymBool)
                (Left (ssymBool "a") :: Either SymBool SymBool)
                  .< Left (ssymBool "b")
                  @?= (ssymBool "a" .< ssymBool "b" :: SymBool)
                (Left (ssymBool "a") :: Either SymBool SymBool)
                  .>= Left (ssymBool "b")
                  @?= (ssymBool "a" .>= ssymBool "b" :: SymBool)
                (Left (ssymBool "a") :: Either SymBool SymBool)
                  .> Left (ssymBool "b")
                  @?= (ssymBool "a" .> ssymBool "b" :: SymBool)
                (Left (ssymBool "a") :: Either SymBool SymBool)
                  `symCompare` Left (ssymBool "b")
                  @?= (ssymBool "a" `symCompare` ssymBool "b")
                (Left (ssymBool "a") :: Either SymBool SymBool)
                  .<= Right (ssymBool "b")
                  @?= conBool True
                (Left (ssymBool "a") :: Either SymBool SymBool)
                  .< Right (ssymBool "b")
                  @?= conBool True
                (Left (ssymBool "a") :: Either SymBool SymBool)
                  .>= Right (ssymBool "b")
                  @?= conBool False
                (Left (ssymBool "a") :: Either SymBool SymBool)
                  .> Right (ssymBool "b")
                  @?= conBool False
                (Left (ssymBool "a") :: Either SymBool SymBool)
                  `symCompare` Right (ssymBool "b")
                  @?= (mrgSingle LT :: UnionM Ordering)
                (Right (ssymBool "a") :: Either SymBool SymBool)
                  .<= Left (ssymBool "b")
                  @?= conBool False
                (Right (ssymBool "a") :: Either SymBool SymBool)
                  .< Left (ssymBool "b")
                  @?= conBool False
                (Right (ssymBool "a") :: Either SymBool SymBool)
                  .>= Left (ssymBool "b")
                  @?= conBool True
                (Right (ssymBool "a") :: Either SymBool SymBool)
                  .> Left (ssymBool "b")
                  @?= conBool True
                (Right (ssymBool "a") :: Either SymBool SymBool)
                  `symCompare` Left (ssymBool "b")
                  @?= (mrgSingle GT :: UnionM Ordering)
                (Right (ssymBool "a") :: Either SymBool SymBool)
                  .<= Right (ssymBool "b")
                  @?= (ssymBool "a" .<= ssymBool "b" :: SymBool)
                (Right (ssymBool "a") :: Either SymBool SymBool)
                  .< Right (ssymBool "b")
                  @?= (ssymBool "a" .< ssymBool "b" :: SymBool)
                (Right (ssymBool "a") :: Either SymBool SymBool)
                  .>= Right (ssymBool "b")
                  @?= (ssymBool "a" .>= ssymBool "b" :: SymBool)
                (Right (ssymBool "a") :: Either SymBool SymBool)
                  .> Right (ssymBool "b")
                  @?= (ssymBool "a" .> ssymBool "b" :: SymBool)
                (Right (ssymBool "a") :: Either SymBool SymBool)
                  `symCompare` Right (ssymBool "b")
                  @?= (ssymBool "a" `symCompare` ssymBool "b")
            ],
          testGroup
            "ExceptT"
            [ testProperty
                "ExceptT Integer Maybe Integer"
                $ ioProperty
                  . concreteOrdOkProp @(ExceptT Integer Maybe Integer)
                  . bimap ExceptT ExceptT,
              testCase "ExceptT SymBool Maybe SymBool" $ do
                (ExceptT Nothing :: ExceptT SymBool Maybe SymBool)
                  .<= ExceptT Nothing
                  @?= conBool True
                (ExceptT Nothing :: ExceptT SymBool Maybe SymBool)
                  .<= ExceptT (Just (Left (ssymBool "a")))
                  @?= conBool True
                (ExceptT Nothing :: ExceptT SymBool Maybe SymBool)
                  .<= ExceptT (Just (Right (ssymBool "a")))
                  @?= conBool True
                ExceptT (Just (Left (ssymBool "a")))
                  .<= (ExceptT Nothing :: ExceptT SymBool Maybe SymBool)
                  @?= conBool False
                ExceptT (Just (Right (ssymBool "a")))
                  .<= (ExceptT Nothing :: ExceptT SymBool Maybe SymBool)
                  @?= conBool False
                ExceptT (Just (Left (ssymBool "a")))
                  .<= ( ExceptT (Just (Left (ssymBool "b"))) ::
                          ExceptT SymBool Maybe SymBool
                      )
                  @?= (ssymBool "a" .<= ssymBool "b" :: SymBool)
                ExceptT (Just (Right (ssymBool "a")))
                  .<= ( ExceptT (Just (Left (ssymBool "b"))) ::
                          ExceptT SymBool Maybe SymBool
                      )
                  @?= conBool False
                ExceptT (Just (Left (ssymBool "a")))
                  .<= ( ExceptT (Just (Right (ssymBool "b"))) ::
                          ExceptT SymBool Maybe SymBool
                      )
                  @?= conBool True
                ExceptT (Just (Right (ssymBool "a")))
                  .<= ( ExceptT (Just (Right (ssymBool "b"))) ::
                          ExceptT SymBool Maybe SymBool
                      )
                  @?= (ssymBool "a" .<= ssymBool "b" :: SymBool)

                (ExceptT Nothing :: ExceptT SymBool Maybe SymBool)
                  .< ExceptT Nothing
                  @?= conBool False
                (ExceptT Nothing :: ExceptT SymBool Maybe SymBool)
                  .< ExceptT (Just (Left (ssymBool "a")))
                  @?= conBool True
                (ExceptT Nothing :: ExceptT SymBool Maybe SymBool)
                  .< ExceptT (Just (Right (ssymBool "a")))
                  @?= conBool True
                ExceptT (Just (Left (ssymBool "a")))
                  .< (ExceptT Nothing :: ExceptT SymBool Maybe SymBool)
                  @?= conBool False
                ExceptT (Just (Right (ssymBool "a")))
                  .< (ExceptT Nothing :: ExceptT SymBool Maybe SymBool)
                  @?= conBool False
                ExceptT (Just (Left (ssymBool "a")))
                  .< ( ExceptT (Just (Left (ssymBool "b"))) ::
                         ExceptT SymBool Maybe SymBool
                     )
                  @?= (ssymBool "a" .< ssymBool "b" :: SymBool)
                ExceptT (Just (Right (ssymBool "a")))
                  .< ( ExceptT (Just (Left (ssymBool "b"))) ::
                         ExceptT SymBool Maybe SymBool
                     )
                  @?= conBool False
                ExceptT (Just (Left (ssymBool "a")))
                  .< ( ExceptT (Just (Right (ssymBool "b"))) ::
                         ExceptT SymBool Maybe SymBool
                     )
                  @?= conBool True
                ExceptT (Just (Right (ssymBool "a")))
                  .< ( ExceptT (Just (Right (ssymBool "b"))) ::
                         ExceptT SymBool Maybe SymBool
                     )
                  @?= (ssymBool "a" .< ssymBool "b" :: SymBool)

                (ExceptT Nothing :: ExceptT SymBool Maybe SymBool)
                  .>= ExceptT Nothing
                  @?= conBool True
                (ExceptT Nothing :: ExceptT SymBool Maybe SymBool)
                  .>= ExceptT (Just (Left (ssymBool "a")))
                  @?= conBool False
                (ExceptT Nothing :: ExceptT SymBool Maybe SymBool)
                  .>= ExceptT (Just (Right (ssymBool "a")))
                  @?= conBool False
                ExceptT (Just (Left (ssymBool "a")))
                  .>= (ExceptT Nothing :: ExceptT SymBool Maybe SymBool)
                  @?= conBool True
                ExceptT (Just (Right (ssymBool "a")))
                  .>= (ExceptT Nothing :: ExceptT SymBool Maybe SymBool)
                  @?= conBool True
                ExceptT (Just (Left (ssymBool "a")))
                  .>= ( ExceptT (Just (Left (ssymBool "b"))) ::
                          ExceptT SymBool Maybe SymBool
                      )
                  @?= (ssymBool "a" .>= ssymBool "b" :: SymBool)
                ExceptT (Just (Right (ssymBool "a")))
                  .>= ( ExceptT (Just (Left (ssymBool "b"))) ::
                          ExceptT SymBool Maybe SymBool
                      )
                  @?= conBool True
                ExceptT (Just (Left (ssymBool "a")))
                  .>= ( ExceptT (Just (Right (ssymBool "b"))) ::
                          ExceptT SymBool Maybe SymBool
                      )
                  @?= conBool False
                ExceptT (Just (Right (ssymBool "a")))
                  .>= ( ExceptT (Just (Right (ssymBool "b"))) ::
                          ExceptT SymBool Maybe SymBool
                      )
                  @?= (ssymBool "a" .>= ssymBool "b" :: SymBool)

                (ExceptT Nothing :: ExceptT SymBool Maybe SymBool)
                  .> ExceptT Nothing
                  @?= conBool False
                (ExceptT Nothing :: ExceptT SymBool Maybe SymBool)
                  .> ExceptT (Just (Left (ssymBool "a")))
                  @?= conBool False
                (ExceptT Nothing :: ExceptT SymBool Maybe SymBool)
                  .> ExceptT (Just (Right (ssymBool "a")))
                  @?= conBool False
                ExceptT (Just (Left (ssymBool "a")))
                  .> (ExceptT Nothing :: ExceptT SymBool Maybe SymBool)
                  @?= conBool True
                ExceptT (Just (Right (ssymBool "a")))
                  .> (ExceptT Nothing :: ExceptT SymBool Maybe SymBool)
                  @?= conBool True
                ExceptT (Just (Left (ssymBool "a")))
                  .> ( ExceptT (Just (Left (ssymBool "b"))) ::
                         ExceptT SymBool Maybe SymBool
                     )
                  @?= (ssymBool "a" .> ssymBool "b" :: SymBool)
                ExceptT (Just (Right (ssymBool "a")))
                  .> ( ExceptT (Just (Left (ssymBool "b"))) ::
                         ExceptT SymBool Maybe SymBool
                     )
                  @?= conBool True
                ExceptT (Just (Left (ssymBool "a")))
                  .> ( ExceptT (Just (Right (ssymBool "b"))) ::
                         ExceptT SymBool Maybe SymBool
                     )
                  @?= conBool False
                ExceptT (Just (Right (ssymBool "a")))
                  .> ( ExceptT (Just (Right (ssymBool "b"))) ::
                         ExceptT SymBool Maybe SymBool
                     )
                  @?= (ssymBool "a" .> ssymBool "b" :: SymBool)

                (ExceptT Nothing :: ExceptT SymBool Maybe SymBool)
                  `symCompare` ExceptT Nothing
                  @?= (mrgSingle EQ :: UnionM Ordering)
                (ExceptT Nothing :: ExceptT SymBool Maybe SymBool)
                  `symCompare` ExceptT (Just (Left (ssymBool "a")))
                  @?= (mrgSingle LT :: UnionM Ordering)
                (ExceptT Nothing :: ExceptT SymBool Maybe SymBool)
                  `symCompare` ExceptT (Just (Right (ssymBool "a")))
                  @?= (mrgSingle LT :: UnionM Ordering)
                ExceptT (Just (Left (ssymBool "a")))
                  `symCompare` ( ExceptT Nothing ::
                                   ExceptT SymBool Maybe SymBool
                               )
                  @?= (mrgSingle GT :: UnionM Ordering)
                ExceptT (Just (Right (ssymBool "a")))
                  `symCompare` ( ExceptT Nothing ::
                                   ExceptT SymBool Maybe SymBool
                               )
                  @?= (mrgSingle GT :: UnionM Ordering)
                ExceptT (Just (Left (ssymBool "a")))
                  `symCompare` ( ExceptT (Just (Left (ssymBool "b"))) ::
                                   ExceptT SymBool Maybe SymBool
                               )
                  @?= (ssymBool "a" `symCompare` ssymBool "b" :: UnionM Ordering)
                ExceptT (Just (Right (ssymBool "a")))
                  `symCompare` ( ExceptT (Just (Left (ssymBool "b"))) ::
                                   ExceptT SymBool Maybe SymBool
                               )
                  @?= (mrgSingle GT :: UnionM Ordering)
                ExceptT (Just (Left (ssymBool "a")))
                  `symCompare` ( ExceptT (Just (Right (ssymBool "b"))) ::
                                   ExceptT SymBool Maybe SymBool
                               )
                  @?= (mrgSingle LT :: UnionM Ordering)
                ExceptT (Just (Right (ssymBool "a")))
                  `symCompare` ( ExceptT (Just (Right (ssymBool "b"))) ::
                                   ExceptT SymBool Maybe SymBool
                               )
                  @?= ( ssymBool "a" `symCompare` ssymBool "b" ::
                          UnionM Ordering
                      )
            ],
          testProperty "()" (ioProperty . concreteOrdOkProp @()),
          testGroup
            "(,)"
            [ testProperty "(Integer, Integer)" $
                ioProperty . concreteOrdOkProp @(Integer, Integer),
              testCase "(SymBool, SymBool)" $ do
                let l = (ssymBool "a", ssymBool "c")
                let r = (ssymBool "b", ssymBool "d")
                let ll = ssymBool "a"
                let lr = ssymBool "c"
                let rl = ssymBool "b"
                let rr = ssymBool "d"
                symbolicProdOrdOkProp l r ll lr rl rr
            ],
          testGroup
            "(,,)"
            [ testProperty "(Integer, Integer, Integer)" $
                ioProperty . concreteOrdOkProp @(Integer, Integer, Integer),
              testCase "(SymBool, SymBool, SymBool)" $ do
                let l = (ssymBool "a", ssymBool "c", ssymBool "e")
                let r = (ssymBool "b", ssymBool "d", ssymBool "f")
                let ll = ssymBool "a"
                let lr = (ssymBool "c", ssymBool "e")
                let rl = ssymBool "b"
                let rr = (ssymBool "d", ssymBool "f")
                symbolicProdOrdOkProp l r ll lr rl rr
            ],
          testGroup
            "(,,,)"
            [ testProperty
                "(Integer, Integer, Integer, Integer)"
                $ ioProperty
                  . concreteOrdOkProp @(Integer, Integer, Integer, Integer),
              testCase "(SymBool, SymBool, SymBool, SymBool)" $ do
                let l = (ssymBool "a", ssymBool "c", ssymBool "e", ssymBool "g")
                let r = (ssymBool "b", ssymBool "d", ssymBool "f", ssymBool "h")
                let ll = (ssymBool "a", ssymBool "c")
                let lr = (ssymBool "e", ssymBool "g")
                let rl = (ssymBool "b", ssymBool "d")
                let rr = (ssymBool "f", ssymBool "h")
                symbolicProdOrdOkProp l r ll lr rl rr
            ],
          testGroup
            "(,,,,)"
            [ testProperty
                "(Integer, Integer, Integer, Integer, Integer)"
                $ ioProperty
                  . concreteOrdOkProp
                    @(Integer, Integer, Integer, Integer, Integer),
              testCase "(SymBool, SymBool, SymBool, SymBool, SymBool)" $ do
                let l =
                      ( ssymBool "a",
                        ssymBool "c",
                        ssymBool "e",
                        ssymBool "g",
                        ssymBool "i"
                      )
                let r =
                      ( ssymBool "b",
                        ssymBool "d",
                        ssymBool "f",
                        ssymBool "h",
                        ssymBool "j"
                      )
                let ll = (ssymBool "a", ssymBool "c")
                let lr = (ssymBool "e", ssymBool "g", ssymBool "i")
                let rl = (ssymBool "b", ssymBool "d")
                let rr = (ssymBool "f", ssymBool "h", ssymBool "j")
                symbolicProdOrdOkProp l r ll lr rl rr
            ],
          testGroup
            "(,,,,,)"
            [ testProperty
                "(Integer, Integer, Integer, Integer, Integer, Integer)"
                $ ioProperty
                  . concreteOrdOkProp
                    @( Integer,
                       Integer,
                       Integer,
                       Integer,
                       Integer,
                       Integer
                     ),
              testCase
                "(SymBool, SymBool, SymBool, SymBool, SymBool, SymBool)"
                $ do
                  let l =
                        ( ssymBool "a",
                          ssymBool "c",
                          ssymBool "e",
                          ssymBool "g",
                          ssymBool "i",
                          ssymBool "k"
                        )
                  let r =
                        ( ssymBool "b",
                          ssymBool "d",
                          ssymBool "f",
                          ssymBool "h",
                          ssymBool "j",
                          ssymBool "l"
                        )
                  let ll = (ssymBool "a", ssymBool "c", ssymBool "e")
                  let lr = (ssymBool "g", ssymBool "i", ssymBool "k")
                  let rl = (ssymBool "b", ssymBool "d", ssymBool "f")
                  let rr = (ssymBool "h", ssymBool "j", ssymBool "l")
                  symbolicProdOrdOkProp l r ll lr rl rr
            ],
          testGroup
            "(,,,,,,)"
            [ testProperty
                "(Integer, Integer, Integer, Integer, Integer, Integer, Integer)"
                $ ioProperty
                  . concreteOrdOkProp @(Integer, Integer, Integer, Integer, Integer, Integer, Integer),
              testCase
                "(SymBool, SymBool, SymBool, SymBool, SymBool, SymBool, SymBool)"
                $ do
                  let l =
                        ( ssymBool "a",
                          ssymBool "c",
                          ssymBool "e",
                          ssymBool "g",
                          ssymBool "i",
                          ssymBool "k",
                          ssymBool "m"
                        )
                  let r =
                        ( ssymBool "b",
                          ssymBool "d",
                          ssymBool "f",
                          ssymBool "h",
                          ssymBool "j",
                          ssymBool "l",
                          ssymBool "n"
                        )
                  let ll = (ssymBool "a", ssymBool "c", ssymBool "e")
                  let lr =
                        ( ssymBool "g",
                          ssymBool "i",
                          ssymBool "k",
                          ssymBool "m"
                        )
                  let rl = (ssymBool "b", ssymBool "d", ssymBool "f")
                  let rr =
                        ( ssymBool "h",
                          ssymBool "j",
                          ssymBool "l",
                          ssymBool "n"
                        )
                  symbolicProdOrdOkProp l r ll lr rl rr
            ],
          testGroup
            "(,,,,,,,)"
            [ testProperty
                "(Integer, Integer, Integer, Integer, Integer, Integer, Integer, Integer)"
                ( ioProperty
                    . concreteOrdOkProp
                      @( Integer,
                         Integer,
                         Integer,
                         Integer,
                         Integer,
                         Integer,
                         Integer,
                         Integer
                       )
                ),
              testCase
                "(SymBool, SymBool, SymBool, SymBool, SymBool, SymBool, SymBool, SymBool)"
                $ do
                  let l =
                        ( ssymBool "a",
                          ssymBool "c",
                          ssymBool "e",
                          ssymBool "g",
                          ssymBool "i",
                          ssymBool "k",
                          ssymBool "m",
                          ssymBool "o"
                        )
                  let r =
                        ( ssymBool "b",
                          ssymBool "d",
                          ssymBool "f",
                          ssymBool "h",
                          ssymBool "j",
                          ssymBool "l",
                          ssymBool "n",
                          ssymBool "p"
                        )
                  let ll =
                        ( ssymBool "a",
                          ssymBool "c",
                          ssymBool "e",
                          ssymBool "g"
                        )
                  let lr =
                        ( ssymBool "i",
                          ssymBool "k",
                          ssymBool "m",
                          ssymBool "o"
                        )
                  let rl =
                        ( ssymBool "b",
                          ssymBool "d",
                          ssymBool "f",
                          ssymBool "h"
                        )
                  let rr =
                        ( ssymBool "j",
                          ssymBool "l",
                          ssymBool "n",
                          ssymBool "p"
                        )
                  symbolicProdOrdOkProp l r ll lr rl rr
            ],
          testGroup
            "Sum"
            [ testProperty
                "Sum Maybe Maybe Integer"
                ( ioProperty . \v ->
                    let eitherToSum ::
                          Either (Maybe Integer) (Maybe Integer) ->
                          Sum Maybe Maybe Integer
                        eitherToSum (Left x) = InL x
                        eitherToSum (Right x) = InR x
                     in concreteOrdOkProp (bimap eitherToSum eitherToSum v)
                ),
              testCase "Sum Maybe Maybe SymBool" $ do
                (InL $ Just $ ssymBool "a" :: Sum Maybe Maybe SymBool)
                  .<= InL (Just $ ssymBool "b")
                  @?= (ssymBool "a" .<= ssymBool "b" :: SymBool)
                (InL $ Just $ ssymBool "a" :: Sum Maybe Maybe SymBool)
                  .< InL (Just $ ssymBool "b")
                  @?= (ssymBool "a" .< ssymBool "b" :: SymBool)
                (InL $ Just $ ssymBool "a" :: Sum Maybe Maybe SymBool)
                  .>= InL (Just $ ssymBool "b")
                  @?= (ssymBool "a" .>= ssymBool "b" :: SymBool)
                (InL $ Just $ ssymBool "a" :: Sum Maybe Maybe SymBool)
                  .> InL (Just $ ssymBool "b")
                  @?= (ssymBool "a" .> ssymBool "b" :: SymBool)
                (InL $ Just $ ssymBool "a" :: Sum Maybe Maybe SymBool)
                  .<= InR (Just $ ssymBool "b")
                  @?= conBool True
                (InL $ Just $ ssymBool "a" :: Sum Maybe Maybe SymBool)
                  .< InR (Just $ ssymBool "b")
                  @?= conBool True
                (InL $ Just $ ssymBool "a" :: Sum Maybe Maybe SymBool)
                  .>= InR (Just $ ssymBool "b")
                  @?= conBool False
                (InL $ Just $ ssymBool "a" :: Sum Maybe Maybe SymBool)
                  .> InR (Just $ ssymBool "b")
                  @?= conBool False
                (InR $ Just $ ssymBool "a" :: Sum Maybe Maybe SymBool)
                  .<= InR (Just $ ssymBool "b")
                  @?= (ssymBool "a" .<= ssymBool "b" :: SymBool)
                (InR $ Just $ ssymBool "a" :: Sum Maybe Maybe SymBool)
                  .< InR (Just $ ssymBool "b")
                  @?= (ssymBool "a" .< ssymBool "b" :: SymBool)
                (InR $ Just $ ssymBool "a" :: Sum Maybe Maybe SymBool)
                  .>= InR (Just $ ssymBool "b")
                  @?= (ssymBool "a" .>= ssymBool "b" :: SymBool)
                (InR $ Just $ ssymBool "a" :: Sum Maybe Maybe SymBool)
                  .> InR (Just $ ssymBool "b")
                  @?= (ssymBool "a" .> ssymBool "b" :: SymBool)
                (InR $ Just $ ssymBool "a" :: Sum Maybe Maybe SymBool)
                  .<= InL (Just $ ssymBool "b")
                  @?= conBool False
                (InR $ Just $ ssymBool "a" :: Sum Maybe Maybe SymBool)
                  .< InL (Just $ ssymBool "b")
                  @?= conBool False
                (InR $ Just $ ssymBool "a" :: Sum Maybe Maybe SymBool)
                  .>= InL (Just $ ssymBool "b")
                  @?= conBool True
                (InR $ Just $ ssymBool "a" :: Sum Maybe Maybe SymBool)
                  .> InL (Just $ ssymBool "b")
                  @?= conBool True
            ],
          testGroup
            "WriterT"
            [ testGroup
                "Lazy"
                [ testProperty
                    "WriterT Integer (Either Integer) Integer"
                    ( ioProperty
                        . \( v1 :: Either Integer (Integer, Integer),
                             v2 :: Either Integer (Integer, Integer)
                             ) ->
                            concreteOrdOkProp
                              ( WriterLazy.WriterT v1,
                                WriterLazy.WriterT v2
                              )
                    ),
                  testCase "WriterT SymBool (Either SymBool) SymBool" $ do
                    ( WriterLazy.WriterT $ Left $ ssymBool "a" ::
                        WriterLazy.WriterT SymBool (Either SymBool) SymBool
                      )
                      .<= WriterLazy.WriterT (Left $ ssymBool "b")
                      @?= (ssymBool "a" .<= ssymBool "b" :: SymBool)
                    ( WriterLazy.WriterT $ Left $ ssymBool "a" ::
                        WriterLazy.WriterT SymBool (Either SymBool) SymBool
                      )
                      .< WriterLazy.WriterT (Left $ ssymBool "b")
                      @?= (ssymBool "a" .< ssymBool "b" :: SymBool)
                    ( WriterLazy.WriterT $ Left $ ssymBool "a" ::
                        WriterLazy.WriterT SymBool (Either SymBool) SymBool
                      )
                      .>= WriterLazy.WriterT (Left $ ssymBool "b")
                      @?= (ssymBool "a" .>= ssymBool "b" :: SymBool)
                    ( WriterLazy.WriterT $ Left $ ssymBool "a" ::
                        WriterLazy.WriterT SymBool (Either SymBool) SymBool
                      )
                      .> WriterLazy.WriterT (Left $ ssymBool "b")
                      @?= (ssymBool "a" .> ssymBool "b" :: SymBool)
                    ( WriterLazy.WriterT $ Left $ ssymBool "a" ::
                        WriterLazy.WriterT SymBool (Either SymBool) SymBool
                      )
                      `symCompare` WriterLazy.WriterT (Left $ ssymBool "b")
                      @?= ( ssymBool "a" `symCompare` ssymBool "b" ::
                              UnionM Ordering
                          )

                    ( WriterLazy.WriterT $ Left $ ssymBool "a" ::
                        WriterLazy.WriterT SymBool (Either SymBool) SymBool
                      )
                      .<= WriterLazy.WriterT
                        (Right (ssymBool "b", ssymBool "d"))
                      @?= conBool True
                    ( WriterLazy.WriterT $ Left $ ssymBool "a" ::
                        WriterLazy.WriterT SymBool (Either SymBool) SymBool
                      )
                      .< WriterLazy.WriterT
                        (Right (ssymBool "b", ssymBool "d"))
                      @?= conBool True
                    ( WriterLazy.WriterT $ Left $ ssymBool "a" ::
                        WriterLazy.WriterT SymBool (Either SymBool) SymBool
                      )
                      .>= WriterLazy.WriterT
                        (Right (ssymBool "b", ssymBool "d"))
                      @?= conBool False
                    ( WriterLazy.WriterT $ Left $ ssymBool "a" ::
                        WriterLazy.WriterT SymBool (Either SymBool) SymBool
                      )
                      .> WriterLazy.WriterT (Right (ssymBool "b", ssymBool "d"))
                      @?= conBool False
                    ( WriterLazy.WriterT $ Left $ ssymBool "a" ::
                        WriterLazy.WriterT SymBool (Either SymBool) SymBool
                      )
                      `symCompare` WriterLazy.WriterT
                        (Right (ssymBool "b", ssymBool "d"))
                      @?= (mrgSingle LT :: UnionM Ordering)

                    ( WriterLazy.WriterT $ Right (ssymBool "a", ssymBool "c") ::
                        WriterLazy.WriterT SymBool (Either SymBool) SymBool
                      )
                      .<= WriterLazy.WriterT (Left $ ssymBool "b")
                      @?= conBool False
                    ( WriterLazy.WriterT $ Right (ssymBool "a", ssymBool "c") ::
                        WriterLazy.WriterT SymBool (Either SymBool) SymBool
                      )
                      .< WriterLazy.WriterT (Left $ ssymBool "b")
                      @?= conBool False
                    ( WriterLazy.WriterT $ Right (ssymBool "a", ssymBool "c") ::
                        WriterLazy.WriterT SymBool (Either SymBool) SymBool
                      )
                      .>= WriterLazy.WriterT (Left $ ssymBool "b")
                      @?= conBool True
                    ( WriterLazy.WriterT $ Right (ssymBool "a", ssymBool "c") ::
                        WriterLazy.WriterT SymBool (Either SymBool) SymBool
                      )
                      .> WriterLazy.WriterT (Left $ ssymBool "b")
                      @?= conBool True
                    ( WriterLazy.WriterT $ Right (ssymBool "a", ssymBool "c") ::
                        WriterLazy.WriterT SymBool (Either SymBool) SymBool
                      )
                      `symCompare` WriterLazy.WriterT (Left $ ssymBool "b")
                      @?= (mrgSingle GT :: UnionM Ordering)

                    ( WriterLazy.WriterT $ Right (ssymBool "a", ssymBool "c") ::
                        WriterLazy.WriterT SymBool (Either SymBool) SymBool
                      )
                      .<= WriterLazy.WriterT (Right (ssymBool "b", ssymBool "d"))
                      @?= ( (ssymBool "a", ssymBool "c")
                              .<= (ssymBool "b", ssymBool "d") ::
                              SymBool
                          )
                    ( WriterLazy.WriterT $ Right (ssymBool "a", ssymBool "c") ::
                        WriterLazy.WriterT SymBool (Either SymBool) SymBool
                      )
                      .< WriterLazy.WriterT (Right (ssymBool "b", ssymBool "d"))
                      @?= ( (ssymBool "a", ssymBool "c")
                              .< (ssymBool "b", ssymBool "d") ::
                              SymBool
                          )
                    ( WriterLazy.WriterT $ Right (ssymBool "a", ssymBool "c") ::
                        WriterLazy.WriterT SymBool (Either SymBool) SymBool
                      )
                      .>= WriterLazy.WriterT
                        (Right (ssymBool "b", ssymBool "d"))
                      @?= ( (ssymBool "a", ssymBool "c")
                              .>= (ssymBool "b", ssymBool "d") ::
                              SymBool
                          )
                    ( WriterLazy.WriterT $ Right (ssymBool "a", ssymBool "c") ::
                        WriterLazy.WriterT SymBool (Either SymBool) SymBool
                      )
                      .> WriterLazy.WriterT (Right (ssymBool "b", ssymBool "d"))
                      @?= ( (ssymBool "a", ssymBool "c")
                              .> (ssymBool "b", ssymBool "d") ::
                              SymBool
                          )
                    ( WriterLazy.WriterT $ Right (ssymBool "a", ssymBool "c") ::
                        WriterLazy.WriterT SymBool (Either SymBool) SymBool
                      )
                      `symCompare` WriterLazy.WriterT
                        (Right (ssymBool "b", ssymBool "d"))
                      @?= ( (ssymBool "a", ssymBool "c")
                              `symCompare` (ssymBool "b", ssymBool "d") ::
                              UnionM Ordering
                          )
                ],
              testGroup
                "Strict"
                [ testProperty
                    "WriterT Integer (Either Integer) Integer"
                    ( ioProperty
                        . \( v1 :: Either Integer (Integer, Integer),
                             v2 :: Either Integer (Integer, Integer)
                             ) ->
                            concreteOrdOkProp
                              ( WriterStrict.WriterT v1,
                                WriterStrict.WriterT v2
                              )
                    ),
                  testCase "WriterT Integer (Either Integer) Integer" $ do
                    ( WriterStrict.WriterT $ Left $ ssymBool "a" ::
                        WriterStrict.WriterT SymBool (Either SymBool) SymBool
                      )
                      .<= WriterStrict.WriterT (Left $ ssymBool "b")
                      @?= (ssymBool "a" .<= ssymBool "b" :: SymBool)
                    ( WriterStrict.WriterT $ Left $ ssymBool "a" ::
                        WriterStrict.WriterT SymBool (Either SymBool) SymBool
                      )
                      .< WriterStrict.WriterT (Left $ ssymBool "b")
                      @?= (ssymBool "a" .< ssymBool "b" :: SymBool)
                    ( WriterStrict.WriterT $ Left $ ssymBool "a" ::
                        WriterStrict.WriterT SymBool (Either SymBool) SymBool
                      )
                      .>= WriterStrict.WriterT (Left $ ssymBool "b")
                      @?= (ssymBool "a" .>= ssymBool "b" :: SymBool)
                    ( WriterStrict.WriterT $ Left $ ssymBool "a" ::
                        WriterStrict.WriterT SymBool (Either SymBool) SymBool
                      )
                      .> WriterStrict.WriterT (Left $ ssymBool "b")
                      @?= (ssymBool "a" .> ssymBool "b" :: SymBool)
                    ( WriterStrict.WriterT $ Left $ ssymBool "a" ::
                        WriterStrict.WriterT SymBool (Either SymBool) SymBool
                      )
                      `symCompare` WriterStrict.WriterT (Left $ ssymBool "b")
                      @?= ( ssymBool "a" `symCompare` ssymBool "b" ::
                              UnionM Ordering
                          )

                    ( WriterStrict.WriterT $ Left $ ssymBool "a" ::
                        WriterStrict.WriterT SymBool (Either SymBool) SymBool
                      )
                      .<= WriterStrict.WriterT
                        (Right (ssymBool "b", ssymBool "d"))
                      @?= conBool True
                    ( WriterStrict.WriterT $ Left $ ssymBool "a" ::
                        WriterStrict.WriterT SymBool (Either SymBool) SymBool
                      )
                      .< WriterStrict.WriterT
                        (Right (ssymBool "b", ssymBool "d"))
                      @?= conBool True
                    ( WriterStrict.WriterT $ Left $ ssymBool "a" ::
                        WriterStrict.WriterT SymBool (Either SymBool) SymBool
                      )
                      .>= WriterStrict.WriterT
                        (Right (ssymBool "b", ssymBool "d"))
                      @?= conBool False
                    ( WriterStrict.WriterT $ Left $ ssymBool "a" ::
                        WriterStrict.WriterT SymBool (Either SymBool) SymBool
                      )
                      .> WriterStrict.WriterT
                        (Right (ssymBool "b", ssymBool "d"))
                      @?= conBool False
                    ( WriterStrict.WriterT $ Left $ ssymBool "a" ::
                        WriterStrict.WriterT SymBool (Either SymBool) SymBool
                      )
                      `symCompare` WriterStrict.WriterT
                        (Right (ssymBool "b", ssymBool "d"))
                      @?= (mrgSingle LT :: UnionM Ordering)

                    ( WriterStrict.WriterT $
                        Right (ssymBool "a", ssymBool "c") ::
                        WriterStrict.WriterT SymBool (Either SymBool) SymBool
                      )
                      .<= WriterStrict.WriterT (Left $ ssymBool "b")
                      @?= conBool False
                    ( WriterStrict.WriterT $
                        Right (ssymBool "a", ssymBool "c") ::
                        WriterStrict.WriterT SymBool (Either SymBool) SymBool
                      )
                      .< WriterStrict.WriterT (Left $ ssymBool "b")
                      @?= conBool False
                    ( WriterStrict.WriterT $
                        Right (ssymBool "a", ssymBool "c") ::
                        WriterStrict.WriterT SymBool (Either SymBool) SymBool
                      )
                      .>= WriterStrict.WriterT (Left $ ssymBool "b")
                      @?= conBool True
                    ( WriterStrict.WriterT $
                        Right (ssymBool "a", ssymBool "c") ::
                        WriterStrict.WriterT SymBool (Either SymBool) SymBool
                      )
                      .> WriterStrict.WriterT (Left $ ssymBool "b")
                      @?= conBool True
                    ( WriterStrict.WriterT $
                        Right (ssymBool "a", ssymBool "c") ::
                        WriterStrict.WriterT SymBool (Either SymBool) SymBool
                      )
                      `symCompare` WriterStrict.WriterT (Left $ ssymBool "b")
                      @?= (mrgSingle GT :: UnionM Ordering)

                    ( WriterStrict.WriterT $
                        Right (ssymBool "a", ssymBool "c") ::
                        WriterStrict.WriterT SymBool (Either SymBool) SymBool
                      )
                      .<= WriterStrict.WriterT
                        (Right (ssymBool "b", ssymBool "d"))
                      @?= ( (ssymBool "a", ssymBool "c")
                              .<= (ssymBool "b", ssymBool "d") ::
                              SymBool
                          )
                    ( WriterStrict.WriterT $
                        Right (ssymBool "a", ssymBool "c") ::
                        WriterStrict.WriterT SymBool (Either SymBool) SymBool
                      )
                      .< WriterStrict.WriterT
                        (Right (ssymBool "b", ssymBool "d"))
                      @?= ( (ssymBool "a", ssymBool "c")
                              .< (ssymBool "b", ssymBool "d") ::
                              SymBool
                          )
                    ( WriterStrict.WriterT $
                        Right (ssymBool "a", ssymBool "c") ::
                        WriterStrict.WriterT SymBool (Either SymBool) SymBool
                      )
                      .>= WriterStrict.WriterT
                        (Right (ssymBool "b", ssymBool "d"))
                      @?= ( (ssymBool "a", ssymBool "c")
                              .>= (ssymBool "b", ssymBool "d") ::
                              SymBool
                          )
                    ( WriterStrict.WriterT $
                        Right (ssymBool "a", ssymBool "c") ::
                        WriterStrict.WriterT SymBool (Either SymBool) SymBool
                      )
                      .> WriterStrict.WriterT
                        (Right (ssymBool "b", ssymBool "d"))
                      @?= ( (ssymBool "a", ssymBool "c")
                              .> (ssymBool "b", ssymBool "d") ::
                              SymBool
                          )
                    ( WriterStrict.WriterT $
                        Right (ssymBool "a", ssymBool "c") ::
                        WriterStrict.WriterT SymBool (Either SymBool) SymBool
                      )
                      `symCompare` WriterStrict.WriterT
                        (Right (ssymBool "b", ssymBool "d"))
                      @?= ( (ssymBool "a", ssymBool "c")
                              `symCompare` (ssymBool "b", ssymBool "d") ::
                              UnionM Ordering
                          )
                ]
            ],
          testGroup
            "Identity"
            [ testProperty
                "Identity Integer"
                ( ioProperty . \(v1 :: Integer, v2) ->
                    concreteOrdOkProp (Identity v1, Identity v2)
                ),
              testCase "Identity SymBool" $ do
                (Identity $ ssymBool "a" :: Identity SymBool)
                  .<= Identity (ssymBool "b")
                  @?= (ssymBool "a" .<= ssymBool "b" :: SymBool)
                (Identity $ ssymBool "a" :: Identity SymBool)
                  .< Identity (ssymBool "b")
                  @?= (ssymBool "a" .< ssymBool "b" :: SymBool)
                (Identity $ ssymBool "a" :: Identity SymBool)
                  .>= Identity (ssymBool "b")
                  @?= (ssymBool "a" .>= ssymBool "b" :: SymBool)
                (Identity $ ssymBool "a" :: Identity SymBool)
                  .> Identity (ssymBool "b")
                  @?= (ssymBool "a" .> ssymBool "b" :: SymBool)
            ],
          testGroup
            "IdentityT"
            [ testProperty
                "IdentityT (Either Integer) Integer"
                ( ioProperty . \(v1 :: Either Integer Integer, v2) ->
                    concreteOrdOkProp (IdentityT v1, IdentityT v2)
                ),
              testCase "IdentityT (Either SymBool) SymBool" $ do
                ( IdentityT $ Left $ ssymBool "a" ::
                    IdentityT (Either SymBool) SymBool
                  )
                  .<= IdentityT (Left $ ssymBool "b")
                  @?= (ssymBool "a" .<= ssymBool "b" :: SymBool)
                ( IdentityT $ Left $ ssymBool "a" ::
                    IdentityT (Either SymBool) SymBool
                  )
                  .< IdentityT (Left $ ssymBool "b")
                  @?= (ssymBool "a" .< ssymBool "b" :: SymBool)
                ( IdentityT $ Left $ ssymBool "a" ::
                    IdentityT (Either SymBool) SymBool
                  )
                  .>= IdentityT (Left $ ssymBool "b")
                  @?= (ssymBool "a" .>= ssymBool "b" :: SymBool)
                ( IdentityT $ Left $ ssymBool "a" ::
                    IdentityT (Either SymBool) SymBool
                  )
                  .> IdentityT (Left $ ssymBool "b")
                  @?= (ssymBool "a" .> ssymBool "b" :: SymBool)
                ( IdentityT $ Left $ ssymBool "a" ::
                    IdentityT (Either SymBool) SymBool
                  )
                  `symCompare` IdentityT (Left $ ssymBool "b")
                  @?= (ssymBool "a" `symCompare` ssymBool "b")

                ( IdentityT $ Left $ ssymBool "a" ::
                    IdentityT (Either SymBool) SymBool
                  )
                  .<= IdentityT (Right $ ssymBool "b")
                  @?= conBool True
                ( IdentityT $ Left $ ssymBool "a" ::
                    IdentityT (Either SymBool) SymBool
                  )
                  .< IdentityT (Right $ ssymBool "b")
                  @?= conBool True
                ( IdentityT $ Left $ ssymBool "a" ::
                    IdentityT (Either SymBool) SymBool
                  )
                  .>= IdentityT (Right $ ssymBool "b")
                  @?= conBool False
                ( IdentityT $ Left $ ssymBool "a" ::
                    IdentityT (Either SymBool) SymBool
                  )
                  .> IdentityT (Right $ ssymBool "b")
                  @?= conBool False
                ( IdentityT $ Left $ ssymBool "a" ::
                    IdentityT (Either SymBool) SymBool
                  )
                  `symCompare` IdentityT (Right $ ssymBool "b")
                  @?= (mrgSingle LT :: UnionM Ordering)

                ( IdentityT $ Right $ ssymBool "a" ::
                    IdentityT (Either SymBool) SymBool
                  )
                  .<= IdentityT (Left $ ssymBool "b")
                  @?= conBool False
                ( IdentityT $ Right $ ssymBool "a" ::
                    IdentityT (Either SymBool) SymBool
                  )
                  .< IdentityT (Left $ ssymBool "b")
                  @?= conBool False
                ( IdentityT $ Right $ ssymBool "a" ::
                    IdentityT (Either SymBool) SymBool
                  )
                  .>= IdentityT (Left $ ssymBool "b")
                  @?= conBool True
                ( IdentityT $ Right $ ssymBool "a" ::
                    IdentityT (Either SymBool) SymBool
                  )
                  .> IdentityT (Left $ ssymBool "b")
                  @?= conBool True
                ( IdentityT $ Right $ ssymBool "a" ::
                    IdentityT (Either SymBool) SymBool
                  )
                  `symCompare` IdentityT (Left $ ssymBool "b")
                  @?= (mrgSingle GT :: UnionM Ordering)

                ( IdentityT $ Right $ ssymBool "a" ::
                    IdentityT (Either SymBool) SymBool
                  )
                  .<= IdentityT (Right $ ssymBool "b")
                  @?= (ssymBool "a" .<= ssymBool "b" :: SymBool)
                ( IdentityT $ Right $ ssymBool "a" ::
                    IdentityT (Either SymBool) SymBool
                  )
                  .< IdentityT (Right $ ssymBool "b")
                  @?= (ssymBool "a" .< ssymBool "b" :: SymBool)
                ( IdentityT $ Right $ ssymBool "a" ::
                    IdentityT (Either SymBool) SymBool
                  )
                  .>= IdentityT (Right $ ssymBool "b")
                  @?= (ssymBool "a" .>= ssymBool "b" :: SymBool)
                ( IdentityT $ Right $ ssymBool "a" ::
                    IdentityT (Either SymBool) SymBool
                  )
                  .> IdentityT (Right $ ssymBool "b")
                  @?= (ssymBool "a" .> ssymBool "b" :: SymBool)
                ( IdentityT $ Right $ ssymBool "a" ::
                    IdentityT (Either SymBool) SymBool
                  )
                  `symCompare` IdentityT (Right $ ssymBool "b")
                  @?= (ssymBool "a" `symCompare` ssymBool "b")
            ],
          testCase "ByteString" $ do
            let bytestrings :: [B.ByteString] =
                  ["", "a", "b", "ab", "ba", "aa", "bb"]
            traverse_
              concreteOrdOkProp
              [(x, y) | x <- bytestrings, y <- bytestrings]
        ],
      testCase "symMax" $ do
        symMax (1 :: SymInteger) 2 @?= 2
        let [a, b] = ["a", "b"] :: [SymInteger]
        symMax a b .@?= symIte (a .>= b) a b,
      testCase "symMin" $ do
        symMin (1 :: SymInteger) 2 @?= 1
        let [a, b] = ["a", "b"] :: [SymInteger]
        symMin a b .@?= symIte (a .>= b) b a,
      testCase "mrgMax" $ do
        mrgMax [1] [0, 3] @?= (mrgReturn [1] :: UnionM [SymInteger])
        let [a, b, c] = ["a", "b", "c"] :: [SymInteger]
        (mrgMax [a] [b, c] :: UnionM [SymInteger])
          .@?= (mrgIf (a .<= b) (return [b, c]) (return [a])),
      testCase "mrgMin" $ do
        mrgMin [1] [0, 3] @?= (mrgReturn [0, 3] :: UnionM [SymInteger])
        let [a, b, c] = ["a", "b", "c"] :: [SymInteger]
        (mrgMin [a] [b, c] :: UnionM [SymInteger])
          .@?= (mrgIf (b .< a) (return [b, c]) (return [a]))
    ]
