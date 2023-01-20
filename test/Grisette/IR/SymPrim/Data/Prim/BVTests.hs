{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Grisette.IR.SymPrim.Data.Prim.BVTests where

import Data.Proxy
import Grisette.IR.SymPrim.Data.BV
import Grisette.IR.SymPrim.Data.Prim.InternedTerm.InternedCtors
import Grisette.IR.SymPrim.Data.Prim.InternedTerm.Term
import Grisette.IR.SymPrim.Data.Prim.PartialEval.BV
import Test.Tasty
import Test.Tasty.HUnit

bvTests :: TestTree
bvTests =
  testGroup
    "BVTests"
    [ testGroup
        "pevalBVSelectTerm"
        [ testCase "On concrete" $ do
            pevalBVSelectTerm
              (Proxy @0)
              (Proxy @1)
              (conTerm 6 :: Term (WordN 4))
              @=? conTerm 0
            pevalBVSelectTerm
              (Proxy @1)
              (Proxy @1)
              (conTerm 6 :: Term (WordN 4))
              @=? conTerm 1
            pevalBVSelectTerm
              (Proxy @2)
              (Proxy @1)
              (conTerm 6 :: Term (WordN 4))
              @=? conTerm 1
            pevalBVSelectTerm
              (Proxy @3)
              (Proxy @1)
              (conTerm 6 :: Term (WordN 4))
              @=? conTerm 0
            pevalBVSelectTerm
              (Proxy @0)
              (Proxy @2)
              (conTerm 6 :: Term (WordN 4))
              @=? conTerm 2
            pevalBVSelectTerm
              (Proxy @1)
              (Proxy @2)
              (conTerm 6 :: Term (WordN 4))
              @=? conTerm 3
            pevalBVSelectTerm
              (Proxy @2)
              (Proxy @2)
              (conTerm 6 :: Term (WordN 4))
              @=? conTerm 1
            pevalBVSelectTerm
              (Proxy @0)
              (Proxy @3)
              (conTerm 6 :: Term (WordN 4))
              @=? conTerm 6
            pevalBVSelectTerm
              (Proxy @1)
              (Proxy @3)
              (conTerm 6 :: Term (WordN 4))
              @=? conTerm 3
            pevalBVSelectTerm
              (Proxy @0)
              (Proxy @4)
              (conTerm 6 :: Term (WordN 4))
              @=? conTerm 6,
          testCase "On symbolic" $ do
            pevalBVSelectTerm
              (Proxy @2)
              (Proxy @1)
              (ssymTerm "a" :: Term (WordN 4))
              @=? bvselectTerm (Proxy @2) (Proxy @1) (ssymTerm "a" :: Term (WordN 4))
        ],
      testGroup
        "Extension"
        [ testCase "On concrete" $ do
            pevalBVExtendTerm True (Proxy @6) (conTerm 15 :: Term (WordN 4))
              @=? (conTerm 63 :: Term (WordN 6))
            pevalBVExtendTerm False (Proxy @6) (conTerm 15 :: Term (WordN 4))
              @=? (conTerm 15 :: Term (WordN 6))
            pevalBVExtendTerm True (Proxy @6) (conTerm 15 :: Term (IntN 4))
              @=? (conTerm 63 :: Term (IntN 6))
            pevalBVExtendTerm False (Proxy @6) (conTerm 15 :: Term (IntN 4))
              @=? (conTerm 15 :: Term (IntN 6)),
          testCase "On symbolic" $ do
            pevalBVExtendTerm True (Proxy @6) (ssymTerm "a" :: Term (WordN 4))
              @=? bvextendTerm True (Proxy @6) (ssymTerm "a" :: Term (WordN 4))
            pevalBVExtendTerm False (Proxy @6) (ssymTerm "a" :: Term (WordN 4))
              @=? bvextendTerm False (Proxy @6) (ssymTerm "a" :: Term (WordN 4))
        ],
      testGroup
        "Concat"
        [ testCase "On concrete" $ do
            pevalBVConcatTerm (conTerm 3 :: Term (WordN 4)) (conTerm 5 :: Term (WordN 3))
              @=? conTerm 29
            pevalBVConcatTerm (conTerm 3 :: Term (IntN 4)) (conTerm 5 :: Term (IntN 3))
              @=? conTerm 29,
          testCase "On symbolic" $ do
            pevalBVConcatTerm (ssymTerm "a" :: Term (WordN 4)) (ssymTerm "b" :: Term (WordN 3))
              @=? bvconcatTerm
                (ssymTerm "a" :: Term (WordN 4))
                (ssymTerm "b" :: Term (WordN 3))
        ]
    ]
