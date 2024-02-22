{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Grisette.IR.SymPrim.Data.Prim.BVTests (bvTests) where

import Data.Proxy (Proxy (Proxy))
import Data.Typeable (Typeable)
import GHC.TypeNats (KnownNat, type (+), type (<=))
import Grisette.Core.Data.BV (IntN, WordN)
import Grisette.Core.Data.Class.BitVector (SizedBV)
import Grisette.IR.SymPrim.Data.Prim.InternedTerm.InternedCtors
  ( bvconcatTerm,
    bvextendTerm,
    bvselectTerm,
    conTerm,
    ssymTerm,
    toSignedTerm,
    toUnsignedTerm,
  )
import Grisette.IR.SymPrim.Data.Prim.InternedTerm.Term (SupportedPrim, Term)
import Grisette.IR.SymPrim.Data.Prim.PartialEval.BV
  ( pevalBVConcatTerm,
    pevalBVExtendTerm,
    pevalBVSelectTerm,
    pevalToSignedTerm,
    pevalToUnsignedTerm,
  )
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit ((@=?), (@?=))

data ToSignedTest = ToSignedTest
  { toSignedTestName :: String,
    toSignedTestTerm :: Term (WordN 4),
    toSignedTestExpected :: Term (IntN 4)
  }

data ToUnsignedTest = ToUnsignedTest
  { toUnsignedTestName :: String,
    toUnsignedTestTerm :: Term (IntN 4),
    toUnsignedTestExpected :: Term (WordN 4)
  }

data BVSelectTest where
  BVSelectTest ::
    forall ix w n bv.
    ( KnownNat ix,
      1 <= n,
      KnownNat w,
      1 <= w,
      KnownNat n,
      ix + w <= n,
      forall n. (KnownNat n, 1 <= n) => SupportedPrim (bv n),
      Typeable bv,
      SizedBV bv
    ) =>
    { bvSelectTestName :: String,
      bvSelectIx :: Proxy ix,
      bvSelectW :: Proxy w,
      bvSelectTestTerm :: Term (bv n),
      bvSelectTestExpected :: Term (bv w)
    } ->
    BVSelectTest

bvTests :: Test
bvTests =
  testGroup
    "BV"
    [ testGroup "pevalToSignedTerm" $ do
        ToSignedTest name term expected <-
          [ ToSignedTest
              { toSignedTestName = "concrete",
                toSignedTestTerm = conTerm 6,
                toSignedTestExpected = conTerm 6
              },
            ToSignedTest
              { toSignedTestName = "symbolic",
                toSignedTestTerm = ssymTerm "a",
                toSignedTestExpected = toSignedTerm $ ssymTerm "a"
              },
            ToSignedTest
              { toSignedTestName = "toUnsigned",
                toSignedTestTerm = toUnsignedTerm $ ssymTerm "a",
                toSignedTestExpected = ssymTerm "a"
              },
            ToSignedTest
              { toSignedTestName = "bvConcat",
                toSignedTestTerm =
                  bvconcatTerm
                    (ssymTerm "a" :: Term (WordN 2))
                    (ssymTerm "b" :: Term (WordN 2)),
                toSignedTestExpected =
                  bvconcatTerm
                    (toSignedTerm (ssymTerm "a" :: Term (WordN 2)))
                    (toSignedTerm (ssymTerm "b" :: Term (WordN 2)))
              },
            ToSignedTest
              { toSignedTestName = "bvExtend",
                toSignedTestTerm =
                  bvextendTerm True (Proxy @4) (ssymTerm "a" :: Term (WordN 2)),
                toSignedTestExpected =
                  bvextendTerm
                    True
                    (Proxy @4)
                    (toSignedTerm (ssymTerm "a" :: Term (WordN 2)))
              }
            ]
        return $ testCase name $ do
          let actual = pevalToSignedTerm term
          actual @?= expected,
      testGroup "pevalToUnsignedTerm" $ do
        ToUnsignedTest name term expected <-
          [ ToUnsignedTest
              { toUnsignedTestName = "concrete",
                toUnsignedTestTerm = conTerm 6,
                toUnsignedTestExpected = conTerm 6
              },
            ToUnsignedTest
              { toUnsignedTestName = "symbolic",
                toUnsignedTestTerm = ssymTerm "a",
                toUnsignedTestExpected = toUnsignedTerm $ ssymTerm "a"
              },
            ToUnsignedTest
              { toUnsignedTestName = "toSigned",
                toUnsignedTestTerm = toSignedTerm $ ssymTerm "a",
                toUnsignedTestExpected = ssymTerm "a"
              },
            ToUnsignedTest
              { toUnsignedTestName = "bvConcat",
                toUnsignedTestTerm =
                  bvconcatTerm
                    (ssymTerm "a" :: Term (IntN 2))
                    (ssymTerm "b" :: Term (IntN 2)),
                toUnsignedTestExpected =
                  bvconcatTerm
                    (toUnsignedTerm (ssymTerm "a" :: Term (IntN 2)))
                    (toUnsignedTerm (ssymTerm "b" :: Term (IntN 2)))
              },
            ToUnsignedTest
              { toUnsignedTestName = "bvExtend",
                toUnsignedTestTerm =
                  bvextendTerm True (Proxy @4) (ssymTerm "a" :: Term (IntN 2)),
                toUnsignedTestExpected =
                  bvextendTerm
                    True
                    (Proxy @4)
                    (toUnsignedTerm (ssymTerm "a" :: Term (IntN 2)))
              }
            ]
        return $ testCase name $ do
          let actual = pevalToUnsignedTerm term
          actual @?= expected,
      testGroup "pevalBVSelectTerm" $ do
        BVSelectTest name ix w term expected <-
          [ BVSelectTest
              { bvSelectTestName = "concrete 0 1",
                bvSelectIx = Proxy @0,
                bvSelectW = Proxy @1,
                bvSelectTestTerm = conTerm 6 :: Term (WordN 4),
                bvSelectTestExpected = conTerm 0
              },
            BVSelectTest
              { bvSelectTestName = "concrete 1 1",
                bvSelectIx = Proxy @1,
                bvSelectW = Proxy @1,
                bvSelectTestTerm = conTerm 6 :: Term (WordN 4),
                bvSelectTestExpected = conTerm 1
              },
            BVSelectTest
              { bvSelectTestName = "concrete 2 1",
                bvSelectIx = Proxy @2,
                bvSelectW = Proxy @1,
                bvSelectTestTerm = conTerm 6 :: Term (WordN 4),
                bvSelectTestExpected = conTerm 1
              },
            BVSelectTest
              { bvSelectTestName = "concrete 3 1",
                bvSelectIx = Proxy @3,
                bvSelectW = Proxy @1,
                bvSelectTestTerm = conTerm 6 :: Term (WordN 4),
                bvSelectTestExpected = conTerm 0
              },
            BVSelectTest
              { bvSelectTestName = "concrete 0 2",
                bvSelectIx = Proxy @0,
                bvSelectW = Proxy @2,
                bvSelectTestTerm = conTerm 6 :: Term (WordN 4),
                bvSelectTestExpected = conTerm 2
              },
            BVSelectTest
              { bvSelectTestName = "concrete 1 2",
                bvSelectIx = Proxy @1,
                bvSelectW = Proxy @2,
                bvSelectTestTerm = conTerm 6 :: Term (WordN 4),
                bvSelectTestExpected = conTerm 3
              },
            BVSelectTest
              { bvSelectTestName = "concrete 0 4",
                bvSelectIx = Proxy @0,
                bvSelectW = Proxy @4,
                bvSelectTestTerm = conTerm 6 :: Term (WordN 4),
                bvSelectTestExpected = conTerm 6
              },
            BVSelectTest
              { bvSelectTestName = "symbolic",
                bvSelectIx = Proxy @2,
                bvSelectW = Proxy @1,
                bvSelectTestTerm = ssymTerm "a" :: Term (WordN 4),
                bvSelectTestExpected =
                  bvselectTerm
                    (Proxy @2)
                    (Proxy @1)
                    (ssymTerm "a" :: Term (WordN 4))
              },
            BVSelectTest
              { bvSelectTestName = "On ToSigned",
                bvSelectIx = Proxy @2,
                bvSelectW = Proxy @1,
                bvSelectTestTerm =
                  toSignedTerm (ssymTerm "a" :: Term (WordN 4)),
                bvSelectTestExpected =
                  toSignedTerm
                    ( bvselectTerm
                        (Proxy @2)
                        (Proxy @1)
                        (ssymTerm "a" :: Term (WordN 4))
                    )
              },
            BVSelectTest
              { bvSelectTestName = "On ToUnsigned",
                bvSelectIx = Proxy @2,
                bvSelectW = Proxy @1,
                bvSelectTestTerm =
                  toUnsignedTerm (ssymTerm "a" :: Term (IntN 4)),
                bvSelectTestExpected =
                  toUnsignedTerm
                    ( bvselectTerm
                        (Proxy @2)
                        (Proxy @1)
                        (ssymTerm "a" :: Term (IntN 4))
                    )
              },
            BVSelectTest
              { bvSelectTestName = "On BVSelect",
                bvSelectIx = Proxy @3,
                bvSelectW = Proxy @2,
                bvSelectTestTerm =
                  bvselectTerm
                    (Proxy @2)
                    (Proxy @6)
                    (ssymTerm "a" :: Term (WordN 16)),
                bvSelectTestExpected =
                  bvselectTerm
                    (Proxy @5)
                    (Proxy @2)
                    (ssymTerm "a" :: Term (WordN 16))
              },
            BVSelectTest
              { bvSelectTestName = "Whole vector",
                bvSelectIx = Proxy @0,
                bvSelectW = Proxy @4,
                bvSelectTestTerm = ssymTerm "a" :: Term (WordN 4),
                bvSelectTestExpected = ssymTerm "a"
              },
            BVSelectTest
              { bvSelectTestName = "bvConcat only lower part",
                bvSelectIx = Proxy @1,
                bvSelectW = Proxy @2,
                bvSelectTestTerm =
                  bvconcatTerm
                    (ssymTerm "a" :: Term (WordN 4))
                    (ssymTerm "b" :: Term (WordN 4)),
                bvSelectTestExpected =
                  bvselectTerm
                    (Proxy @1)
                    (Proxy @2)
                    (ssymTerm "b" :: Term (WordN 4))
              },
            BVSelectTest
              { bvSelectTestName = "bvConcat whole lower part",
                bvSelectIx = Proxy @0,
                bvSelectW = Proxy @4,
                bvSelectTestTerm =
                  bvconcatTerm
                    (ssymTerm "a" :: Term (WordN 4))
                    (ssymTerm "b" :: Term (WordN 4)),
                bvSelectTestExpected = ssymTerm "b" :: Term (WordN 4)
              },
            BVSelectTest
              { bvSelectTestName = "bvConcat only higher part",
                bvSelectIx = Proxy @5,
                bvSelectW = Proxy @2,
                bvSelectTestTerm =
                  bvconcatTerm
                    (ssymTerm "a" :: Term (WordN 4))
                    (ssymTerm "b" :: Term (WordN 4)),
                bvSelectTestExpected =
                  bvselectTerm
                    (Proxy @1)
                    (Proxy @2)
                    (ssymTerm "a" :: Term (WordN 4))
              },
            BVSelectTest
              { bvSelectTestName = "bvConcat whole higher part",
                bvSelectIx = Proxy @4,
                bvSelectW = Proxy @4,
                bvSelectTestTerm =
                  bvconcatTerm
                    (ssymTerm "a" :: Term (WordN 4))
                    (ssymTerm "b" :: Term (WordN 4)),
                bvSelectTestExpected = ssymTerm "a" :: Term (WordN 4)
              },
            BVSelectTest
              { bvSelectTestName = "bvConcat cross border",
                bvSelectIx = Proxy @3,
                bvSelectW = Proxy @4,
                bvSelectTestTerm =
                  bvconcatTerm
                    (ssymTerm "a" :: Term (WordN 4))
                    (ssymTerm "b" :: Term (WordN 4)),
                bvSelectTestExpected =
                  bvconcatTerm
                    ( bvselectTerm
                        (Proxy @0)
                        (Proxy @3)
                        (ssymTerm "a" :: Term (WordN 4))
                    )
                    ( bvselectTerm
                        (Proxy @3)
                        (Proxy @1)
                        (ssymTerm "b" :: Term (WordN 4))
                    )
              },
            BVSelectTest
              { bvSelectTestName = "bvExtend only lower part",
                bvSelectIx = Proxy @1,
                bvSelectW = Proxy @2,
                bvSelectTestTerm =
                  bvextendTerm True (Proxy @8) (ssymTerm "a" :: Term (WordN 4)),
                bvSelectTestExpected =
                  bvselectTerm
                    (Proxy @1)
                    (Proxy @2)
                    (ssymTerm "a" :: Term (WordN 4))
              },
            BVSelectTest
              { bvSelectTestName = "bvExtend whole lower part",
                bvSelectIx = Proxy @0,
                bvSelectW = Proxy @4,
                bvSelectTestTerm =
                  bvextendTerm True (Proxy @8) (ssymTerm "a" :: Term (WordN 4)),
                bvSelectTestExpected = ssymTerm "a" :: Term (WordN 4)
              },
            BVSelectTest
              { bvSelectTestName = "bvExtend cross boarder",
                bvSelectIx = Proxy @3,
                bvSelectW = Proxy @4,
                bvSelectTestTerm =
                  bvextendTerm True (Proxy @8) (ssymTerm "a" :: Term (WordN 4)),
                bvSelectTestExpected =
                  bvextendTerm True (Proxy @4) $
                    bvselectTerm
                      (Proxy @3)
                      (Proxy @1)
                      (ssymTerm "a" :: Term (WordN 4))
              }
            ]
        return . testCase name $
          pevalBVSelectTerm ix w term @?= expected,
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
