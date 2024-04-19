{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Grisette.SymPrim.Prim.BVTests (bvTests) where

import Data.Proxy (Proxy (Proxy))
import GHC.TypeNats (KnownNat, type (+), type (<=))
import Grisette.Core.Data.BV (IntN, WordN)
import Grisette.SymPrim.Prim.Term
  ( PEvalBVSignConversionTerm
      ( pevalBVToSignedTerm,
        pevalBVToUnsignedTerm
      ),
    PEvalBVTerm
      ( pevalBVConcatTerm,
        pevalBVExtendTerm,
        pevalBVSelectTerm
      ),
    Term,
    bvconcatTerm,
    bvextendTerm,
    bvselectTerm,
    conTerm,
    ssymTerm,
    toSignedTerm,
    toUnsignedTerm,
  )
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit ((@?=))

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
    ( PEvalBVTerm bv,
      KnownNat ix,
      1 <= n,
      KnownNat w,
      1 <= w,
      KnownNat n,
      ix + w <= n
    ) =>
    { bvSelectTestName :: String,
      bvSelectIx :: Proxy ix,
      bvSelectW :: Proxy w,
      bvSelectTestTerm :: Term (bv n),
      bvSelectTestExpected :: Term (bv w)
    } ->
    BVSelectTest

data BVExtendTest where
  BVExtendTest ::
    forall l r bv.
    ( PEvalBVTerm bv,
      KnownNat l,
      1 <= l,
      KnownNat r,
      1 <= r,
      l <= r
    ) =>
    { bvExtendTestName :: String,
      bvExtendSigned :: Bool,
      bvExtendR :: Proxy r,
      bvExtendTestTerm :: Term (bv l),
      bvExtendExpected :: Term (bv r)
    } ->
    BVExtendTest

data BVConcatTest where
  BVConcatTest ::
    forall l r bv.
    ( PEvalBVTerm bv,
      KnownNat l,
      KnownNat r,
      KnownNat (l + r),
      1 <= l,
      1 <= r,
      1 <= l + r
    ) =>
    { bvConcatTestName :: String,
      bvConcatTestLhs :: Term (bv l),
      bvConcatTestRhs :: Term (bv r),
      bvConcatTestExpected :: Term (bv (l + r))
    } ->
    BVConcatTest

bvTests :: Test
bvTests =
  testGroup
    "BV"
    [ testGroup "pevalBVToSignedTerm" $ do
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
          let actual = pevalBVToSignedTerm term
          actual @?= expected,
      testGroup "pevalBVToUnsignedTerm" $ do
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
          let actual = pevalBVToUnsignedTerm term
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
      testGroup "pevalBVExtendTerm" $ do
        BVExtendTest name signed pr term expected <-
          [ BVExtendTest
              { bvExtendTestName = "Concrete zext on negative",
                bvExtendSigned = False,
                bvExtendR = Proxy @6,
                bvExtendTestTerm = conTerm 15 :: Term (WordN 4),
                bvExtendExpected = conTerm 15 :: Term (WordN 6)
              },
            BVExtendTest
              { bvExtendTestName = "Concrete sext on negative",
                bvExtendSigned = True,
                bvExtendR = Proxy @6,
                bvExtendTestTerm = conTerm 15 :: Term (WordN 4),
                bvExtendExpected = conTerm 63 :: Term (WordN 6)
              },
            BVExtendTest
              { bvExtendTestName = "Concrete zext on positive",
                bvExtendSigned = False,
                bvExtendR = Proxy @6,
                bvExtendTestTerm = conTerm 7 :: Term (WordN 4),
                bvExtendExpected = conTerm 7 :: Term (WordN 6)
              },
            BVExtendTest
              { bvExtendTestName = "Concrete sext on positive",
                bvExtendSigned = True,
                bvExtendR = Proxy @6,
                bvExtendTestTerm = conTerm 7 :: Term (WordN 4),
                bvExtendExpected = conTerm 7 :: Term (WordN 6)
              },
            BVExtendTest
              { bvExtendTestName = "Same width",
                bvExtendSigned = False,
                bvExtendR = Proxy @4,
                bvExtendTestTerm = ssymTerm "a" :: Term (WordN 4),
                bvExtendExpected = ssymTerm "a" :: Term (WordN 4)
              },
            BVExtendTest
              { bvExtendTestName = "Symbolic zext",
                bvExtendSigned = False,
                bvExtendR = Proxy @6,
                bvExtendTestTerm = ssymTerm "a" :: Term (WordN 4),
                bvExtendExpected =
                  bvconcatTerm
                    (conTerm 0 :: Term (WordN 2))
                    (ssymTerm "a" :: Term (WordN 4))
              },
            BVExtendTest
              { bvExtendTestName = "Symbolic sext on sext",
                bvExtendSigned = True,
                bvExtendR = Proxy @6,
                bvExtendTestTerm =
                  pevalBVExtendTerm
                    True
                    (Proxy @4)
                    (ssymTerm "a" :: Term (WordN 2)),
                bvExtendExpected =
                  bvextendTerm True (Proxy @6) (ssymTerm "a" :: Term (WordN 2))
              }
            ]
        return . testCase name $
          pevalBVExtendTerm signed pr term @?= expected,
      testGroup "pevalBVConcatTerm" $ do
        BVConcatTest name lhs rhs expected <-
          [ BVConcatTest
              { bvConcatTestName = "[c1 c2] -> c1c2",
                bvConcatTestLhs = conTerm 3 :: Term (WordN 4),
                bvConcatTestRhs = conTerm 5 :: Term (WordN 3),
                bvConcatTestExpected = conTerm 29
              },
            BVConcatTest
              { bvConcatTestName = "[c1 (c2 s)] -> (c1c2 s)",
                bvConcatTestLhs = conTerm 3 :: Term (WordN 4),
                bvConcatTestRhs =
                  bvconcatTerm
                    (conTerm 5 :: Term (WordN 3))
                    (ssymTerm "b" :: Term (WordN 3)),
                bvConcatTestExpected =
                  bvconcatTerm
                    (conTerm 29 :: Term (WordN 7))
                    ( ssymTerm "b" :: Term (WordN 3)
                    )
              },
            BVConcatTest
              { bvConcatTestName = "[c1 (s c2)] -> (c1 (s c2))",
                bvConcatTestLhs = conTerm 3 :: Term (WordN 4),
                bvConcatTestRhs =
                  bvconcatTerm
                    (ssymTerm "b" :: Term (WordN 3))
                    (conTerm 5 :: Term (WordN 3)),
                bvConcatTestExpected =
                  bvconcatTerm
                    (conTerm 3 :: Term (WordN 4))
                    ( bvconcatTerm
                        (ssymTerm "b" :: Term (WordN 3))
                        (conTerm 5 :: Term (WordN 3))
                    )
              },
            BVConcatTest
              { bvConcatTestName = "[c1 (c2 (s c3))] -> (c1c2 (s c3))",
                bvConcatTestLhs = conTerm 3 :: Term (WordN 4),
                bvConcatTestRhs =
                  bvconcatTerm (conTerm 5 :: Term (WordN 5)) $
                    bvconcatTerm
                      (ssymTerm "b" :: Term (WordN 6))
                      (conTerm 7 :: Term (WordN 7)),
                bvConcatTestExpected =
                  bvconcatTerm
                    (conTerm 101 :: Term (WordN 9))
                    ( bvconcatTerm
                        (ssymTerm "b" :: Term (WordN 6))
                        (conTerm 7 :: Term (WordN 7))
                    )
              },
            BVConcatTest
              { bvConcatTestName = "[c s] -> (c s)",
                bvConcatTestLhs = conTerm 3 :: Term (WordN 4),
                bvConcatTestRhs = ssymTerm "b" :: Term (WordN 3),
                bvConcatTestExpected =
                  bvconcatTerm
                    (conTerm 3 :: Term (WordN 4))
                    (ssymTerm "b" :: Term (WordN 3))
              },
            BVConcatTest
              { bvConcatTestName = "[(c1 s) c2] -> (c1 (s c2))",
                bvConcatTestLhs =
                  bvconcatTerm
                    (conTerm 3 :: Term (WordN 4))
                    (ssymTerm "a" :: Term (WordN 4)),
                bvConcatTestRhs = conTerm 5 :: Term (WordN 3),
                bvConcatTestExpected =
                  bvconcatTerm
                    (conTerm 3 :: Term (WordN 4))
                    ( bvconcatTerm
                        (ssymTerm "a" :: Term (WordN 4))
                        ( conTerm 5 :: Term (WordN 3)
                        )
                    )
              },
            BVConcatTest
              { bvConcatTestName = "[(c1 s1) (c2 s2)] -> (c1 (s1 (c2 s2)))",
                bvConcatTestLhs =
                  bvconcatTerm
                    (conTerm 3 :: Term (WordN 4))
                    (ssymTerm "a" :: Term (WordN 4)),
                bvConcatTestRhs =
                  bvconcatTerm
                    (conTerm 5 :: Term (WordN 4))
                    (ssymTerm "b" :: Term (WordN 4)),
                bvConcatTestExpected =
                  bvconcatTerm
                    (conTerm 3 :: Term (WordN 4))
                    ( bvconcatTerm
                        (ssymTerm "a" :: Term (WordN 4))
                        ( bvconcatTerm
                            (conTerm 5 :: Term (WordN 4))
                            (ssymTerm "b" :: Term (WordN 4))
                        )
                    )
              },
            BVConcatTest
              { bvConcatTestName = "[(c1 s1) (s2 c2)] -> (c1 ((s1 s2) c2))",
                bvConcatTestLhs =
                  bvconcatTerm
                    (conTerm 3 :: Term (WordN 4))
                    (ssymTerm "a" :: Term (WordN 4)),
                bvConcatTestRhs =
                  bvconcatTerm
                    (ssymTerm "b" :: Term (WordN 4))
                    (conTerm 5 :: Term (WordN 4)),
                bvConcatTestExpected =
                  bvconcatTerm
                    (conTerm 3 :: Term (WordN 4))
                    ( bvconcatTerm
                        ( bvconcatTerm
                            (ssymTerm "a" :: Term (WordN 4))
                            (ssymTerm "b" :: Term (WordN 4))
                        )
                        (conTerm 5 :: Term (WordN 4))
                    )
              },
            BVConcatTest
              { bvConcatTestName =
                  "[(c1 s1) (c2 (s2 c3))] -> (c1 (((s1 c2) s2)) c3))",
                bvConcatTestLhs =
                  bvconcatTerm
                    (conTerm 3 :: Term (WordN 4))
                    (ssymTerm "a" :: Term (WordN 4)),
                bvConcatTestRhs =
                  bvconcatTerm
                    (conTerm 5 :: Term (WordN 4))
                    ( bvconcatTerm
                        (ssymTerm "b" :: Term (WordN 4))
                        (conTerm 7 :: Term (WordN 4))
                    ),
                bvConcatTestExpected =
                  bvconcatTerm
                    (conTerm 3 :: Term (WordN 4))
                    ( bvconcatTerm
                        ( bvconcatTerm
                            ( bvconcatTerm
                                (ssymTerm "a" :: Term (WordN 4))
                                (conTerm 5 :: Term (WordN 4))
                            )
                            (ssymTerm "b" :: Term (WordN 4))
                        )
                        (conTerm 7 :: Term (WordN 4))
                    )
              },
            BVConcatTest
              { bvConcatTestName = "[(c s1) s2] -> (c (s1 s2))",
                bvConcatTestLhs =
                  bvconcatTerm
                    (conTerm 3 :: Term (WordN 4))
                    (ssymTerm "a" :: Term (WordN 4)),
                bvConcatTestRhs = ssymTerm "b" :: Term (WordN 3),
                bvConcatTestExpected =
                  bvconcatTerm
                    (conTerm 3 :: Term (WordN 4))
                    ( bvconcatTerm
                        (ssymTerm "a" :: Term (WordN 4))
                        (ssymTerm "b" :: Term (WordN 3))
                    )
              },
            BVConcatTest
              { bvConcatTestName = "[(s c1) c2] -> (s c1c2)",
                bvConcatTestLhs =
                  bvconcatTerm
                    (ssymTerm "a" :: Term (WordN 4))
                    (conTerm 5 :: Term (WordN 3)),
                bvConcatTestRhs = conTerm 3 :: Term (WordN 4),
                bvConcatTestExpected =
                  bvconcatTerm
                    (ssymTerm "a" :: Term (WordN 4))
                    (conTerm 83 :: Term (WordN 7))
              },
            BVConcatTest
              { bvConcatTestName = "[(s1 c1) (c2 s2)] -> (s1 (c1c2 s2))",
                bvConcatTestLhs =
                  bvconcatTerm
                    (ssymTerm "a" :: Term (WordN 4))
                    (conTerm 5 :: Term (WordN 4)),
                bvConcatTestRhs =
                  bvconcatTerm
                    (conTerm 3 :: Term (WordN 4))
                    (ssymTerm "b" :: Term (WordN 4)),
                bvConcatTestExpected =
                  bvconcatTerm
                    (ssymTerm "a" :: Term (WordN 4))
                    ( bvconcatTerm
                        (conTerm 83 :: Term (WordN 8))
                        (ssymTerm "b" :: Term (WordN 4))
                    )
              },
            BVConcatTest
              { bvConcatTestName = "[(s1 c1) (s2 c2)] -> (((s1 c1) s2) c2)",
                bvConcatTestLhs =
                  bvconcatTerm
                    (ssymTerm "a" :: Term (WordN 4))
                    (conTerm 5 :: Term (WordN 4)),
                bvConcatTestRhs =
                  bvconcatTerm
                    (ssymTerm "b" :: Term (WordN 4))
                    (conTerm 3 :: Term (WordN 4)),
                bvConcatTestExpected =
                  bvconcatTerm
                    ( bvconcatTerm
                        ( bvconcatTerm
                            (ssymTerm "a" :: Term (WordN 4))
                            (conTerm 5 :: Term (WordN 4))
                        )
                        (ssymTerm "b" :: Term (WordN 4))
                    )
                    (conTerm 3 :: Term (WordN 4))
              },
            BVConcatTest
              { bvConcatTestName =
                  "[(s1 c1) (c2 (s2 c3))] -> (((s1 c1c2) s2) c3)",
                bvConcatTestLhs =
                  bvconcatTerm
                    (ssymTerm "a" :: Term (WordN 4))
                    (conTerm 5 :: Term (WordN 4)),
                bvConcatTestRhs =
                  bvconcatTerm
                    (conTerm 3 :: Term (WordN 4))
                    ( bvconcatTerm
                        (ssymTerm "b" :: Term (WordN 4))
                        (conTerm 7 :: Term (WordN 4))
                    ),
                bvConcatTestExpected =
                  bvconcatTerm
                    ( bvconcatTerm
                        ( bvconcatTerm
                            (ssymTerm "a" :: Term (WordN 4))
                            (conTerm 83 :: Term (WordN 8))
                        )
                        ( ssymTerm "b" :: Term (WordN 4)
                        )
                    )
                    (conTerm 7 :: Term (WordN 4))
              },
            BVConcatTest
              { bvConcatTestName = "[(s1 c1) s2] -> ((s1 c1) s2)",
                bvConcatTestLhs =
                  bvconcatTerm
                    (ssymTerm "a" :: Term (WordN 4))
                    (conTerm 5 :: Term (WordN 4)),
                bvConcatTestRhs = ssymTerm "b" :: Term (WordN 3),
                bvConcatTestExpected =
                  bvconcatTerm
                    ( bvconcatTerm
                        (ssymTerm "a" :: Term (WordN 4))
                        (conTerm 5 :: Term (WordN 4))
                    )
                    (ssymTerm "b" :: Term (WordN 3))
              },
            BVConcatTest
              { bvConcatTestName = "[(c1 (s1 c2)) c3] -> (c1 (s1 c2c3))",
                bvConcatTestLhs =
                  bvconcatTerm
                    (conTerm 3 :: Term (WordN 4))
                    ( bvconcatTerm
                        (ssymTerm "a" :: Term (WordN 4))
                        (conTerm 5 :: Term (WordN 4))
                    ),
                bvConcatTestRhs = conTerm 7 :: Term (WordN 4),
                bvConcatTestExpected =
                  bvconcatTerm
                    (conTerm 3 :: Term (WordN 4))
                    ( bvconcatTerm
                        (ssymTerm "a" :: Term (WordN 4))
                        (conTerm 87 :: Term (WordN 8))
                    )
              },
            BVConcatTest
              { bvConcatTestName =
                  "[(c1 (s1 c2)) (c3 s3)] -> (c1 (s1 (c2c3 s3)))",
                bvConcatTestLhs =
                  bvconcatTerm
                    (conTerm 3 :: Term (WordN 4))
                    ( bvconcatTerm
                        (ssymTerm "a" :: Term (WordN 4))
                        (conTerm 5 :: Term (WordN 4))
                    ),
                bvConcatTestRhs =
                  bvconcatTerm
                    (conTerm 7 :: Term (WordN 4))
                    (ssymTerm "b" :: Term (WordN 4)),
                bvConcatTestExpected =
                  bvconcatTerm
                    (conTerm 3 :: Term (WordN 4))
                    ( bvconcatTerm
                        (ssymTerm "a" :: Term (WordN 4))
                        ( bvconcatTerm
                            (conTerm 87 :: Term (WordN 8))
                            ( ssymTerm "b" :: Term (WordN 4)
                            )
                        )
                    )
              },
            BVConcatTest
              { bvConcatTestName =
                  "[(c1 (s1 c2)) (s2 c3)] -> (c1 (((s1 c2) s2) c3))",
                bvConcatTestLhs =
                  bvconcatTerm
                    (conTerm 3 :: Term (WordN 4))
                    ( bvconcatTerm
                        (ssymTerm "a" :: Term (WordN 4))
                        (conTerm 5 :: Term (WordN 4))
                    ),
                bvConcatTestRhs =
                  bvconcatTerm
                    (ssymTerm "b" :: Term (WordN 4))
                    (conTerm 7 :: Term (WordN 4)),
                bvConcatTestExpected =
                  bvconcatTerm
                    (conTerm 3 :: Term (WordN 4))
                    ( bvconcatTerm
                        ( bvconcatTerm
                            ( bvconcatTerm
                                (ssymTerm "a" :: Term (WordN 4))
                                (conTerm 5 :: Term (WordN 4))
                            )
                            (ssymTerm "b" :: Term (WordN 4))
                        )
                        (conTerm 7 :: Term (WordN 4))
                    )
              },
            BVConcatTest
              { bvConcatTestName =
                  "[(c1 (s1 c2)) (c3 (s2 c4))] -> (c1 (((s1 c2c3) s2) c4))",
                bvConcatTestLhs =
                  bvconcatTerm
                    (conTerm 3 :: Term (WordN 4))
                    ( bvconcatTerm
                        (ssymTerm "a" :: Term (WordN 4))
                        (conTerm 5 :: Term (WordN 4))
                    ),
                bvConcatTestRhs =
                  bvconcatTerm
                    (conTerm 7 :: Term (WordN 4))
                    ( bvconcatTerm
                        (ssymTerm "b" :: Term (WordN 4))
                        (conTerm 9 :: Term (WordN 4))
                    ),
                bvConcatTestExpected =
                  bvconcatTerm
                    (conTerm 3 :: Term (WordN 4))
                    ( bvconcatTerm
                        ( bvconcatTerm
                            ( bvconcatTerm
                                (ssymTerm "a" :: Term (WordN 4))
                                (conTerm 87 :: Term (WordN 8))
                            )
                            (ssymTerm "b" :: Term (WordN 4))
                        )
                        (conTerm 9 :: Term (WordN 4))
                    )
              },
            BVConcatTest
              { bvConcatTestName = "[(c1 (s1 c2)) s2] -> (c1 ((s1 c2) s2))",
                bvConcatTestLhs =
                  bvconcatTerm
                    (conTerm 3 :: Term (WordN 4))
                    ( bvconcatTerm
                        (ssymTerm "a" :: Term (WordN 4))
                        (conTerm 5 :: Term (WordN 4))
                    ),
                bvConcatTestRhs = ssymTerm "b" :: Term (WordN 3),
                bvConcatTestExpected =
                  bvconcatTerm
                    (conTerm 3 :: Term (WordN 4))
                    ( bvconcatTerm
                        ( bvconcatTerm
                            (ssymTerm "a" :: Term (WordN 4))
                            (conTerm 5 :: Term (WordN 4))
                        )
                        ( ssymTerm "b" :: Term (WordN 3)
                        )
                    )
              },
            BVConcatTest
              { bvConcatTestName = "[s c] -> (s c)",
                bvConcatTestLhs = ssymTerm "a" :: Term (WordN 4),
                bvConcatTestRhs = conTerm 5 :: Term (WordN 4),
                bvConcatTestExpected =
                  bvconcatTerm
                    (ssymTerm "a" :: Term (WordN 4))
                    (conTerm 5 :: Term (WordN 4))
              },
            BVConcatTest
              { bvConcatTestName = "[s (c s)] -> (s (c s))",
                bvConcatTestLhs = ssymTerm "a" :: Term (WordN 4),
                bvConcatTestRhs =
                  bvconcatTerm
                    (conTerm 5 :: Term (WordN 4))
                    (ssymTerm "b" :: Term (WordN 4)),
                bvConcatTestExpected =
                  bvconcatTerm
                    (ssymTerm "a" :: Term (WordN 4))
                    ( bvconcatTerm
                        (conTerm 5 :: Term (WordN 4))
                        (ssymTerm "b" :: Term (WordN 4))
                    )
              },
            BVConcatTest
              { bvConcatTestName = "[s (s c)] -> ((s s) c))",
                bvConcatTestLhs = ssymTerm "a" :: Term (WordN 4),
                bvConcatTestRhs =
                  bvconcatTerm
                    (ssymTerm "b" :: Term (WordN 4))
                    (conTerm 5 :: Term (WordN 4)),
                bvConcatTestExpected =
                  bvconcatTerm
                    ( bvconcatTerm
                        (ssymTerm "a" :: Term (WordN 4))
                        (ssymTerm "b" :: Term (WordN 4))
                    )
                    (conTerm 5 :: Term (WordN 4))
              },
            BVConcatTest
              { bvConcatTestName = "[s (c (s c))] -> (((s c) s)) c)",
                bvConcatTestLhs = ssymTerm "a" :: Term (WordN 4),
                bvConcatTestRhs =
                  bvconcatTerm
                    (conTerm 5 :: Term (WordN 4))
                    ( bvconcatTerm
                        (ssymTerm "b" :: Term (WordN 4))
                        (conTerm 7 :: Term (WordN 4))
                    ),
                bvConcatTestExpected =
                  bvconcatTerm
                    ( bvconcatTerm
                        ( bvconcatTerm
                            (ssymTerm "a" :: Term (WordN 4))
                            (conTerm 5 :: Term (WordN 4))
                        )
                        (ssymTerm "b" :: Term (WordN 4))
                    )
                    (conTerm 7 :: Term (WordN 4))
              },
            BVConcatTest
              { bvConcatTestName = "[s1 s2] -> (s1 s2)",
                bvConcatTestLhs = ssymTerm "a" :: Term (WordN 4),
                bvConcatTestRhs = ssymTerm "b" :: Term (WordN 3),
                bvConcatTestExpected =
                  bvconcatTerm
                    (ssymTerm "a" :: Term (WordN 4))
                    (ssymTerm "b" :: Term (WordN 3))
              }
            ]
        return . testCase name $
          pevalBVConcatTerm lhs rhs @?= expected
    ]
