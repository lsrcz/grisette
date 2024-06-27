{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}

module Grisette.Core.Data.Class.GPrettyTests (gprettyTests) where

import Control.Monad.Except (ExceptT (ExceptT))
import Control.Monad.Identity (Identity, IdentityT (IdentityT))
import Control.Monad.Trans.Maybe (MaybeT (MaybeT))
import qualified Control.Monad.Trans.Writer.Lazy as WriterLazy
import qualified Data.HashMap.Lazy as HM
import qualified Data.HashSet as HS
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Text as T (Text, intercalate, pack, unpack)
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.Generics (Generic, Generic1)
import GHC.Stack (HasCallStack)
import Generics.Deriving (Default (Default), Default1 (Default1))
import Grisette
  ( GPretty (gpretty),
    IntN,
    LogicalOp ((.&&)),
    SymBool,
    WordN,
    pattern SomeIntN,
    pattern SomeWordN,
  )
import Grisette.Internal.Core.Data.Class.GPretty
  ( GPretty1,
    GPretty2,
    docToTextWithWidth,
    formatTextWithWidth,
    gprettyPrec1,
    gprettyPrec2,
  )
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.HUnit ((@?=))
import Test.QuickCheck (Arbitrary (arbitrary), Gen, forAll, oneof)

#if MIN_VERSION_prettyprinter(1,7,0)
import Prettyprinter
  ( PageWidth(AvailablePerLine, Unbounded),
    layoutPretty,
    LayoutOptions(LayoutOptions),
  )
import Prettyprinter.Render.Text (renderStrict)
#else
import Data.Text.Prettyprint.Doc
  ( PageWidth(AvailablePerLine, Unbounded),
    layoutPretty,
    LayoutOptions(LayoutOptions),
  )
import Data.Text.Prettyprint.Doc.Render.Text (renderStrict)
#endif

testGPretty :: (HasCallStack, GPretty a) => String -> Int -> a -> T.Text -> Test
testGPretty n i a s = testCase n $ formatTextWithWidth i a @?= s

testGPretty1 ::
  (HasCallStack, GPretty1 f, GPretty a) =>
  String ->
  Int ->
  f a ->
  T.Text ->
  Test
testGPretty1 n i a s = testCase n $ do
  formatTextWithWidth i a @?= s
  docToTextWithWidth i (gprettyPrec1 0 a) @?= s

testGPretty2 ::
  (HasCallStack, GPretty2 f, GPretty a, GPretty b) =>
  String ->
  Int ->
  f a b ->
  T.Text ->
  Test
testGPretty2 n i a s = testCase n $ do
  formatTextWithWidth i a @?= s
  docToTextWithWidth i (gprettyPrec1 0 a) @?= s
  docToTextWithWidth i (gprettyPrec2 0 a) @?= s

propertyGPrettyShow ::
  forall a.
  (HasCallStack, GPretty a, Show a) =>
  String ->
  Gen a ->
  Test
propertyGPrettyShow n g =
  testProperty n $ forAll g $ \(a :: a) -> do
    renderStrict (layoutPretty (LayoutOptions Unbounded) (gpretty a))
      == T.pack (show a)

propertyGPrettyRead ::
  forall a.
  (HasCallStack, GPretty a, Read a, Show a, Eq a) =>
  String ->
  Gen a ->
  Test
propertyGPrettyRead n g =
  testProperty n $ \i -> forAll g $ \(a :: a) -> do
    read
      ( T.unpack
          ( renderStrict
              ( layoutPretty
                  (LayoutOptions $ AvailablePerLine (abs i) 0.8)
                  (gpretty a)
              )
          )
      )
      == a

data I5 a = a :-: a
  deriving (Generic, Show, Read, Eq)
  deriving (GPretty) via (Default (I5 a))

infixl 5 :-:

data I6 a = a :--: a
  deriving (Generic, Show, Read, Eq)
  deriving (GPretty) via (Default (I6 a))

infixl 6 :--:

instance
  (Arbitrary a) =>
  Arbitrary (I5 a)
  where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ a :-: b

instance
  (Arbitrary a) =>
  Arbitrary (I6 a)
  where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ a :--: b

data Record a b = Record {ra :: a, rb :: b}
  deriving (Generic, Show, Read, Eq)
  deriving (GPretty) via (Default (Record a b))

instance
  (Arbitrary a, Arbitrary b) =>
  Arbitrary (Record a b)
  where
  arbitrary = do
    a <- arbitrary
    Record a <$> arbitrary

data U1Test = U1Test
  deriving (Generic, Show, Read, Eq)
  deriving (GPretty) via (Default U1Test)

newtype K1TestInt = K1Test Int
  deriving (Generic, Show, Read, Eq)
  deriving (GPretty) via (Default K1TestInt)

newtype K1TestA a = K1TestA a
  deriving (Generic, Generic1, Show, Read, Eq, Functor)
  deriving (GPretty) via (Default (K1TestA a))
  deriving (GPretty1) via (Default1 K1TestA)

newtype RecordK1TestA a = RecordK1TestA {recordK1TestA :: a}
  deriving (Generic, Show, Read, Eq)
  deriving (GPretty) via (Default (RecordK1TestA a))

data SumTestAB a b = SumTestAB a b
  deriving (Generic, Generic1, Show, Read, Eq)
  deriving (GPretty) via (Default (SumTestAB a b))
  deriving (GPretty1) via (Default1 (SumTestAB a))

data RecordSumTest a = RecordSumTest
  { recordSumTest1 :: a,
    recordSumTest2 :: a
  }
  deriving (Generic, Show, Read, Eq)
  deriving (GPretty) via (Default (RecordSumTest a))

newtype Rec1Test f b = Rec1Test (f b)
  deriving (Generic, Generic1, Show, Eq)

deriving via
  (Default (Rec1Test f a))
  instance
    (GPretty1 f, GPretty a) => GPretty (Rec1Test f a)

deriving via
  (Default1 (Rec1Test f))
  instance
    (GPretty1 f) => GPretty1 (Rec1Test f)

newtype Comp1Test f g b = Comp1Test (f (g b))
  deriving (Generic, Generic1, Show, Eq)

deriving via
  (Default (Comp1Test f g a))
  instance
    (GPretty1 f, GPretty1 g, GPretty a) => GPretty (Comp1Test f g a)

deriving via
  (Default1 (Comp1Test f g))
  instance
    (GPretty1 f, GPretty1 g, Functor f) => GPretty1 (Comp1Test f g)

gprettyTests :: Test
gprettyTests =
  testGroup
    "GPretty"
    [ testGroup
        "Derivation"
        [ testGroup
            "List"
            [ testGPretty1 "List Compact 0" 1 ([] :: [U1Test]) "[]",
              testGPretty1 "List Compact 1" 1 [U1Test] "[ U1Test\n]",
              testGPretty1 "List Compact 2" 1 [U1Test, U1Test] $
                T.intercalate
                  "\n"
                  [ "[ U1Test,",
                    "  U1Test",
                    "]"
                  ],
              testGPretty1
                "List Compact nested in 1"
                1
                [ [ U1Test,
                    U1Test
                  ]
                ]
                $ T.intercalate
                  "\n"
                  [ "[ [ U1Test,",
                    "    U1Test",
                    "  ]",
                    "]"
                  ],
              testGPretty1
                "List Compact nested in >= 2"
                1
                [ [],
                  [U1Test],
                  [ U1Test,
                    U1Test
                  ]
                ]
                $ T.intercalate
                  "\n"
                  [ "[ [],",
                    "  [ U1Test",
                    "  ],",
                    "  [ U1Test,",
                    "    U1Test",
                    "  ]",
                    "]"
                  ],
              testGPretty1
                "List unbounded nested"
                0
                [[], [U1Test], [U1Test, U1Test]]
                "[[], [U1Test], [U1Test, U1Test]]"
            ],
          testGroup
            "U1"
            [ testGPretty "Unbounded" 0 U1Test "U1Test",
              testGPretty "Compact" 1 U1Test "U1Test",
              testGPretty "List Compact" 1 [U1Test, U1Test] $
                T.intercalate
                  "\n"
                  [ "[ U1Test,",
                    "  U1Test",
                    "]"
                  ]
            ],
          testGroup
            "K1[Int]"
            [ testGPretty "Unbounded" 0 (K1Test 1) "K1Test 1",
              testGPretty "Compact" 1 (K1Test 1) "K1Test\n  1"
            ],
          testGroup
            "K1 U1"
            [ testGPretty1 "Unbounded" 0 (K1TestA U1Test) "K1TestA U1Test",
              testGPretty1 "Compact" 1 (K1TestA U1Test) "K1TestA\n  U1Test"
            ],
          testGroup
            "K1 (K1 Int)"
            [ testGPretty1
                "Unbounded"
                0
                (K1TestA (K1Test 1))
                "K1TestA (K1Test 1)",
              testGPretty1 "Compact" 1 (K1TestA (K1Test 1)) $
                T.intercalate
                  "\n"
                  [ "K1TestA",
                    "  ( K1Test",
                    "      1",
                    "  )"
                  ]
            ],
          testGroup "K1 (I5 (K1 U1) (K1 U1))" $ do
            let value =
                  K1TestA
                    ( K1TestA
                        U1Test
                        :-: K1TestA
                          U1Test
                    )
            [ testGPretty1
                "Unbounded"
                0
                value
                "K1TestA (K1TestA U1Test :-: K1TestA U1Test)",
              testGPretty1 "Compact" 1 value $
                T.intercalate
                  "\n"
                  [ "K1TestA",
                    "  ( K1TestA",
                    "      U1Test",
                    "      :-: K1TestA",
                    "        U1Test",
                    "  )"
                  ]
              ],
          testGroup "K1 (RecordK1 U1)" $ do
            let value =
                  K1TestA
                    ( RecordK1TestA
                        { recordK1TestA =
                            U1Test
                        }
                    )
            [ testGPretty1
                "Unbounded"
                0
                value
                "K1TestA (RecordK1TestA {recordK1TestA = U1Test})",
              testGPretty1 "Compact" 1 value $
                T.intercalate
                  "\n"
                  [ "K1TestA",
                    "  ( RecordK1TestA",
                    "      { recordK1TestA =",
                    "          U1Test",
                    "      }",
                    "  )"
                  ]
              ],
          testGroup "K1 (RecordK1 (K1 U1))" $ do
            let value =
                  K1TestA
                    ( RecordK1TestA
                        { recordK1TestA =
                            K1TestA
                              U1Test
                        }
                    )
            [ testGPretty1
                "Unbounded"
                0
                value
                "K1TestA (RecordK1TestA {recordK1TestA = K1TestA U1Test})",
              testGPretty1 "Compact" 1 value $
                T.intercalate
                  "\n"
                  [ "K1TestA",
                    "  ( RecordK1TestA",
                    "      { recordK1TestA =",
                    "          K1TestA",
                    "            U1Test",
                    "      }",
                    "  )"
                  ]
              ],
          testGroup "K1 (RecordSum (K1 U1))" $ do
            let value =
                  K1TestA
                    ( RecordSumTest
                        { recordSumTest1 =
                            K1TestA
                              U1Test,
                          recordSumTest2 =
                            K1TestA
                              U1Test
                        }
                    )
            [ testGPretty1 "Unbounded" 0 value $
                "K1TestA (RecordSumTest {recordSumTest1 = K1TestA U1Test, "
                  <> "recordSumTest2 = K1TestA U1Test})",
              testGPretty1 "Compact" 1 value $
                T.intercalate
                  "\n"
                  [ "K1TestA",
                    "  ( RecordSumTest",
                    "      { recordSumTest1 =",
                    "          K1TestA",
                    "            U1Test,",
                    "        recordSumTest2 =",
                    "          K1TestA",
                    "            U1Test",
                    "      }",
                    "  )"
                  ]
              ],
          testGroup "K1 (K1 U1, K1 U1)" $ do
            let value =
                  K1TestA
                    ( K1TestA
                        U1Test,
                      K1TestA
                        U1Test
                    )
            [ testGPretty1
                "Unbounded"
                0
                value
                "K1TestA (K1TestA U1Test, K1TestA U1Test)",
              testGPretty1 "Compact" 1 value $
                T.intercalate
                  "\n"
                  [ "K1TestA",
                    "  ( K1TestA",
                    "      U1Test,",
                    "    K1TestA",
                    "      U1Test",
                    "  )"
                  ]
              ],
          testGroup "Sum (K1 U1) (K1 U1)" $ do
            let value = SumTestAB (K1TestA U1Test) U1Test
            [ testGPretty1
                "Unbounded"
                0
                value
                "SumTestAB (K1TestA U1Test) U1Test",
              testGPretty1 "Compact" 1 value $
                T.intercalate
                  "\n"
                  [ "SumTestAB",
                    "  ( K1TestA",
                    "      U1Test",
                    "  )",
                    "  U1Test"
                  ]
              ],
          testGroup "Rec1 K1 (K1 U1)" $ do
            let value =
                  Rec1Test
                    ( K1TestA
                        ( K1TestA
                            U1Test
                        )
                    )
            [ testGPretty1
                "Unbounded"
                0
                value
                "Rec1Test (K1TestA (K1TestA U1Test))",
              testGPretty1 "Compact" 1 value $
                T.intercalate
                  "\n"
                  [ "Rec1Test",
                    "  ( K1TestA",
                    "      ( K1TestA",
                    "          U1Test",
                    "      )",
                    "  )"
                  ]
              ],
          testGroup "Comp1 K1 K1 (K1 U1)" $ do
            let value =
                  Comp1Test
                    ( K1TestA
                        ( K1TestA
                            ( K1TestA
                                U1Test
                            )
                        )
                    )
            [ testGPretty1
                "Unbounded"
                0
                value
                "Comp1Test (K1TestA (K1TestA (K1TestA U1Test)))",
              testGPretty1 "Compact" 1 value $
                T.intercalate
                  "\n"
                  [ "Comp1Test",
                    "  ( K1TestA",
                    "      ( K1TestA",
                    "          ( K1TestA",
                    "              U1Test",
                    "          )",
                    "      )",
                    "  )"
                  ]
              ],
          testGroup "Comp1 [] [] (K1 U1)" $ do
            let value =
                  Comp1Test
                    [ [ K1TestA
                          U1Test,
                        K1TestA
                          U1Test
                      ]
                    ]
            [ testGPretty1
                "Unbounded"
                0
                value
                "Comp1Test [[K1TestA U1Test, K1TestA U1Test]]",
              testGPretty1 "Compact" 1 value $
                T.intercalate
                  "\n"
                  [ "Comp1Test",
                    "  [ [ K1TestA",
                    "        U1Test,",
                    "      K1TestA",
                    "        U1Test",
                    "    ]",
                    "  ]"
                  ]
              ]
        ],
      testGroup
        "GPretty2"
        [ testGroup
            "Either"
            [ testGPretty2
                "Unbounded Left"
                0
                (Left (K1TestA U1Test) :: Either (K1TestA U1Test) ())
                "Left (K1TestA U1Test)",
              testGPretty2
                "Unbounded Right"
                0
                (Right (K1TestA U1Test) :: Either () (K1TestA U1Test))
                "Right (K1TestA U1Test)",
              testGPretty2
                "Compact Left"
                1
                (Left (K1TestA U1Test) :: Either (K1TestA U1Test) ())
                "Left\n  ( K1TestA\n      U1Test\n  )",
              testGPretty2
                "Compact Right"
                1
                (Right (K1TestA U1Test) :: Either () (K1TestA U1Test))
                "Right\n  ( K1TestA\n      U1Test\n  )"
            ],
          testGroup "(,)" $ do
            let value =
                  (K1TestA U1Test, K1TestA U1Test) ::
                    (K1TestA U1Test, K1TestA U1Test)
            [ testGPretty2
                "Unbounded"
                0
                value
                "(K1TestA U1Test, K1TestA U1Test)",
              testGPretty2
                "Compact"
                1
                value
                "( K1TestA\n    U1Test,\n  K1TestA\n    U1Test\n)"
              ]
        ],
      testGroup
        "simple tests"
        [ propertyGPrettyRead "Bool" (arbitrary :: Gen Bool),
          propertyGPrettyRead "Integer" (arbitrary :: Gen Integer),
          propertyGPrettyRead "Int" (arbitrary :: Gen Int),
          propertyGPrettyRead "Int8" (arbitrary :: Gen Int8),
          propertyGPrettyRead "Int16" (arbitrary :: Gen Int16),
          propertyGPrettyRead "Int32" (arbitrary :: Gen Int32),
          propertyGPrettyRead "Int64" (arbitrary :: Gen Int64),
          propertyGPrettyRead "Word" (arbitrary :: Gen Word),
          propertyGPrettyRead "Word8" (arbitrary :: Gen Word8),
          propertyGPrettyRead "Word16" (arbitrary :: Gen Word16),
          propertyGPrettyRead "Word32" (arbitrary :: Gen Word32),
          propertyGPrettyRead "Word64" (arbitrary :: Gen Word64),
          propertyGPrettyShow
            "SomeWordN"
            ( oneof
                [ SomeWordN <$> (arbitrary :: Gen (WordN 8)),
                  SomeWordN <$> (arbitrary :: Gen (WordN 9)),
                  SomeWordN <$> (arbitrary :: Gen (WordN 10))
                ]
            ),
          propertyGPrettyRead "WordN 8" (arbitrary :: Gen (WordN 8)),
          propertyGPrettyRead "WordN 9" (arbitrary :: Gen (WordN 9)),
          propertyGPrettyShow
            "SomeIntN"
            ( oneof
                [ SomeIntN <$> (arbitrary :: Gen (IntN 8)),
                  SomeIntN <$> (arbitrary :: Gen (IntN 9)),
                  SomeIntN <$> (arbitrary :: Gen (IntN 10))
                ]
            ),
          propertyGPrettyRead "IntN 8" (arbitrary :: Gen (IntN 8)),
          propertyGPrettyRead "IntN 9" (arbitrary :: Gen (IntN 9))
        ],
      testGroup
        "Combined types"
        [ propertyGPrettyRead
            "Maybe Maybe"
            (arbitrary :: Gen (Maybe (Maybe Int))),
          propertyGPrettyRead
            "Maybe (,)"
            ( arbitrary :: Gen (Maybe (Int, Int))
            ),
          propertyGPrettyRead
            "Maybe I5"
            ( arbitrary :: Gen (Maybe (I5 Int))
            ),
          propertyGPrettyRead
            "Maybe []"
            ( arbitrary :: Gen (Maybe [Int])
            ),
          propertyGPrettyRead
            "Maybe Record"
            ( arbitrary :: Gen (Maybe (Record Int Int))
            ),
          propertyGPrettyRead
            "(Maybe,Either)"
            ( arbitrary :: Gen (Maybe Int, Either Int Int)
            ),
          propertyGPrettyRead
            "((,),(,))"
            ( arbitrary :: Gen ((Int, Int), (Int, Int))
            ),
          propertyGPrettyRead
            "(I5,I5)"
            ( arbitrary :: Gen (I5 Int, I5 Int)
            ),
          propertyGPrettyRead
            "([],[])"
            ( arbitrary :: Gen ([Int], [Int])
            ),
          propertyGPrettyRead
            "(Record,Record)"
            ( arbitrary :: Gen (Record Int Int, Record Int Int)
            ),
          propertyGPrettyRead "I5 (,)" (arbitrary :: Gen (I5 (Int, Int))),
          propertyGPrettyRead "I5 I6" (arbitrary :: Gen (I5 (I6 Int))),
          propertyGPrettyRead "I5 I5" (arbitrary :: Gen (I5 (I5 Int))),
          propertyGPrettyRead "I6 I5" (arbitrary :: Gen (I6 (I5 Int))),
          propertyGPrettyRead "I6 I6" (arbitrary :: Gen (I6 (I6 Int))),
          propertyGPrettyRead "I5 []" (arbitrary :: Gen (I5 [Int])),
          propertyGPrettyRead
            "I5 Record"
            (arbitrary :: Gen (I5 (Record Int Int))),
          propertyGPrettyRead
            "[Maybe]"
            ( arbitrary :: Gen [Maybe Int]
            ),
          propertyGPrettyRead
            "[(,)]"
            ( arbitrary :: Gen [(Int, Int)]
            ),
          propertyGPrettyRead "[I5]" (arbitrary :: Gen [I5 Int]),
          propertyGPrettyRead
            "[[]]"
            ( arbitrary :: Gen [[Int]]
            ),
          propertyGPrettyRead
            "[Record]"
            ( arbitrary :: Gen [Record Int Int]
            ),
          propertyGPrettyRead
            "Record Maybe Either"
            ( arbitrary :: Gen (Record (Maybe Int) (Either Int Int))
            ),
          propertyGPrettyRead
            "Record (,) (,)"
            ( arbitrary :: Gen (Record (Int, Int) (Int, Int))
            ),
          propertyGPrettyRead
            "Record I5 I6"
            ( arbitrary :: Gen (Record (I5 Int) (I6 Int))
            ),
          propertyGPrettyRead
            "Record []"
            ( arbitrary :: Gen (Record [Int] [Int])
            ),
          propertyGPrettyRead
            "Record Record"
            ( arbitrary :: Gen (Record (Record Int Int) (Record Int Int))
            ),
          propertyGPrettyRead
            "Maybe (MaybeT Identity Int)"
            (Just . MaybeT <$> arbitrary :: Gen (Maybe (MaybeT Identity Int))),
          propertyGPrettyRead
            "Maybe (ExceptT Int Identity Int)"
            ( Just . ExceptT <$> arbitrary ::
                Gen (Maybe (ExceptT Int Identity Int))
            ),
          propertyGPrettyRead
            "Maybe (LazyWriterT Int Identity Int)"
            ( Just . WriterLazy.WriterT <$> arbitrary ::
                Gen (Maybe (WriterLazy.WriterT Int Identity Int))
            ),
          propertyGPrettyRead
            "Maybe (StrictWriterT Int Identity Int)"
            ( Just . WriterLazy.WriterT <$> arbitrary ::
                Gen (Maybe (WriterLazy.WriterT Int Identity Int))
            ),
          propertyGPrettyRead
            "Maybe (IdentityT Identity Int)"
            ( Just . IdentityT <$> arbitrary ::
                Gen (Maybe (IdentityT Identity Int))
            ),
          propertyGPrettyRead
            "HS.HashSet Int"
            (HS.fromList <$> arbitrary :: Gen (HS.HashSet Int)),
          propertyGPrettyRead
            "HM.HashMap Int Int"
            (HM.fromList <$> arbitrary :: Gen (HM.HashMap Int Int))
        ],
      testGroup
        "Symbolic types"
        [ testGPretty
            "enough space"
            80
            ("a" .&& "b" :: SymBool)
            "(&& a b)",
          testGPretty
            "not enough space"
            6
            ("a" .&& "b" :: SymBool)
            "..."
        ]
    ]
