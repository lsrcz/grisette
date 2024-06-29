{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}

module Grisette.Core.Data.Class.FormatTests (formatTests) where

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
  ( Format (format),
    IntN,
    LogicalOp ((.&&)),
    SymBool,
    WordN,
    pattern SomeIntN,
    pattern SomeWordN,
  )
import Grisette.Internal.Core.Data.Class.Format
  ( Format1,
    Format2,
    docToTextWithWidth,
    formatPrec1,
    formatPrec2,
    formatTextWithWidth,
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

testFormat :: (HasCallStack, Format a) => String -> Int -> a -> T.Text -> Test
testFormat n i a s = testCase n $ formatTextWithWidth i a @?= s

testFormat1 ::
  (HasCallStack, Format1 f, Format a) =>
  String ->
  Int ->
  f a ->
  T.Text ->
  Test
testFormat1 n i a s = testCase n $ do
  formatTextWithWidth i a @?= s
  docToTextWithWidth i (formatPrec1 0 a) @?= s

testFormat2 ::
  (HasCallStack, Format2 f, Format a, Format b) =>
  String ->
  Int ->
  f a b ->
  T.Text ->
  Test
testFormat2 n i a s = testCase n $ do
  formatTextWithWidth i a @?= s
  docToTextWithWidth i (formatPrec1 0 a) @?= s
  docToTextWithWidth i (formatPrec2 0 a) @?= s

propertyFormatShow ::
  forall a.
  (HasCallStack, Format a, Show a) =>
  String ->
  Gen a ->
  Test
propertyFormatShow n g =
  testProperty n $ forAll g $ \(a :: a) -> do
    renderStrict (layoutPretty (LayoutOptions Unbounded) (format a))
      == T.pack (show a)

propertyFormatRead ::
  forall a.
  (HasCallStack, Format a, Read a, Show a, Eq a) =>
  String ->
  Gen a ->
  Test
propertyFormatRead n g =
  testProperty n $ \i -> forAll g $ \(a :: a) -> do
    read
      ( T.unpack
          ( renderStrict
              ( layoutPretty
                  (LayoutOptions $ AvailablePerLine (abs i) 0.8)
                  (format a)
              )
          )
      )
      == a

data I5 a = a :-: a
  deriving (Generic, Show, Read, Eq)
  deriving (Format) via (Default (I5 a))

infixl 5 :-:

data I6 a = a :--: a
  deriving (Generic, Show, Read, Eq)
  deriving (Format) via (Default (I6 a))

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
  deriving (Format) via (Default (Record a b))

instance
  (Arbitrary a, Arbitrary b) =>
  Arbitrary (Record a b)
  where
  arbitrary = do
    a <- arbitrary
    Record a <$> arbitrary

data U1Test = U1Test
  deriving (Generic, Show, Read, Eq)
  deriving (Format) via (Default U1Test)

newtype K1TestInt = K1Test Int
  deriving (Generic, Show, Read, Eq)
  deriving (Format) via (Default K1TestInt)

newtype K1TestA a = K1TestA a
  deriving (Generic, Generic1, Show, Read, Eq, Functor)
  deriving (Format) via (Default (K1TestA a))
  deriving (Format1) via (Default1 K1TestA)

newtype RecordK1TestA a = RecordK1TestA {recordK1TestA :: a}
  deriving (Generic, Show, Read, Eq)
  deriving (Format) via (Default (RecordK1TestA a))

data SumTestAB a b = SumTestAB a b
  deriving (Generic, Generic1, Show, Read, Eq)
  deriving (Format) via (Default (SumTestAB a b))
  deriving (Format1) via (Default1 (SumTestAB a))

data RecordSumTest a = RecordSumTest
  { recordSumTest1 :: a,
    recordSumTest2 :: a
  }
  deriving (Generic, Show, Read, Eq)
  deriving (Format) via (Default (RecordSumTest a))

newtype Rec1Test f b = Rec1Test (f b)
  deriving (Generic, Generic1, Show, Eq)

deriving via
  (Default (Rec1Test f a))
  instance
    (Format1 f, Format a) => Format (Rec1Test f a)

deriving via
  (Default1 (Rec1Test f))
  instance
    (Format1 f) => Format1 (Rec1Test f)

newtype Comp1Test f g b = Comp1Test (f (g b))
  deriving (Generic, Generic1, Show, Eq)

deriving via
  (Default (Comp1Test f g a))
  instance
    (Format1 f, Format1 g, Format a) => Format (Comp1Test f g a)

deriving via
  (Default1 (Comp1Test f g))
  instance
    (Format1 f, Format1 g, Functor f) => Format1 (Comp1Test f g)

formatTests :: Test
formatTests =
  testGroup
    "Format"
    [ testGroup
        "Derivation"
        [ testGroup
            "List"
            [ testFormat1 "List Compact 0" 1 ([] :: [U1Test]) "[]",
              testFormat1 "List Compact 1" 1 [U1Test] "[ U1Test\n]",
              testFormat1 "List Compact 2" 1 [U1Test, U1Test] $
                T.intercalate
                  "\n"
                  [ "[ U1Test,",
                    "  U1Test",
                    "]"
                  ],
              testFormat1
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
              testFormat1
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
              testFormat1
                "List unbounded nested"
                0
                [[], [U1Test], [U1Test, U1Test]]
                "[[], [U1Test], [U1Test, U1Test]]"
            ],
          testGroup
            "U1"
            [ testFormat "Unbounded" 0 U1Test "U1Test",
              testFormat "Compact" 1 U1Test "U1Test",
              testFormat "List Compact" 1 [U1Test, U1Test] $
                T.intercalate
                  "\n"
                  [ "[ U1Test,",
                    "  U1Test",
                    "]"
                  ]
            ],
          testGroup
            "K1[Int]"
            [ testFormat "Unbounded" 0 (K1Test 1) "K1Test 1",
              testFormat "Compact" 1 (K1Test 1) "K1Test\n  1"
            ],
          testGroup
            "K1 U1"
            [ testFormat1 "Unbounded" 0 (K1TestA U1Test) "K1TestA U1Test",
              testFormat1 "Compact" 1 (K1TestA U1Test) "K1TestA\n  U1Test"
            ],
          testGroup
            "K1 (K1 Int)"
            [ testFormat1
                "Unbounded"
                0
                (K1TestA (K1Test 1))
                "K1TestA (K1Test 1)",
              testFormat1 "Compact" 1 (K1TestA (K1Test 1)) $
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
            [ testFormat1
                "Unbounded"
                0
                value
                "K1TestA (K1TestA U1Test :-: K1TestA U1Test)",
              testFormat1 "Compact" 1 value $
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
            [ testFormat1
                "Unbounded"
                0
                value
                "K1TestA (RecordK1TestA {recordK1TestA = U1Test})",
              testFormat1 "Compact" 1 value $
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
            [ testFormat1
                "Unbounded"
                0
                value
                "K1TestA (RecordK1TestA {recordK1TestA = K1TestA U1Test})",
              testFormat1 "Compact" 1 value $
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
            [ testFormat1 "Unbounded" 0 value $
                "K1TestA (RecordSumTest {recordSumTest1 = K1TestA U1Test, "
                  <> "recordSumTest2 = K1TestA U1Test})",
              testFormat1 "Compact" 1 value $
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
            [ testFormat1
                "Unbounded"
                0
                value
                "K1TestA (K1TestA U1Test, K1TestA U1Test)",
              testFormat1 "Compact" 1 value $
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
            [ testFormat1
                "Unbounded"
                0
                value
                "SumTestAB (K1TestA U1Test) U1Test",
              testFormat1 "Compact" 1 value $
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
            [ testFormat1
                "Unbounded"
                0
                value
                "Rec1Test (K1TestA (K1TestA U1Test))",
              testFormat1 "Compact" 1 value $
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
            [ testFormat1
                "Unbounded"
                0
                value
                "Comp1Test (K1TestA (K1TestA (K1TestA U1Test)))",
              testFormat1 "Compact" 1 value $
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
            [ testFormat1
                "Unbounded"
                0
                value
                "Comp1Test [[K1TestA U1Test, K1TestA U1Test]]",
              testFormat1 "Compact" 1 value $
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
        "Format2"
        [ testGroup
            "Either"
            [ testFormat2
                "Unbounded Left"
                0
                (Left (K1TestA U1Test) :: Either (K1TestA U1Test) ())
                "Left (K1TestA U1Test)",
              testFormat2
                "Unbounded Right"
                0
                (Right (K1TestA U1Test) :: Either () (K1TestA U1Test))
                "Right (K1TestA U1Test)",
              testFormat2
                "Compact Left"
                1
                (Left (K1TestA U1Test) :: Either (K1TestA U1Test) ())
                "Left\n  ( K1TestA\n      U1Test\n  )",
              testFormat2
                "Compact Right"
                1
                (Right (K1TestA U1Test) :: Either () (K1TestA U1Test))
                "Right\n  ( K1TestA\n      U1Test\n  )"
            ],
          testGroup "(,)" $ do
            let value =
                  (K1TestA U1Test, K1TestA U1Test) ::
                    (K1TestA U1Test, K1TestA U1Test)
            [ testFormat2
                "Unbounded"
                0
                value
                "(K1TestA U1Test, K1TestA U1Test)",
              testFormat2
                "Compact"
                1
                value
                "( K1TestA\n    U1Test,\n  K1TestA\n    U1Test\n)"
              ]
        ],
      testGroup
        "simple tests"
        [ propertyFormatRead "Bool" (arbitrary :: Gen Bool),
          propertyFormatRead "Integer" (arbitrary :: Gen Integer),
          propertyFormatRead "Int" (arbitrary :: Gen Int),
          propertyFormatRead "Int8" (arbitrary :: Gen Int8),
          propertyFormatRead "Int16" (arbitrary :: Gen Int16),
          propertyFormatRead "Int32" (arbitrary :: Gen Int32),
          propertyFormatRead "Int64" (arbitrary :: Gen Int64),
          propertyFormatRead "Word" (arbitrary :: Gen Word),
          propertyFormatRead "Word8" (arbitrary :: Gen Word8),
          propertyFormatRead "Word16" (arbitrary :: Gen Word16),
          propertyFormatRead "Word32" (arbitrary :: Gen Word32),
          propertyFormatRead "Word64" (arbitrary :: Gen Word64),
          propertyFormatShow
            "SomeWordN"
            ( oneof
                [ SomeWordN <$> (arbitrary :: Gen (WordN 8)),
                  SomeWordN <$> (arbitrary :: Gen (WordN 9)),
                  SomeWordN <$> (arbitrary :: Gen (WordN 10))
                ]
            ),
          propertyFormatRead "WordN 8" (arbitrary :: Gen (WordN 8)),
          propertyFormatRead "WordN 9" (arbitrary :: Gen (WordN 9)),
          propertyFormatShow
            "SomeIntN"
            ( oneof
                [ SomeIntN <$> (arbitrary :: Gen (IntN 8)),
                  SomeIntN <$> (arbitrary :: Gen (IntN 9)),
                  SomeIntN <$> (arbitrary :: Gen (IntN 10))
                ]
            ),
          propertyFormatRead "IntN 8" (arbitrary :: Gen (IntN 8)),
          propertyFormatRead "IntN 9" (arbitrary :: Gen (IntN 9))
        ],
      testGroup
        "Combined types"
        [ propertyFormatRead
            "Maybe Maybe"
            (arbitrary :: Gen (Maybe (Maybe Int))),
          propertyFormatRead
            "Maybe (,)"
            ( arbitrary :: Gen (Maybe (Int, Int))
            ),
          propertyFormatRead
            "Maybe I5"
            ( arbitrary :: Gen (Maybe (I5 Int))
            ),
          propertyFormatRead
            "Maybe []"
            ( arbitrary :: Gen (Maybe [Int])
            ),
          propertyFormatRead
            "Maybe Record"
            ( arbitrary :: Gen (Maybe (Record Int Int))
            ),
          propertyFormatRead
            "(Maybe,Either)"
            ( arbitrary :: Gen (Maybe Int, Either Int Int)
            ),
          propertyFormatRead
            "((,),(,))"
            ( arbitrary :: Gen ((Int, Int), (Int, Int))
            ),
          propertyFormatRead
            "(I5,I5)"
            ( arbitrary :: Gen (I5 Int, I5 Int)
            ),
          propertyFormatRead
            "([],[])"
            ( arbitrary :: Gen ([Int], [Int])
            ),
          propertyFormatRead
            "(Record,Record)"
            ( arbitrary :: Gen (Record Int Int, Record Int Int)
            ),
          propertyFormatRead "I5 (,)" (arbitrary :: Gen (I5 (Int, Int))),
          propertyFormatRead "I5 I6" (arbitrary :: Gen (I5 (I6 Int))),
          propertyFormatRead "I5 I5" (arbitrary :: Gen (I5 (I5 Int))),
          propertyFormatRead "I6 I5" (arbitrary :: Gen (I6 (I5 Int))),
          propertyFormatRead "I6 I6" (arbitrary :: Gen (I6 (I6 Int))),
          propertyFormatRead "I5 []" (arbitrary :: Gen (I5 [Int])),
          propertyFormatRead
            "I5 Record"
            (arbitrary :: Gen (I5 (Record Int Int))),
          propertyFormatRead
            "[Maybe]"
            ( arbitrary :: Gen [Maybe Int]
            ),
          propertyFormatRead
            "[(,)]"
            ( arbitrary :: Gen [(Int, Int)]
            ),
          propertyFormatRead "[I5]" (arbitrary :: Gen [I5 Int]),
          propertyFormatRead
            "[[]]"
            ( arbitrary :: Gen [[Int]]
            ),
          propertyFormatRead
            "[Record]"
            ( arbitrary :: Gen [Record Int Int]
            ),
          propertyFormatRead
            "Record Maybe Either"
            ( arbitrary :: Gen (Record (Maybe Int) (Either Int Int))
            ),
          propertyFormatRead
            "Record (,) (,)"
            ( arbitrary :: Gen (Record (Int, Int) (Int, Int))
            ),
          propertyFormatRead
            "Record I5 I6"
            ( arbitrary :: Gen (Record (I5 Int) (I6 Int))
            ),
          propertyFormatRead
            "Record []"
            ( arbitrary :: Gen (Record [Int] [Int])
            ),
          propertyFormatRead
            "Record Record"
            ( arbitrary :: Gen (Record (Record Int Int) (Record Int Int))
            ),
          propertyFormatRead
            "Maybe (MaybeT Identity Int)"
            (Just . MaybeT <$> arbitrary :: Gen (Maybe (MaybeT Identity Int))),
          propertyFormatRead
            "Maybe (ExceptT Int Identity Int)"
            ( Just . ExceptT <$> arbitrary ::
                Gen (Maybe (ExceptT Int Identity Int))
            ),
          propertyFormatRead
            "Maybe (LazyWriterT Int Identity Int)"
            ( Just . WriterLazy.WriterT <$> arbitrary ::
                Gen (Maybe (WriterLazy.WriterT Int Identity Int))
            ),
          propertyFormatRead
            "Maybe (StrictWriterT Int Identity Int)"
            ( Just . WriterLazy.WriterT <$> arbitrary ::
                Gen (Maybe (WriterLazy.WriterT Int Identity Int))
            ),
          propertyFormatRead
            "Maybe (IdentityT Identity Int)"
            ( Just . IdentityT <$> arbitrary ::
                Gen (Maybe (IdentityT Identity Int))
            ),
          propertyFormatRead
            "HS.HashSet Int"
            (HS.fromList <$> arbitrary :: Gen (HS.HashSet Int)),
          propertyFormatRead
            "HM.HashMap Int Int"
            (HM.fromList <$> arbitrary :: Gen (HM.HashMap Int Int))
        ],
      testGroup
        "Symbolic types"
        [ testFormat
            "enough space"
            80
            ("a" .&& "b" :: SymBool)
            "(&& a b)",
          testFormat
            "not enough space"
            6
            ("a" .&& "b" :: SymBool)
            "..."
        ]
    ]
