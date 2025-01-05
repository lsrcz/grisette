{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}

module Grisette.Core.Data.Class.PPrintTests (pprintTests) where

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

import Control.Monad.Except (ExceptT (ExceptT))
import Control.Monad.Identity (Identity (Identity), IdentityT (IdentityT))
import Control.Monad.Trans.Maybe (MaybeT (MaybeT))
import qualified Control.Monad.Trans.Writer.Lazy as WriterLazy
import qualified Control.Monad.Writer.Strict as WriterStrict
import qualified Data.HashMap.Lazy as HM
import qualified Data.HashSet as HS
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Text as T (Text, intercalate, pack, unpack)
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.Generics (Generic, Generic1)
import GHC.Stack (HasCallStack)
import Generics.Deriving (Default (Default), Default1 (Default1))
import Grisette
  ( IntN,
    LogicalOp ((.&&)),
    PPrint (pformat),
    SymBool,
    WordN,
    pattern SomeIntN,
    pattern SomeWordN,
  )
import Grisette.Internal.Core.Data.Class.PPrint
  ( PPrint1,
    PPrint2,
    docToTextWithWidth,
    pformatPrec1,
    pformatPrec2,
    pformatTextWithWidth,
  )
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.HUnit ((@?=))
import Test.QuickCheck (Arbitrary (arbitrary), Gen, forAll, oneof)

testPPrint :: (HasCallStack, PPrint a) => String -> Int -> a -> T.Text -> Test
testPPrint n i a s = testCase n $ pformatTextWithWidth i a @?= s

testPPrint1 ::
  (HasCallStack, PPrint1 f, PPrint a) =>
  String ->
  Int ->
  f a ->
  T.Text ->
  Test
testPPrint1 n i a s = testCase n $ do
  pformatTextWithWidth i a @?= s
  docToTextWithWidth i (pformatPrec1 0 a) @?= s

testPPrint2 ::
  (HasCallStack, PPrint2 f, PPrint a, PPrint b) =>
  String ->
  Int ->
  f a b ->
  T.Text ->
  Test
testPPrint2 n i a s = testCase n $ do
  pformatTextWithWidth i a @?= s
  docToTextWithWidth i (pformatPrec1 0 a) @?= s
  docToTextWithWidth i (pformatPrec2 0 a) @?= s

propertyPFormatShow ::
  forall a.
  (PPrint a, Show a) =>
  String ->
  Gen a ->
  Test
propertyPFormatShow n g =
  testProperty n $ forAll g $ \(a :: a) -> do
    renderStrict (layoutPretty (LayoutOptions Unbounded) (pformat a))
      == T.pack (show a)

propertyPFormatRead ::
  forall a.
  (PPrint a, Read a, Show a, Eq a) =>
  String ->
  Gen a ->
  Test
propertyPFormatRead n g =
  testProperty n $ \i -> forAll g $ \(a :: a) -> do
    read
      ( T.unpack
          ( renderStrict
              ( layoutPretty
                  (LayoutOptions $ AvailablePerLine (abs i) 0.8)
                  (pformat a)
              )
          )
      )
      == a

data I5 a = a :-: a
  deriving (Generic, Show, Read, Eq)
  deriving (PPrint) via (Default (I5 a))

infixl 5 :-:

data I6 a = a :--: a
  deriving (Generic, Show, Read, Eq)
  deriving (PPrint) via (Default (I6 a))

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
  deriving (PPrint) via (Default (Record a b))

instance
  (Arbitrary a, Arbitrary b) =>
  Arbitrary (Record a b)
  where
  arbitrary = do
    a <- arbitrary
    Record a <$> arbitrary

data U1Test = U1Test
  deriving (Generic, Show, Read, Eq)
  deriving (PPrint) via (Default U1Test)

newtype K1TestInt = K1Test Int
  deriving (Generic, Show, Read, Eq)
  deriving (PPrint) via (Default K1TestInt)

newtype K1TestA a = K1TestA a
  deriving (Generic, Generic1, Show, Read, Eq, Functor)
  deriving (PPrint) via (Default (K1TestA a))
  deriving (PPrint1) via (Default1 K1TestA)

newtype RecordK1TestA a = RecordK1TestA {recordK1TestA :: a}
  deriving (Generic, Show, Read, Eq)
  deriving (PPrint) via (Default (RecordK1TestA a))

data SumTestAB a b = SumTestAB a b
  deriving (Generic, Generic1, Show, Read, Eq)
  deriving (PPrint) via (Default (SumTestAB a b))
  deriving (PPrint1) via (Default1 (SumTestAB a))

data RecordSumTest a = RecordSumTest
  { recordSumTest1 :: a,
    recordSumTest2 :: a
  }
  deriving (Generic, Show, Read, Eq)
  deriving (PPrint) via (Default (RecordSumTest a))

newtype Rec1Test f b = Rec1Test (f b)
  deriving (Generic, Generic1, Show, Eq)

deriving via
  (Default (Rec1Test f a))
  instance
    (PPrint1 f, PPrint a) => PPrint (Rec1Test f a)

deriving via
  (Default1 (Rec1Test f))
  instance
    (PPrint1 f) => PPrint1 (Rec1Test f)

newtype Comp1Test f g b = Comp1Test (f (g b))
  deriving (Generic, Generic1, Show, Eq)

deriving via
  (Default (Comp1Test f g a))
  instance
    (PPrint1 f, PPrint1 g, PPrint a) => PPrint (Comp1Test f g a)

deriving via
  (Default1 (Comp1Test f g))
  instance
    (PPrint1 f, PPrint1 g, Functor f) => PPrint1 (Comp1Test f g)

pprintTests :: Test
pprintTests =
  testGroup
    "PPrint"
    [ testGroup
        "Derivation"
        [ testGroup
            "List"
            [ testPPrint1 "List Compact 0" 1 ([] :: [U1Test]) "[]",
              testPPrint1 "List Compact 1" 1 [U1Test] "[ U1Test\n]",
              testPPrint1 "List Compact 2" 1 [U1Test, U1Test] $
                T.intercalate
                  "\n"
                  [ "[ U1Test,",
                    "  U1Test",
                    "]"
                  ],
              testPPrint1
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
              testPPrint1
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
              testPPrint1
                "List unbounded nested"
                0
                [[], [U1Test], [U1Test, U1Test]]
                "[[], [U1Test], [U1Test, U1Test]]"
            ],
          testGroup
            "U1"
            [ testPPrint "Unbounded" 0 U1Test "U1Test",
              testPPrint "Compact" 1 U1Test "U1Test",
              testPPrint "List Compact" 1 [U1Test, U1Test] $
                T.intercalate
                  "\n"
                  [ "[ U1Test,",
                    "  U1Test",
                    "]"
                  ]
            ],
          testGroup
            "K1[Int]"
            [ testPPrint "Unbounded" 0 (K1Test 1) "K1Test 1",
              testPPrint "Compact" 1 (K1Test 1) "K1Test\n  1"
            ],
          testGroup
            "K1 U1"
            [ testPPrint1 "Unbounded" 0 (K1TestA U1Test) "K1TestA U1Test",
              testPPrint1 "Compact" 1 (K1TestA U1Test) "K1TestA\n  U1Test"
            ],
          testGroup
            "K1 (K1 Int)"
            [ testPPrint1
                "Unbounded"
                0
                (K1TestA (K1Test 1))
                "K1TestA (K1Test 1)",
              testPPrint1 "Compact" 1 (K1TestA (K1Test 1)) $
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
            [ testPPrint1
                "Unbounded"
                0
                value
                "K1TestA (K1TestA U1Test :-: K1TestA U1Test)",
              testPPrint1 "Compact" 1 value $
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
            [ testPPrint1
                "Unbounded"
                0
                value
                "K1TestA (RecordK1TestA {recordK1TestA = U1Test})",
              testPPrint1 "Compact" 1 value $
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
            [ testPPrint1
                "Unbounded"
                0
                value
                "K1TestA (RecordK1TestA {recordK1TestA = K1TestA U1Test})",
              testPPrint1 "Compact" 1 value $
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
            [ testPPrint1 "Unbounded" 0 value $
                "K1TestA (RecordSumTest {recordSumTest1 = K1TestA U1Test, "
                  <> "recordSumTest2 = K1TestA U1Test})",
              testPPrint1 "Compact" 1 value $
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
            [ testPPrint1
                "Unbounded"
                0
                value
                "K1TestA (K1TestA U1Test, K1TestA U1Test)",
              testPPrint1 "Compact" 1 value $
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
            [ testPPrint1
                "Unbounded"
                0
                value
                "SumTestAB (K1TestA U1Test) U1Test",
              testPPrint1 "Compact" 1 value $
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
            [ testPPrint1
                "Unbounded"
                0
                value
                "Rec1Test (K1TestA (K1TestA U1Test))",
              testPPrint1 "Compact" 1 value $
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
            [ testPPrint1
                "Unbounded"
                0
                value
                "Comp1Test (K1TestA (K1TestA (K1TestA U1Test)))",
              testPPrint1 "Compact" 1 value $
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
            [ testPPrint1
                "Unbounded"
                0
                value
                "Comp1Test [[K1TestA U1Test, K1TestA U1Test]]",
              testPPrint1 "Compact" 1 value $
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
        "PPrint2"
        [ testGroup
            "Either"
            [ testPPrint2
                "Unbounded Left"
                0
                (Left (K1TestA U1Test) :: Either (K1TestA U1Test) ())
                "Left (K1TestA U1Test)",
              testPPrint2
                "Unbounded Right"
                0
                (Right (K1TestA U1Test) :: Either () (K1TestA U1Test))
                "Right (K1TestA U1Test)",
              testPPrint2
                "Compact Left"
                1
                (Left (K1TestA U1Test) :: Either (K1TestA U1Test) ())
                "Left\n  ( K1TestA\n      U1Test\n  )",
              testPPrint2
                "Compact Right"
                1
                (Right (K1TestA U1Test) :: Either () (K1TestA U1Test))
                "Right\n  ( K1TestA\n      U1Test\n  )"
            ],
          testGroup "(,)" $ do
            let value =
                  (K1TestA U1Test, K1TestA U1Test) ::
                    (K1TestA U1Test, K1TestA U1Test)
            [ testPPrint2
                "Unbounded"
                0
                value
                "(K1TestA U1Test, K1TestA U1Test)",
              testPPrint2
                "Compact"
                1
                value
                "( K1TestA\n    U1Test,\n  K1TestA\n    U1Test\n)"
              ]
        ],
      testGroup
        "simple tests"
        [ propertyPFormatRead "Bool" (arbitrary :: Gen Bool),
          propertyPFormatRead "Integer" (arbitrary :: Gen Integer),
          propertyPFormatRead "Int" (arbitrary :: Gen Int),
          propertyPFormatRead "Int8" (arbitrary :: Gen Int8),
          propertyPFormatRead "Int16" (arbitrary :: Gen Int16),
          propertyPFormatRead "Int32" (arbitrary :: Gen Int32),
          propertyPFormatRead "Int64" (arbitrary :: Gen Int64),
          propertyPFormatRead "Word" (arbitrary :: Gen Word),
          propertyPFormatRead "Word8" (arbitrary :: Gen Word8),
          propertyPFormatRead "Word16" (arbitrary :: Gen Word16),
          propertyPFormatRead "Word32" (arbitrary :: Gen Word32),
          propertyPFormatRead "Word64" (arbitrary :: Gen Word64),
          propertyPFormatShow
            "SomeWordN"
            ( oneof
                [ SomeWordN <$> (arbitrary :: Gen (WordN 8)),
                  SomeWordN <$> (arbitrary :: Gen (WordN 9)),
                  SomeWordN <$> (arbitrary :: Gen (WordN 10))
                ]
            ),
          propertyPFormatRead "WordN 8" (arbitrary :: Gen (WordN 8)),
          propertyPFormatRead "WordN 9" (arbitrary :: Gen (WordN 9)),
          propertyPFormatShow
            "SomeIntN"
            ( oneof
                [ SomeIntN <$> (arbitrary :: Gen (IntN 8)),
                  SomeIntN <$> (arbitrary :: Gen (IntN 9)),
                  SomeIntN <$> (arbitrary :: Gen (IntN 10))
                ]
            ),
          propertyPFormatRead "IntN 8" (arbitrary :: Gen (IntN 8)),
          propertyPFormatRead "IntN 9" (arbitrary :: Gen (IntN 9))
        ],
      testGroup
        "Combined types"
        [ propertyPFormatRead
            "Maybe Maybe"
            (arbitrary :: Gen (Maybe (Maybe Int))),
          propertyPFormatRead
            "Maybe (,)"
            (arbitrary :: Gen (Maybe (Int, Int))),
          propertyPFormatRead
            "Maybe I5"
            (arbitrary :: Gen (Maybe (I5 Int))),
          propertyPFormatRead
            "Maybe []"
            (arbitrary :: Gen (Maybe [Int])),
          propertyPFormatRead
            "Maybe Record"
            (arbitrary :: Gen (Maybe (Record Int Int))),
          propertyPFormatRead
            "(Maybe,Either)"
            (arbitrary :: Gen (Maybe Int, Either Int Int)),
          propertyPFormatRead
            "((,),(,))"
            (arbitrary :: Gen ((Int, Int), (Int, Int))),
          propertyPFormatRead
            "(I5,I5)"
            (arbitrary :: Gen (I5 Int, I5 Int)),
          propertyPFormatRead
            "([],[])"
            (arbitrary :: Gen ([Int], [Int])),
          propertyPFormatRead
            "(Record,Record)"
            (arbitrary :: Gen (Record Int Int, Record Int Int)),
          propertyPFormatRead "I5 (,)" (arbitrary :: Gen (I5 (Int, Int))),
          propertyPFormatRead "I5 I6" (arbitrary :: Gen (I5 (I6 Int))),
          propertyPFormatRead "I5 I5" (arbitrary :: Gen (I5 (I5 Int))),
          propertyPFormatRead "I6 I5" (arbitrary :: Gen (I6 (I5 Int))),
          propertyPFormatRead "I6 I6" (arbitrary :: Gen (I6 (I6 Int))),
          propertyPFormatRead "I5 []" (arbitrary :: Gen (I5 [Int])),
          propertyPFormatRead
            "I5 Record"
            (arbitrary :: Gen (I5 (Record Int Int))),
          propertyPFormatRead
            "[Maybe]"
            (arbitrary :: Gen [Maybe Int]),
          propertyPFormatRead
            "[(,)]"
            (arbitrary :: Gen [(Int, Int)]),
          propertyPFormatRead "[I5]" (arbitrary :: Gen [I5 Int]),
          propertyPFormatRead
            "[[]]"
            (arbitrary :: Gen [[Int]]),
          propertyPFormatRead
            "[Record]"
            (arbitrary :: Gen [Record Int Int]),
          propertyPFormatRead
            "Record Maybe Either"
            (arbitrary :: Gen (Record (Maybe Int) (Either Int Int))),
          propertyPFormatRead
            "Record (,) (,)"
            (arbitrary :: Gen (Record (Int, Int) (Int, Int))),
          propertyPFormatRead
            "Record I5 I6"
            (arbitrary :: Gen (Record (I5 Int) (I6 Int))),
          propertyPFormatRead
            "Record []"
            (arbitrary :: Gen (Record [Int] [Int])),
          propertyPFormatRead
            "Record Record"
            (arbitrary :: Gen (Record (Record Int Int) (Record Int Int))),
          testPPrint1
            "Identity"
            0
            (Identity $ Just $ Just 1 :: Identity (Maybe (Maybe Int)))
            "Just (Just 1)",
          testPPrint1
            "Maybe (MaybeT Maybe Int)"
            0
            (Just $ MaybeT (Just (Just 1)) :: Maybe (MaybeT Maybe Int))
            "Just (MaybeT {runMaybeT = Just (Just 1)})",
          propertyPFormatRead
            "Maybe (ExceptT Int Maybe Int)"
            ( Just . ExceptT <$> arbitrary ::
                Gen (Maybe (ExceptT Int Maybe Int))
            ),
          testPPrint1
            "Maybe (LazyWriterT Int Maybe Int)"
            0
            ( Just $ WriterLazy.WriterT (Just (1, 2)) ::
                Maybe (WriterLazy.WriterT Int Maybe Int)
            )
            "Just (WriterT {runWriterT = Just (1, 2)})",
          testPPrint1
            "Maybe (StrictWriterT Int Maybe Int)"
            0
            ( Just $ WriterStrict.WriterT (Just (1, 2)) ::
                Maybe (WriterStrict.WriterT Int Maybe Int)
            )
            "Just (WriterT {runWriterT = Just (1, 2)})",
          propertyPFormatRead
            "Maybe (IdentityT Maybe Int)"
            ( Just . IdentityT <$> arbitrary ::
                Gen (Maybe (IdentityT Maybe Int))
            ),
          testGroup
            "HashSet"
            [ testPPrint1
                "Unbounded empty"
                0
                (HS.fromList [] :: HS.HashSet Int)
                "HashSet []",
              testPPrint1
                "Unbounded singleton"
                0
                (HS.fromList [1] :: HS.HashSet Int)
                "HashSet [1]",
              testPPrint1
                "Unbounded two elem"
                0
                (HS.fromList [1, 2] :: HS.HashSet Int)
                "HashSet [1, 2]",
              testPPrint1
                "Compact empty"
                1
                (HS.fromList [] :: HS.HashSet Int)
                "HashSet\n  []",
              testPPrint1
                "Compact singleton"
                1
                (HS.fromList [1] :: HS.HashSet Int)
                "HashSet\n  [ 1\n  ]",
              testPPrint1
                "Unbounded two elem"
                1
                (HS.fromList [1, 2] :: HS.HashSet Int)
                "HashSet\n  [ 1,\n    2\n  ]"
            ],
          testGroup
            "HashMap"
            [ testPPrint1
                "Unbounded empty"
                0
                (HM.fromList [] :: HM.HashMap Int Int)
                "HashMap []",
              testPPrint1
                "Unbounded singleton"
                0
                (HM.fromList [(1, 2)] :: HM.HashMap Int Int)
                "HashMap [(1, 2)]",
              testPPrint1
                "Unbounded two elem"
                0
                (HM.fromList [(1, 2), (3, 4)] :: HM.HashMap Int Int)
                "HashMap [(1, 2), (3, 4)]",
              testPPrint1
                "Compact empty"
                1
                (HM.fromList [] :: HM.HashMap Int Int)
                "HashMap\n  []",
              testPPrint1
                "Compact singleton"
                1
                (HM.fromList [(1, 2)] :: HM.HashMap Int Int)
                ( T.intercalate
                    "\n"
                    [ "HashMap",
                      "  [ ( 1,",
                      "      2",
                      "    )",
                      "  ]"
                    ]
                ),
              testPPrint1
                "Unbounded two elem"
                1
                (HM.fromList [(1, 2), (3, 4)] :: HM.HashMap Int Int)
                ( T.intercalate
                    "\n"
                    [ "HashMap",
                      "  [ ( 1,",
                      "      2",
                      "    ),",
                      "    ( 3,",
                      "      4",
                      "    )",
                      "  ]"
                    ]
                )
            ]
        ],
      testGroup
        "Symbolic types"
        [ testPPrint
            "enough space"
            80
            ("a" .&& "b" :: SymBool)
            "(&& a b)",
          testPPrint
            "not enough space"
            6
            ("a" .&& "b" :: SymBool)
            "..."
        ]
    ]
