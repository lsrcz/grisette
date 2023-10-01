{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Grisette.Core.Data.Class.GPrettyTests (gprettyTests) where

import Data.Int (Int16, Int32, Int64, Int8)
import Data.Text as T (Text, pack, unpack)
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.Generics (Generic)
import GHC.Stack (HasCallStack)
import Generics.Deriving (Default (Default))
import Grisette.Core.Data.BV
  ( IntN,
    SomeIntN (SomeIntN),
    SomeWordN (SomeWordN),
    WordN,
  )
import Grisette.Core.Data.Class.Bool (LogicalOp ((&&~)))
import Grisette.Core.Data.Class.GPretty (GPretty (gpretty))
import Grisette.IR.SymPrim.Data.SymPrim (SymBool)
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.HUnit ((@=?))
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
testGPretty n i a s =
  testCase n $
    renderStrict
      ( layoutPretty
          (LayoutOptions $ AvailablePerLine i 1)
          (gpretty a)
      )
      @=? s

propertyGPrettyShow ::
  forall a.
  (HasCallStack, GPretty a, Show a) =>
  String ->
  Gen a ->
  Test
propertyGPrettyShow n g =
  testProperty n $ forAll g $ \(a :: a) -> do
    renderStrict (layoutPretty (LayoutOptions Unbounded) (gpretty a)) == T.pack (show a)

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

propertyGPretty ::
  forall a.
  (HasCallStack, GPretty a, Read a, Show a, Eq a) =>
  String ->
  Gen a ->
  Test
propertyGPretty n g =
  testGroup
    n
    [ propertyGPrettyShow "single line" g,
      propertyGPrettyRead "compact" g
    ]

data I5 a b = a :-: b
  deriving (Generic, Show, Read, Eq)
  deriving (GPretty) via (Default (I5 a b))

infixl 5 :-:

data I6 a b = a :--: b
  deriving (Generic, Show, Read, Eq)
  deriving (GPretty) via (Default (I6 a b))

infixl 6 :--:

instance
  (Arbitrary a, Arbitrary b) =>
  Arbitrary (I5 a b)
  where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ a :-: b

instance
  (Arbitrary a, Arbitrary b) =>
  Arbitrary (I6 a b)
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

gprettyTests :: Test
gprettyTests =
  testGroup
    "GPretty"
    [ testGroup
        "simple tests"
        [ propertyGPretty "Bool" (arbitrary :: Gen Bool),
          propertyGPretty "Integer" (arbitrary :: Gen Integer),
          propertyGPretty "Int" (arbitrary :: Gen Int),
          propertyGPretty "Int8" (arbitrary :: Gen Int8),
          propertyGPretty "Int16" (arbitrary :: Gen Int16),
          propertyGPretty "Int32" (arbitrary :: Gen Int32),
          propertyGPretty "Int64" (arbitrary :: Gen Int64),
          propertyGPretty "Word" (arbitrary :: Gen Word),
          propertyGPretty "Word8" (arbitrary :: Gen Word8),
          propertyGPretty "Word16" (arbitrary :: Gen Word16),
          propertyGPretty "Word32" (arbitrary :: Gen Word32),
          propertyGPretty "Word64" (arbitrary :: Gen Word64),
          propertyGPrettyShow
            "SomeWordN"
            ( oneof
                [ SomeWordN <$> (arbitrary :: Gen (WordN 8)),
                  SomeWordN <$> (arbitrary :: Gen (WordN 9)),
                  SomeWordN <$> (arbitrary :: Gen (WordN 10))
                ]
            ),
          propertyGPretty "WordN 8" (arbitrary :: Gen (WordN 8)),
          propertyGPretty "WordN 9" (arbitrary :: Gen (WordN 9)),
          propertyGPrettyShow
            "SomeIntN"
            ( oneof
                [ SomeIntN <$> (arbitrary :: Gen (IntN 8)),
                  SomeIntN <$> (arbitrary :: Gen (IntN 9)),
                  SomeIntN <$> (arbitrary :: Gen (IntN 10))
                ]
            ),
          propertyGPretty "IntN 8" (arbitrary :: Gen (IntN 8)),
          propertyGPretty "IntN 9" (arbitrary :: Gen (IntN 9)),
          propertyGPretty "()" (arbitrary :: Gen ()),
          propertyGPretty
            "Either"
            ( arbitrary :: Gen (Either Int Bool)
            ),
          propertyGPretty
            "Maybe"
            ( arbitrary :: Gen (Maybe Int)
            ),
          propertyGPretty
            "List"
            ( arbitrary :: Gen [Int]
            ),
          propertyGPretty
            "(,)"
            ( arbitrary :: Gen (Int, Int)
            ),
          propertyGPretty
            "(,,)"
            ( arbitrary :: Gen (Int, Int, Int)
            ),
          propertyGPretty
            "(,,,)"
            ( arbitrary :: Gen (Int, Int, Int, Int)
            ),
          propertyGPretty
            "(,,,,)"
            ( arbitrary :: Gen (Int, Int, Int, Int, Int)
            ),
          propertyGPretty
            "(,,,,,)"
            ( arbitrary :: Gen (Int, Int, Int, Int, Int, Int)
            ),
          propertyGPretty
            "(,,,,,,)"
            ( arbitrary :: Gen (Int, Int, Int, Int, Int, Int, Int)
            ),
          propertyGPretty
            "(,,,,,,,)"
            ( arbitrary :: Gen (Int, Int, Int, Int, Int, Int, Int, Int)
            ),
          propertyGPretty
            "I5"
            ( arbitrary :: Gen (I5 Int Int)
            ),
          propertyGPretty
            "Record"
            ( arbitrary :: Gen (Record Int Int)
            )
        ],
      testGroup
        "Combined types"
        [ propertyGPretty
            "Maybe Maybe"
            ( arbitrary :: Gen (Maybe (Maybe Int))
            ),
          propertyGPretty
            "Maybe (,)"
            ( arbitrary :: Gen (Maybe (Int, Int))
            ),
          propertyGPretty
            "Maybe I5"
            ( arbitrary :: Gen (Maybe (I5 Int Int))
            ),
          propertyGPretty
            "Maybe []"
            ( arbitrary :: Gen (Maybe [Int])
            ),
          propertyGPretty
            "Maybe Record"
            ( arbitrary :: Gen (Maybe (Record Int Int))
            ),
          propertyGPretty
            "(Maybe,Either)"
            ( arbitrary :: Gen (Maybe Int, Either Int Int)
            ),
          propertyGPretty
            "((,),(,))"
            ( arbitrary :: Gen ((Int, Int), (Int, Int))
            ),
          propertyGPretty
            "(I5,I5)"
            ( arbitrary :: Gen (I5 Int Int, I5 Int Int)
            ),
          propertyGPretty
            "([],[])"
            ( arbitrary :: Gen ([Int], [Int])
            ),
          propertyGPretty
            "(Record,Record)"
            ( arbitrary :: Gen (Record Int Int, Record Int Int)
            ),
          propertyGPretty
            "I5 Maybe Either"
            ( arbitrary :: Gen (I5 (Maybe Int) (Either Int Int))
            ),
          propertyGPretty
            "I5 (,) (,)"
            ( arbitrary :: Gen (I5 (Int, Int) (Int, Int))
            ),
          propertyGPretty
            "I5 I6 I6"
            ( arbitrary :: Gen (I5 (I6 Int Int) (I6 Int Int))
            ),
          propertyGPretty
            "I5 I5 I5"
            ( arbitrary :: Gen (I5 (I5 Int Int) (I5 Int Int))
            ),
          propertyGPretty
            "I6 I5 I5"
            ( arbitrary :: Gen (I6 (I5 Int Int) (I5 Int Int))
            ),
          propertyGPretty
            "I6 I6 I6"
            ( arbitrary :: Gen (I6 (I6 Int Int) (I6 Int Int))
            ),
          propertyGPretty
            "I5 [] []"
            ( arbitrary :: Gen (I5 [Int] [Int])
            ),
          propertyGPretty
            "I5 Record Record"
            ( arbitrary :: Gen (I5 (Record Int Int) (Record Int Int))
            ),
          propertyGPretty
            "[Maybe]"
            ( arbitrary :: Gen [Maybe Int]
            ),
          propertyGPretty
            "[(,)]"
            ( arbitrary :: Gen [(Int, Int)]
            ),
          propertyGPretty
            "[I5]"
            ( arbitrary :: Gen [I5 Int Int]
            ),
          propertyGPretty
            "[[]]"
            ( arbitrary :: Gen [[Int]]
            ),
          propertyGPretty
            "[Record]"
            ( arbitrary :: Gen [Record Int Int]
            ),
          propertyGPretty
            "Record Maybe Either"
            ( arbitrary :: Gen (Record (Maybe Int) (Either Int Int))
            ),
          propertyGPretty
            "Record (,) (,)"
            ( arbitrary :: Gen (Record (Int, Int) (Int, Int))
            ),
          propertyGPretty
            "Record I5 I6"
            ( arbitrary :: Gen (Record (I5 Int Int) (I6 Int Int))
            ),
          propertyGPretty
            "Record []"
            ( arbitrary :: Gen (Record [Int] [Int])
            ),
          propertyGPretty
            "Record Record"
            ( arbitrary :: Gen (Record (Record Int Int) (Record Int Int))
            )
        ],
      testGroup
        "Symbolic types"
        [ testGPretty
            "enough space"
            80
            ("a" &&~ "b" :: SymBool)
            "(&& a b)",
          testGPretty
            "not enough space"
            6
            ("a" &&~ "b" :: SymBool)
            "..."
        ]
    ]
