{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -ddump-splices #-}

#if MIN_VERSION_base(4,17,0)
{-# LANGUAGE TypeAbstractions #-}
#endif

module Grisette.Core.TH.DerivationTest
  ( concreteT,
    symbolicT,
    GGG (..),
    VVV (..),
    derivationTest,
  )
where

import Control.DeepSeq (NFData, NFData1, NFData2)
import Control.Monad.Identity (Identity (Identity))
import Data.Functor.Classes
  ( Eq1 (liftEq),
    Eq2 (liftEq2),
    Show1,
    Show2,
    showsPrec1,
    showsPrec2,
  )
import Data.Hashable (Hashable)
import Data.Hashable.Lifted (Hashable1, Hashable2)
import Data.Maybe (fromJust)
import qualified Data.Text as T
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Grisette
  ( AllSyms,
    AllSyms1,
    AllSyms2,
    BasicSymPrim,
    Default (Default),
    EvalSym,
    EvalSym1,
    EvalSym2,
    ExtractSym,
    ExtractSym1,
    ExtractSym2,
    Mergeable,
    Mergeable1,
    Mergeable2,
    PPrint (pformat, pformatPrec),
    PPrint1,
    PPrint2,
    PlainUnion (toGuardedList),
    SubstSym,
    SubstSym1,
    SubstSym2,
    SymBool,
    SymInteger,
    ToCon (toCon),
    ToSym (toSym),
    Union,
    derive,
    deriveAll,
    deriveGADT,
    docToTextWithWidth,
    mrgIf,
    pformatPrec1,
    pformatPrec2,
  )
import Grisette.Unified (EvalModeTag (C, S), GetBool, GetData, GetWordN)
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.HUnit ((@?=))
import Test.QuickCheck (Arbitrary, oneof, (.&.), (===))
import Test.QuickCheck.Arbitrary (Arbitrary (arbitrary))

data T mode n a
  = T (GetBool mode) [GetWordN mode n] [a] (GetData mode (T mode n a))
  | TNil

deriveAll ''T

concreteT :: T 'C 10 Integer
concreteT =
  toSym (T True [10] [10 :: Integer] (Identity TNil) :: T 'C 10 Integer)

symbolicT :: T 'S 10 SymInteger
symbolicT = fromJust $ toCon (toSym concreteT :: T 'S 10 SymInteger)

newtype X mode = X [GetBool mode]

deriveAll ''X

data IdenticalFields mode n = IdenticalFields
  { a :: n,
    b :: n,
    c :: Maybe Int,
    d :: Maybe Int
  }

deriveAll ''IdenticalFields

data Basic = Basic0 | Basic1 Int | Basic2 String [Int]

deriveGADT ''Basic [''Mergeable]

data Expr f a where
  I :: SymInteger -> Expr f SymInteger
  B :: SymBool -> Expr f SymBool
  Add :: Union (Expr f SymInteger) -> Union (Expr f SymInteger) -> Expr f SymInteger
  Mul :: Union (Expr f SymInteger) -> Union (Expr f SymInteger) -> Expr f SymInteger
  Eq :: (BasicSymPrim a, Typeable a) => Union (Expr f a) -> Union (Expr f a) -> Expr f SymBool
  Eq3 ::
    (BasicSymPrim a, Typeable b) =>
    Union (Expr f a) ->
    Union (Expr f a) ->
    Union (Expr f b) ->
    Union (Expr f b) ->
    Expr f b
  XExpr :: f a -> Expr f a

instance (Eq1 f, Eq a) => Eq (Expr f a) where
  (==) = undefined

instance (Eq1 f) => Eq1 (Expr f) where
  liftEq = undefined

deriveGADT
  ''Expr
  [ ''Mergeable,
    ''Mergeable1,
    ''EvalSym,
    ''EvalSym1,
    ''ExtractSym,
    ''ExtractSym1,
    ''SubstSym,
    ''SubstSym1,
    ''NFData,
    ''NFData1,
    ''Hashable,
    ''Hashable1,
    ''Show,
    ''Show1,
    ''PPrint,
    ''PPrint1,
    ''AllSyms,
    ''AllSyms1
  ]

data P a b = P a | Q Int

instance (Eq a) => Eq (P a b) where
  (==) = undefined

instance (Eq a) => Eq1 (P a) where
  liftEq = undefined

instance Eq2 P where
  liftEq2 = undefined

deriveGADT
  ''P
  [ ''Mergeable,
    ''Mergeable1,
    ''Mergeable2,
    ''EvalSym,
    ''EvalSym1,
    ''EvalSym2,
    ''ExtractSym,
    ''ExtractSym1,
    ''ExtractSym2,
    ''SubstSym,
    ''SubstSym1,
    ''SubstSym2,
    ''NFData,
    ''NFData1,
    ''NFData2,
    ''Hashable,
    ''Hashable1,
    ''Hashable2,
    ''Show,
    ''Show1,
    ''Show2,
    ''PPrint,
    ''PPrint1,
    ''PPrint2,
    ''AllSyms,
    ''AllSyms1,
    ''AllSyms2
  ]

data GGG a b where
  GGG2 :: a -> b -> GGG a b
  GGG1 :: a -> GGG a b
  GGG0 :: GGG a b
  GGGRec :: a -> b -> GGG a b
  (:|) :: a -> b -> GGG a b
  GGGLst :: [a] -> [b] -> GGG a b

infixr 5 :|

deriveGADT ''GGG [''Show, ''Show1, ''Show2, ''PPrint, ''PPrint1, ''PPrint2]

instance (Arbitrary a, Arbitrary b) => Arbitrary (GGG a b) where
  arbitrary =
    oneof
      [ GGG2 <$> arbitrary <*> arbitrary,
        GGG1 <$> arbitrary,
        return GGG0,
        GGGRec <$> arbitrary <*> arbitrary,
        (:|) <$> arbitrary <*> arbitrary,
        GGGLst <$> arbitrary <*> arbitrary
      ]

gggToVVV :: GGG a b -> VVV a b
gggToVVV (GGG2 a b) = VVV2 a b
gggToVVV (GGG1 a) = VVV1 a
gggToVVV GGG0 = VVV0
gggToVVV (GGGRec a b) = VVVRec a b
gggToVVV (a :| b) = a :. b
gggToVVV (GGGLst a b) = VVVLst a b

data VVV a b where
  VVV2 :: a -> b -> VVV a b
  VVV1 :: a -> VVV a b
  VVV0 :: VVV a b
  VVVRec :: a -> b -> VVV a b
  (:.) :: a -> b -> VVV a b
  VVVLst :: [a] -> [b] -> VVV a b

infixr 5 :.

derive ''VVV [''Generic, ''PPrint]

instance (Arbitrary a, Arbitrary b) => Arbitrary (VVV a b) where
  arbitrary =
    oneof
      [ VVV2 <$> arbitrary <*> arbitrary,
        VVV1 <$> arbitrary,
        return VVV0,
        VVVRec <$> arbitrary <*> arbitrary,
        (:.) <$> arbitrary <*> arbitrary
      ]

replaceVVVShown :: T.Text -> T.Text
replaceVVVShown =
  T.replace ":." ":|" . T.replace "VVV" "GGG" . T.replace "vvv" "ggg"

deriving instance (Show a, Show b) => Show (VVV a b)

#if MIN_VERSION_base(4,17,0)
data Ambiguous where
  Ambiguous :: forall a. (BasicSymPrim a) => Ambiguous

deriveGADT ''Ambiguous [''Mergeable, ''Show, ''PPrint]

extraTest :: [Test]
extraTest =
  [ testCase "Ambiguous existential variable" $ do
      let a = Ambiguous @SymInteger :: Ambiguous
      let b = Ambiguous @SymBool :: Ambiguous
      let different =
            toGuardedList (mrgIf "a" (return a) (return b) :: Union Ambiguous)
      let same =
            toGuardedList (mrgIf "a" (return a) (return a) :: Union Ambiguous)
      length different @?= 2
      length same @?= 1
  ]
#else
extraTest :: [Test]
extraTest = []
#endif

derivationTest :: Test
derivationTest =
  testGroup
    "Derivation"
    $ [ testProperty "GADT Show instance for regular types" $
          \(g :: GGG (GGG Int String) [Int]) ->
            let v = gggToVVV g
             in replaceVVVShown (T.pack (show g))
                  === replaceVVVShown (T.pack (show v)),
        testProperty "GADT Show and Show1 are consistent" $
          \(g :: GGG (GGG Char String) [Int]) ->
            T.pack (show g) === T.pack (showsPrec1 0 g "")
              .&. T.pack (showsPrec 11 g "") === T.pack (showsPrec1 11 g ""),
        testProperty "GADT Show and Show2 are consistent" $
          \(g :: GGG (GGG Char String) [Int]) ->
            T.pack (show g) === T.pack (showsPrec2 0 g "")
              .&. T.pack (showsPrec 11 g "") === T.pack (showsPrec2 11 g ""),
        testProperty "GADT PPrint instance for regular types" $
          \(g :: GGG (GGG Int String) [Int]) ->
            let v = gggToVVV g
             in replaceVVVShown (docToTextWithWidth 1000 (pformat g))
                  === replaceVVVShown (docToTextWithWidth 1000 (pformat v))
                  .&. replaceVVVShown (docToTextWithWidth 0 (pformat g))
                    === replaceVVVShown (docToTextWithWidth 0 (pformat v)),
        testProperty "GADT PPrint and PPrint1 are consistent" $
          \(g :: GGG (GGG Char String) [Int]) ->
            docToTextWithWidth 1000 (pformatPrec 0 g)
              === docToTextWithWidth 1000 (pformatPrec1 0 g)
              .&. docToTextWithWidth 1000 (pformatPrec 11 g)
                === docToTextWithWidth 1000 (pformatPrec1 11 g)
              .&. docToTextWithWidth 0 (pformatPrec 0 g)
                === docToTextWithWidth 0 (pformatPrec1 0 g)
              .&. docToTextWithWidth 0 (pformatPrec 11 g)
                === docToTextWithWidth 0 (pformatPrec1 11 g),
        testProperty "GADT PPrint and PPrint2 are consistent" $
          \(g :: GGG (GGG Char String) [Int]) ->
            docToTextWithWidth 1000 (pformatPrec 0 g)
              === docToTextWithWidth 1000 (pformatPrec2 0 g)
              .&. docToTextWithWidth 1000 (pformatPrec 11 g)
                === docToTextWithWidth 1000 (pformatPrec2 11 g)
              .&. docToTextWithWidth 0 (pformatPrec 0 g)
                === docToTextWithWidth 0 (pformatPrec2 0 g)
              .&. docToTextWithWidth 0 (pformatPrec 11 g)
                === docToTextWithWidth 0 (pformatPrec2 11 g)
      ]
      ++ extraTest
