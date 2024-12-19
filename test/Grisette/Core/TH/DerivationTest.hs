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
import Data.Functor.Classes (Eq1 (liftEq), Eq2 (liftEq2), Show1, Show2, showsPrec1, showsPrec2)
import Data.Hashable (Hashable)
import Data.Hashable.Lifted (Hashable1, Hashable2)
import Data.Maybe (fromJust)
import qualified Data.Text as T
import Data.Typeable (Typeable)
import Grisette
  ( BasicSymPrim,
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
    SubstSym,
    SubstSym1,
    SymBool,
    SymInteger,
    ToCon (toCon),
    ToSym (toSym),
    Union,
    deriveAll,
    deriveGADT,
  )
import Grisette.Unified (EvalModeTag (C, S), GetBool, GetData, GetWordN)
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck (Arbitrary, Gen, oneof, (===))
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
    ''NFData,
    ''NFData1,
    ''SubstSym,
    ''SubstSym1,
    ''Hashable,
    ''Hashable1
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
    ''NFData,
    ''NFData1,
    ''NFData2,
    ''Hashable,
    ''Hashable1,
    ''Hashable2,
    ''Show
  ]

data GGG a b where
  GGG2 :: a -> b -> GGG a b
  GGG1 :: a -> GGG a b
  GGG0 :: GGG a b
  GGGRec :: a -> b -> GGG a b
  (:|) :: a -> b -> GGG a b
  GGGLst :: [a] -> [b] -> GGG a b
  GGGExistential ::
    forall x a b. (Show x) => x -> a -> b -> GGG a b

infixr 5 :|

deriveGADT ''GGG [''Show, ''Show1, ''Show2]

instance (Arbitrary a, Arbitrary b) => Arbitrary (GGG a b) where
  arbitrary =
    oneof
      [ GGG2 <$> arbitrary <*> arbitrary,
        GGG1 <$> arbitrary,
        return GGG0,
        GGGRec <$> arbitrary <*> arbitrary,
        (:|) <$> arbitrary <*> arbitrary,
        GGGLst <$> arbitrary <*> arbitrary,
        GGGExistential <$> (arbitrary :: Gen Int) <*> arbitrary <*> arbitrary
      ]

gggToVVV :: GGG a b -> VVV a b
gggToVVV (GGG2 a b) = VVV2 a b
gggToVVV (GGG1 a) = VVV1 a
gggToVVV GGG0 = VVV0
gggToVVV (GGGRec a b) = VVVRec a b
gggToVVV (a :| b) = a :. b
gggToVVV (GGGLst a b) = VVVLst a b
gggToVVV (GGGExistential x a b) = VVVExistential x a b

data VVV a b where
  VVV2 :: a -> b -> VVV a b
  VVV1 :: a -> VVV a b
  VVV0 :: VVV a b
  VVVRec :: a -> b -> VVV a b
  (:.) :: a -> b -> VVV a b
  VVVLst :: [a] -> [b] -> VVV a b
  VVVExistential :: forall x a b. (Show x) => x -> a -> b -> VVV a b

infixr 5 :.

instance (Arbitrary a, Arbitrary b) => Arbitrary (VVV a b) where
  arbitrary =
    oneof
      [ VVV2 <$> arbitrary <*> arbitrary,
        VVV1 <$> arbitrary,
        return VVV0,
        VVVRec <$> arbitrary <*> arbitrary,
        (:.) <$> arbitrary <*> arbitrary,
        VVVExistential <$> (arbitrary :: Gen Int) <*> arbitrary <*> arbitrary
      ]

replaceVVVShown :: T.Text -> T.Text
replaceVVVShown =
  T.replace ":." ":|" . T.replace "VVV" "GGG" . T.replace "vvv" "ggg"

deriving instance (Show a, Show b) => Show (VVV a b)

derivationTest :: Test
derivationTest =
  testGroup
    "Derivation"
    [ testProperty "GADT show instance for regular types" $
        \(g :: GGG (GGG Int String) [Int]) ->
          let v = gggToVVV g
           in replaceVVVShown (T.pack (show g))
                === replaceVVVShown (T.pack (show v)),
      testProperty "GADT show and show1 are consistent" $
        \(g :: GGG (GGG Char String) [Int]) ->
          T.pack (show g) === T.pack (showsPrec1 0 g ""),
      testProperty "GADT show and show1 are consistent for prec 11" $
        \(g :: GGG (GGG Char String) [Int]) ->
          T.pack (showsPrec 11 g "") === T.pack (showsPrec1 11 g ""),
      testProperty "GADT show and show2 are consistent" $
        \(g :: GGG (GGG Char String) [Int]) ->
          T.pack (showsPrec 11 g "") === T.pack (showsPrec2 11 g ""),
      testProperty "GADT show and show2 are consistent for prec 11" $
        \(g :: GGG (GGG Char String) [Int]) ->
          T.pack (showsPrec 11 g "") === T.pack (showsPrec2 11 g "")
    ]
