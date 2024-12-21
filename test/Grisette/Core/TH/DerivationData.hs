{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -ddump-timings #-}

module Grisette.Core.TH.DerivationData
  ( T (..),
    concreteT,
    symbolicT,
    IdenticalFields (..),
    GGG (..),
    VVV (..),
    Extra (..),
    -- Extra0 (..),
    Ambiguous (..),
    replaceVVVShown,
    gggToVVV,
  )
where

import Control.DeepSeq (NFData, NFData1, NFData2)
import Control.Monad.Identity (Identity (Identity))
import Data.Functor.Classes
  ( Eq1 (liftEq),
    Eq2,
    Ord1,
    Ord2,
    Show1,
    Show2,
  )
import Data.Hashable (Hashable)
import Data.Hashable.Lifted (Hashable1, Hashable2)
import Data.Maybe (fromJust)
import qualified Data.Text as T
import Data.Typeable (Proxy, Typeable)
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
    PPrint,
    PPrint1,
    PPrint2,
    SubstSym,
    SubstSym1,
    SubstSym2,
    SymBool,
    SymEq,
    SymEq1,
    SymEq2,
    SymInteger,
    SymOrd,
    SymOrd1,
    SymOrd2,
    ToCon (toCon),
    ToSym (toSym),
    Union,
    derive,
    deriveAll,
    deriveGADT,
    deriveGADTAll,
  )
import Grisette.Core.TH.PartialEvalMode (PartialEvalMode)
import Grisette.Internal.TH.GADT.Common
  ( ExtraConstraint
      ( ExtraConstraint,
        bitSizeConstraint,
        evalModeConstraint,
        evalModeSpecificConstraint,
        fpBitSizeConstraint,
        needExtraMergeable
      ),
  )
import Grisette.Internal.TH.GADT.DeriveGADT (deriveGADTWith)
import Grisette.Unified
  ( EvalModeTag (C, S),
    GetBool,
    GetData,
    GetFP,
    GetWordN,
  )
import Test.QuickCheck (Arbitrary, oneof)
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

deriveGADTAll ''IdenticalFields

data Basic = Basic0 | Basic1 Int | Basic2 String [Int]

deriveGADT ''Basic [''Mergeable]

data Extra mode n eb sb a where
  Extra ::
    GetBool mode ->
    [GetWordN mode n] ->
    [GetData mode [Extra mode n eb sb a]] ->
    GetFP mode eb sb ->
    GetFP mode eb sb ->
    a ->
    a ->
    Extra mode n eb sb a

deriveGADTWith
  ( ExtraConstraint
      { evalModeConstraint = [(0, ''PartialEvalMode)],
        evalModeSpecificConstraint = [],
        bitSizeConstraint = [1],
        fpBitSizeConstraint = [(2, 3)],
        needExtraMergeable = True
      }
  )
  ''Extra
  [ ''Show,
    ''PPrint,
    ''Mergeable,
    ''EvalSym,
    ''ExtractSym,
    ''SubstSym,
    ''NFData,
    ''AllSyms,
    ''Eq,
    ''Ord,
    ''SymEq,
    ''SymOrd
  ]

data Expr f a where
  I :: SymInteger -> Expr f SymInteger
  B :: SymBool -> Expr f SymBool
  Add :: Union (Expr f SymInteger) -> Union (Expr f SymInteger) -> Expr f SymInteger
  Mul :: Union (Expr f SymInteger) -> Union (Expr f SymInteger) -> Expr f SymInteger
  Eq :: (BasicSymPrim a) => Union (Expr f a) -> Union (Expr f a) -> Expr f SymBool
  Eq3 ::
    (BasicSymPrim a) =>
    Union (Expr f a) ->
    Union (Expr f a) ->
    Union (Expr f b) ->
    Union (Expr f b) ->
    Expr f b
  WExpr ::
    (BasicSymPrim a, BasicSymPrim b, BasicSymPrim c) =>
    a ->
    b ->
    c ->
    d ->
    Expr f d
  XExpr :: f a -> Expr f a
  YExpr :: (BasicSymPrim a) => f a -> Expr f (f a)
  ZExpr ::
    ( AllSyms1 f,
      Mergeable1 f,
      Eq1 f,
      EvalSym1 f,
      ExtractSym1 f,
      SubstSym1 f,
      NFData1 f,
      PPrint1 f,
      Show1 f,
      Typeable f,
      Hashable1 f,
      BasicSymPrim a,
      SymEq1 f,
      SymOrd1 f
    ) =>
    f a ->
    Expr g b

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
    ''Show,
    ''Show1,
    ''PPrint,
    ''PPrint1,
    ''AllSyms,
    ''AllSyms1,
    ''Eq,
    ''SymEq,
    ''SymOrd
  ]

instance (Eq1 f) => Eq1 (Expr f) where
  liftEq = undefined

deriveGADT ''Expr [''Hashable, ''Hashable1]

data P a b = P a | Q Int

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
    ''AllSyms2,
    ''Eq,
    ''Eq1,
    ''Eq2,
    ''SymEq,
    ''SymEq1,
    ''SymEq2,
    ''Ord,
    ''Ord1,
    ''Ord2,
    ''SymOrd,
    ''SymOrd1,
    ''SymOrd2
  ]

data GGG a b where
  GGG2 :: a -> b -> GGG a b
  GGG1 :: a -> GGG a b
  GGG0 :: GGG a b
  GGGRec :: a -> b -> GGG a b
  (:|) :: a -> b -> GGG a b
  GGGLst :: [a] -> [b] -> GGG a b

infixr 5 :|

deriveGADT
  ''GGG
  [ ''Show,
    ''Show1,
    ''Show2,
    ''PPrint,
    ''PPrint1,
    ''PPrint2,
    ''Eq,
    ''Eq1,
    ''Eq2,
    ''Ord,
    ''Ord1,
    ''Ord2,
    ''SymEq,
    ''SymEq1,
    ''SymEq2,
    ''SymOrd,
    ''SymOrd1,
    ''SymOrd2
  ]

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

derive
  ''VVV
  [ ''Generic,
    ''PPrint,
    ''Eq,
    ''Ord,
    ''SymEq,
    ''SymOrd
  ]

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

data Ambiguous x where
  Ambiguous :: forall x a. (BasicSymPrim a) => Proxy a -> Ambiguous x

instance Eq (Ambiguous x) where
  (==) = undefined

deriveGADT
  ''Ambiguous
  [ ''AllSyms,
    ''Mergeable,
    ''ExtractSym,
    ''NFData,
    ''PPrint,
    ''Show,
    ''Hashable
  ]