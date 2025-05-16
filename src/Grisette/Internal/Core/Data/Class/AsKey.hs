{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Grisette.Internal.Core.Data.Class.AsKey
  ( KeyEq (..),
    KeyOrd (..),
    KeyHashable (..),
    KeyEq1 (..),
    KeyOrd1 (..),
    KeyHashable1 (..),
    AsKey (..),
    AsKey1 (..),
    shouldUseAsKeyError,
    shouldUseAsKeyHasSymbolicVersionError,
    shouldUseSymbolicVersionError,
  )
where

import Control.DeepSeq (NFData, NFData1)
import Control.Monad.Identity (Identity)
import qualified Data.Binary as Binary
import Data.Bits (Bits, FiniteBits)
import qualified Data.Bytes.Serial as Serial
import Data.Functor.Classes
  ( Eq1 (liftEq),
    Ord1 (liftCompare),
    Show1,
    compare1,
    eq1,
  )
import Data.Hashable (Hashable (hashWithSalt))
import Data.Hashable.Lifted (Hashable1 (liftHashWithSalt), hashWithSalt1)
import Data.Proxy (Proxy (Proxy))
import qualified Data.Serialize as Cereal
import Data.String (IsString)
import GHC.Stack (HasCallStack)
import GHC.TypeLits (KnownNat, type (<=))
import Grisette.Internal.Core.Data.Class.BitCast
  ( BitCast (bitCast),
    BitCastCanonical (bitCastCanonicalValue),
  )
import Grisette.Internal.Core.Data.Class.BitVector
  ( BV (bv, bvConcat, bvExt, bvSelect, bvSext, bvZext),
  )
import Grisette.Internal.Core.Data.Class.Concrete (Concrete)
import Grisette.Internal.Core.Data.Class.Function
  ( Apply (FunType, apply),
    Function ((#)),
  )
import Grisette.Internal.Core.Data.Class.IEEEFP
  ( IEEEFPConstants
      ( fpMaxNormalized,
        fpMaxSubnormal,
        fpMinNormalized,
        fpMinSubnormal,
        fpNaN,
        fpNegativeInfinite,
        fpNegativeZero,
        fpPositiveInfinite,
        fpPositiveZero
      ),
    IEEEFPConvertible (fromFPOr, toFP),
    IEEEFPOp
      ( fpAbs,
        fpMaximum,
        fpMaximumNumber,
        fpMinimum,
        fpMinimumNumber,
        fpNeg,
        fpRem
      ),
    IEEEFPRoundingMode (rna, rne, rtn, rtp, rtz),
    IEEEFPRoundingOp
      ( fpAdd,
        fpDiv,
        fpFMA,
        fpMul,
        fpRoundToIntegral,
        fpSqrt,
        fpSub
      ),
    IEEEFPToAlgReal (fpToAlgReal),
  )
import Grisette.Internal.Core.Data.Class.SignConversion
  ( SignConversion (toSigned, toUnsigned),
  )
import Grisette.Internal.SymPrim.AlgReal (AlgReal)
import Grisette.Internal.SymPrim.BV (IntN, WordN)
import Grisette.Internal.SymPrim.FP (FP, ValidFP)
import Grisette.Internal.SymPrim.Prim.Internal.Term (ConRep (ConType))
import Language.Haskell.TH.Syntax (Lift)

class KeyEq a where
  keyEq :: a -> a -> Bool

infix 4 `keyEq`

instance (KnownNat n, 1 <= n) => KeyEq (WordN n) where
  keyEq = (==)

instance (KnownNat n, 1 <= n) => KeyEq (IntN n) where
  keyEq = (==)

instance KeyEq Integer where
  keyEq = (==)

instance KeyEq Bool where
  keyEq = (==)

instance KeyEq AlgReal where
  keyEq = (==)

instance (ValidFP a b) => KeyEq (FP a b) where
  keyEq = (==)

instance (Eq a) => KeyEq (Identity a) where
  keyEq = (==)

class (KeyEq a) => KeyOrd a where
  keyCompare :: a -> a -> Ordering

infix 4 `keyCompare`

class (KeyEq a) => KeyHashable a where
  keyHashWithSalt :: Int -> a -> Int

instance (KnownNat n, 1 <= n) => KeyHashable (WordN n) where
  keyHashWithSalt = hashWithSalt

instance (KnownNat n, 1 <= n) => KeyHashable (IntN n) where
  keyHashWithSalt = hashWithSalt

instance KeyHashable Integer where
  keyHashWithSalt = hashWithSalt

instance KeyHashable Bool where
  keyHashWithSalt = hashWithSalt

instance KeyHashable AlgReal where
  keyHashWithSalt = hashWithSalt

instance (ValidFP a b) => KeyHashable (FP a b) where
  keyHashWithSalt = hashWithSalt

instance (Hashable a) => KeyHashable (Identity a) where
  keyHashWithSalt = hashWithSalt

class KeyEq1 f where
  liftKeyEq :: (a -> b -> Bool) -> f a -> f b -> Bool

class (KeyEq1 f) => KeyOrd1 f where
  liftKeyCompare :: (a -> b -> Ordering) -> f a -> f b -> Ordering

class (KeyEq1 f) => KeyHashable1 f where
  liftKeyHashWithSalt :: (Int -> a -> Int) -> Int -> f a -> Int

infixl 0 `keyHashWithSalt`

newtype AsKey a = AsKey {getAsKey :: a}
  deriving newtype
    ( Binary.Binary,
      Cereal.Serialize,
      NFData,
      IsString,
      Show,
      Num,
      Bits,
      FiniteBits,
      Enum,
      Bounded,
      Fractional,
      Floating
    )
  deriving stock (Functor, Lift)

newtype AsKey1 f a = AsKey1 {getAsKey1 :: f a}
  deriving newtype
    ( Binary.Binary,
      Cereal.Serialize,
      IsString,
      Show,
      Show1,
      Functor,
      NFData,
      NFData1,
      Applicative,
      Monad,
      Num
    )
  deriving stock (Lift)

instance (Serial.Serial a) => Serial.Serial (AsKey a) where
  serialize = Serial.serialize . getAsKey
  deserialize = AsKey <$> Serial.deserialize

instance (KeyEq a) => Eq (AsKey a) where
  (AsKey a) == (AsKey b) = keyEq a b

instance (KeyOrd a) => Ord (AsKey a) where
  compare (AsKey a) (AsKey b) = keyCompare a b

instance (KeyHashable a) => Hashable (AsKey a) where
  hashWithSalt salt = keyHashWithSalt salt . getAsKey

instance (KeyEq1 f, Eq a) => Eq (AsKey1 f a) where
  (==) = eq1

instance (KeyEq1 f) => Eq1 (AsKey1 f) where
  liftEq f (AsKey1 a) (AsKey1 b) = liftKeyEq f a b

instance (KeyOrd1 f, Ord a) => Ord (AsKey1 f a) where
  compare = compare1

instance (KeyOrd1 f) => Ord1 (AsKey1 f) where
  liftCompare f (AsKey1 a) (AsKey1 b) = liftKeyCompare f a b

instance (KeyHashable1 f, Hashable a) => Hashable (AsKey1 f a) where
  hashWithSalt = hashWithSalt1

instance (KeyHashable1 f) => Hashable1 (AsKey1 f) where
  liftHashWithSalt f salt (AsKey1 a) = liftKeyHashWithSalt f salt a

shouldUseAsKeyError :: (HasCallStack) => String -> String -> a
shouldUseAsKeyError typ op =
  error $
    "As "
      <> typ
      <> " is a symbolic type, "
      <> op
      <> " is likely not going to work as expected.\n"
      <> "You should use AsKey if you do want term identity based "
      <> op
      <> " on "
      <> typ
      <> "."

shouldUseAsKeyHasSymbolicVersionError ::
  (HasCallStack) => String -> String -> String -> a
shouldUseAsKeyHasSymbolicVersionError typ op symop =
  error $
    "As "
      <> typ
      <> " is a symbolic type, "
      <> op
      <> " is likely not going to work as expected.\n"
      <> "You should use AsKey if you do want term identity based "
      <> op
      <> " on "
      <> typ
      <> ",\n or use "
      <> symop
      <> " instead if you want symbolic version of "
      <> op
      <> "."

shouldUseSymbolicVersionError ::
  (HasCallStack) => String -> String -> String -> a
shouldUseSymbolicVersionError typ op symop =
  error $
    "As "
      <> typ
      <> " is a symbolic type, "
      <> op
      <> " is likely not going to work as expected.\n"
      <> "You should use "
      <> symop
      <> " instead if you want symbolic version of "
      <> op
      <> "."

instance (Function a arg res) => Function (AsKey a) arg res where
  (AsKey a) # b = a # b

instance
  (Function (f a) arg (f res)) =>
  Function (AsKey1 f a) arg (AsKey1 f res)
  where
  (AsKey1 f) # b = AsKey1 $ f # b

instance (Apply a) => Apply (AsKey a) where
  type FunType (AsKey a) = FunType a
  apply (AsKey a) = apply a

instance (ConRep a) => ConRep (AsKey a) where
  type ConType (AsKey a) = ConType a

instance {-# INCOHERENT #-} (BitCast a b) => BitCast (AsKey a) (AsKey b) where
  bitCast (AsKey a) = AsKey $ bitCast a
  {-# INLINE bitCast #-}

instance {-# INCOHERENT #-} (BitCast a b) => BitCast a (AsKey b) where
  bitCast a = AsKey $ bitCast a
  {-# INLINE bitCast #-}

instance {-# INCOHERENT #-} (BitCast a b) => BitCast (AsKey a) b where
  bitCast (AsKey a) = bitCast a
  {-# INLINE bitCast #-}

instance
  {-# INCOHERENT #-}
  (BitCastCanonical a b) =>
  BitCastCanonical (AsKey a) (AsKey b)
  where
  bitCastCanonicalValue _ = AsKey $ bitCastCanonicalValue (Proxy @a)

instance
  {-# INCOHERENT #-}
  (BitCastCanonical a b) =>
  BitCastCanonical (AsKey a) b
  where
  bitCastCanonicalValue _ = bitCastCanonicalValue (Proxy @a)

instance
  {-# INCOHERENT #-}
  (BitCastCanonical a b) =>
  BitCastCanonical a (AsKey b)
  where
  bitCastCanonicalValue p = AsKey $ bitCastCanonicalValue p

instance (SignConversion a b) => SignConversion (AsKey a) (AsKey b) where
  toSigned (AsKey a) = AsKey $ toSigned a
  toUnsigned (AsKey a) = AsKey $ toUnsigned a

instance (IEEEFPConstants a) => IEEEFPConstants (AsKey a) where
  fpPositiveInfinite = AsKey fpPositiveInfinite
  fpNegativeInfinite = AsKey fpNegativeInfinite
  fpNaN = AsKey fpNaN
  fpNegativeZero = AsKey fpNegativeZero
  fpPositiveZero = AsKey fpPositiveZero
  fpMinNormalized = AsKey fpMinNormalized
  fpMinSubnormal = AsKey fpMinSubnormal
  fpMaxNormalized = AsKey fpMaxNormalized
  fpMaxSubnormal = AsKey fpMaxSubnormal

instance (IEEEFPOp a) => IEEEFPOp (AsKey a) where
  fpAbs (AsKey a) = AsKey $ fpAbs a
  fpNeg (AsKey a) = AsKey $ fpNeg a
  fpRem (AsKey a) (AsKey b) = AsKey $ fpRem a b
  fpMinimum (AsKey a) (AsKey b) = AsKey $ fpMinimum a b
  fpMaximum (AsKey a) (AsKey b) = AsKey $ fpMaximum a b
  fpMinimumNumber (AsKey a) (AsKey b) = AsKey $ fpMinimumNumber a b
  fpMaximumNumber (AsKey a) (AsKey b) = AsKey $ fpMaximumNumber a b

instance (IEEEFPRoundingMode a) => IEEEFPRoundingMode (AsKey a) where
  rne = AsKey rne
  rna = AsKey rna
  rtp = AsKey rtp
  rtn = AsKey rtn
  rtz = AsKey rtz

instance
  (IEEEFPRoundingOp a mode) =>
  IEEEFPRoundingOp (AsKey a) (AsKey mode)
  where
  fpAdd (AsKey mode) (AsKey a) (AsKey b) = AsKey $ fpAdd mode a b
  fpSub (AsKey mode) (AsKey a) (AsKey b) = AsKey $ fpSub mode a b
  fpMul (AsKey mode) (AsKey a) (AsKey b) = AsKey $ fpMul mode a b
  fpDiv (AsKey mode) (AsKey a) (AsKey b) = AsKey $ fpDiv mode a b
  fpFMA (AsKey mode) (AsKey a) (AsKey b) (AsKey c) = AsKey $ fpFMA mode a b c
  fpSqrt (AsKey mode) (AsKey a) = AsKey $ fpSqrt mode a
  fpRoundToIntegral (AsKey mode) (AsKey a) = AsKey $ fpRoundToIntegral mode a

instance
  {-# INCOHERENT #-}
  (IEEEFPConvertible a fp mode) =>
  IEEEFPConvertible (AsKey a) (AsKey fp) (AsKey mode)
  where
  fromFPOr (AsKey d) (AsKey mode) (AsKey fp) = AsKey $ fromFPOr d mode fp
  toFP (AsKey mode) (AsKey a) = AsKey $ toFP mode a

instance
  {-# INCOHERENT #-}
  (IEEEFPConvertible a fp mode) =>
  IEEEFPConvertible a (AsKey fp) (AsKey mode)
  where
  fromFPOr a (AsKey mode) (AsKey fp) = fromFPOr a mode fp
  toFP (AsKey mode) a = AsKey $ toFP mode a

instance
  {-# INCOHERENT #-}
  (IEEEFPConvertible a fp mode) =>
  IEEEFPConvertible (AsKey a) fp mode
  where
  fromFPOr (AsKey a) mode fp = AsKey $ fromFPOr a mode fp
  toFP mode (AsKey a) = toFP mode a

instance
  {-# INCOHERENT #-}
  (IEEEFPToAlgReal a fp mode) =>
  IEEEFPToAlgReal (AsKey a) (AsKey fp) (AsKey mode)
  where
  fpToAlgReal (AsKey d) (AsKey fp) = AsKey $ fpToAlgReal d fp

instance
  {-# INCOHERENT #-}
  (IEEEFPToAlgReal a fp mode) =>
  IEEEFPToAlgReal a (AsKey fp) (AsKey mode)
  where
  fpToAlgReal a (AsKey fp) = fpToAlgReal a fp

instance
  {-# INCOHERENT #-}
  (IEEEFPToAlgReal a fp mode) =>
  IEEEFPToAlgReal (AsKey a) fp mode
  where
  fpToAlgReal (AsKey d) fp = AsKey $ fpToAlgReal d fp

instance Concrete (AsKey a)

instance (BV a) => BV (AsKey a) where
  bvConcat (AsKey a) (AsKey b) = AsKey $ bvConcat a b
  bvZext n (AsKey a) = AsKey $ bvZext n a
  bvSext n (AsKey a) = AsKey $ bvSext n a
  bvExt n (AsKey a) = AsKey $ bvExt n a
  bvSelect ix w (AsKey a) = AsKey $ bvSelect ix w a
  bv n a = AsKey $ bv n a
