{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      :   Grisette.Core.Data.Class.SOrd
-- Copyright   :   (c) Sirui Lu 2021-2023
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Core.Data.Class.SOrd
  ( -- * Symbolic total order relation
    SOrd (..),
    SOrd' (..),
  )
where

import Control.Monad.Except (ExceptT (ExceptT))
import Control.Monad.Identity
  ( Identity (Identity),
    IdentityT (IdentityT),
  )
import Control.Monad.Trans.Maybe (MaybeT (MaybeT))
import qualified Control.Monad.Writer.Lazy as WriterLazy
import qualified Control.Monad.Writer.Strict as WriterStrict
import qualified Data.ByteString as B
import Data.Functor.Sum (Sum)
import Data.Int (Int16, Int32, Int64, Int8)
import qualified Data.Text as T
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.TypeLits (KnownNat, type (<=))
import Generics.Deriving
  ( Default (Default),
    Generic (Rep, from),
    K1 (K1),
    M1 (M1),
    U1,
    V1,
    type (:*:) ((:*:)),
    type (:+:) (L1, R1),
  )
import Grisette.Core.Control.Exception (AssertionError, VerificationConditions)
import Grisette.Core.Control.Monad.UnionM (UnionM, liftToMonadUnion)
import Grisette.Core.Data.BV (IntN, SomeIntN, SomeWordN, WordN)
import Grisette.Core.Data.Class.LogicalOp (LogicalOp (symNot, (.&&), (.||)))
import Grisette.Core.Data.Class.Mergeable (Mergeable)
import Grisette.Core.Data.Class.SEq (SEq ((./=), (.==)), SEq' ((..==)))
import Grisette.Core.Data.Class.SimpleMergeable
  ( mrgIf,
  )
import Grisette.Core.Data.Class.Solvable (Solvable (con))
import Grisette.Core.Data.Class.TryMerge
  ( mrgPure,
    tryMerge,
  )
import Grisette.Core.Data.Class.UnionLike
  ( simpleMerge,
  )
import Grisette.IR.SymPrim.Data.Prim.PartialEval.Num
  ( pevalGeNumTerm,
    pevalGtNumTerm,
    pevalLeNumTerm,
    pevalLtNumTerm,
  )
import Grisette.IR.SymPrim.Data.SymPrim
  ( SomeSymIntN,
    SomeSymWordN,
    SymBool (SymBool),
    SymIntN (SymIntN),
    SymInteger (SymInteger),
    SymWordN (SymWordN),
    binSomeSymIntN,
    binSomeSymWordN,
  )

-- $setup
-- >>> import Grisette.Core
-- >>> import Grisette.IR.SymPrim
-- >>> :set -XDataKinds
-- >>> :set -XBinaryLiterals
-- >>> :set -XFlexibleContexts
-- >>> :set -XFlexibleInstances
-- >>> :set -XFunctionalDependencies

-- | Symbolic total order. Note that we can't use Haskell's 'Ord' class since
-- symbolic comparison won't necessarily return a concrete 'Bool' or 'Ordering'
-- value.
--
-- >>> let a = 1 :: SymInteger
-- >>> let b = 2 :: SymInteger
-- >>> a .< b
-- true
-- >>> a .> b
-- false
--
-- >>> let a = "a" :: SymInteger
-- >>> let b = "b" :: SymInteger
-- >>> a .< b
-- (< a b)
-- >>> a .<= b
-- (<= a b)
-- >>> a .> b
-- (< b a)
-- >>> a .>= b
-- (<= b a)
--
-- For `symCompare`, `Ordering` is not a solvable type, and the result would
-- be wrapped in a union-like monad. See `Grisette.Core.Control.Monad.UnionMBase` and `UnionLike` for more
-- information.
--
-- >>> a `symCompare` b :: UnionM Ordering -- UnionM is UnionMBase specialized with SymBool
-- {If (< a b) LT (If (= a b) EQ GT)}
--
-- __Note:__ This type class can be derived for algebraic data types.
-- You may need the @DerivingVia@ and @DerivingStrategies@ extensions.
--
-- > data X = ... deriving Generic deriving SOrd via (Default X)
class (SEq a) => SOrd a where
  (.<) :: a -> a -> SymBool
  infix 4 .<
  (.<=) :: a -> a -> SymBool
  infix 4 .<=
  (.>) :: a -> a -> SymBool
  infix 4 .>
  (.>=) :: a -> a -> SymBool
  infix 4 .>=
  x .< y = x .<= y .&& x ./= y
  x .> y = y .< x
  x .>= y = y .<= x
  symCompare :: a -> a -> UnionM Ordering
  symCompare l r =
    mrgIf
      (l .< r)
      (mrgPure LT)
      (mrgIf (l .== r) (mrgPure EQ) (mrgPure GT))
  {-# MINIMAL (.<=) #-}

instance (SEq a, Generic a, SOrd' (Rep a)) => SOrd (Default a) where
  (Default l) .<= (Default r) = l `derivedSymLe` r
  (Default l) .< (Default r) = l `derivedSymLt` r
  (Default l) .>= (Default r) = l `derivedSymGe` r
  (Default l) .> (Default r) = l `derivedSymGt` r
  symCompare (Default l) (Default r) = derivedSymCompare l r

#define CONCRETE_SORD(type) \
instance SOrd type where \
  l .<= r = con $ l <= r; \
  l .< r = con $ l < r; \
  l .>= r = con $ l >= r; \
  l .> r = con $ l > r; \
  symCompare l r = mrgPure $ compare l r

#define CONCRETE_SORD_BV(type) \
instance (KnownNat n, 1 <= n) => SOrd (type n) where \
  l .<= r = con $ l <= r; \
  l .< r = con $ l < r; \
  l .>= r = con $ l >= r; \
  l .> r = con $ l > r; \
  symCompare l r = mrgPure $ compare l r

#if 1
CONCRETE_SORD(Bool)
CONCRETE_SORD(Integer)
CONCRETE_SORD(Char)
CONCRETE_SORD(Int)
CONCRETE_SORD(Int8)
CONCRETE_SORD(Int16)
CONCRETE_SORD(Int32)
CONCRETE_SORD(Int64)
CONCRETE_SORD(Word)
CONCRETE_SORD(Word8)
CONCRETE_SORD(Word16)
CONCRETE_SORD(Word32)
CONCRETE_SORD(Word64)
CONCRETE_SORD(SomeWordN)
CONCRETE_SORD(SomeIntN)
CONCRETE_SORD(B.ByteString)
CONCRETE_SORD(T.Text)
CONCRETE_SORD_BV(WordN)
CONCRETE_SORD_BV(IntN)
#endif

symCompareSingleList :: (SOrd a) => Bool -> Bool -> [a] -> [a] -> SymBool
symCompareSingleList isLess isStrict = go
  where
    go [] [] = con (not isStrict)
    go (x : xs) (y : ys) = (if isLess then x .< y else x .> y) .|| (x .== y .&& go xs ys)
    go [] _ = if isLess then con True else con False
    go _ [] = if isLess then con False else con True

symCompareList :: (SOrd a) => [a] -> [a] -> UnionM Ordering
symCompareList [] [] = mrgPure EQ
symCompareList (x : xs) (y : ys) = do
  oxy <- symCompare x y
  case oxy of
    LT -> mrgPure LT
    EQ -> symCompareList xs ys
    GT -> mrgPure GT
symCompareList [] _ = mrgPure LT
symCompareList _ [] = mrgPure GT

instance (SOrd a) => SOrd [a] where
  (.<=) = symCompareSingleList True False
  (.<) = symCompareSingleList True True
  (.>=) = symCompareSingleList False False
  (.>) = symCompareSingleList False True
  symCompare = symCompareList

deriving via (Default (Maybe a)) instance (SOrd a) => SOrd (Maybe a)

deriving via (Default (Either a b)) instance (SOrd a, SOrd b) => SOrd (Either a b)

deriving via (Default ()) instance SOrd ()

deriving via (Default (a, b)) instance (SOrd a, SOrd b) => SOrd (a, b)

deriving via (Default (a, b, c)) instance (SOrd a, SOrd b, SOrd c) => SOrd (a, b, c)

deriving via
  (Default (a, b, c, d))
  instance
    (SOrd a, SOrd b, SOrd c, SOrd d) =>
    SOrd (a, b, c, d)

deriving via
  (Default (a, b, c, d, e))
  instance
    (SOrd a, SOrd b, SOrd c, SOrd d, SOrd e) =>
    SOrd (a, b, c, d, e)

deriving via
  (Default (a, b, c, d, e, f))
  instance
    (SOrd a, SOrd b, SOrd c, SOrd d, SOrd e, SOrd f) =>
    SOrd (a, b, c, d, e, f)

deriving via
  (Default (a, b, c, d, e, f, g))
  instance
    (SOrd a, SOrd b, SOrd c, SOrd d, SOrd e, SOrd f, SOrd g) =>
    SOrd (a, b, c, d, e, f, g)

deriving via
  (Default (a, b, c, d, e, f, g, h))
  instance
    ( SOrd a,
      SOrd b,
      SOrd c,
      SOrd d,
      SOrd e,
      SOrd f,
      SOrd g,
      SOrd h
    ) =>
    SOrd (a, b, c, d, e, f, g, h)

deriving via
  (Default (Sum f g a))
  instance
    (SOrd (f a), SOrd (g a)) => SOrd (Sum f g a)

instance (SOrd (m (Maybe a))) => SOrd (MaybeT m a) where
  (MaybeT l) .<= (MaybeT r) = l .<= r
  (MaybeT l) .< (MaybeT r) = l .< r
  (MaybeT l) .>= (MaybeT r) = l .>= r
  (MaybeT l) .> (MaybeT r) = l .> r
  symCompare (MaybeT l) (MaybeT r) = symCompare l r

instance (SOrd (m (Either e a))) => SOrd (ExceptT e m a) where
  (ExceptT l) .<= (ExceptT r) = l .<= r
  (ExceptT l) .< (ExceptT r) = l .< r
  (ExceptT l) .>= (ExceptT r) = l .>= r
  (ExceptT l) .> (ExceptT r) = l .> r
  symCompare (ExceptT l) (ExceptT r) = symCompare l r

instance (SOrd (m (a, s))) => SOrd (WriterLazy.WriterT s m a) where
  (WriterLazy.WriterT l) .<= (WriterLazy.WriterT r) = l .<= r
  (WriterLazy.WriterT l) .< (WriterLazy.WriterT r) = l .< r
  (WriterLazy.WriterT l) .>= (WriterLazy.WriterT r) = l .>= r
  (WriterLazy.WriterT l) .> (WriterLazy.WriterT r) = l .> r
  symCompare (WriterLazy.WriterT l) (WriterLazy.WriterT r) = symCompare l r

instance (SOrd (m (a, s))) => SOrd (WriterStrict.WriterT s m a) where
  (WriterStrict.WriterT l) .<= (WriterStrict.WriterT r) = l .<= r
  (WriterStrict.WriterT l) .< (WriterStrict.WriterT r) = l .< r
  (WriterStrict.WriterT l) .>= (WriterStrict.WriterT r) = l .>= r
  (WriterStrict.WriterT l) .> (WriterStrict.WriterT r) = l .> r
  symCompare (WriterStrict.WriterT l) (WriterStrict.WriterT r) = symCompare l r

instance (SOrd a) => SOrd (Identity a) where
  (Identity l) .<= (Identity r) = l .<= r
  (Identity l) .< (Identity r) = l .< r
  (Identity l) .>= (Identity r) = l .>= r
  (Identity l) .> (Identity r) = l .> r
  (Identity l) `symCompare` (Identity r) = l `symCompare` r

instance (SOrd (m a)) => SOrd (IdentityT m a) where
  (IdentityT l) .<= (IdentityT r) = l .<= r
  (IdentityT l) .< (IdentityT r) = l .< r
  (IdentityT l) .>= (IdentityT r) = l .>= r
  (IdentityT l) .> (IdentityT r) = l .> r
  (IdentityT l) `symCompare` (IdentityT r) = l `symCompare` r

-- SOrd
#define SORD_SIMPLE(symtype) \
instance SOrd symtype where \
  (symtype a) .<= (symtype b) = SymBool $ pevalLeNumTerm a b; \
  (symtype a) .< (symtype b) = SymBool $ pevalLtNumTerm a b; \
  (symtype a) .>= (symtype b) = SymBool $ pevalGeNumTerm a b; \
  (symtype a) .> (symtype b) = SymBool $ pevalGtNumTerm a b; \
  a `symCompare` b = mrgIf \
    (a .< b) \
    (mrgPure LT) \
    (mrgIf (a .== b) (mrgPure EQ) (mrgPure GT))

#define SORD_BV(symtype) \
instance (KnownNat n, 1 <= n) => SOrd (symtype n) where \
  (symtype a) .<= (symtype b) = SymBool $ pevalLeNumTerm a b; \
  (symtype a) .< (symtype b) = SymBool $ pevalLtNumTerm a b; \
  (symtype a) .>= (symtype b) = SymBool $ pevalGeNumTerm a b; \
  (symtype a) .> (symtype b) = SymBool $ pevalGtNumTerm a b; \
  a `symCompare` b = mrgIf \
    (a .< b) \
    (mrgPure LT) \
    (mrgIf (a .== b) (mrgPure EQ) (mrgPure GT))

#define SORD_BV_SOME(somety, bf) \
instance SOrd somety where \
  (.<=) = bf (.<=) ".<="; \
  {-# INLINE (.<=) #-}; \
  (.<) = bf (.<) ".<"; \
  {-# INLINE (.<) #-}; \
  (.>=) = bf (.>=) ".>="; \
  {-# INLINE (.>=) #-}; \
  (.>) = bf (.>) ".>"; \
  {-# INLINE (.>) #-}; \
  symCompare = bf symCompare "symCompare"; \
  {-# INLINE symCompare #-}

instance SOrd SymBool where
  l .<= r = symNot l .|| r
  l .< r = symNot l .&& r
  l .>= r = l .|| symNot r
  l .> r = l .&& symNot r
  symCompare l r =
    mrgIf
      (symNot l .&& r)
      (mrgPure LT)
      (mrgIf (l .== r) (mrgPure EQ) (mrgPure GT))

#if 1
SORD_SIMPLE(SymInteger)
SORD_BV(SymIntN)
SORD_BV(SymWordN)
SORD_BV_SOME(SomeSymIntN, binSomeSymIntN)
SORD_BV_SOME(SomeSymWordN, binSomeSymWordN)
#endif

-- Exception
instance SOrd AssertionError where
  _ .<= _ = con True
  _ .< _ = con False
  _ .>= _ = con True
  _ .> _ = con False
  _ `symCompare` _ = mrgPure EQ

instance SOrd VerificationConditions where
  l .>= r = con $ l >= r
  l .> r = con $ l > r
  l .<= r = con $ l <= r
  l .< r = con $ l < r
  l `symCompare` r = mrgPure $ l `compare` r

-- UnionM
instance (SOrd a, Mergeable a) => SOrd (UnionM a) where
  x .<= y = simpleMerge $ do
    x1 <- tryMerge x
    y1 <- tryMerge y
    mrgPure $ x1 .<= y1
  x .< y = simpleMerge $ do
    x1 <- tryMerge x
    y1 <- tryMerge y
    mrgPure $ x1 .< y1
  x .>= y = simpleMerge $ do
    x1 <- tryMerge x
    y1 <- tryMerge y
    mrgPure $ x1 .>= y1
  x .> y = simpleMerge $ do
    x1 <- tryMerge x
    y1 <- tryMerge y
    mrgPure $ x1 .> y1
  x `symCompare` y = liftToMonadUnion $ do
    x1 <- tryMerge x
    y1 <- tryMerge y
    x1 `symCompare` y1

-- | Auxiliary class for 'SOrd' instance derivation
class (SEq' f) => SOrd' f where
  -- | Auxiliary function for '(..<) derivation
  (..<) :: f a -> f a -> SymBool

  infix 4 ..<

  -- | Auxiliary function for '(..<=) derivation
  (..<=) :: f a -> f a -> SymBool

  infix 4 ..<=

  -- | Auxiliary function for '(..>) derivation
  (..>) :: f a -> f a -> SymBool

  infix 4 ..>

  -- | Auxiliary function for '(..>=) derivation
  (..>=) :: f a -> f a -> SymBool

  infix 4 ..>=

  -- | Auxiliary function for 'symCompare' derivation
  symCompare' :: f a -> f a -> UnionM Ordering

instance SOrd' U1 where
  _ ..< _ = con False
  _ ..<= _ = con True
  _ ..> _ = con False
  _ ..>= _ = con True
  symCompare' _ _ = mrgPure EQ

instance SOrd' V1 where
  _ ..< _ = con False
  _ ..<= _ = con True
  _ ..> _ = con False
  _ ..>= _ = con True
  symCompare' _ _ = mrgPure EQ

instance (SOrd c) => SOrd' (K1 i c) where
  (K1 a) ..< (K1 b) = a .< b
  (K1 a) ..<= (K1 b) = a .<= b
  (K1 a) ..> (K1 b) = a .> b
  (K1 a) ..>= (K1 b) = a .>= b
  symCompare' (K1 a) (K1 b) = symCompare a b

instance (SOrd' a) => SOrd' (M1 i c a) where
  (M1 a) ..< (M1 b) = a ..< b
  (M1 a) ..<= (M1 b) = a ..<= b
  (M1 a) ..> (M1 b) = a ..> b
  (M1 a) ..>= (M1 b) = a ..>= b
  symCompare' (M1 a) (M1 b) = symCompare' a b

instance (SOrd' a, SOrd' b) => SOrd' (a :+: b) where
  (L1 _) ..< (R1 _) = con True
  (L1 a) ..< (L1 b) = a ..< b
  (R1 _) ..< (L1 _) = con False
  (R1 a) ..< (R1 b) = a ..< b
  (L1 _) ..<= (R1 _) = con True
  (L1 a) ..<= (L1 b) = a ..<= b
  (R1 _) ..<= (L1 _) = con False
  (R1 a) ..<= (R1 b) = a ..<= b

  (L1 _) ..> (R1 _) = con False
  (L1 a) ..> (L1 b) = a ..> b
  (R1 _) ..> (L1 _) = con True
  (R1 a) ..> (R1 b) = a ..> b
  (L1 _) ..>= (R1 _) = con False
  (L1 a) ..>= (L1 b) = a ..>= b
  (R1 _) ..>= (L1 _) = con True
  (R1 a) ..>= (R1 b) = a ..>= b

  symCompare' (L1 a) (L1 b) = symCompare' a b
  symCompare' (L1 _) (R1 _) = mrgPure LT
  symCompare' (R1 a) (R1 b) = symCompare' a b
  symCompare' (R1 _) (L1 _) = mrgPure GT

instance (SOrd' a, SOrd' b) => SOrd' (a :*: b) where
  (a1 :*: b1) ..< (a2 :*: b2) = (a1 ..< a2) .|| ((a1 ..== a2) .&& (b1 ..< b2))
  (a1 :*: b1) ..<= (a2 :*: b2) = (a1 ..< a2) .|| ((a1 ..== a2) .&& (b1 ..<= b2))
  (a1 :*: b1) ..> (a2 :*: b2) = (a1 ..> a2) .|| ((a1 ..== a2) .&& (b1 ..> b2))
  (a1 :*: b1) ..>= (a2 :*: b2) = (a1 ..> a2) .|| ((a1 ..== a2) .&& (b1 ..>= b2))
  symCompare' (a1 :*: b1) (a2 :*: b2) = do
    l <- symCompare' a1 a2
    case l of
      EQ -> symCompare' b1 b2
      _ -> mrgPure l

derivedSymLt :: (Generic a, SOrd' (Rep a)) => a -> a -> SymBool
derivedSymLt x y = from x ..< from y

derivedSymLe :: (Generic a, SOrd' (Rep a)) => a -> a -> SymBool
derivedSymLe x y = from x ..<= from y

derivedSymGt :: (Generic a, SOrd' (Rep a)) => a -> a -> SymBool
derivedSymGt x y = from x ..> from y

derivedSymGe :: (Generic a, SOrd' (Rep a)) => a -> a -> SymBool
derivedSymGe x y = from x ..>= from y

derivedSymCompare :: (Generic a, SOrd' (Rep a)) => a -> a -> UnionM Ordering
derivedSymCompare x y = symCompare' (from x) (from y)
