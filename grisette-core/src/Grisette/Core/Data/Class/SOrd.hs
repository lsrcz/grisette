{-# LANGUAGE CPP #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      :   Grisette.Core.Data.Class.SOrd
-- Copyright   :   (c) Sirui Lu 2021-2022
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Core.Data.Class.SOrd
  ( -- * Note for the examples

    --

    -- | This module does not contain the implementation for solvable (see "Grisette.Core#solvable")
    -- types, and the examples in this module rely on the implementations in
    -- the [grisette-symir](https://hackage.haskell.org/package/grisette-symir) package.

    -- * Symbolic total order relation
    GSOrd (..),
    GSOrd' (..),
  )
where

import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.Trans.Maybe
import qualified Control.Monad.Writer.Lazy as WriterLazy
import qualified Control.Monad.Writer.Strict as WriterStrict
import qualified Data.ByteString as B
import Data.Functor.Sum
import Data.Int
import Data.Word
import Generics.Deriving
import Grisette.Core.Data.Class.Bool
import Grisette.Core.Data.Class.SimpleMergeable
import Grisette.Core.Data.Class.Solvable

-- $setup
-- >>> import Grisette.Core
-- >>> import Grisette.IR.SymPrim
-- >>> :set -XDataKinds
-- >>> :set -XBinaryLiterals
-- >>> :set -XFlexibleContexts
-- >>> :set -XFlexibleInstances
-- >>> :set -XFunctionalDependencies

-- | Auxiliary class for 'SOrd' instance derivation
class (GSEq' bool f) => GSOrd' bool f where
  -- | Auxiliary function for 'gsymlt' derivation
  gsymlt' :: f a -> f a -> bool

  -- | Auxiliary function for 'gsymle' derivation
  gsymle' :: f a -> f a -> bool

  -- | Auxiliary function for 'gsymgt' derivation
  gsymgt' :: f a -> f a -> bool

  -- | Auxiliary function for 'gsymge' derivation
  gsymge' :: f a -> f a -> bool

  -- | Auxiliary function for 'gsymCompare' derivation
  gsymCompare' :: (GUnionLike bool u, Monad u) => f a -> f a -> u Ordering

instance (SymBoolOp bool) => GSOrd' bool U1 where
  _ `gsymlt'` _ = con False
  _ `gsymle'` _ = con True
  _ `gsymgt'` _ = con False
  _ `gsymge'` _ = con True
  gsymCompare' _ _ = mrgSingle EQ

instance (SymBoolOp bool) => GSOrd' bool V1 where
  _ `gsymlt'` _ = con False
  _ `gsymle'` _ = con True
  _ `gsymgt'` _ = con False
  _ `gsymge'` _ = con True
  gsymCompare' _ _ = mrgSingle EQ

instance (SymBoolOp bool, GSOrd bool c) => GSOrd' bool (K1 i c) where
  (K1 a) `gsymlt'` (K1 b) = a `gsymlt` b
  (K1 a) `gsymle'` (K1 b) = a `gsymle` b
  (K1 a) `gsymgt'` (K1 b) = a `gsymgt` b
  (K1 a) `gsymge'` (K1 b) = a `gsymge` b
  gsymCompare' (K1 a) (K1 b) = gsymCompare a b

instance (SymBoolOp bool, GSOrd' bool a) => GSOrd' bool (M1 i c a) where
  (M1 a) `gsymlt'` (M1 b) = a `gsymlt'` b
  (M1 a) `gsymle'` (M1 b) = a `gsymle'` b
  (M1 a) `gsymgt'` (M1 b) = a `gsymgt'` b
  (M1 a) `gsymge'` (M1 b) = a `gsymge'` b
  gsymCompare' (M1 a) (M1 b) = gsymCompare' a b

instance (SymBoolOp bool, GSOrd' bool a, GSOrd' bool b) => GSOrd' bool (a :+: b) where
  (L1 _) `gsymlt'` (R1 _) = con True
  (L1 a) `gsymlt'` (L1 b) = a `gsymlt'` b
  (R1 _) `gsymlt'` (L1 _) = con False
  (R1 a) `gsymlt'` (R1 b) = a `gsymlt'` b
  (L1 _) `gsymle'` (R1 _) = con True
  (L1 a) `gsymle'` (L1 b) = a `gsymle'` b
  (R1 _) `gsymle'` (L1 _) = con False
  (R1 a) `gsymle'` (R1 b) = a `gsymle'` b

  (L1 _) `gsymgt'` (R1 _) = con False
  (L1 a) `gsymgt'` (L1 b) = a `gsymgt'` b
  (R1 _) `gsymgt'` (L1 _) = con True
  (R1 a) `gsymgt'` (R1 b) = a `gsymgt'` b
  (L1 _) `gsymge'` (R1 _) = con False
  (L1 a) `gsymge'` (L1 b) = a `gsymge'` b
  (R1 _) `gsymge'` (L1 _) = con True
  (R1 a) `gsymge'` (R1 b) = a `gsymge'` b

  gsymCompare' (L1 a) (L1 b) = gsymCompare' a b
  gsymCompare' (L1 _) (R1 _) = mrgSingle LT
  gsymCompare' (R1 a) (R1 b) = gsymCompare' a b
  gsymCompare' (R1 _) (L1 _) = mrgSingle GT

instance (SymBoolOp bool, GSOrd' bool a, GSOrd' bool b) => GSOrd' bool (a :*: b) where
  (a1 :*: b1) `gsymlt'` (a2 :*: b2) = (a1 `gsymlt'` a2) ||~ ((a1 `gsymeq'` a2) &&~ (b1 `gsymlt'` b2))
  (a1 :*: b1) `gsymle'` (a2 :*: b2) = (a1 `gsymlt'` a2) ||~ ((a1 `gsymeq'` a2) &&~ (b1 `gsymle'` b2))
  (a1 :*: b1) `gsymgt'` (a2 :*: b2) = (a1 `gsymgt'` a2) ||~ ((a1 `gsymeq'` a2) &&~ (b1 `gsymgt'` b2))
  (a1 :*: b1) `gsymge'` (a2 :*: b2) = (a1 `gsymgt'` a2) ||~ ((a1 `gsymeq'` a2) &&~ (b1 `gsymge'` b2))
  gsymCompare' (a1 :*: b1) (a2 :*: b2) = do
    l <- gsymCompare' a1 a2
    case l of
      EQ -> gsymCompare' b1 b2
      _ -> mrgSingle l

derivedGSymLt :: (Generic a, GSOrd' bool (Rep a)) => a -> a -> bool
derivedGSymLt x y = from x `gsymlt'` from y

derivedGSymLe :: (Generic a, GSOrd' bool (Rep a)) => a -> a -> bool
derivedGSymLe x y = from x `gsymle'` from y

derivedGSymGt :: (Generic a, GSOrd' bool (Rep a)) => a -> a -> bool
derivedGSymGt x y = from x `gsymgt'` from y

derivedGSymGe :: (Generic a, GSOrd' bool (Rep a)) => a -> a -> bool
derivedGSymGe x y = from x `gsymge'` from y

derivedGSymCompare :: (Generic a, GSOrd' bool (Rep a), GUnionLike bool u, Monad u) => a -> a -> u Ordering
derivedGSymCompare x y = gsymCompare' (from x) (from y)

-- | Symbolic total order. Note that we can't use Haskell's 'Ord' class since
-- symbolic comparison won't necessarily return a concrete 'Bool' or 'Ordering'
-- value.
--
-- >>> let a = 1 :: SymInteger
-- >>> let b = 2 :: SymInteger
-- >>> a `gsymlt` b :: SymBool
-- true
-- >>> a `gsymgt` b :: SymBool
-- false
--
-- >>> let a = "a" :: SymInteger
-- >>> let b = "b" :: SymInteger
-- >>> a `gsymlt` b :: SymBool
-- (< a b)
-- >>> a `gsymle` b :: SymBool
-- (<= a b)
-- >>> a `gsymgt` b :: SymBool
-- (< b a)
-- >>> a `gsymge` b :: SymBool
-- (<= b a)
--
-- For `gsymCompare`, `Ordering` is not a solvable type, and the result would
-- be wrapped in a union-like monad. See `Grisette.Core.Control.Monad.UnionMBase` and `GUnionLike` for more
-- information.
--
-- >>> a `gsymCompare` b :: UnionM Ordering -- UnionM is UnionMBase specialized with SymBool
-- {If (< a b) LT (If (= a b) EQ GT)}
--
-- __Note 1:__ This type class can be derived for algebraic data types.
-- You may need the @DerivingVia@ and @DerivingStrategies@ extensions.
--
-- > data X = ... deriving Generic deriving (GMergeable SymBool) via (Default X)
--
-- __Note 2:__ The @bool@ type is the symbolic boolean type to return. It should
-- be an instance of `SymBoolOp`. If you do not need to use an alternative
-- symbolic Boolean type, and will use the 'SymBool' type provided by the
-- [grisette-symir](https://hackage.haskell.org/package/grisette-symir) package, you can use the specialized `SOrd` type synonym for
-- the constraints and use specialized operators like `(<~)` and `(<=~)` from
-- [grisette-symir](https://hackage.haskell.org/package/grisette-symir) to write code with fewer type annotations.
-- However, you still need @'GSOrd' SymBool@ for implementing or deriving the
-- type class due to GHC's limitation.
class (GSEq bool a) => GSOrd bool a where
  gsymlt :: a -> a -> bool
  gsymle :: a -> a -> bool
  gsymgt :: a -> a -> bool
  gsymge :: a -> a -> bool
  x `gsymlt` y = x `gsymle` y &&~ x `gsymne` y
  x `gsymgt` y = y `gsymlt` x
  x `gsymge` y = y `gsymle` x
  gsymCompare :: (GUnionLike bool u, Monad u) => a -> a -> u Ordering
  gsymCompare l r =
    mrgIf
      (l `gsymlt` r :: bool)
      (mrgSingle LT)
      (mrgIf (l `gsymeq` r :: bool) (mrgSingle EQ) (mrgSingle GT))
  {-# MINIMAL gsymle #-}

instance (GSEq bool a, Generic a, GSOrd' bool (Rep a)) => GSOrd bool (Default a) where
  (Default l) `gsymle` (Default r) = l `derivedGSymLe` r
  (Default l) `gsymlt` (Default r) = l `derivedGSymLt` r
  (Default l) `gsymge` (Default r) = l `derivedGSymGe` r
  (Default l) `gsymgt` (Default r) = l `derivedGSymGt` r
  gsymCompare (Default l) (Default r) = derivedGSymCompare l r

#define CONCRETE_SORD(type) \
instance (SymBoolOp bool) => GSOrd bool type where \
  l `gsymle` r = con $ l <= r; \
  l `gsymlt` r = con $ l < r; \
  l `gsymge` r = con $ l >= r; \
  l `gsymgt` r = con $ l > r; \
  gsymCompare l r = mrgSingle $ compare l r

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
CONCRETE_SORD(B.ByteString)
#endif

symCompareSingleList :: (SymBoolOp bool, GSOrd bool a) => Bool -> Bool -> [a] -> [a] -> bool
symCompareSingleList isLess isStrict = go
  where
    go [] [] = con (not isStrict)
    go (x : xs) (y : ys) = (if isLess then x `gsymlt` y else x `gsymgt` y) ||~ (x `gsymeq` y &&~ go xs ys)
    go [] _ = if isLess then con True else con False
    go _ [] = if isLess then con False else con True

symCompareList :: (SymBoolOp bool, GSOrd bool a, GUnionLike bool u, Monad u) => [a] -> [a] -> u Ordering
symCompareList [] [] = mrgSingle EQ
symCompareList (x : xs) (y : ys) = do
  oxy <- gsymCompare x y
  case oxy of
    LT -> mrgSingle LT
    EQ -> symCompareList xs ys
    GT -> mrgSingle GT
symCompareList [] _ = mrgSingle LT
symCompareList _ [] = mrgSingle GT

instance (SymBoolOp bool, GSOrd bool a) => GSOrd bool [a] where
  gsymle = symCompareSingleList True False
  gsymlt = symCompareSingleList True True
  gsymge = symCompareSingleList False False
  gsymgt = symCompareSingleList False True
  gsymCompare = symCompareList

deriving via (Default (Maybe a)) instance (SymBoolOp bool, GSOrd bool a) => GSOrd bool (Maybe a)

deriving via (Default (Either a b)) instance (SymBoolOp bool, GSOrd bool a, GSOrd bool b) => GSOrd bool (Either a b)

deriving via (Default ()) instance (SymBoolOp bool) => GSOrd bool ()

deriving via (Default (a, b)) instance (SymBoolOp bool, GSOrd bool a, GSOrd bool b) => GSOrd bool (a, b)

deriving via (Default (a, b, c)) instance (SymBoolOp bool, GSOrd bool a, GSOrd bool b, GSOrd bool c) => GSOrd bool (a, b, c)

deriving via
  (Default (a, b, c, d))
  instance
    (SymBoolOp bool, GSOrd bool a, GSOrd bool b, GSOrd bool c, GSOrd bool d) =>
    GSOrd bool (a, b, c, d)

deriving via
  (Default (a, b, c, d, e))
  instance
    (SymBoolOp bool, GSOrd bool a, GSOrd bool b, GSOrd bool c, GSOrd bool d, GSOrd bool e) =>
    GSOrd bool (a, b, c, d, e)

deriving via
  (Default (a, b, c, d, e, f))
  instance
    (SymBoolOp bool, GSOrd bool a, GSOrd bool b, GSOrd bool c, GSOrd bool d, GSOrd bool e, GSOrd bool f) =>
    GSOrd bool (a, b, c, d, e, f)

deriving via
  (Default (a, b, c, d, e, f, g))
  instance
    (SymBoolOp bool, GSOrd bool a, GSOrd bool b, GSOrd bool c, GSOrd bool d, GSOrd bool e, GSOrd bool f, GSOrd bool g) =>
    GSOrd bool (a, b, c, d, e, f, g)

deriving via
  (Default (a, b, c, d, e, f, g, h))
  instance
    ( SymBoolOp bool,
      GSOrd bool a,
      GSOrd bool b,
      GSOrd bool c,
      GSOrd bool d,
      GSOrd bool e,
      GSOrd bool f,
      GSOrd bool g,
      GSOrd bool h
    ) =>
    GSOrd bool (a, b, c, d, e, f, g, h)

deriving via
  (Default (Sum f g a))
  instance
    (SymBoolOp bool, GSOrd bool (f a), GSOrd bool (g a)) => GSOrd bool (Sum f g a)

instance (SymBoolOp bool, GSOrd bool (m (Maybe a))) => GSOrd bool (MaybeT m a) where
  (MaybeT l) `gsymle` (MaybeT r) = l `gsymle` r
  (MaybeT l) `gsymlt` (MaybeT r) = l `gsymlt` r
  (MaybeT l) `gsymge` (MaybeT r) = l `gsymge` r
  (MaybeT l) `gsymgt` (MaybeT r) = l `gsymgt` r
  gsymCompare (MaybeT l) (MaybeT r) = gsymCompare l r

instance (SymBoolOp bool, GSOrd bool (m (Either e a))) => GSOrd bool (ExceptT e m a) where
  (ExceptT l) `gsymle` (ExceptT r) = l `gsymle` r
  (ExceptT l) `gsymlt` (ExceptT r) = l `gsymlt` r
  (ExceptT l) `gsymge` (ExceptT r) = l `gsymge` r
  (ExceptT l) `gsymgt` (ExceptT r) = l `gsymgt` r
  gsymCompare (ExceptT l) (ExceptT r) = gsymCompare l r

instance (SymBoolOp bool, GSOrd bool (m (a, s))) => GSOrd bool (WriterLazy.WriterT s m a) where
  (WriterLazy.WriterT l) `gsymle` (WriterLazy.WriterT r) = l `gsymle` r
  (WriterLazy.WriterT l) `gsymlt` (WriterLazy.WriterT r) = l `gsymlt` r
  (WriterLazy.WriterT l) `gsymge` (WriterLazy.WriterT r) = l `gsymge` r
  (WriterLazy.WriterT l) `gsymgt` (WriterLazy.WriterT r) = l `gsymgt` r
  gsymCompare (WriterLazy.WriterT l) (WriterLazy.WriterT r) = gsymCompare l r

instance (SymBoolOp bool, GSOrd bool (m (a, s))) => GSOrd bool (WriterStrict.WriterT s m a) where
  (WriterStrict.WriterT l) `gsymle` (WriterStrict.WriterT r) = l `gsymle` r
  (WriterStrict.WriterT l) `gsymlt` (WriterStrict.WriterT r) = l `gsymlt` r
  (WriterStrict.WriterT l) `gsymge` (WriterStrict.WriterT r) = l `gsymge` r
  (WriterStrict.WriterT l) `gsymgt` (WriterStrict.WriterT r) = l `gsymgt` r
  gsymCompare (WriterStrict.WriterT l) (WriterStrict.WriterT r) = gsymCompare l r

instance (SymBoolOp bool, GSOrd bool a) => GSOrd bool (Identity a) where
  (Identity l) `gsymle` (Identity r) = l `gsymle` r
  (Identity l) `gsymlt` (Identity r) = l `gsymlt` r
  (Identity l) `gsymge` (Identity r) = l `gsymge` r
  (Identity l) `gsymgt` (Identity r) = l `gsymgt` r
  (Identity l) `gsymCompare` (Identity r) = l `gsymCompare` r

instance (SymBoolOp bool, GSOrd bool (m a)) => GSOrd bool (IdentityT m a) where
  (IdentityT l) `gsymle` (IdentityT r) = l `gsymle` r
  (IdentityT l) `gsymlt` (IdentityT r) = l `gsymlt` r
  (IdentityT l) `gsymge` (IdentityT r) = l `gsymge` r
  (IdentityT l) `gsymgt` (IdentityT r) = l `gsymgt` r
  (IdentityT l) `gsymCompare` (IdentityT r) = l `gsymCompare` r
