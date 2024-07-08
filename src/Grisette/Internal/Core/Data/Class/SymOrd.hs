{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      :   Grisette.Internal.Core.Data.Class.SymOrd
-- Copyright   :   (c) Sirui Lu 2021-2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Internal.Core.Data.Class.SymOrd
  ( -- * Symbolic total order relation
    SymOrd (..),
    SymOrd1 (..),
    symCompare1,
    SymOrd2 (..),
    symCompare2,

    -- * Min and max
    symMax,
    symMin,
    mrgMax,
    mrgMin,

    -- * Generic 'SymOrd'
    SymOrdArgs (..),
    GSymOrd (..),
    genericSymCompare,
    genericLiftSymCompare,
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
import Data.Functor.Compose (Compose (Compose))
import Data.Functor.Const (Const)
import Data.Functor.Product (Product)
import Data.Functor.Sum (Sum)
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Kind (Type)
import Data.Monoid (Alt, Ap)
import qualified Data.Monoid as Monoid
import Data.Ord (Down (Down))
import qualified Data.Text as T
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.TypeLits (KnownNat, type (<=))
import Generics.Deriving
  ( Default (Default),
    Default1 (Default1),
    Generic (Rep, from),
    Generic1 (Rep1, from1),
    K1 (K1),
    M1 (M1),
    Par1 (Par1),
    Rec1 (Rec1),
    U1,
    V1,
    (:.:) (Comp1),
    type (:*:) ((:*:)),
    type (:+:) (L1, R1),
  )
import Grisette.Internal.Core.Control.Exception
  ( AssertionError,
    VerificationConditions,
  )
import Grisette.Internal.Core.Control.Monad.Union (Union)
import Grisette.Internal.Core.Data.Class.ITEOp (ITEOp, symIte)
import Grisette.Internal.Core.Data.Class.LogicalOp
  ( LogicalOp (symNot, (.&&), (.||)),
  )
import Grisette.Internal.Core.Data.Class.Mergeable (Mergeable)
import Grisette.Internal.Core.Data.Class.PlainUnion
  ( simpleMerge,
  )
import Grisette.Internal.Core.Data.Class.SimpleMergeable
  ( SymBranching,
    mrgIf,
  )
import Grisette.Internal.Core.Data.Class.Solvable (Solvable (con))
import Grisette.Internal.Core.Data.Class.SymEq
  ( GSymEq,
    SymEq ((.==)),
    SymEq1,
    SymEq2,
  )
import Grisette.Internal.Core.Data.Class.TryMerge
  ( mrgSingle,
    tryMerge,
  )
import Grisette.Internal.SymPrim.BV (IntN, WordN)
import Grisette.Internal.SymPrim.FP (FP, FPRoundingMode, ValidFP)
import Grisette.Internal.SymPrim.Prim.Term
  ( PEvalOrdTerm
      ( pevalLeOrdTerm,
        pevalLtOrdTerm
      ),
    pevalGeOrdTerm,
    pevalGtOrdTerm,
  )
import Grisette.Internal.SymPrim.SymAlgReal (SymAlgReal (SymAlgReal))
import Grisette.Internal.SymPrim.SymBV
  ( SymIntN (SymIntN),
    SymWordN (SymWordN),
  )
import Grisette.Internal.SymPrim.SymBool (SymBool (SymBool))
import Grisette.Internal.SymPrim.SymFP
  ( SymFP (SymFP),
    SymFPRoundingMode (SymFPRoundingMode),
  )
import Grisette.Internal.SymPrim.SymInteger (SymInteger (SymInteger))
import Grisette.Internal.TH.DeriveBuiltin (deriveBuiltins)
import Grisette.Internal.TH.DeriveInstanceProvider
  ( Strategy (ViaDefault, ViaDefault1),
  )
import Grisette.Internal.Utils.Derive (Arity0, Arity1)

-- $setup
-- >>> import Grisette.Core
-- >>> import Grisette.SymPrim
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
-- be wrapped in a union-like monad. See
-- `Grisette.Core.Control.Monad.Union` and `Grisette.Core.PlainUnion` for more
-- information.
--
-- >>> a `symCompare` b :: Union Ordering
-- {If (< a b) LT (If (= a b) EQ GT)}
--
-- __Note:__ This type class can be derived for algebraic data types.
-- You may need the @DerivingVia@ and @DerivingStrategies@ extensions.
--
-- > data X = ... deriving Generic deriving SymOrd via (Default X)
class (SymEq a) => SymOrd a where
  (.<) :: a -> a -> SymBool
  infix 4 .<
  (.<=) :: a -> a -> SymBool
  infix 4 .<=
  (.>) :: a -> a -> SymBool
  infix 4 .>
  (.>=) :: a -> a -> SymBool
  infix 4 .>=
  x .< y =
    simpleMerge $
      symCompare x y >>= \case
        LT -> con True
        EQ -> con False
        GT -> con False
  {-# INLINE (.<) #-}
  x .<= y = symNot (x .> y)
  {-# INLINE (.<=) #-}
  x .> y = y .< x
  {-# INLINE (.>) #-}
  x .>= y = y .<= x
  {-# INLINE (.>=) #-}
  symCompare :: a -> a -> Union Ordering
  symCompare l r =
    mrgIf
      (l .< r)
      (mrgSingle LT)
      (mrgIf (l .== r) (mrgSingle EQ) (mrgSingle GT))
  {-# INLINE symCompare #-}
  {-# MINIMAL (.<) | symCompare #-}

-- | Lifting of the 'SymOrd' class to unary type constructors.
--
-- Any instance should be subject to the following law that canonicity is
-- preserved:
--
-- @liftSymCompare symCompare@ should be equivalent to @symCompare@, under the
-- symbolic semantics.
--
-- This class therefore represents the generalization of 'SymOrd' by decomposing
-- its main method into a canonical lifting on a canonical inner method, so that
-- the lifting can be reused for other arguments than the canonical one.
class (SymEq1 f, forall a. (SymOrd a) => SymOrd (f a)) => SymOrd1 f where
  -- | Lift a 'symCompare' function through the type constructor.
  --
  -- The function will usually be applied to an symbolic comparison function,
  -- but the more general type ensures that the implementation uses it to
  -- compare elements of the first container with elements of the second.
  liftSymCompare :: (a -> b -> Union Ordering) -> f a -> f b -> Union Ordering

-- | Lift the standard 'symCompare' function to binary type constructors.
symCompare1 :: (SymOrd1 f, SymOrd a) => f a -> f a -> Union Ordering
symCompare1 = liftSymCompare symCompare
{-# INLINE symCompare1 #-}

-- | Lifting of the 'SymOrd' class to binary type constructors.
class (SymEq2 f, forall a. (SymOrd a) => SymOrd1 (f a)) => SymOrd2 f where
  -- | Lift a 'symCompare' function through the type constructor.
  --
  -- The function will usually be applied to an symbolic comparison function,
  -- but the more general type ensures that the implementation uses it to
  -- compare elements of the first container with elements of the second.
  liftSymCompare2 ::
    (a -> b -> Union Ordering) ->
    (c -> d -> Union Ordering) ->
    f a c ->
    f b d ->
    Union Ordering

-- | Lift the standard 'symCompare' function through the type constructors.
symCompare2 :: (SymOrd2 f, SymOrd a, SymOrd b) => f a b -> f a b -> Union Ordering
symCompare2 = liftSymCompare2 symCompare symCompare
{-# INLINE symCompare2 #-}

-- | Symbolic maximum.
symMax :: (SymOrd a, ITEOp a) => a -> a -> a
symMax x y = symIte (x .>= y) x y
{-# INLINE symMax #-}

-- | Symbolic minimum.
symMin :: (SymOrd a, ITEOp a) => a -> a -> a
symMin x y = symIte (x .>= y) y x
{-# INLINE symMin #-}

-- | Symbolic maximum, with a union-like monad.
mrgMax ::
  (SymOrd a, Mergeable a, SymBranching m, Applicative m) =>
  a ->
  a ->
  m a
mrgMax x y = mrgIf (x .>= y) (pure x) (pure y)
{-# INLINE mrgMax #-}

-- | Symbolic minimum, with a union-like monad.
mrgMin ::
  (SymOrd a, Mergeable a, SymBranching m, Applicative m) =>
  a ->
  a ->
  m a
mrgMin x y = mrgIf (x .>= y) (pure y) (pure x)
{-# INLINE mrgMin #-}

-- Derivations

-- | The arguments to the generic comparison function.
data family SymOrdArgs arity a b :: Type

data instance SymOrdArgs Arity0 _ _ = SymOrdArgs0

newtype instance SymOrdArgs Arity1 a b
  = SymOrdArgs1 (a -> b -> Union Ordering)

-- | The class of types that can be generically symbolically compared.
class GSymOrd arity f where
  gsymCompare :: SymOrdArgs arity a b -> f a -> f b -> Union Ordering

instance GSymOrd arity V1 where
  gsymCompare _ _ _ = mrgSingle EQ
  {-# INLINE gsymCompare #-}

instance GSymOrd arity U1 where
  gsymCompare _ _ _ = mrgSingle EQ
  {-# INLINE gsymCompare #-}

instance
  (GSymOrd arity a, GSymOrd arity b) =>
  GSymOrd arity (a :*: b)
  where
  gsymCompare args (a1 :*: b1) (a2 :*: b2) = do
    l <- gsymCompare args a1 a2
    case l of
      EQ -> gsymCompare args b1 b2
      _ -> mrgSingle l
  {-# INLINE gsymCompare #-}

instance
  (GSymOrd arity a, GSymOrd arity b) =>
  GSymOrd arity (a :+: b)
  where
  gsymCompare args (L1 a) (L1 b) = gsymCompare args a b
  gsymCompare _ (L1 _) (R1 _) = mrgSingle LT
  gsymCompare args (R1 a) (R1 b) = gsymCompare args a b
  gsymCompare _ (R1 _) (L1 _) = mrgSingle GT
  {-# INLINE gsymCompare #-}

instance (GSymOrd arity a) => GSymOrd arity (M1 i c a) where
  gsymCompare args (M1 a) (M1 b) = gsymCompare args a b
  {-# INLINE gsymCompare #-}

instance (SymOrd a) => GSymOrd arity (K1 i a) where
  gsymCompare _ (K1 a) (K1 b) = a `symCompare` b
  {-# INLINE gsymCompare #-}

instance GSymOrd Arity1 Par1 where
  gsymCompare (SymOrdArgs1 c) (Par1 a) (Par1 b) = c a b
  {-# INLINE gsymCompare #-}

instance (SymOrd1 f) => GSymOrd Arity1 (Rec1 f) where
  gsymCompare (SymOrdArgs1 c) (Rec1 a) (Rec1 b) = liftSymCompare c a b
  {-# INLINE gsymCompare #-}

instance (SymOrd1 f, GSymOrd Arity1 g) => GSymOrd Arity1 (f :.: g) where
  gsymCompare targs (Comp1 a) (Comp1 b) = liftSymCompare (gsymCompare targs) a b
  {-# INLINE gsymCompare #-}

instance
  (Generic a, GSymOrd Arity0 (Rep a), GSymEq Arity0 (Rep a)) =>
  SymOrd (Default a)
  where
  symCompare (Default l) (Default r) = genericSymCompare l r
  {-# INLINE symCompare #-}

-- | Generic 'symCompare' function.
genericSymCompare :: (Generic a, GSymOrd Arity0 (Rep a)) => a -> a -> Union Ordering
genericSymCompare l r = gsymCompare SymOrdArgs0 (from l) (from r)
{-# INLINE genericSymCompare #-}

instance
  (Generic1 f, GSymOrd Arity1 (Rep1 f), GSymEq Arity1 (Rep1 f), SymOrd a) =>
  SymOrd (Default1 f a)
  where
  symCompare = symCompare1
  {-# INLINE symCompare #-}

instance
  (Generic1 f, GSymOrd Arity1 (Rep1 f), GSymEq Arity1 (Rep1 f)) =>
  SymOrd1 (Default1 f)
  where
  liftSymCompare c (Default1 l) (Default1 r) = genericLiftSymCompare c l r
  {-# INLINE liftSymCompare #-}

-- | Generic 'liftSymCompare' function.
genericLiftSymCompare ::
  (Generic1 f, GSymOrd Arity1 (Rep1 f)) =>
  (a -> b -> Union Ordering) ->
  f a ->
  f b ->
  Union Ordering
genericLiftSymCompare c l r = gsymCompare (SymOrdArgs1 c) (from1 l) (from1 r)
{-# INLINE genericLiftSymCompare #-}

#define CONCRETE_SORD(type) \
instance SymOrd type where \
  l .<= r = con $ l <= r; \
  l .< r = con $ l < r; \
  l .>= r = con $ l >= r; \
  l .> r = con $ l > r; \
  symCompare l r = mrgSingle $ compare l r; \
  {-# INLINE (.<=) #-}; \
  {-# INLINE (.<) #-}; \
  {-# INLINE (.>=) #-}; \
  {-# INLINE (.>) #-}; \
  {-# INLINE symCompare #-}

#define CONCRETE_SORD_BV(type) \
instance (KnownNat n, 1 <= n) => SymOrd (type n) where \
  l .<= r = con $ l <= r; \
  l .< r = con $ l < r; \
  l .>= r = con $ l >= r; \
  l .> r = con $ l > r; \
  symCompare l r = mrgSingle $ compare l r; \
  {-# INLINE (.<=) #-}; \
  {-# INLINE (.<) #-}; \
  {-# INLINE (.>=) #-}; \
  {-# INLINE (.>) #-}; \
  {-# INLINE symCompare #-}

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
CONCRETE_SORD(Float)
CONCRETE_SORD(Double)
CONCRETE_SORD(B.ByteString)
CONCRETE_SORD(T.Text)
CONCRETE_SORD(FPRoundingMode)
CONCRETE_SORD(Monoid.All)
CONCRETE_SORD(Monoid.Any)
CONCRETE_SORD(Ordering)
CONCRETE_SORD_BV(WordN)
CONCRETE_SORD_BV(IntN)
#endif

instance (ValidFP eb sb) => SymOrd (FP eb sb) where
  l .<= r = con $ l <= r
  {-# INLINE (.<=) #-}
  l .< r = con $ l < r
  {-# INLINE (.<) #-}
  l .>= r = con $ l >= r
  {-# INLINE (.>=) #-}
  l .> r = con $ l > r
  {-# INLINE (.>) #-}

-- SymOrd
#define SORD_SIMPLE(symtype) \
instance SymOrd symtype where \
  (symtype a) .<= (symtype b) = SymBool $ pevalLeOrdTerm a b; \
  (symtype a) .< (symtype b) = SymBool $ pevalLtOrdTerm a b; \
  (symtype a) .>= (symtype b) = SymBool $ pevalGeOrdTerm a b; \
  (symtype a) .> (symtype b) = SymBool $ pevalGtOrdTerm a b; \
  a `symCompare` b = mrgIf \
    (a .< b) \
    (mrgSingle LT) \
    (mrgIf (a .== b) (mrgSingle EQ) (mrgSingle GT)); \
  {-# INLINE (.<=) #-}; \
  {-# INLINE (.<) #-}; \
  {-# INLINE (.>=) #-}; \
  {-# INLINE (.>) #-}; \
  {-# INLINE symCompare #-}

#define SORD_BV(symtype) \
instance (KnownNat n, 1 <= n) => SymOrd (symtype n) where \
  (symtype a) .<= (symtype b) = SymBool $ pevalLeOrdTerm a b; \
  (symtype a) .< (symtype b) = SymBool $ pevalLtOrdTerm a b; \
  (symtype a) .>= (symtype b) = SymBool $ pevalGeOrdTerm a b; \
  (symtype a) .> (symtype b) = SymBool $ pevalGtOrdTerm a b; \
  a `symCompare` b = mrgIf \
    (a .< b) \
    (mrgSingle LT) \
    (mrgIf (a .== b) (mrgSingle EQ) (mrgSingle GT)); \
  {-# INLINE (.<=) #-}; \
  {-# INLINE (.<) #-}; \
  {-# INLINE (.>=) #-}; \
  {-# INLINE (.>) #-}; \
  {-# INLINE symCompare #-}

instance (ValidFP eb sb) => SymOrd (SymFP eb sb) where
  (SymFP a) .<= (SymFP b) = SymBool $ pevalLeOrdTerm a b
  {-# INLINE (.<=) #-}
  (SymFP a) .< (SymFP b) = SymBool $ pevalLtOrdTerm a b
  {-# INLINE (.<) #-}
  (SymFP a) .>= (SymFP b) = SymBool $ pevalGeOrdTerm a b
  {-# INLINE (.>=) #-}
  (SymFP a) .> (SymFP b) = SymBool $ pevalGtOrdTerm a b
  {-# INLINE (.>) #-}

instance SymOrd SymBool where
  l .<= r = symNot l .|| r
  {-# INLINE (.<=) #-}
  l .< r = symNot l .&& r
  {-# INLINE (.<) #-}
  l .>= r = l .|| symNot r
  {-# INLINE (.>=) #-}
  l .> r = l .&& symNot r
  {-# INLINE (.>) #-}
  symCompare l r =
    mrgIf
      (symNot l .&& r)
      (mrgSingle LT)
      (mrgIf (l .== r) (mrgSingle EQ) (mrgSingle GT))
  {-# INLINE symCompare #-}

#if 1
SORD_SIMPLE(SymInteger)
SORD_SIMPLE(SymAlgReal)
SORD_SIMPLE(SymFPRoundingMode)
SORD_BV(SymIntN)
SORD_BV(SymWordN)
#endif

-- Union
instance (SymOrd a) => SymOrd (Union a) where
  x .<= y = simpleMerge $ do
    x1 <- x
    y1 <- y
    mrgSingle $ x1 .<= y1
  x .< y = simpleMerge $ do
    x1 <- x
    y1 <- y
    mrgSingle $ x1 .< y1
  x .>= y = simpleMerge $ do
    x1 <- x
    y1 <- y
    mrgSingle $ x1 .>= y1
  x .> y = simpleMerge $ do
    x1 <- x
    y1 <- y
    mrgSingle $ x1 .> y1
  x `symCompare` y = tryMerge $ do
    x1 <- x
    y1 <- y
    x1 `symCompare` y1

instance SymOrd1 Union where
  liftSymCompare f x y = tryMerge $ do
    x1 <- x
    y1 <- y
    f x1 y1

-- Instances
deriveBuiltins
  (ViaDefault ''SymOrd)
  [''SymOrd]
  [ ''Maybe,
    ''Either,
    ''(),
    ''(,),
    ''(,,),
    ''(,,,),
    ''(,,,,),
    ''(,,,,,),
    ''(,,,,,,),
    ''(,,,,,,,),
    ''(,,,,,,,,),
    ''(,,,,,,,,,),
    ''(,,,,,,,,,,),
    ''(,,,,,,,,,,,),
    ''(,,,,,,,,,,,,),
    ''(,,,,,,,,,,,,,),
    ''(,,,,,,,,,,,,,,),
    ''AssertionError,
    ''VerificationConditions,
    ''Identity,
    ''Monoid.Dual,
    ''Monoid.Sum,
    ''Monoid.Product,
    ''Monoid.First,
    ''Monoid.Last
  ]

deriveBuiltins
  (ViaDefault1 ''SymOrd1)
  [''SymOrd, ''SymOrd1]
  [ ''Maybe,
    ''Either,
    ''(,),
    ''(,,),
    ''(,,,),
    ''(,,,,),
    ''(,,,,,),
    ''(,,,,,,),
    ''(,,,,,,,),
    ''(,,,,,,,,),
    ''(,,,,,,,,,),
    ''(,,,,,,,,,,),
    ''(,,,,,,,,,,,),
    ''(,,,,,,,,,,,,),
    ''(,,,,,,,,,,,,,),
    ''(,,,,,,,,,,,,,,),
    ''Identity,
    ''Monoid.Dual,
    ''Monoid.Sum,
    ''Monoid.Product,
    ''Monoid.First,
    ''Monoid.Last
  ]

symCompareSingleList :: (SymOrd a) => Bool -> Bool -> [a] -> [a] -> SymBool
symCompareSingleList isLess isStrict = go
  where
    go [] [] = con (not isStrict)
    go (x : xs) (y : ys) =
      (if isLess then x .< y else x .> y) .|| (x .== y .&& go xs ys)
    go [] _ = if isLess then con True else con False
    go _ [] = if isLess then con False else con True

symLiftCompareList ::
  (a -> b -> Union Ordering) -> [a] -> [b] -> Union Ordering
symLiftCompareList _ [] [] = mrgSingle EQ
symLiftCompareList f (x : xs) (y : ys) = do
  oxy <- f x y
  case oxy of
    LT -> mrgSingle LT
    EQ -> symLiftCompareList f xs ys
    GT -> mrgSingle GT
symLiftCompareList _ [] _ = mrgSingle LT
symLiftCompareList _ _ [] = mrgSingle GT

-- []
instance (SymOrd a) => SymOrd [a] where
  {-# INLINE (.<=) #-}
  {-# INLINE (.<) #-}
  {-# INLINE symCompare #-}
  {-# INLINE (.>=) #-}
  {-# INLINE (.>) #-}
  (.<=) = symCompareSingleList True False
  (.<) = symCompareSingleList True True
  (.>=) = symCompareSingleList False False
  (.>) = symCompareSingleList False True
  symCompare = symLiftCompareList symCompare

instance SymOrd1 [] where
  liftSymCompare = symLiftCompareList
  {-# INLINE liftSymCompare #-}

-- ExceptT
instance (SymOrd1 m, SymOrd e, SymOrd a) => SymOrd (ExceptT e m a) where
  symCompare = symCompare1
  {-# INLINE symCompare #-}

instance (SymOrd1 m, SymOrd e) => SymOrd1 (ExceptT e m) where
  liftSymCompare f (ExceptT l) (ExceptT r) =
    liftSymCompare (liftSymCompare f) l r
  {-# INLINE liftSymCompare #-}

-- MaybeT
instance (SymOrd1 m, SymOrd a) => SymOrd (MaybeT m a) where
  symCompare = symCompare1
  {-# INLINE symCompare #-}

instance (SymOrd1 m) => SymOrd1 (MaybeT m) where
  liftSymCompare f (MaybeT l) (MaybeT r) = liftSymCompare (liftSymCompare f) l r
  {-# INLINE liftSymCompare #-}

-- Writer
instance (SymOrd1 m, SymOrd w, SymOrd a) => SymOrd (WriterLazy.WriterT w m a) where
  symCompare = symCompare1
  {-# INLINE symCompare #-}

instance (SymOrd1 m, SymOrd w) => SymOrd1 (WriterLazy.WriterT w m) where
  liftSymCompare f (WriterLazy.WriterT l) (WriterLazy.WriterT r) =
    liftSymCompare (liftSymCompare2 f symCompare) l r
  {-# INLINE liftSymCompare #-}

instance (SymOrd1 m, SymOrd w, SymOrd a) => SymOrd (WriterStrict.WriterT w m a) where
  symCompare = symCompare1
  {-# INLINE symCompare #-}

instance (SymOrd1 m, SymOrd w) => SymOrd1 (WriterStrict.WriterT w m) where
  liftSymCompare f (WriterStrict.WriterT l) (WriterStrict.WriterT r) =
    liftSymCompare (liftSymCompare2 f symCompare) l r
  {-# INLINE liftSymCompare #-}

-- IdentityT
instance (SymOrd1 m, SymOrd a) => SymOrd (IdentityT m a) where
  symCompare = symCompare1
  {-# INLINE symCompare #-}

instance (SymOrd1 m) => SymOrd1 (IdentityT m) where
  liftSymCompare f (IdentityT l) (IdentityT r) = liftSymCompare f l r
  {-# INLINE liftSymCompare #-}

-- Product
deriving via
  (Default (Product l r a))
  instance
    (SymOrd (l a), SymOrd (r a)) => SymOrd (Product l r a)

deriving via
  (Default1 (Product l r))
  instance
    (SymOrd1 l, SymOrd1 r) => SymOrd1 (Product l r)

-- Sum
deriving via
  (Default (Sum l r a))
  instance
    (SymOrd (l a), SymOrd (r a)) => SymOrd (Sum l r a)

deriving via
  (Default1 (Sum l r))
  instance
    (SymOrd1 l, SymOrd1 r) => SymOrd1 (Sum l r)

-- Compose
deriving via
  (Default (Compose f g a))
  instance
    (SymOrd (f (g a))) => SymOrd (Compose f g a)

instance (SymOrd1 f, SymOrd1 g) => SymOrd1 (Compose f g) where
  liftSymCompare f (Compose l) (Compose r) =
    liftSymCompare (liftSymCompare f) l r

-- Const
deriving via (Default (Const a b)) instance (SymOrd a) => SymOrd (Const a b)

deriving via (Default1 (Const a)) instance (SymOrd a) => SymOrd1 (Const a)

-- Alt
deriving via (Default (Alt f a)) instance (SymOrd (f a)) => SymOrd (Alt f a)

deriving via (Default1 (Alt f)) instance (SymOrd1 f) => SymOrd1 (Alt f)

-- Ap
deriving via (Default (Ap f a)) instance (SymOrd (f a)) => SymOrd (Ap f a)

deriving via (Default1 (Ap f)) instance (SymOrd1 f) => SymOrd1 (Ap f)

-- Generic
deriving via (Default (U1 p)) instance SymOrd (U1 p)

deriving via (Default (V1 p)) instance SymOrd (V1 p)

deriving via
  (Default (K1 i c p))
  instance
    (SymOrd c) => SymOrd (K1 i c p)

deriving via
  (Default (M1 i c f p))
  instance
    (SymOrd (f p)) => SymOrd (M1 i c f p)

deriving via
  (Default ((f :+: g) p))
  instance
    (SymOrd (f p), SymOrd (g p)) => SymOrd ((f :+: g) p)

deriving via
  (Default ((f :*: g) p))
  instance
    (SymOrd (f p), SymOrd (g p)) => SymOrd ((f :*: g) p)

deriving via
  (Default (Par1 p))
  instance
    (SymOrd p) => SymOrd (Par1 p)

deriving via
  (Default (Rec1 f p))
  instance
    (SymOrd (f p)) => SymOrd (Rec1 f p)

deriving via
  (Default ((f :.: g) p))
  instance
    (SymOrd (f (g p))) => SymOrd ((f :.: g) p)

-- Down
instance (SymOrd a) => SymOrd (Down a) where
  symCompare = symCompare1
  {-# INLINE symCompare #-}

instance SymOrd1 Down where
  liftSymCompare comp (Down l) (Down r) = do
    res <- comp l r
    case res of
      LT -> mrgSingle GT
      EQ -> mrgSingle EQ
      GT -> mrgSingle LT
  {-# INLINE liftSymCompare #-}

instance SymOrd2 Either where
  liftSymCompare2 f _ (Left l) (Left r) = f l r
  liftSymCompare2 _ g (Right l) (Right r) = g l r
  liftSymCompare2 _ _ (Left _) (Right _) = mrgSingle LT
  liftSymCompare2 _ _ (Right _) (Left _) = mrgSingle GT
  {-# INLINE liftSymCompare2 #-}

instance SymOrd2 (,) where
  liftSymCompare2 f g (a1, b1) (a2, b2) = do
    ma <- f a1 a2
    mb <- g b1 b2
    mrgSingle $ ma <> mb
  {-# INLINE liftSymCompare2 #-}

instance (SymOrd a) => SymOrd2 ((,,) a) where
  liftSymCompare2 f g (a1, b1, c1) (a2, b2, c2) = do
    ma <- symCompare a1 a2
    mb <- f b1 b2
    mc <- g c1 c2
    mrgSingle $ ma <> mb <> mc
  {-# INLINE liftSymCompare2 #-}

instance (SymOrd a, SymOrd b) => SymOrd2 ((,,,) a b) where
  liftSymCompare2 f g (a1, b1, c1, d1) (a2, b2, c2, d2) = do
    ma <- symCompare a1 a2
    mb <- symCompare b1 b2
    mc <- f c1 c2
    md <- g d1 d2
    mrgSingle $ ma <> mb <> mc <> md
  {-# INLINE liftSymCompare2 #-}
