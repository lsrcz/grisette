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
-- Module      :   Grisette.Internal.Core.Data.Class.SOrd
-- Copyright   :   (c) Sirui Lu 2021-2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Internal.Core.Data.Class.SOrd
  ( -- * Symbolic total order relation
    SOrd (..),
    SOrd1 (..),
    symCompare1,
    SOrd2 (..),
    symCompare2,

    -- * Min and max
    symMax,
    symMin,
    mrgMax,
    mrgMin,

    -- * Generic 'SOrd'
    SOrdArgs (..),
    GSOrd (..),
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
import Data.Functor.Sum (Sum)
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Kind (Type)
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
import Grisette.Internal.Core.Control.Monad.UnionM (UnionM, liftToMonadUnion)
import Grisette.Internal.Core.Data.Class.ITEOp (ITEOp, symIte)
import Grisette.Internal.Core.Data.Class.LogicalOp
  ( LogicalOp (symNot, (.&&), (.||)),
  )
import Grisette.Internal.Core.Data.Class.Mergeable (Mergeable)
import Grisette.Internal.Core.Data.Class.PlainUnion
  ( simpleMerge,
  )
import Grisette.Internal.Core.Data.Class.SEq (GSEq, SEq ((.==)), SEq1, SEq2)
import Grisette.Internal.Core.Data.Class.SimpleMergeable
  ( UnionMergeable1,
    mrgIf,
  )
import Grisette.Internal.Core.Data.Class.Solvable (Solvable (con))
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
import Grisette.Internal.TH.Derivation
  ( Strategy (ViaDefault, ViaDefault1),
    deriveFunctorArgBuiltins,
    deriveSimpleBuiltin1s,
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
-- `Grisette.Core.Control.Monad.UnionMBase` and `PlainUnion` for more
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
  symCompare :: a -> a -> UnionM Ordering
  symCompare l r =
    mrgIf
      (l .< r)
      (mrgSingle LT)
      (mrgIf (l .== r) (mrgSingle EQ) (mrgSingle GT))
  {-# INLINE symCompare #-}
  {-# MINIMAL (.<) | symCompare #-}

-- | Lifting of the 'SOrd' class to unary type constructors.
--
-- Any instance should be subject to the following law that canonicity is
-- preserved:
--
-- @liftSymCompare symCompare@ should be equivalent to @symCompare@, under the
-- symbolic semantics.
--
-- This class therefore represents the generalization of 'SOrd' by decomposing
-- its main method into a canonical lifting on a canonical inner method, so that
-- the lifting can be reused for other arguments than the canonical one.
class (SEq1 f, forall a. (SOrd a) => SOrd (f a)) => SOrd1 f where
  -- | Lift a 'symCompare' function through the type constructor.
  --
  -- The function will usually be applied to an symbolic comparison function,
  -- but the more general type ensures that the implementation uses it to
  -- compare elements of the first container with elements of the second.
  liftSymCompare :: (a -> b -> UnionM Ordering) -> f a -> f b -> UnionM Ordering

-- | Lift the standard 'symCompare' function to binary type constructors.
symCompare1 :: (SOrd1 f, SOrd a) => f a -> f a -> UnionM Ordering
symCompare1 = liftSymCompare symCompare
{-# INLINE symCompare1 #-}

-- | Lifting of the 'SOrd' class to binary type constructors.
class (SEq2 f, forall a. (SOrd a) => SOrd1 (f a)) => SOrd2 f where
  -- | Lift a 'symCompare' function through the type constructor.
  --
  -- The function will usually be applied to an symbolic comparison function,
  -- but the more general type ensures that the implementation uses it to
  -- compare elements of the first container with elements of the second.
  liftSymCompare2 ::
    (a -> b -> UnionM Ordering) ->
    (c -> d -> UnionM Ordering) ->
    f a c ->
    f b d ->
    UnionM Ordering

-- | Lift the standard 'symCompare' function through the type constructors.
symCompare2 :: (SOrd2 f, SOrd a, SOrd b) => f a b -> f a b -> UnionM Ordering
symCompare2 = liftSymCompare2 symCompare symCompare
{-# INLINE symCompare2 #-}

-- | Symbolic maximum.
symMax :: (SOrd a, ITEOp a) => a -> a -> a
symMax x y = symIte (x .>= y) x y
{-# INLINE symMax #-}

-- | Symbolic minimum.
symMin :: (SOrd a, ITEOp a) => a -> a -> a
symMin x y = symIte (x .>= y) y x
{-# INLINE symMin #-}

-- | Symbolic maximum, with a union-like monad.
mrgMax ::
  (SOrd a, Mergeable a, UnionMergeable1 m, Applicative m) =>
  a ->
  a ->
  m a
mrgMax x y = mrgIf (x .>= y) (pure x) (pure y)
{-# INLINE mrgMax #-}

-- | Symbolic minimum, with a union-like monad.
mrgMin ::
  (SOrd a, Mergeable a, UnionMergeable1 m, Applicative m) =>
  a ->
  a ->
  m a
mrgMin x y = mrgIf (x .>= y) (pure y) (pure x)
{-# INLINE mrgMin #-}

-- Derivations

-- | The arguments to the generic comparison function.
data family SOrdArgs arity a b :: Type

data instance SOrdArgs Arity0 _ _ = SOrdArgs0

newtype instance SOrdArgs Arity1 a b
  = SOrdArgs1 (a -> b -> UnionM Ordering)

-- | The class of types that can be generically symbolically compared.
class GSOrd arity f where
  gsymCompare :: SOrdArgs arity a b -> f a -> f b -> UnionM Ordering

instance GSOrd arity V1 where
  gsymCompare _ _ _ = mrgSingle EQ
  {-# INLINE gsymCompare #-}

instance GSOrd arity U1 where
  gsymCompare _ _ _ = mrgSingle EQ
  {-# INLINE gsymCompare #-}

instance
  (GSOrd arity a, GSOrd arity b) =>
  GSOrd arity (a :*: b)
  where
  gsymCompare args (a1 :*: b1) (a2 :*: b2) = do
    l <- gsymCompare args a1 a2
    case l of
      EQ -> gsymCompare args b1 b2
      _ -> mrgSingle l
  {-# INLINE gsymCompare #-}

instance
  (GSOrd arity a, GSOrd arity b) =>
  GSOrd arity (a :+: b)
  where
  gsymCompare args (L1 a) (L1 b) = gsymCompare args a b
  gsymCompare _ (L1 _) (R1 _) = mrgSingle LT
  gsymCompare args (R1 a) (R1 b) = gsymCompare args a b
  gsymCompare _ (R1 _) (L1 _) = mrgSingle GT
  {-# INLINE gsymCompare #-}

instance (GSOrd arity a) => GSOrd arity (M1 i c a) where
  gsymCompare args (M1 a) (M1 b) = gsymCompare args a b
  {-# INLINE gsymCompare #-}

instance (SOrd a) => GSOrd arity (K1 i a) where
  gsymCompare _ (K1 a) (K1 b) = a `symCompare` b
  {-# INLINE gsymCompare #-}

instance GSOrd Arity1 Par1 where
  gsymCompare (SOrdArgs1 c) (Par1 a) (Par1 b) = c a b
  {-# INLINE gsymCompare #-}

instance (SOrd1 f) => GSOrd Arity1 (Rec1 f) where
  gsymCompare (SOrdArgs1 c) (Rec1 a) (Rec1 b) = liftSymCompare c a b
  {-# INLINE gsymCompare #-}

instance (SOrd1 f, GSOrd Arity1 g) => GSOrd Arity1 (f :.: g) where
  gsymCompare targs (Comp1 a) (Comp1 b) = liftSymCompare (gsymCompare targs) a b
  {-# INLINE gsymCompare #-}

instance
  (Generic a, GSOrd Arity0 (Rep a), GSEq Arity0 (Rep a)) =>
  SOrd (Default a)
  where
  symCompare (Default l) (Default r) = genericSymCompare l r
  {-# INLINE symCompare #-}

-- | Generic 'symCompare' function.
genericSymCompare :: (Generic a, GSOrd Arity0 (Rep a)) => a -> a -> UnionM Ordering
genericSymCompare l r = gsymCompare SOrdArgs0 (from l) (from r)
{-# INLINE genericSymCompare #-}

instance
  (Generic1 f, GSOrd Arity1 (Rep1 f), GSEq Arity1 (Rep1 f), SOrd a) =>
  SOrd (Default1 f a)
  where
  symCompare = symCompare1
  {-# INLINE symCompare #-}

instance
  (Generic1 f, GSOrd Arity1 (Rep1 f), GSEq Arity1 (Rep1 f)) =>
  SOrd1 (Default1 f)
  where
  liftSymCompare c (Default1 l) (Default1 r) = genericLiftSymCompare c l r
  {-# INLINE liftSymCompare #-}

-- | Generic 'liftSymCompare' function.
genericLiftSymCompare ::
  (Generic1 f, GSOrd Arity1 (Rep1 f)) =>
  (a -> b -> UnionM Ordering) ->
  f a ->
  f b ->
  UnionM Ordering
genericLiftSymCompare c l r = gsymCompare (SOrdArgs1 c) (from1 l) (from1 r)
{-# INLINE genericLiftSymCompare #-}

-- Instances
deriveFunctorArgBuiltins
  (ViaDefault ''SOrd)
  ''SOrd
  ''SOrd1
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
    ''Identity
  ]

deriveSimpleBuiltin1s
  (ViaDefault1 ''SOrd1)
  ''SOrd
  ''SOrd1
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
    ''(,,,,,,,,,,,,,,)
  ]

symCompareSingleList :: (SOrd a) => Bool -> Bool -> [a] -> [a] -> SymBool
symCompareSingleList isLess isStrict = go
  where
    go [] [] = con (not isStrict)
    go (x : xs) (y : ys) =
      (if isLess then x .< y else x .> y) .|| (x .== y .&& go xs ys)
    go [] _ = if isLess then con True else con False
    go _ [] = if isLess then con False else con True

symLiftCompareList ::
  (a -> b -> UnionM Ordering) -> [a] -> [b] -> UnionM Ordering
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
instance (SOrd a) => SOrd [a] where
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

instance SOrd1 [] where
  liftSymCompare = symLiftCompareList
  {-# INLINE liftSymCompare #-}

-- ExceptT
instance (SOrd1 m, SOrd e, SOrd a) => SOrd (ExceptT e m a) where
  symCompare = symCompare1
  {-# INLINE symCompare #-}

instance (SOrd1 m, SOrd e) => SOrd1 (ExceptT e m) where
  liftSymCompare f (ExceptT l) (ExceptT r) =
    liftSymCompare (liftSymCompare f) l r
  {-# INLINE liftSymCompare #-}

-- MaybeT
instance (SOrd1 m, SOrd a) => SOrd (MaybeT m a) where
  symCompare = symCompare1
  {-# INLINE symCompare #-}

instance (SOrd1 m) => SOrd1 (MaybeT m) where
  liftSymCompare f (MaybeT l) (MaybeT r) = liftSymCompare (liftSymCompare f) l r
  {-# INLINE liftSymCompare #-}

-- Writer
instance (SOrd1 m, SOrd w, SOrd a) => SOrd (WriterLazy.WriterT w m a) where
  symCompare = symCompare1
  {-# INLINE symCompare #-}

instance (SOrd1 m, SOrd w) => SOrd1 (WriterLazy.WriterT w m) where
  liftSymCompare f (WriterLazy.WriterT l) (WriterLazy.WriterT r) =
    liftSymCompare (liftSymCompare2 f symCompare) l r
  {-# INLINE liftSymCompare #-}

instance (SOrd1 m, SOrd w, SOrd a) => SOrd (WriterStrict.WriterT w m a) where
  symCompare = symCompare1
  {-# INLINE symCompare #-}

instance (SOrd1 m, SOrd w) => SOrd1 (WriterStrict.WriterT w m) where
  liftSymCompare f (WriterStrict.WriterT l) (WriterStrict.WriterT r) =
    liftSymCompare (liftSymCompare2 f symCompare) l r
  {-# INLINE liftSymCompare #-}

-- Sum
deriving via
  (Default (Sum f g a))
  instance
    (SOrd (f a), SOrd (g a)) => SOrd (Sum f g a)

deriving via
  (Default1 (Sum f g))
  instance
    (SOrd1 f, SOrd1 g) => SOrd1 (Sum f g)

-- IdentityT
instance (SOrd1 m, SOrd a) => SOrd (IdentityT m a) where
  symCompare = symCompare1
  {-# INLINE symCompare #-}

instance (SOrd1 m) => SOrd1 (IdentityT m) where
  liftSymCompare f (IdentityT l) (IdentityT r) = liftSymCompare f l r
  {-# INLINE liftSymCompare #-}

instance SOrd2 Either where
  liftSymCompare2 f _ (Left l) (Left r) = f l r
  liftSymCompare2 _ g (Right l) (Right r) = g l r
  liftSymCompare2 _ _ (Left _) (Right _) = mrgSingle LT
  liftSymCompare2 _ _ (Right _) (Left _) = mrgSingle GT
  {-# INLINE liftSymCompare2 #-}

instance SOrd2 (,) where
  liftSymCompare2 f g (a1, b1) (a2, b2) = do
    ma <- f a1 a2
    mb <- g b1 b2
    mrgSingle $ ma <> mb
  {-# INLINE liftSymCompare2 #-}

instance (SOrd a) => SOrd2 ((,,) a) where
  liftSymCompare2 f g (a1, b1, c1) (a2, b2, c2) = do
    ma <- symCompare a1 a2
    mb <- f b1 b2
    mc <- g c1 c2
    mrgSingle $ ma <> mb <> mc
  {-# INLINE liftSymCompare2 #-}

instance (SOrd a, SOrd b) => SOrd2 ((,,,) a b) where
  liftSymCompare2 f g (a1, b1, c1, d1) (a2, b2, c2, d2) = do
    ma <- symCompare a1 a2
    mb <- symCompare b1 b2
    mc <- f c1 c2
    md <- g d1 d2
    mrgSingle $ ma <> mb <> mc <> md
  {-# INLINE liftSymCompare2 #-}

#define CONCRETE_SORD(type) \
instance SOrd type where \
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
instance (KnownNat n, 1 <= n) => SOrd (type n) where \
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
CONCRETE_SORD(B.ByteString)
CONCRETE_SORD(T.Text)
CONCRETE_SORD(FPRoundingMode)
CONCRETE_SORD_BV(WordN)
CONCRETE_SORD_BV(IntN)
#endif

instance (ValidFP eb sb) => SOrd (FP eb sb) where
  l .<= r = con $ l <= r
  {-# INLINE (.<=) #-}
  l .< r = con $ l < r
  {-# INLINE (.<) #-}
  l .>= r = con $ l >= r
  {-# INLINE (.>=) #-}
  l .> r = con $ l > r
  {-# INLINE (.>) #-}

-- SOrd
#define SORD_SIMPLE(symtype) \
instance SOrd symtype where \
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
instance (KnownNat n, 1 <= n) => SOrd (symtype n) where \
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

instance (ValidFP eb sb) => SOrd (SymFP eb sb) where
  (SymFP a) .<= (SymFP b) = SymBool $ pevalLeOrdTerm a b
  {-# INLINE (.<=) #-}
  (SymFP a) .< (SymFP b) = SymBool $ pevalLtOrdTerm a b
  {-# INLINE (.<) #-}
  (SymFP a) .>= (SymFP b) = SymBool $ pevalGeOrdTerm a b
  {-# INLINE (.>=) #-}
  (SymFP a) .> (SymFP b) = SymBool $ pevalGtOrdTerm a b
  {-# INLINE (.>) #-}

instance SOrd SymBool where
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
SORD_SIMPLE(SymFPRoundingMode)
SORD_BV(SymIntN)
SORD_BV(SymWordN)
#endif

-- UnionM
instance (SOrd a, Mergeable a) => SOrd (UnionM a) where
  x .<= y = simpleMerge $ do
    x1 <- tryMerge x
    y1 <- tryMerge y
    mrgSingle $ x1 .<= y1
  x .< y = simpleMerge $ do
    x1 <- tryMerge x
    y1 <- tryMerge y
    mrgSingle $ x1 .< y1
  x .>= y = simpleMerge $ do
    x1 <- tryMerge x
    y1 <- tryMerge y
    mrgSingle $ x1 .>= y1
  x .> y = simpleMerge $ do
    x1 <- tryMerge x
    y1 <- tryMerge y
    mrgSingle $ x1 .> y1
  x `symCompare` y = liftToMonadUnion $ do
    x1 <- tryMerge x
    y1 <- tryMerge y
    x1 `symCompare` y1
