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
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      :   Grisette.Internal.Internal.Decl.Core.Data.Class.SymOrd
-- Copyright   :   (c) Sirui Lu 2021-2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Internal.Internal.Decl.Core.Data.Class.SymOrd
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

import Data.Kind (Type)
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
import Grisette.Internal.Core.Data.Class.ITEOp (ITEOp, symIte)
import Grisette.Internal.Core.Data.Class.LogicalOp
  ( LogicalOp (symNot),
  )
import Grisette.Internal.Core.Data.Class.PlainUnion
  ( simpleMerge,
  )
import Grisette.Internal.Core.Data.Class.Solvable (Solvable (con))
import Grisette.Internal.Internal.Decl.Core.Control.Monad.Union (Union)
import Grisette.Internal.Internal.Decl.Core.Data.Class.Mergeable
  ( Mergeable,
  )
import Grisette.Internal.Internal.Decl.Core.Data.Class.SimpleMergeable
  ( SymBranching,
    mrgIf,
  )
import Grisette.Internal.Internal.Decl.Core.Data.Class.SymEq
  ( GSymEq,
    SymEq ((.==)),
    SymEq1,
    SymEq2,
  )
import Grisette.Internal.Internal.Decl.Core.Data.Class.TryMerge
  ( mrgSingle,
  )
import Grisette.Internal.SymPrim.SymBool (SymBool)
import Grisette.Internal.Utils.Derive (Arity0, Arity1)

-- $setup
-- >>> import Grisette.Core
-- >>> import Grisette.SymPrim

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
