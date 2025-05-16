{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      :   Grisette.Internal.Internal.Decl.SymPrim.AllSyms
-- Copyright   :   (c) Sirui Lu 2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Internal.Internal.Decl.SymPrim.AllSyms
  ( -- * Get all symbolic primitive values in a value
    SomeSym (..),
    AllSyms (..),
    AllSyms1 (..),
    allSymsS1,
    AllSyms2 (..),
    allSymsS2,
    allSymsSize,
    symSize,
    symsSize,

    -- * Generic 'AllSyms'
    AllSymsArgs (..),
    GAllSyms (..),
    genericAllSymsS,
    genericLiftAllSymsS,
  )
where

import Data.Kind (Type)
import GHC.Generics
  ( Generic (Rep, from),
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
import Generics.Deriving
  ( Default (unDefault),
    Default1 (unDefault1),
  )
import Grisette.Internal.Core.Data.Class.AsKey (AsKey (AsKey), AsKey1 (AsKey1))
import Grisette.Internal.SymPrim.Prim.SomeTerm
  ( SomeTerm (SomeTerm),
  )
import Grisette.Internal.SymPrim.Prim.Term
  ( LinkedRep (underlyingTerm),
    pformatTerm,
  )
import Grisette.Internal.SymPrim.Prim.TermUtils
  ( someTermsSize,
    termSize,
    termsSize,
  )
import Grisette.Internal.Utils.Derive (Arity0, Arity1)

-- $setup
-- >>> import Grisette.Core
-- >>> import Grisette.SymPrim
-- >>> import Grisette.Backend
-- >>> import Data.Proxy

-- | Some symbolic value with 'LinkedRep' constraint.
data SomeSym where
  SomeSym :: (LinkedRep con sym) => sym -> SomeSym

instance Show SomeSym where
  show (SomeSym s) = pformatTerm $ underlyingTerm s

-- | Extract all symbolic primitive values that are represented as SMT terms.
--
-- >>> allSyms (["a" + 1 :: SymInteger, -"b"], "c" :: SymBool)
-- [(+ 1 a),(- b),c]
--
-- This is usually used for getting a statistical summary of the size of
-- a symbolic value with 'allSymsSize'.
--
-- __Note:__ This type class can be derived for algebraic data types. You may
-- need the @DerivingVia@ and @DerivingStrategies@ extenstions.
--
-- > data X = ... deriving Generic deriving AllSyms via (Default X)
class AllSyms a where
  -- | Convert a value to a list of symbolic primitive values. It should
  -- prepend to an existing list of symbolic primitive values.
  allSymsS :: a -> [SomeSym] -> [SomeSym]
  allSymsS a l = allSyms a ++ l

  -- | Specialized 'allSymsS' that prepends to an empty list.
  allSyms :: a -> [SomeSym]
  allSyms a = allSymsS a []

  {-# MINIMAL allSymsS | allSyms #-}

-- | Get the sum of the sizes of a list of symbolic terms.
-- Duplicate sub-terms are counted for only once.
--
-- >>> symsSize [1, "a" :: SymInteger, "a" + 1 :: SymInteger]
-- 3
symsSize :: forall con sym. (LinkedRep con sym) => [sym] -> Int
symsSize = termsSize . fmap (underlyingTerm @con)
{-# INLINE symsSize #-}

-- | Get the size of a symbolic term.
-- Duplicate sub-terms are counted for only once.
--
-- >>> symSize (1 :: SymInteger)
-- 1
-- >>> symSize ("a" :: SymInteger)
-- 1
-- >>> symSize ("a" + 1 :: SymInteger)
-- 3
-- >>> symSize (("a" + 1) * ("a" + 1) :: SymInteger)
-- 4
symSize :: forall con sym. (LinkedRep con sym) => sym -> Int
symSize = termSize . underlyingTerm @con
{-# INLINE symSize #-}

someUnderlyingTerm :: SomeSym -> SomeTerm
someUnderlyingTerm (SomeSym s) = SomeTerm $ underlyingTerm s

someSymsSize :: [SomeSym] -> Int
someSymsSize = someTermsSize . fmap someUnderlyingTerm
{-# INLINE someSymsSize #-}

-- | Get the total size of symbolic terms in a value.
-- Duplicate sub-terms are counted for only once.
--
-- >>> allSymsSize ("a" :: SymInteger, "a" + "b" :: SymInteger, ("a" + "b") * "c" :: SymInteger)
-- 5
--
-- The 5 terms are @a@, @b@, @(+ a b)@, @c@, and @(* (+ a b) c)@.
allSymsSize :: (AllSyms a) => a -> Int
allSymsSize = someSymsSize . allSyms

-- | Lifting of the 'AllSyms' class to unary type constructors.
class (forall a. (AllSyms a) => AllSyms (f a)) => AllSyms1 f where
  -- | Lift the 'allSymsS' function to unary type constructors.
  liftAllSymsS :: (a -> [SomeSym] -> [SomeSym]) -> f a -> [SomeSym] -> [SomeSym]

-- | Lift the standard 'allSymsS' function to unary type constructors.
allSymsS1 :: (AllSyms1 f, AllSyms a) => f a -> [SomeSym] -> [SomeSym]
allSymsS1 = liftAllSymsS allSymsS
{-# INLINE allSymsS1 #-}

-- | Lifting of the 'AllSyms' class to binary type constructors.
class (forall a. (AllSyms a) => AllSyms1 (f a)) => AllSyms2 f where
  -- | Lift the 'allSymsS' function to binary type constructors.
  liftAllSymsS2 ::
    (a -> [SomeSym] -> [SomeSym]) ->
    (b -> [SomeSym] -> [SomeSym]) ->
    f a b ->
    [SomeSym] ->
    [SomeSym]

-- | Lift the standard 'allSymsS' function to binary type constructors.
allSymsS2 ::
  (AllSyms2 f, AllSyms a, AllSyms b) => f a b -> [SomeSym] -> [SomeSym]
allSymsS2 = liftAllSymsS2 allSymsS allSymsS
{-# INLINE allSymsS2 #-}

-- Derivation

-- | The arguments to the generic 'AllSyms' function.
data family AllSymsArgs arity a :: Type

data instance AllSymsArgs Arity0 _ = AllSymsArgs0

newtype instance AllSymsArgs Arity1 a
  = AllSymsArgs1 (a -> [SomeSym] -> [SomeSym])

-- | The class of types that can generically extract all symbolic primitives.
class GAllSyms arity f where
  gallSymsS :: AllSymsArgs arity a -> f a -> [SomeSym] -> [SomeSym]

instance GAllSyms arity V1 where
  gallSymsS _ _ = id

instance GAllSyms arity U1 where
  gallSymsS _ _ = id

instance (AllSyms c) => GAllSyms arity (K1 i c) where
  gallSymsS _ (K1 x) = allSymsS x

instance (GAllSyms arity a) => GAllSyms arity (M1 i c a) where
  gallSymsS args (M1 x) = gallSymsS args x

instance (GAllSyms arity a, GAllSyms arity b) => GAllSyms arity (a :+: b) where
  gallSymsS args (L1 l) = gallSymsS args l
  gallSymsS args (R1 r) = gallSymsS args r

instance (GAllSyms arity a, GAllSyms arity b) => GAllSyms arity (a :*: b) where
  gallSymsS args (a :*: b) = gallSymsS args a . gallSymsS args b

instance GAllSyms Arity1 Par1 where
  gallSymsS (AllSymsArgs1 f) (Par1 x) = f x

instance (AllSyms1 f) => GAllSyms Arity1 (Rec1 f) where
  gallSymsS (AllSymsArgs1 f) (Rec1 x) = liftAllSymsS f x

instance (AllSyms1 f, GAllSyms Arity1 g) => GAllSyms Arity1 (f :.: g) where
  gallSymsS targs (Comp1 x) = liftAllSymsS (gallSymsS targs) x

-- | Generic 'allSymsS' function.
genericAllSymsS ::
  (Generic a, GAllSyms Arity0 (Rep a)) =>
  a ->
  [SomeSym] ->
  [SomeSym]
genericAllSymsS x = gallSymsS AllSymsArgs0 (from x)
{-# INLINE genericAllSymsS #-}

-- | Generic 'liftAllSymsS' function.
genericLiftAllSymsS ::
  (Generic1 f, GAllSyms Arity1 (Rep1 f)) =>
  (a -> [SomeSym] -> [SomeSym]) ->
  f a ->
  [SomeSym] ->
  [SomeSym]
genericLiftAllSymsS f x = gallSymsS (AllSymsArgs1 f) (from1 x)
{-# INLINE genericLiftAllSymsS #-}

instance (Generic a, GAllSyms Arity0 (Rep a)) => AllSyms (Default a) where
  allSymsS = genericAllSymsS . unDefault
  {-# INLINE allSymsS #-}

instance
  (Generic1 f, GAllSyms Arity1 (Rep1 f), AllSyms a) =>
  AllSyms (Default1 f a)
  where
  allSymsS = allSymsS1
  {-# INLINE allSymsS #-}

instance (Generic1 f, GAllSyms Arity1 (Rep1 f)) => AllSyms1 (Default1 f) where
  liftAllSymsS f = genericLiftAllSymsS f . unDefault1
  {-# INLINE liftAllSymsS #-}

instance (AllSyms a) => AllSyms (AsKey a) where
  allSymsS (AsKey a) = allSymsS a
  {-# INLINE allSymsS #-}

instance (AllSyms1 f) => AllSyms1 (AsKey1 f) where
  liftAllSymsS f (AsKey1 a) = liftAllSymsS f a
  {-# INLINE liftAllSymsS #-}

instance (AllSyms1 f, AllSyms a) => AllSyms (AsKey1 f a) where
  allSymsS (AsKey1 a) = allSymsS a
  {-# INLINE allSymsS #-}
