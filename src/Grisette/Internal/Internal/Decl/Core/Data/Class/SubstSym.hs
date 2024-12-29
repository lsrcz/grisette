{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      :   Grisette.Internal.Internal.Decl.Core.Data.Class.SubstSym
-- Copyright   :   (c) Sirui Lu 2021-2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Internal.Internal.Decl.Core.Data.Class.SubstSym
  ( -- * Substituting symbolic constants
    SubstSym (..),
    SubstSym1 (..),
    substSym1,
    SubstSym2 (..),
    substSym2,

    -- * Generic 'SubstSym'
    SubstSymArgs (..),
    GSubstSym (..),
    genericSubstSym,
    genericLiftSubstSym,
  )
where

import Data.Kind (Type)
import Generics.Deriving
  ( Default (Default, unDefault),
    Default1 (Default1, unDefault1),
    Generic (Rep, from, to),
    Generic1 (Rep1, from1, to1),
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
import Generics.Deriving.Instances ()
import Grisette.Internal.SymPrim.Prim.Term
  ( IsSymbolKind,
    LinkedRep,
    SymbolKind,
    TypedSymbol,
  )
import Grisette.Internal.Utils.Derive (Arity0, Arity1)

-- $setup
-- >>> import Grisette.Core
-- >>> import Grisette.SymPrim

-- | Substitution of symbols (symbolic constants) to a symbolic value.
--
-- >>> a = "a" :: TypedAnySymbol Bool
-- >>> v = "x" .&& "y" :: SymBool
-- >>> substSym a v (["a" .&& "b", "a"] :: [SymBool])
-- [(&& (&& x y) b),(&& x y)]
--
-- __Note 1:__ This type class can be derived for algebraic data types.
-- You may need the @DerivingVia@ and @DerivingStrategies@ extensions.
--
-- > data X = ... deriving Generic deriving SubstSym via (Default X)
class SubstSym a where
  -- Substitute a symbolic constant to some symbolic value
  --
  -- >>> substSym "a" ("c" .&& "d" :: Sym Bool) ["a" .&& "b" :: Sym Bool, "a"]
  -- [(&& (&& c d) b),(&& c d)]
  substSym ::
    (LinkedRep cb sb, IsSymbolKind knd) =>
    TypedSymbol knd cb ->
    sb ->
    a ->
    a

-- | Lifting of 'SubstSym' to unary type constructors.
class
  (forall a. (SubstSym a) => SubstSym (f a)) =>
  SubstSym1 f
  where
  -- | Lift a symbol substitution function to unary type constructors.
  liftSubstSym ::
    (LinkedRep cb sb, IsSymbolKind knd) =>
    (TypedSymbol knd cb -> sb -> a -> a) ->
    TypedSymbol knd cb ->
    sb ->
    f a ->
    f a

-- | Lifting the standard 'substSym' to unary type constructors.
substSym1 ::
  (SubstSym1 f, SubstSym a, LinkedRep cb sb, IsSymbolKind knd) =>
  TypedSymbol knd cb ->
  sb ->
  f a ->
  f a
substSym1 = liftSubstSym substSym

-- | Lifting of 'SubstSym' to binary type constructors.
class
  (forall a. (SubstSym a) => SubstSym1 (f a)) =>
  SubstSym2 f
  where
  -- | Lift a symbol substitution function to binary type constructors.
  liftSubstSym2 ::
    (LinkedRep cb sb, IsSymbolKind knd) =>
    (TypedSymbol knd cb -> sb -> a -> a) ->
    (TypedSymbol knd cb -> sb -> b -> b) ->
    TypedSymbol knd cb ->
    sb ->
    f a b ->
    f a b

-- | Lifting the standard 'substSym' to binary type constructors.
substSym2 ::
  (SubstSym2 f, SubstSym a, SubstSym b, LinkedRep cb sb, IsSymbolKind knd) =>
  TypedSymbol knd cb ->
  sb ->
  f a b ->
  f a b
substSym2 = liftSubstSym2 substSym substSym

-- Derivations

-- | The arguments to the generic 'substSym' function.
data family SubstSymArgs arity (knd :: SymbolKind) a cb sb :: Type

data instance SubstSymArgs Arity0 _ _ _ _ = SubstSymArgs0

newtype instance SubstSymArgs Arity1 knd a cb sb
  = SubstSymArgs1 (TypedSymbol knd cb -> sb -> a -> a)

-- | The class of types where we can generically substitute the symbols in a
-- value.
class GSubstSym arity f where
  gsubstSym ::
    (LinkedRep cb sb, IsSymbolKind knd) =>
    SubstSymArgs arity knd a cb sb ->
    TypedSymbol knd cb ->
    sb ->
    f a ->
    f a

instance GSubstSym arity V1 where
  gsubstSym _ _ _ = id
  {-# INLINE gsubstSym #-}

instance GSubstSym arity U1 where
  gsubstSym _ _ _ = id
  {-# INLINE gsubstSym #-}

instance (SubstSym a) => GSubstSym arity (K1 i a) where
  gsubstSym _ sym val (K1 v) = K1 $ substSym sym val v
  {-# INLINE gsubstSym #-}

instance (GSubstSym arity a) => GSubstSym arity (M1 i c a) where
  gsubstSym args sym val (M1 v) = M1 $ gsubstSym args sym val v
  {-# INLINE gsubstSym #-}

instance (GSubstSym arity a, GSubstSym arity b) => GSubstSym arity (a :*: b) where
  gsubstSym args sym val (a :*: b) =
    gsubstSym args sym val a :*: gsubstSym args sym val b
  {-# INLINE gsubstSym #-}

instance (GSubstSym arity a, GSubstSym arity b) => GSubstSym arity (a :+: b) where
  gsubstSym args sym val (L1 l) = L1 $ gsubstSym args sym val l
  gsubstSym args sym val (R1 r) = R1 $ gsubstSym args sym val r
  {-# INLINE gsubstSym #-}

instance (SubstSym1 a) => GSubstSym Arity1 (Rec1 a) where
  gsubstSym (SubstSymArgs1 f) sym val (Rec1 v) =
    Rec1 $ liftSubstSym f sym val v
  {-# INLINE gsubstSym #-}

instance GSubstSym Arity1 Par1 where
  gsubstSym (SubstSymArgs1 f) sym val (Par1 v) = Par1 $ f sym val v
  {-# INLINE gsubstSym #-}

instance
  (SubstSym1 f, GSubstSym Arity1 g) =>
  GSubstSym Arity1 (f :.: g)
  where
  gsubstSym targs sym val (Comp1 x) =
    Comp1 $ liftSubstSym (gsubstSym targs) sym val x
  {-# INLINE gsubstSym #-}

-- | Generic 'substSym' function.
genericSubstSym ::
  (Generic a, GSubstSym Arity0 (Rep a), LinkedRep cb sb, IsSymbolKind knd) =>
  TypedSymbol knd cb ->
  sb ->
  a ->
  a
genericSubstSym sym val =
  to . gsubstSym SubstSymArgs0 sym val . from
{-# INLINE genericSubstSym #-}

-- | Generic 'liftSubstSym' function.
genericLiftSubstSym ::
  (Generic1 f, GSubstSym Arity1 (Rep1 f), LinkedRep cb sb, IsSymbolKind knd) =>
  (TypedSymbol knd cb -> sb -> a -> a) ->
  TypedSymbol knd cb ->
  sb ->
  f a ->
  f a
genericLiftSubstSym f sym val =
  to1 . gsubstSym (SubstSymArgs1 f) sym val . from1
{-# INLINE genericLiftSubstSym #-}

instance
  (Generic a, GSubstSym Arity0 (Rep a)) =>
  SubstSym (Default a)
  where
  substSym sym val = Default . genericSubstSym sym val . unDefault
  {-# INLINE substSym #-}

instance
  (Generic1 f, GSubstSym Arity1 (Rep1 f), SubstSym a) =>
  SubstSym (Default1 f a)
  where
  substSym = substSym1
  {-# INLINE substSym #-}

instance
  (Generic1 f, GSubstSym Arity1 (Rep1 f)) =>
  SubstSym1 (Default1 f)
  where
  liftSubstSym f sym val =
    Default1 . genericLiftSubstSym f sym val . unDefault1
  {-# INLINE liftSubstSym #-}
