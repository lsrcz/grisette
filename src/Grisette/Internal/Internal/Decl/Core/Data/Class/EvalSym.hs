{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      :   Grisette.Internal.Internal.Decl.Core.Data.Class.EvalSym
-- Copyright   :   (c) Sirui Lu 2021-2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Internal.Internal.Decl.Core.Data.Class.EvalSym
  ( -- * Evaluating symbolic values with model
    EvalSym (..),
    evalSymToCon,
    EvalSym1 (..),
    evalSym1,
    evalSymToCon1,
    EvalSym2 (..),
    evalSym2,
    evalSymToCon2,

    -- * Generic 'EvalSym'
    EvalSymArgs (..),
    GEvalSym (..),
    genericEvalSym,
    genericLiftEvalSym,
  )
where

import Data.Kind (Type)
import Data.Maybe (fromJust)
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
import Grisette.Internal.Core.Data.Class.ToCon
  ( ToCon (toCon),
    ToCon1,
    ToCon2,
    toCon1,
    toCon2,
  )
import Grisette.Internal.SymPrim.Prim.Model (Model)
import Grisette.Internal.Utils.Derive (Arity0, Arity1)

-- $setup
-- >>> import Grisette.Core
-- >>> import Grisette.SymPrim
-- >>> import Data.Proxy

-- | Evaluating symbolic values with some model. This would substitute the
-- symbols (symbolic constants) with the values in the model.
--
-- >>> let model = insertValue "a" (1 :: Integer) emptyModel :: Model
-- >>> evalSym False model ([ssym "a", ssym "b"] :: [SymInteger])
-- [1,b]
--
-- If we set the first argument true, the missing symbols will be filled in
-- with some default values:
--
-- >>> evalSym True model ([ssym "a", ssym "b"] :: [SymInteger])
-- [1,0]
--
-- __Note 1:__ This type class can be derived for algebraic data types.
-- You may need the @DerivingVia@ and @DerivingStrategies@ extensions.
--
-- > data X = ... deriving Generic deriving EvalSym via (Default X)
class EvalSym a where
  -- | Evaluate a symbolic value with some model, possibly fill in values for
  -- the missing symbols.
  evalSym :: Bool -> Model -> a -> a

-- | Evaluate a symbolic value with some model, fill in values for the missing
-- symbols, and convert the result to a concrete value.
--
-- >>> let model = insertValue "a" (1 :: Integer) emptyModel :: Model
-- >>> evalSymToCon model ([ssym "a", ssym "b"] :: [SymInteger]) :: [Integer]
-- [1,0]
evalSymToCon :: (ToCon a b, EvalSym a) => Model -> a -> b
evalSymToCon model a = fromJust $ toCon $ evalSym True model a

-- | Lifting of 'EvalSym' to unary type constructors.
class (forall a. (EvalSym a) => EvalSym (f a)) => EvalSym1 f where
  -- | Lift the 'evalSym' function to unary type constructors.
  liftEvalSym :: (Bool -> Model -> a -> a) -> (Bool -> Model -> f a -> f a)

-- | Lifting the standard 'evalSym' to unary type constructors.
evalSym1 :: (EvalSym1 f, EvalSym a) => Bool -> Model -> f a -> f a
evalSym1 = liftEvalSym evalSym
{-# INLINE evalSym1 #-}

-- | Evaluate and convert to concrete values with lifted standard 'evalSym' for
-- unary type constructors. See 'evalSymToCon'.
evalSymToCon1 ::
  (EvalSym1 f, EvalSym a, ToCon1 f g, ToCon a b) =>
  Model ->
  f a ->
  g b
evalSymToCon1 model a = fromJust $ toCon1 $ evalSym1 True model a
{-# INLINE evalSymToCon1 #-}

-- | Lifting of 'EvalSym1' to binary type constructors.
class (forall a. (EvalSym a) => EvalSym1 (f a)) => EvalSym2 f where
  -- | Lift the 'evalSym' function to binary type constructors.
  liftEvalSym2 ::
    (Bool -> Model -> a -> a) ->
    (Bool -> Model -> b -> b) ->
    (Bool -> Model -> f a b -> f a b)

-- | Lifting the standard 'evalSym' to binary type constructors.
evalSym2 ::
  (EvalSym2 f, EvalSym a, EvalSym b) =>
  Bool ->
  Model ->
  f a b ->
  f a b
evalSym2 = liftEvalSym2 evalSym evalSym
{-# INLINE evalSym2 #-}

-- | Evaluate and convert to concrete values with lifted standard 'evalSym' for
-- binary type constructors. See 'evalSymToCon'.
evalSymToCon2 ::
  ( EvalSym2 f,
    EvalSym a,
    EvalSym c,
    ToCon2 f g,
    ToCon a b,
    ToCon c d
  ) =>
  Model ->
  f a c ->
  g b d
evalSymToCon2 model a = fromJust $ toCon2 $ evalSym2 True model a
{-# INLINE evalSymToCon2 #-}

-- Derivations

-- | The arguments to the generic 'evalSym' function.
data family EvalSymArgs arity a :: Type

data instance EvalSymArgs Arity0 _ = EvalSymArgs0

newtype instance EvalSymArgs Arity1 a
  = EvalSymArgs1 (Bool -> Model -> a -> a)

-- | The class of types that can be generically evaluated.
class GEvalSym arity f where
  gevalSym :: EvalSymArgs arity a -> Bool -> Model -> f a -> f a

instance GEvalSym arity V1 where
  gevalSym _ _ _ = id
  {-# INLINE gevalSym #-}

instance GEvalSym arity U1 where
  gevalSym _ _ _ = id
  {-# INLINE gevalSym #-}

instance
  (GEvalSym arity a, GEvalSym arity b) =>
  GEvalSym arity (a :*: b)
  where
  gevalSym args fillDefault model (a :*: b) =
    gevalSym args fillDefault model a
      :*: gevalSym args fillDefault model b
  {-# INLINE gevalSym #-}

instance
  (GEvalSym arity a, GEvalSym arity b) =>
  GEvalSym arity (a :+: b)
  where
  gevalSym args fillDefault model (L1 l) =
    L1 $ gevalSym args fillDefault model l
  gevalSym args fillDefault model (R1 r) =
    R1 $ gevalSym args fillDefault model r
  {-# INLINE gevalSym #-}

instance (GEvalSym arity a) => GEvalSym arity (M1 i c a) where
  gevalSym args fillDefault model (M1 x) =
    M1 $ gevalSym args fillDefault model x
  {-# INLINE gevalSym #-}

instance (EvalSym a) => GEvalSym arity (K1 i a) where
  gevalSym _ fillDefault model (K1 x) = K1 $ evalSym fillDefault model x
  {-# INLINE gevalSym #-}

instance GEvalSym Arity1 Par1 where
  gevalSym (EvalSymArgs1 f) fillDefault model (Par1 x) =
    Par1 $ f fillDefault model x
  {-# INLINE gevalSym #-}

instance (EvalSym1 a) => GEvalSym Arity1 (Rec1 a) where
  gevalSym (EvalSymArgs1 f) fillDefault model (Rec1 x) =
    Rec1 $ liftEvalSym f fillDefault model x
  {-# INLINE gevalSym #-}

instance
  (EvalSym1 f, GEvalSym Arity1 g) =>
  GEvalSym Arity1 (f :.: g)
  where
  gevalSym targs fillDefault model (Comp1 x) =
    Comp1 $ liftEvalSym (gevalSym targs) fillDefault model x
  {-# INLINE gevalSym #-}

-- | Generic 'evalSym' function.
genericEvalSym ::
  (Generic a, GEvalSym Arity0 (Rep a)) => Bool -> Model -> a -> a
genericEvalSym fillDefault model =
  to . gevalSym EvalSymArgs0 fillDefault model . from
{-# INLINE genericEvalSym #-}

-- | Generic 'liftEvalSym' function.
genericLiftEvalSym ::
  (Generic1 f, GEvalSym Arity1 (Rep1 f)) =>
  (Bool -> Model -> a -> a) ->
  Bool ->
  Model ->
  f a ->
  f a
genericLiftEvalSym f fillDefault model =
  to1 . gevalSym (EvalSymArgs1 f) fillDefault model . from1
{-# INLINE genericLiftEvalSym #-}

instance
  (Generic a, GEvalSym Arity0 (Rep a)) =>
  EvalSym (Default a)
  where
  evalSym fillDefault model =
    Default . genericEvalSym fillDefault model . unDefault
  {-# INLINE evalSym #-}

instance
  (Generic1 f, GEvalSym Arity1 (Rep1 f), EvalSym a) =>
  EvalSym (Default1 f a)
  where
  evalSym = evalSym1
  {-# INLINE evalSym #-}

instance
  (Generic1 f, GEvalSym Arity1 (Rep1 f)) =>
  EvalSym1 (Default1 f)
  where
  liftEvalSym f fillDefault model =
    Default1 . genericLiftEvalSym f fillDefault model . unDefault1
  {-# INLINE liftEvalSym #-}
