{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      :   Grisette.Internal.Internal.Decl.Core.Data.Class.ToCon
-- Copyright   :   (c) Sirui Lu 2021-2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Internal.Internal.Decl.Core.Data.Class.ToCon
  ( -- * Converting to concrete values
    ToCon (..),
    ToCon1 (..),
    toCon1,
    ToCon2 (..),
    toCon2,

    -- * Generic 'ToCon'
    ToConArgs (..),
    GToCon (..),
    genericToCon,
    genericLiftToCon,
  )
where

import Data.Kind (Type)
import GHC.Generics
  ( Generic (Rep, from, to),
    Generic1 (Rep1, from1, to1),
    K1 (K1),
    M1 (M1),
    Par1 (Par1),
    Rec1 (Rec1),
    U1 (U1),
    V1,
    (:.:) (Comp1),
    type (:*:) ((:*:)),
    type (:+:) (L1, R1),
  )
import Generics.Deriving (Default (Default), Default1 (Default1))
import Generics.Deriving.Instances ()
import Grisette.Internal.Core.Data.Class.Solvable (Solvable (conView))
import Grisette.Internal.SymPrim.SymBool (SymBool)
import Grisette.Internal.Utils.Derive (Arity0, Arity1)

-- $setup
-- >>> import Grisette.Core
-- >>> import Grisette.SymPrim

-- | Convert a symbolic value to concrete value if possible.
class ToCon a b where
  -- | Convert a symbolic value to concrete value if possible.
  -- If the symbolic value cannot be converted to concrete, the result will be 'Nothing'.
  --
  -- >>> toCon (ssym "a" :: SymInteger) :: Maybe Integer
  -- Nothing
  --
  -- >>> toCon (con 1 :: SymInteger) :: Maybe Integer
  -- Just 1
  --
  -- 'toCon' works on complex types too.
  --
  -- >>> toCon ([con 1, con 2] :: [SymInteger]) :: Maybe [Integer]
  -- Just [1,2]
  --
  -- >>> toCon ([con 1, ssym "a"] :: [SymInteger]) :: Maybe [Integer]
  -- Nothing
  toCon :: a -> Maybe b

instance {-# INCOHERENT #-} ToCon v v where
  toCon = Just

-- | Lifting of 'ToCon' to unary type constructors.
class (forall a b. (ToCon a b) => ToCon (f1 a) (f2 b)) => ToCon1 f1 f2 where
  -- | Lift a conversion to concrete function to unary type constructors.
  liftToCon :: (a -> Maybe b) -> f1 a -> Maybe (f2 b)

-- | Lift the standard 'toCon' to unary type constructors.
toCon1 :: (ToCon1 f1 f2, ToCon a b) => f1 a -> Maybe (f2 b)
toCon1 = liftToCon toCon
{-# INLINE toCon1 #-}

-- | Lifting of 'ToCon' to binary type constructors.
class (forall a b. (ToCon a b) => ToCon1 (f1 a) (f2 b)) => ToCon2 f1 f2 where
  -- | Lift conversion to concrete functions to binary type constructors.
  liftToCon2 :: (a -> Maybe b) -> (c -> Maybe d) -> f1 a c -> Maybe (f2 b d)

-- | Lift the standard 'toCon' to binary type constructors.
toCon2 :: (ToCon2 f1 f2, ToCon a b, ToCon c d) => f1 a c -> Maybe (f2 b d)
toCon2 = liftToCon2 toCon toCon
{-# INLINE toCon2 #-}

-- Derivations

-- | The arguments to the generic 'toCon' function.
data family ToConArgs arity a b :: Type

data instance ToConArgs Arity0 _ _ = ToConArgs0

newtype instance ToConArgs Arity1 a b
  = ToConArgs1 (a -> Maybe b)

-- | The class of types that can be generically converted to concrete values.
class GToCon arity f1 f2 where
  gtoCon :: ToConArgs arity a b -> f1 a -> Maybe (f2 b)

instance GToCon arity V1 V1 where
  gtoCon _ _ = error "Impossible"
  {-# INLINE gtoCon #-}

instance GToCon arity U1 U1 where
  gtoCon _ _ = Just U1
  {-# INLINE gtoCon #-}

instance
  (GToCon arity a b, GToCon arity c d) =>
  GToCon arity (a :*: c) (b :*: d)
  where
  gtoCon args (a :*: c) = do
    a' <- gtoCon args a
    c' <- gtoCon args c
    return $ a' :*: c'
  {-# INLINE gtoCon #-}

instance
  (GToCon arity a b, GToCon arity c d) =>
  GToCon arity (a :+: c) (b :+: d)
  where
  gtoCon args (L1 a) = L1 <$> gtoCon args a
  gtoCon args (R1 a) = R1 <$> gtoCon args a
  {-# INLINE gtoCon #-}

instance (GToCon arity a b) => GToCon arity (M1 i c1 a) (M1 i c2 b) where
  gtoCon args (M1 a) = M1 <$> gtoCon args a
  {-# INLINE gtoCon #-}

instance (ToCon a b) => GToCon arity (K1 i a) (K1 i b) where
  gtoCon _ (K1 a) = K1 <$> toCon a
  {-# INLINE gtoCon #-}

instance GToCon Arity1 Par1 Par1 where
  gtoCon (ToConArgs1 f) (Par1 a) = Par1 <$> f a
  {-# INLINE gtoCon #-}

instance (ToCon1 f1 f2) => GToCon Arity1 (Rec1 f1) (Rec1 f2) where
  gtoCon (ToConArgs1 f) (Rec1 a) = Rec1 <$> liftToCon f a
  {-# INLINE gtoCon #-}

instance
  (ToCon1 f1 f2, GToCon Arity1 g1 g2) =>
  GToCon Arity1 (f1 :.: g1) (f2 :.: g2)
  where
  gtoCon targs (Comp1 a) = Comp1 <$> liftToCon (gtoCon targs) a
  {-# INLINE gtoCon #-}

-- | Generic 'toCon' function.
genericToCon ::
  (Generic a, Generic b, GToCon Arity0 (Rep a) (Rep b)) =>
  a ->
  Maybe b
genericToCon = fmap to . gtoCon ToConArgs0 . from
{-# INLINE genericToCon #-}

-- | Generic 'liftToCon' function.
genericLiftToCon ::
  (Generic1 f1, Generic1 f2, GToCon Arity1 (Rep1 f1) (Rep1 f2)) =>
  (a -> Maybe b) ->
  f1 a ->
  Maybe (f2 b)
genericLiftToCon f = fmap to1 . gtoCon (ToConArgs1 f) . from1
{-# INLINE genericLiftToCon #-}

instance
  (Generic a, Generic b, GToCon Arity0 (Rep a) (Rep b)) =>
  ToCon a (Default b)
  where
  toCon = fmap Default . genericToCon
  {-# INLINE toCon #-}

instance
  (Generic1 f1, Generic1 f2, GToCon Arity1 (Rep1 f1) (Rep1 f2), ToCon a b) =>
  ToCon (f1 a) (Default1 f2 b)
  where
  toCon = toCon1

instance
  (Generic1 f1, Generic1 f2, GToCon Arity1 (Rep1 f1) (Rep1 f2)) =>
  ToCon1 f1 (Default1 f2)
  where
  liftToCon f = fmap Default1 . genericLiftToCon f
  {-# INLINE liftToCon #-}

instance ToCon SymBool Bool where
  toCon = conView
