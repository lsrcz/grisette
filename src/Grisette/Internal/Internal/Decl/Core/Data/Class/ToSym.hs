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
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      :   Grisette.Internal.Internal.Decl.Core.Data.Class.ToSym
-- Copyright   :   (c) Sirui Lu 2021-2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Internal.Internal.Decl.Core.Data.Class.ToSym
  ( -- * Converting to symbolic values
    ToSym (..),
    ToSym1 (..),
    toSym1,
    ToSym2 (..),
    toSym2,

    -- * Generic 'ToSym'
    ToSymArgs (..),
    GToSym (..),
    genericToSym,
    genericLiftToSym,
  )
where

import Data.Kind (Type)
import Generics.Deriving
  ( Default (Default),
    Default1 (Default1),
    Generic (Rep, from, to),
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
import Grisette.Internal.Utils.Derive (Arity0, Arity1)

-- $setup
-- >>> import Grisette.SymPrim

-- | Convert a concrete value to symbolic value.
class ToSym a b where
  -- | Convert a concrete value to symbolic value.
  --
  -- >>> toSym False :: SymBool
  -- false
  --
  -- >>> toSym [False, True] :: [SymBool]
  -- [false,true]
  toSym :: a -> b

instance {-# INCOHERENT #-} ToSym a a where
  toSym = id
  {-# INLINE toSym #-}

-- | Lifting of 'ToSym' to unary type constructors.
class
  (forall a b. (ToSym a b) => ToSym (f1 a) (f2 b)) =>
  ToSym1 f1 f2
  where
  -- | Lift a conversion to symbolic function to unary type constructors.
  liftToSym :: (a -> b) -> f1 a -> f2 b

-- | Lift the standard 'toSym' to unary type constructors.
toSym1 :: (ToSym1 f1 f2, ToSym a b) => f1 a -> f2 b
toSym1 = liftToSym toSym
{-# INLINE toSym1 #-}

-- | Lifting of 'ToSym' to binary type constructors.
class
  (forall a b. (ToSym a b) => ToSym1 (f1 a) (f2 b)) =>
  ToSym2 f1 f2
  where
  -- | Lift conversion to symbolic functions to binary type constructors.
  liftToSym2 :: (a -> b) -> (c -> d) -> f1 a c -> f2 b d

-- | Lift the standard 'toSym' to binary type constructors.
toSym2 :: (ToSym2 f1 f2, ToSym a b, ToSym c d) => f1 a c -> f2 b d
toSym2 = liftToSym2 toSym toSym
{-# INLINE toSym2 #-}

-- Derivations

-- | The arguments to the generic 'toSym' function.
data family ToSymArgs arity a b :: Type

data instance ToSymArgs Arity0 _ _ = ToSymArgs0

data instance ToSymArgs Arity1 _ _ where
  ToSymArgs1 :: (a -> b) -> ToSymArgs Arity1 a b

-- | The class of types that can be generically converted to symbolic values.
class GToSym arity f1 f2 where
  gtoSym :: ToSymArgs arity a b -> f1 a -> f2 b

instance GToSym arity V1 V1 where
  gtoSym _ _ = error "Impossible"
  {-# INLINE gtoSym #-}

instance GToSym arity U1 U1 where
  gtoSym _ _ = U1
  {-# INLINE gtoSym #-}

instance
  (GToSym arity a b, GToSym arity c d) =>
  GToSym arity (a :+: c) (b :+: d)
  where
  gtoSym args (L1 a) = L1 $ gtoSym args a
  gtoSym args (R1 b) = R1 $ gtoSym args b
  {-# INLINE gtoSym #-}

instance
  (GToSym arity a b, GToSym arity c d) =>
  GToSym arity (a :*: c) (b :*: d)
  where
  gtoSym args (a :*: c) = gtoSym args a :*: gtoSym args c
  {-# INLINE gtoSym #-}

instance (ToSym a b) => GToSym arity (K1 i a) (K1 i b) where
  gtoSym _ (K1 a) = K1 $ toSym a
  {-# INLINE gtoSym #-}

instance (GToSym arity f1 f2) => GToSym arity (M1 i c1 f1) (M1 i c2 f2) where
  gtoSym args (M1 a) = M1 $ gtoSym args a
  {-# INLINE gtoSym #-}

instance GToSym Arity1 Par1 Par1 where
  gtoSym (ToSymArgs1 f) (Par1 a) = Par1 $ f a
  {-# INLINE gtoSym #-}

instance (ToSym1 f1 f2) => GToSym Arity1 (Rec1 f1) (Rec1 f2) where
  gtoSym (ToSymArgs1 f) (Rec1 a) = Rec1 $ liftToSym f a
  {-# INLINE gtoSym #-}

instance
  (ToSym1 f1 f2, GToSym Arity1 g1 g2) =>
  GToSym Arity1 (f1 :.: g1) (f2 :.: g2)
  where
  gtoSym targs@ToSymArgs1 {} (Comp1 a) = Comp1 $ liftToSym (gtoSym targs) a
  {-# INLINE gtoSym #-}

-- | Generic 'toSym' function.
genericToSym ::
  (Generic a, Generic b, GToSym Arity0 (Rep a) (Rep b)) =>
  a ->
  b
genericToSym = to . gtoSym ToSymArgs0 . from
{-# INLINE genericToSym #-}

-- | Generic 'liftToSym' function.
genericLiftToSym ::
  (Generic1 f1, Generic1 f2, GToSym Arity1 (Rep1 f1) (Rep1 f2)) =>
  (a -> b) ->
  f1 a ->
  f2 b
genericLiftToSym f = to1 . gtoSym (ToSymArgs1 f) . from1
{-# INLINE genericLiftToSym #-}

instance
  ( Generic a,
    Generic b,
    GToSym Arity0 (Rep a) (Rep b)
  ) =>
  ToSym a (Default b)
  where
  toSym = Default . genericToSym
  {-# INLINE toSym #-}

instance
  ( Generic1 f1,
    Generic1 f2,
    GToSym Arity1 (Rep1 f1) (Rep1 f2),
    ToSym a b
  ) =>
  ToSym (f1 a) (Default1 f2 b)
  where
  toSym = toSym1

instance
  ( Generic1 f1,
    Generic1 f2,
    GToSym Arity1 (Rep1 f1) (Rep1 f2)
  ) =>
  ToSym1 f1 (Default1 f2)
  where
  liftToSym f = Default1 . genericLiftToSym f
  {-# INLINE liftToSym #-}
