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
-- Module      :   Grisette.Internal.Internal.Decl.Core.Data.Class.ExtractSym
-- Copyright   :   (c) Sirui Lu 2021-2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Internal.Internal.Decl.Core.Data.Class.ExtractSym
  ( -- * Extracting symbolic constant set from a value
    ExtractSym (..),
    ExtractSym1 (..),
    extractSymMaybe1,
    extractSym1,
    ExtractSym2 (..),
    extractSymMaybe2,
    extractSym2,

    -- * Generic 'ExtractSym'
    ExtractSymArgs (..),
    GExtractSym (..),
    genericExtractSymMaybe,
    genericLiftExtractSymMaybe,
  )
where

import Data.Kind (Type)
import Data.Maybe (fromJust)
import Generics.Deriving
  ( Default (unDefault),
    Default1 (unDefault1),
    Generic (Rep, from),
    Generic1 (Rep1, from1),
    K1 (K1),
    M1 (M1),
    Par1 (Par1),
    Rec1 (Rec1),
    U1,
    V1,
    type (:*:) ((:*:)),
    type (:+:) (L1, R1),
    type (:.:) (Comp1),
  )
import Grisette.Internal.SymPrim.Prim.Model
  ( AnySymbolSet,
    SymbolSet,
  )
import Grisette.Internal.SymPrim.Prim.Term (IsSymbolKind, SymbolKind)
import Grisette.Internal.Utils.Derive (Arity0, Arity1)

-- $setup
-- >>> import Grisette.Core
-- >>> import Grisette.SymPrim
-- >>> import Grisette.Lib.Base
-- >>> import Data.HashSet as HashSet
-- >>> import Data.List (sort)

-- | Extracts all the symbols (symbolic constants) that are transitively
-- contained in the given value.
--
-- >>> extractSym ("a" :: SymBool)
-- SymbolSet {a :: Bool}
--
-- >>> extractSym (mrgIf "a" (mrgReturn ["b"]) (mrgReturn ["c", "d"]) :: Union [SymBool])
-- SymbolSet {a :: Bool, b :: Bool, c :: Bool, d :: Bool}
--
-- __Note 1:__ This type class can be derived for algebraic data types.
-- You may need the @DerivingVia@ and @DerivingStrategies@ extensions.
--
-- > data X = ... deriving Generic deriving ExtractSym via (Default X)
class ExtractSym a where
  extractSym :: a -> AnySymbolSet
  extractSym = fromJust . extractSymMaybe
  {-# INLINE extractSym #-}
  extractSymMaybe :: (IsSymbolKind knd) => a -> Maybe (SymbolSet knd)

-- | Lifting of 'ExtractSym' to unary type constructors.
class
  (forall a. (ExtractSym a) => ExtractSym (f a)) =>
  ExtractSym1 f
  where
  -- | Lifts the 'extractSymMaybe' function to unary type constructors.
  liftExtractSymMaybe ::
    (IsSymbolKind knd) =>
    (a -> Maybe (SymbolSet knd)) ->
    f a ->
    Maybe (SymbolSet knd)

-- | Lift the standard 'extractSym' to unary type constructors.
extractSym1 ::
  (ExtractSym1 f, ExtractSym a, IsSymbolKind knd) =>
  f a ->
  SymbolSet knd
extractSym1 = fromJust . liftExtractSymMaybe extractSymMaybe
{-# INLINE extractSym1 #-}

-- | Lift the standard 'extractSymMaybe' to unary type constructors.
extractSymMaybe1 ::
  (ExtractSym1 f, ExtractSym a, IsSymbolKind knd) =>
  f a ->
  Maybe (SymbolSet knd)
extractSymMaybe1 = liftExtractSymMaybe extractSymMaybe
{-# INLINE extractSymMaybe1 #-}

-- | Lifting of 'ExtractSym' to binary type constructors.
class
  (forall a. (ExtractSym a) => ExtractSym1 (f a)) =>
  ExtractSym2 f
  where
  -- | Lifts the 'extractSymMaybe' function to binary type constructors.
  liftExtractSymMaybe2 ::
    (IsSymbolKind knd) =>
    (a -> Maybe (SymbolSet knd)) ->
    (b -> Maybe (SymbolSet knd)) ->
    f a b ->
    Maybe (SymbolSet knd)

-- | Lift the standard 'extractSym' to binary type constructors.
extractSym2 ::
  (ExtractSym2 f, ExtractSym a, ExtractSym b, IsSymbolKind knd) =>
  f a b ->
  SymbolSet knd
extractSym2 = fromJust . liftExtractSymMaybe2 extractSymMaybe extractSymMaybe

-- | Lift the standard 'extractSymMaybe' to binary type constructors.
extractSymMaybe2 ::
  (ExtractSym2 f, ExtractSym a, ExtractSym b, IsSymbolKind knd) =>
  f a b ->
  Maybe (SymbolSet knd)
extractSymMaybe2 = liftExtractSymMaybe2 extractSymMaybe extractSymMaybe
{-# INLINE extractSymMaybe2 #-}

-- Derivations

-- | The arguments to the generic 'extractSym' function.
data family ExtractSymArgs arity (knd :: SymbolKind) a :: Type

data instance ExtractSymArgs Arity0 _ _ = ExtractSymArgs0

newtype instance ExtractSymArgs Arity1 knd a
  = ExtractSymArgs1 (a -> Maybe (SymbolSet knd))

-- | The class of types that can generically extract the symbols.
class GExtractSym arity f where
  gextractSymMaybe ::
    (IsSymbolKind knd) =>
    ExtractSymArgs arity knd a ->
    f a ->
    Maybe (SymbolSet knd)

instance GExtractSym arity V1 where
  gextractSymMaybe _ _ = Just mempty
  {-# INLINE gextractSymMaybe #-}

instance GExtractSym arity U1 where
  gextractSymMaybe _ _ = Just mempty
  {-# INLINE gextractSymMaybe #-}

instance (GExtractSym arity a) => GExtractSym arity (M1 i c a) where
  gextractSymMaybe args (M1 x) = gextractSymMaybe args x
  {-# INLINE gextractSymMaybe #-}

instance (ExtractSym a) => GExtractSym arity (K1 i a) where
  gextractSymMaybe _ (K1 x) = extractSymMaybe x
  {-# INLINE gextractSymMaybe #-}

instance
  (GExtractSym arity a, GExtractSym arity b) =>
  GExtractSym arity (a :+: b)
  where
  gextractSymMaybe args (L1 x) = gextractSymMaybe args x
  gextractSymMaybe args (R1 x) = gextractSymMaybe args x
  {-# INLINE gextractSymMaybe #-}

instance
  (GExtractSym arity a, GExtractSym arity b) =>
  GExtractSym arity (a :*: b)
  where
  gextractSymMaybe args (x :*: y) =
    gextractSymMaybe args x <> gextractSymMaybe args y
  {-# INLINE gextractSymMaybe #-}

instance GExtractSym Arity1 Par1 where
  gextractSymMaybe (ExtractSymArgs1 f) (Par1 x) = f x
  {-# INLINE gextractSymMaybe #-}

instance (ExtractSym1 a) => GExtractSym Arity1 (Rec1 a) where
  gextractSymMaybe (ExtractSymArgs1 f) (Rec1 x) =
    liftExtractSymMaybe f x
  {-# INLINE gextractSymMaybe #-}

instance
  (ExtractSym1 f, GExtractSym Arity1 g) =>
  GExtractSym Arity1 (f :.: g)
  where
  gextractSymMaybe targs (Comp1 x) =
    liftExtractSymMaybe (gextractSymMaybe targs) x
  {-# INLINE gextractSymMaybe #-}

-- | Generic 'extractSym' function.
genericExtractSymMaybe ::
  (Generic a, GExtractSym Arity0 (Rep a), IsSymbolKind knd) =>
  a ->
  Maybe (SymbolSet knd)
genericExtractSymMaybe = gextractSymMaybe ExtractSymArgs0 . from

-- | Generic 'liftExtractSymMaybe' function.
genericLiftExtractSymMaybe ::
  (Generic1 f, GExtractSym Arity1 (Rep1 f), IsSymbolKind knd) =>
  (a -> Maybe (SymbolSet knd)) ->
  f a ->
  Maybe (SymbolSet knd)
genericLiftExtractSymMaybe f =
  gextractSymMaybe (ExtractSymArgs1 f) . from1

instance
  (Generic a, GExtractSym Arity0 (Rep a)) =>
  ExtractSym (Default a)
  where
  extractSymMaybe = genericExtractSymMaybe . unDefault
  {-# INLINE extractSymMaybe #-}

instance
  (Generic1 f, GExtractSym Arity1 (Rep1 f), ExtractSym a) =>
  ExtractSym (Default1 f a)
  where
  extractSymMaybe = extractSymMaybe1
  {-# INLINE extractSymMaybe #-}

instance
  (Generic1 f, GExtractSym Arity1 (Rep1 f)) =>
  ExtractSym1 (Default1 f)
  where
  liftExtractSymMaybe f = genericLiftExtractSymMaybe f . unDefault1
  {-# INLINE liftExtractSymMaybe #-}
