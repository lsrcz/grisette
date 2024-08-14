{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- Module      :   Grisette.Internal.SymPrim.Prim.Internal.Utils
-- Copyright   :   (c) Sirui Lu 2021-2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Internal.SymPrim.Prim.Internal.Utils
  ( pattern Dyn,
    cmpHetero,
    eqHetero,
    cmpHeteroRep,
    eqHeteroRep,
    eqTypeRepBool,
  )
where

import Data.Typeable (cast)
import Type.Reflection
  ( TypeRep,
    Typeable,
    eqTypeRep,
    typeRep,
    type (:~~:) (HRefl),
  )

-- | Pattern synonym for dynamic type casting.
pattern Dyn :: (Typeable a, Typeable b) => a -> b
pattern Dyn x <- (cast -> Just x)

-- | Compare two values of different types, resolve the type equality using the
-- type representation.
cmpHeteroRep :: forall a b. TypeRep a -> TypeRep b -> (a -> a -> Bool) -> a -> b -> Bool
cmpHeteroRep ta tb f a b = case eqTypeRep ta tb of
  Just HRefl -> f a b
  _ -> False
{-# INLINE cmpHeteroRep #-}

-- | Compare two values of different types.
cmpHetero :: forall a b. (Typeable a, Typeable b) => (a -> a -> Bool) -> a -> b -> Bool
cmpHetero = cmpHeteroRep (typeRep @a) (typeRep @b)
{-# INLINE cmpHetero #-}

-- | Compare two values of different types for equality.
eqHetero :: forall a b. (Typeable a, Typeable b, Eq a) => a -> b -> Bool
eqHetero = cmpHetero (==)
{-# INLINE eqHetero #-}

-- | Compare two values of different types for equality, resolve the type
-- equality using the type representation.
eqHeteroRep :: forall a b. (Eq a) => TypeRep a -> TypeRep b -> a -> b -> Bool
eqHeteroRep ta tb = cmpHeteroRep ta tb (==)
{-# INLINE eqHeteroRep #-}

-- | Compare two type representations for equality.
eqTypeRepBool :: forall ka kb (a :: ka) (b :: kb). TypeRep a -> TypeRep b -> Bool
eqTypeRepBool a b = case eqTypeRep a b of
  Just HRefl -> True
  _ -> False
{-# INLINE eqTypeRepBool #-}
