{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module      :   Grisette.IR.SymPrim.Data.Prim.ModelValue
-- Copyright   :   (c) Sirui Lu 2021-2023
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.IR.SymPrim.Data.Prim.ModelValue
  ( ModelValue (..),
    toModelValue,
    unsafeFromModelValue,
  )
where

import Data.Hashable (Hashable (hashWithSalt))
import Type.Reflection
  ( TypeRep,
    Typeable,
    eqTypeRep,
    typeRep,
    type (:~~:) (HRefl),
  )

data ModelValue where
  ModelValue :: forall v. (Show v, Eq v, Hashable v) => TypeRep v -> v -> ModelValue

instance Show ModelValue where
  show (ModelValue t v) = show v ++ " :: " ++ show t

instance Eq ModelValue where
  (ModelValue t1 v1) == (ModelValue t2 v2) =
    case eqTypeRep t1 t2 of
      Just HRefl -> v1 == v2
      _ -> False

instance Hashable ModelValue where
  s `hashWithSalt` (ModelValue t v) = s `hashWithSalt` t `hashWithSalt` v

unsafeFromModelValue :: forall a. (Typeable a) => ModelValue -> a
unsafeFromModelValue (ModelValue t v) = case eqTypeRep t (typeRep @a) of
  Just HRefl -> v
  _ -> error $ "Bad model value type, expected type: " ++ show (typeRep @a) ++ ", but got: " ++ show t

toModelValue :: forall a. (Show a, Eq a, Hashable a, Typeable a) => a -> ModelValue
toModelValue = ModelValue (typeRep @a)
