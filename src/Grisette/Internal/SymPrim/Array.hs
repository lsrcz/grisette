{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE ImportQualifiedPost #-}

-- |
-- Module      :   Grisette.Internal.SymPrim.Array
-- Copyright   :   (c) Sirui Lu 2021-2023
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Internal.SymPrim.Array
  ( Array (..)
  , const
  , select
  , store
  ) where

import Control.DeepSeq (NFData)
import Data.Binary qualified as Binary
import Data.Bytes.Serial (Serial (serialize, deserialize))
import Data.Hashable (Hashable)
import Data.HashMap.Strict qualified as HM
import Data.Serialize qualified as Cereal
import GHC.Generics (Generic)
import Language.Haskell.TH.Syntax (Lift)
import Prelude (Show, Eq, Ord)

-- TODO: The equality of this array model is incorrect. The easy solution is
-- to disallow it entirely. Alternatively, I already have a version with a
-- working equality check. It works by canonicalising the array.
--
-- Canonicalisation will not happen for keys with an infinite domain and
-- realistically also not for keys with a sufficiently large domain. In fact,
-- we avoid tracking information for canonicalisation in these cases altogether!
-- The main gripe with this is that at that point is that insertions do require
-- keys for which we know both their cardinality and can enumerate their domain.
-- The latter we could restrict to only enumerable domains given a finite
-- cardinality with type-level shenanigans, but still.
--
-- Yet another alternative would be to simply accept that we cannot conclude
-- inequality if one of the arrays would require canonicalisation? Then we don't
-- need the additional typeclass constraints. This way, we could still perform
-- normalisation of most terms.
data Array k v = Array (HM.HashMap k v) v
  deriving (Show, Eq, Ord, Generic, Lift, Hashable, NFData)

instance (Hashable k, Serial k, Serial v) => Serial (Array k v)

instance (Hashable k, Serial k, Serial v) => Cereal.Serialize (Array k v) where
  put = serialize
  get = deserialize

instance (Hashable k, Serial k, Serial v) => Binary.Binary (Array k v) where
  put = serialize
  get = deserialize

-- TODO: Perhaps it is nice to make this a typeclass and give it names that do
-- not require qualified imports? I don't necessarily mind the qualified import,
-- but we'll see what the library author thinks.
const :: forall k v. v -> Array k v
const = Array HM.empty

select :: forall k v. Hashable k => Array k v -> k -> v
select (Array entries root) key = HM.lookupDefault root key entries

store :: forall k v. Hashable k => Array k v -> k -> v -> Array k v
store (Array entries root) key value = do
  let entries' = HM.insert key value entries
  Array entries' root
