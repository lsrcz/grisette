{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      :   Grisette.Internal.Utils.Derive
-- Copyright   :   (c) Sirui Lu 2021-2023
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Internal.Utils.Derive (Arity0, Arity1) where

-- | Type-level tag for generic derivation with arity 0.
data Arity0

-- | Type-level tag for generic derivation with arity 1.
data Arity1
