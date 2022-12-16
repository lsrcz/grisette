{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- Module      :   Grisette.Core.Data.Class.Solvable
-- Copyright   :   (c) Sirui Lu 2021-2022
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Core.Data.Class.Solvable
  ( -- * Note for the examples

    --

    -- | This module does not contain the implementation for symbolic primitive
    -- types, and the examples in this module rely on the implementations in
    -- the [grisette-symir](https://hackage.haskell.org/package/grisette-symir) package.

    -- * Solvable type interface
    Solvable (..),
    pattern Conc,
  )
where

import Control.DeepSeq
import Data.Hashable
import Data.String
import Data.Typeable
import Language.Haskell.TH.Syntax

-- $setup
-- >>> import Grisette.Core
-- >>> import Grisette.IR.SymPrim

-- | The class defines the creation and pattern matching of solvable type
-- values.
class IsString t => Solvable c t | t -> c where
  -- | Wrap a concrete value in a symbolic value.
  --
  -- >>> conc True :: SymBool
  -- true
  conc :: c -> t

  -- | Extract the concrete value from a symbolic value.
  --
  -- >>> concView (conc True :: SymBool)
  -- Just True
  --
  -- >>> concView (ssymb "a" :: SymBool)
  -- Nothing
  concView :: t -> Maybe c

  -- | Generate simply-named symbolic constants.
  --
  -- Two symbolic constants with the same name are the same symbolic constant,
  -- and will always be assigned with the same value by the solver.
  --
  -- >>> ssymb "a" :: SymBool
  -- a
  -- >>> (ssymb "a" :: SymBool) == ssymb "a"
  -- True
  -- >>> (ssymb "a" :: SymBool) == ssymb "b"
  -- False
  -- >>> (ssymb "a" :: SymBool) &&~ ssymb "a"
  -- a
  ssymb :: String -> t

  -- | Generate indexed symbolic constants.
  --
  -- Two symbolic constants with the same name but different indices are
  -- not the same symbolic constants.
  --
  -- >>> isymb "a" 1 :: SymBool
  -- a@1
  isymb :: String -> Int -> t

  -- | Generate simply-named symbolic constants with some extra information for
  -- disambiguation.
  --
  -- Two symbolic constants with the same name but different extra information
  -- (including info with different types) are considered to be different.
  --
  -- >>> sinfosymb "a" "someInfo" :: SymInteger
  -- a:"someInfo"
  sinfosymb :: (Typeable a, Ord a, Lift a, NFData a, Show a, Hashable a) => String -> a -> t

  -- | Generate indexed symbolic constants with some extra information for
  -- disambiguation.
  --
  -- Two symbolic constants with the same name and index but different extra
  -- information (including info with different types) are considered to be
  -- different.
  --
  -- >>> iinfosymb "a" 1 "someInfo" :: SymInteger
  -- a@1:"someInfo"
  iinfosymb :: (Typeable a, Ord a, Lift a, NFData a, Show a, Hashable a) => String -> Int -> a -> t

-- | Extract the concrete value from a solvable value with 'concView'.
--
-- >>> case conc True :: SymBool of Conc v -> v
-- True
pattern Conc :: Solvable c t => c -> t
pattern Conc c <-
  (concView -> Just c)
  where
    Conc c = conc c
