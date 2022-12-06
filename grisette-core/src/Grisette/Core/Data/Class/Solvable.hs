{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module Grisette.Core.Data.Class.Solvable
  ( -- * Note for the examples

    --

    -- | This module does not contain actual implementation for symbolic primitive types, and
    -- the examples in this module cannot be executed solely with @grisette-core@ package.
    -- They rely on the implementation in @grisette-symir@ package.

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
  -- >>> sinfosymb "a" "someinfo" :: SymInteger
  -- a:"someinfo"
  sinfosymb :: (Typeable a, Ord a, Lift a, NFData a, Show a, Hashable a) => String -> a -> t

  -- | Generate indexed symbolic constants with some extra information for
  -- disambiguation.
  --
  -- Two symbolic constants with the same name and index but different extra
  -- information (including info with different types) are considered to be
  -- different.
  --
  -- >>> iinfosymb "a" 1 "someinfo" :: SymInteger
  -- a@1:"someinfo"
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
