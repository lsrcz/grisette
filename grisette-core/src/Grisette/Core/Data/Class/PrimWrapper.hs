{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module Grisette.Core.Data.Class.PrimWrapper
  ( PrimWrapper (..),
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

-- | The class establish the link between concrete primitive types
-- and symbolic primitive types.
class IsString t => PrimWrapper t c | t -> c where
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

  -- | Generate simply-named symbolic variables.
  --
  -- >>> ssymb "a" :: SymBool
  -- a
  ssymb :: String -> t

  -- | Generate indexed symbolic variables.
  --
  -- >>> isymb "a" 1 :: SymBool
  -- a@1
  isymb :: String -> Int -> t

  sinfosymb :: (Typeable a, Ord a, Lift a, NFData a, Show a, Hashable a) => String -> a -> t
  iinfosymb :: (Typeable a, Ord a, Lift a, NFData a, Show a, Hashable a) => String -> Int -> a -> t

-- | Extract the concrete value from a symbolic value with 'concView'.
--
-- >>> case conc True :: SymBool of Conc v -> v
-- True
pattern Conc :: PrimWrapper t c => c -> t
pattern Conc c <-
  (concView -> Just c)
  where
    Conc c = conc c
