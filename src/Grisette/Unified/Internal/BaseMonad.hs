{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilyDependencies #-}

-- |
-- Module      :   Grisette.Unified.Internal.BaseMonad
-- Copyright   :   (c) Sirui Lu 2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Unified.Internal.BaseMonad
  ( BaseMonad,
  )
where

import Control.Monad.Identity (Identity)
import Data.Kind (Type)
import Grisette.Internal.Core.Control.Monad.Union (Union)
import Grisette.Unified.Internal.EvalModeTag (EvalModeTag (C, S))

-- | A type family that specifies the base monad for the evaluation mode.
--
-- Resolves to 'Identity' for `Con` mode, and 'Union' for `Sym` mode.
type family
  BaseMonad (mode :: EvalModeTag) =
    (r :: Type -> Type) | r -> mode
  where
  BaseMonad 'C = Identity
  BaseMonad 'S = Union
