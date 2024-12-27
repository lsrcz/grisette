{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilyDependencies #-}

-- |
-- Module      :   Grisette.Internal.Unified.BaseMonad
-- Copyright   :   (c) Sirui Lu 2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Internal.Unified.BaseMonad
  ( BaseMonad,
  )
where

import Control.Monad.Identity (Identity)
import Data.Kind (Type)
import Grisette.Internal.Core.Control.Monad.Union (Union)
import Grisette.Internal.Unified.EvalModeTag (EvalModeTag (C, S))

-- | A type family that specifies the base monad for the evaluation mode.
--
-- Resolves to 'Identity' for `C` mode, and 'Union' for `S` mode.
type family
  BaseMonad (mode :: EvalModeTag) =
    (r :: Type -> Type) | r -> mode
  where
  BaseMonad 'C = Identity
  BaseMonad 'S = Union
