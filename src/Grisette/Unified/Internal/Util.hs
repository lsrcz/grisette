{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- Module      :   Grisette.Unified.Internal.Util
-- Copyright   :   (c) Sirui Lu 2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Unified.Internal.Util (withMode) where

import Data.Typeable (Typeable, eqT, type (:~:) (Refl))
import Grisette.Unified.Internal.EvalModeTag (EvalModeTag (C, S))

-- | Case analysis on the mode.
withMode ::
  forall mode r.
  (Typeable mode) =>
  ((mode ~ 'C) => r) ->
  ((mode ~ 'S) => r) ->
  r
withMode con sym = case (eqT @mode @'C, eqT @mode @'S) of
  (Just Refl, _) -> con
  (_, Just Refl) -> sym
  _ -> error "impossible"
{-# INLINE withMode #-}
