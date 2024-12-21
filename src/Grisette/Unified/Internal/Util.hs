{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
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
module Grisette.Unified.Internal.Util (DecideEvalMode (..), withMode) where

import Data.Typeable (type (:~:) (Refl))
import Grisette.Internal.Utils.Parameterized (unsafeAxiom)
import Grisette.Unified.Internal.EvalModeTag (EvalModeTag (C, S))

-- | A class that provides the mode tag at runtime.
class DecideEvalMode (mode :: EvalModeTag) where
  decideEvalMode :: EvalModeTag

instance DecideEvalMode 'C where
  decideEvalMode = C
  {-# INLINE decideEvalMode #-}

instance DecideEvalMode 'S where
  decideEvalMode = S
  {-# INLINE decideEvalMode #-}

-- | Case analysis on the mode.
withMode ::
  forall mode r.
  (DecideEvalMode mode) =>
  ((mode ~ 'C) => r) ->
  ((mode ~ 'S) => r) ->
  r
withMode con sym =
  case decideEvalMode @mode of
    C -> case unsafeAxiom @mode @'C of
      Refl -> con
    S -> case unsafeAxiom @mode @'S of
      Refl -> sym
{-# INLINE withMode #-}
