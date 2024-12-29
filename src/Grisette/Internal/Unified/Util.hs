{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Eta reduce" #-}

-- |
-- Module      :   Grisette.Internal.Unified.Util
-- Copyright   :   (c) Sirui Lu 2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Internal.Unified.Util
  ( DecideEvalMode (..),
    withMode,
    EvalModeConvertible (..),
  )
where

import Data.Typeable (type (:~:) (Refl))
import Grisette.Internal.Unified.EvalModeTag (EvalModeTag (C, S))
import Grisette.Internal.Utils.Parameterized (unsafeAxiom)

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

class
  (DecideEvalMode c, DecideEvalMode s) =>
  EvalModeConvertible (c :: EvalModeTag) (s :: EvalModeTag)
  where
  withModeConvertible ::
    ((c ~ 'C) => r) ->
    ((s ~ 'S) => r) ->
    r
  withModeConvertible' ::
    ((c ~ 'C, s ~ 'C) => r) ->
    ((c ~ 'C, s ~ 'S) => r) ->
    ((c ~ 'S, s ~ 'S) => r) ->
    r

instance {-# INCOHERENT #-} (DecideEvalMode s) => EvalModeConvertible 'C s where
  withModeConvertible con _ = con
  {-# INLINE withModeConvertible #-}
  withModeConvertible' con0 con1 _ = withMode @s con0 con1
  {-# INLINE withModeConvertible' #-}

instance {-# INCOHERENT #-} (DecideEvalMode c) => EvalModeConvertible c 'S where
  withModeConvertible _ sym = sym
  {-# INLINE withModeConvertible #-}
  withModeConvertible' _ sym0 sym1 = withMode @c sym0 sym1
  {-# INLINE withModeConvertible' #-}
