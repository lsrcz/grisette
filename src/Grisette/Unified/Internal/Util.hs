{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Grisette.Unified.Internal.Util (withMode) where

import Data.Typeable (Typeable, eqT, type (:~:) (Refl))
import Grisette.Unified.Internal.EvaluationMode
  ( EvaluationMode (Con, Sym),
  )

withMode ::
  forall mode r.
  (Typeable mode) =>
  ((mode ~ 'Con) => r) ->
  ((mode ~ 'Sym) => r) ->
  r
withMode con sym = case (eqT @mode @'Con, eqT @mode @'Sym) of
  (Just Refl, _) -> con
  (_, Just Refl) -> sym
  _ -> error "impossible"
