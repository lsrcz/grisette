{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module Grisette.Unified.Internal.BaseMonad
  ( BaseMonad,
  )
where

import Control.Monad.Identity (Identity)
import Data.Kind (Type)
import Grisette.Internal.Core.Control.Monad.UnionM (UnionM)
import Grisette.Unified.Internal.EvaluationMode (EvaluationMode (Con, Sym))

type family
  BaseMonad (mode :: EvaluationMode) =
    (r :: Type -> Type) | r -> mode
  where
  BaseMonad 'Con = Identity
  BaseMonad 'Sym = UnionM
