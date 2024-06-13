{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Grisette.Unified.Internal.UnifiedInteger
  ( UnifiedInteger (..),
    SafeUnifiedInteger,
  )
where

import Control.Exception (ArithException)
import Control.Monad.Except (ExceptT)
import Data.Kind (Type)
import Grisette (SymInteger)
import Grisette.Unified.Internal.BaseConstraint
  ( BasicGrisetteType,
    ConSymConversion,
    SafeIntegral,
  )
import Grisette.Unified.Internal.EvaluationMode (EvaluationMode (Con, Sym))
import Grisette.Unified.Internal.UnifiedConstraint (UnifiedPrimitive)

class
  ( BasicGrisetteType (GetInteger mode),
    ConSymConversion Integer SymInteger (GetInteger mode),
    Num (GetInteger mode),
    UnifiedPrimitive mode (GetInteger mode)
  ) =>
  UnifiedInteger (mode :: EvaluationMode)
  where
  -- | Get a unified Integer type. Resolves to 'Integer' in 'Con' mode, and
  -- 'SymInteger' in 'Sym' mode.
  type GetInteger mode = bool | bool -> mode

instance UnifiedInteger 'Con where
  type GetInteger 'Con = Integer

instance UnifiedInteger 'Sym where
  type GetInteger 'Sym = SymInteger

class
  ( UnifiedInteger mode,
    SafeIntegral ArithException (GetInteger mode) (ExceptT ArithException m)
  ) =>
  SafeUnifiedInteger (mode :: EvaluationMode) (m :: Type -> Type)

instance
  ( SafeIntegral ArithException Integer (ExceptT ArithException m)
  ) =>
  SafeUnifiedInteger 'Con m

instance
  ( SafeIntegral ArithException SymInteger (ExceptT ArithException m)
  ) =>
  SafeUnifiedInteger 'Sym m
