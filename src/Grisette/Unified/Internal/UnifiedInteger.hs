{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Grisette.Unified.Internal.UnifiedInteger
  ( GetInteger,
    UnifiedInteger,
    -- SafeUnifiedInteger,
    -- SafeUnifiedInteger',
  )
where

import Control.Exception (ArithException)
import Control.Monad.Except (MonadError)
import Grisette.Internal.SymPrim.SymInteger (SymInteger)
import Grisette.Unified.Internal.BaseConstraint
  ( BasicGrisetteType,
    ConSymConversion,
  )
import Grisette.Unified.Internal.Class.UnifiedSafeDivision (UnifiedSafeDivision)
import Grisette.Unified.Internal.Class.UnifiedSafeLinearArith (UnifiedSafeLinearArith)
import Grisette.Unified.Internal.Class.UnifiedSimpleMergeable (UnifiedBranching)
import Grisette.Unified.Internal.EvaluationMode (EvaluationMode (Con, Sym))
import Grisette.Unified.Internal.UnifiedConstraint (UnifiedPrimitive)

class
  ( BasicGrisetteType (GetInteger mode),
    ConSymConversion Integer SymInteger (GetInteger mode),
    Num (GetInteger mode),
    UnifiedPrimitive mode (GetInteger mode),
    forall m.
    (UnifiedBranching mode m, MonadError ArithException m) =>
    UnifiedSafeDivision mode ArithException i m,
    forall m.
    (UnifiedBranching mode m, MonadError ArithException m) =>
    UnifiedSafeLinearArith mode ArithException i m,
    i ~ GetInteger mode
  ) =>
  UnifiedIntegerImpl (mode :: EvaluationMode) i
    | mode -> i
  where
  -- | Get a unified Integer type. Resolves to 'Integer' in 'Con' mode, and
  -- 'SymInteger' in 'Sym' mode.
  type GetInteger mode = int | int -> mode

instance UnifiedIntegerImpl 'Con Integer where
  type GetInteger 'Con = Integer

instance UnifiedIntegerImpl 'Sym SymInteger where
  type GetInteger 'Sym = SymInteger

class
  (UnifiedIntegerImpl mode (GetInteger mode)) =>
  UnifiedInteger (mode :: EvaluationMode)

instance UnifiedInteger 'Con

instance UnifiedInteger 'Sym
