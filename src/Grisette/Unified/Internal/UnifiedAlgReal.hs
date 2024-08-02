{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- Module      :   Grisette.Unified.Internal.UnifiedAlgReal
-- Copyright   :   (c) Sirui Lu 2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Unified.Internal.UnifiedAlgReal
  ( UnifiedAlgReal,
    GetAlgReal,
  )
where

import Control.Exception (ArithException)
import Control.Monad.Error.Class (MonadError)
import Grisette.Internal.Core.Data.Class.SafeFdiv (FdivOr)
import Grisette.Internal.SymPrim.AlgReal (AlgReal)
import Grisette.Internal.SymPrim.SymAlgReal (SymAlgReal)
import Grisette.Unified.Internal.BaseConstraint
  ( BasicGrisetteType,
    ConSymConversion,
  )
import Grisette.Unified.Internal.Class.UnifiedSafeFdiv (UnifiedSafeFdiv)
import Grisette.Unified.Internal.Class.UnifiedSimpleMergeable (UnifiedBranching)
import Grisette.Unified.Internal.EvalModeTag (EvalModeTag (Con, Sym))
import Grisette.Unified.Internal.UnifiedConstraint (UnifiedPrimitive)

class
  ( BasicGrisetteType (GetAlgReal mode),
    ConSymConversion AlgReal SymAlgReal (GetAlgReal mode),
    Num (GetAlgReal mode),
    Fractional (GetAlgReal mode),
    UnifiedPrimitive mode (GetAlgReal mode),
    FdivOr (GetAlgReal mode),
    forall m.
    (UnifiedBranching mode m, MonadError ArithException m) =>
    UnifiedSafeFdiv mode ArithException r m,
    r ~ GetAlgReal mode
  ) =>
  UnifiedAlgRealImpl (mode :: EvalModeTag) r
    | mode -> r
  where
  -- | Get a unified algebraic real type. Resolves to 'AlgReal' in 'Con' mode,
  -- and 'SymAlgReal' in 'Sym' mode.
  --
  -- 'Floating', 'Grisette.LogBaseOr' and 'Grisette.SafeLogBase' for
  -- 'SymAlgReal' are not provided as they are not available for 'AlgReal'.
  type GetAlgReal mode = real | real -> mode

instance UnifiedAlgRealImpl 'Con AlgReal where
  type GetAlgReal 'Con = AlgReal

instance UnifiedAlgRealImpl 'Sym SymAlgReal where
  type GetAlgReal 'Sym = SymAlgReal

-- | Evaluation mode with unified 'AlgReal' type.
class
  (UnifiedAlgRealImpl mode (GetAlgReal mode)) =>
  UnifiedAlgReal (mode :: EvalModeTag)

instance UnifiedAlgReal 'Con

instance UnifiedAlgReal 'Sym
