{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- Module      :   Grisette.Internal.Unified.UnifiedAlgReal
-- Copyright   :   (c) Sirui Lu 2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Internal.Unified.UnifiedAlgReal
  ( UnifiedAlgReal,
    GetAlgReal,
  )
where

import Control.Exception (ArithException)
import Control.Monad.Error.Class (MonadError)
import Grisette.Internal.Core.Data.Class.SafeFdiv (FdivOr)
import Grisette.Internal.SymPrim.AlgReal (AlgReal)
import Grisette.Internal.SymPrim.SymAlgReal (SymAlgReal)
import Grisette.Internal.SymPrim.SymPrim (Prim)
import Grisette.Internal.Unified.Class.UnifiedFromIntegral (UnifiedFromIntegral)
import Grisette.Internal.Unified.Class.UnifiedRep
  ( UnifiedConRep (ConType),
    UnifiedSymRep (SymType),
  )
import Grisette.Internal.Unified.Class.UnifiedSafeFdiv (UnifiedSafeFdiv)
import Grisette.Internal.Unified.Class.UnifiedSimpleMergeable (UnifiedBranching)
import Grisette.Internal.Unified.EvalModeTag (EvalModeTag (C, S))
import Grisette.Internal.Unified.UnifiedInteger (GetInteger)
import Grisette.Internal.Unified.UnifiedPrim (UnifiedBasicPrim)

class
  ( r ~ GetAlgReal mode,
    UnifiedConRep r,
    UnifiedSymRep r,
    ConType r ~ AlgReal,
    SymType r ~ SymAlgReal,
    UnifiedBasicPrim mode r,
    Prim r,
    Num r,
    Fractional r,
    FdivOr r,
    forall m.
    (UnifiedBranching mode m, MonadError ArithException m) =>
    UnifiedSafeFdiv mode ArithException r m,
    UnifiedFromIntegral mode (GetInteger mode) r
  ) =>
  UnifiedAlgRealImpl (mode :: EvalModeTag) r
    | mode -> r
  where
  -- | Get a unified algebraic real type. Resolves to 'AlgReal' in 'C' mode,
  -- and 'SymAlgReal' in 'S' mode.
  --
  -- 'Floating', 'Grisette.LogBaseOr' and 'Grisette.SafeLogBase' for
  -- 'SymAlgReal' are not provided as they are not available for 'AlgReal'.
  type GetAlgReal mode = real | real -> mode

instance UnifiedAlgRealImpl 'C AlgReal where
  type GetAlgReal 'C = AlgReal

instance UnifiedAlgRealImpl 'S SymAlgReal where
  type GetAlgReal 'S = SymAlgReal

-- | Evaluation mode with unified 'AlgReal' type.
class
  (UnifiedAlgRealImpl mode (GetAlgReal mode)) =>
  UnifiedAlgReal (mode :: EvalModeTag)

instance UnifiedAlgReal 'C

instance UnifiedAlgReal 'S
