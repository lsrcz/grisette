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
import Grisette.Internal.SymPrim.SymPrim (Prim)
import Grisette.Unified.Internal.BaseConstraint (ConSymConversion)
import Grisette.Unified.Internal.Class.UnifiedFromIntegral (UnifiedFromIntegral)
import Grisette.Unified.Internal.Class.UnifiedRep
  ( UnifiedConRep (ConType),
    UnifiedSymRep (SymType),
  )
import Grisette.Unified.Internal.Class.UnifiedSafeFdiv (UnifiedSafeFdiv)
import Grisette.Unified.Internal.Class.UnifiedSimpleMergeable (UnifiedBranching)
import Grisette.Unified.Internal.EvalModeTag (EvalModeTag (C, S))
import Grisette.Unified.Internal.UnifiedInteger (GetInteger)
import Grisette.Unified.Internal.UnifiedPrim (BasicUnifiedPrim)

class
  ( r ~ GetAlgReal mode,
    UnifiedConRep r,
    UnifiedSymRep r,
    ConType r ~ AlgReal,
    SymType r ~ SymAlgReal,
    BasicUnifiedPrim mode r,
    Prim r,
    ConSymConversion AlgReal SymAlgReal r,
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
  -- | Get a unified algebraic real type. Resolves to 'AlgReal' in 'Con' mode,
  -- and 'SymAlgReal' in 'Sym' mode.
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
