{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

-- |
-- Module      :   Grisette.Unified.Internal.UnifiedInteger
-- Copyright   :   (c) Sirui Lu 2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Unified.Internal.UnifiedInteger
  ( GetInteger,
    UnifiedInteger,
  )
where

import Control.Exception (ArithException)
import Control.Monad.Except (MonadError)
import Grisette.Internal.SymPrim.SymInteger (SymInteger)
import Grisette.Unified.Internal.Class.UnifiedFromIntegral (UnifiedFromIntegral)
import Grisette.Unified.Internal.Class.UnifiedRep
  ( UnifiedConRep (ConType),
    UnifiedSymRep (SymType),
  )
import Grisette.Unified.Internal.Class.UnifiedSafeDiv (UnifiedSafeDiv)
import Grisette.Unified.Internal.Class.UnifiedSafeLinearArith
  ( UnifiedSafeLinearArith,
  )
import Grisette.Unified.Internal.Class.UnifiedSimpleMergeable (UnifiedBranching)
import Grisette.Unified.Internal.EvalModeTag (EvalModeTag (C, S))
import Grisette.Unified.Internal.UnifiedPrim (BasicUnifiedPrim)

class
  ( i ~ GetInteger mode,
    UnifiedConRep i,
    UnifiedSymRep i,
    ConType i ~ Integer,
    SymType i ~ SymInteger,
    BasicUnifiedPrim mode i,
    Num i,
    forall m.
    (UnifiedBranching mode m, MonadError ArithException m) =>
    UnifiedSafeDiv mode ArithException i m,
    forall m.
    (UnifiedBranching mode m, MonadError ArithException m) =>
    UnifiedSafeLinearArith mode ArithException i m,
    UnifiedFromIntegral mode i i
  ) =>
  UnifiedIntegerImpl (mode :: EvalModeTag) i
    | mode -> i
  where
  -- | Get a unified Integer type. Resolves to 'Integer' in 'Con' mode, and
  -- 'SymInteger' in 'Sym' mode.
  type GetInteger mode = int | int -> mode

instance UnifiedIntegerImpl 'C Integer where
  type GetInteger 'C = Integer

instance UnifiedIntegerImpl 'S SymInteger where
  type GetInteger 'S = SymInteger

-- | Evaluation mode with unified 'Integer' type.
class
  (UnifiedIntegerImpl mode (GetInteger mode)) =>
  UnifiedInteger (mode :: EvalModeTag)

instance UnifiedInteger 'C

instance UnifiedInteger 'S
