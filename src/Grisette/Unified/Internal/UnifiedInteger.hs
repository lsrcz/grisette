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
import Grisette.Internal.SymPrim.SymPrim (Prim)
import Grisette.Unified.Internal.BaseConstraint
  ( ConSymConversion,
  )
import Grisette.Unified.Internal.Class.UnifiedFromIntegral (UnifiedFromIntegral)
import Grisette.Unified.Internal.Class.UnifiedSafeDiv (UnifiedSafeDiv)
import Grisette.Unified.Internal.Class.UnifiedSafeLinearArith
  ( UnifiedSafeLinearArith,
  )
import Grisette.Unified.Internal.Class.UnifiedSimpleMergeable (UnifiedBranching)
import Grisette.Unified.Internal.EvalModeTag (EvalModeTag (Con, Sym))
import Grisette.Unified.Internal.UnifiedConstraint (UnifiedPrimitive)

class
  ( Prim (GetInteger mode),
    ConSymConversion Integer SymInteger (GetInteger mode),
    Num (GetInteger mode),
    UnifiedPrimitive mode (GetInteger mode),
    forall m.
    (UnifiedBranching mode m, MonadError ArithException m) =>
    UnifiedSafeDiv mode ArithException i m,
    forall m.
    (UnifiedBranching mode m, MonadError ArithException m) =>
    UnifiedSafeLinearArith mode ArithException i m,
    UnifiedFromIntegral mode i i,
    i ~ GetInteger mode
  ) =>
  UnifiedIntegerImpl (mode :: EvalModeTag) i
    | mode -> i
  where
  -- | Get a unified Integer type. Resolves to 'Integer' in 'Con' mode, and
  -- 'SymInteger' in 'Sym' mode.
  type GetInteger mode = int | int -> mode

instance UnifiedIntegerImpl 'Con Integer where
  type GetInteger 'Con = Integer

instance UnifiedIntegerImpl 'Sym SymInteger where
  type GetInteger 'Sym = SymInteger

-- | Evaluation mode with unified 'Integer' type.
class
  (UnifiedIntegerImpl mode (GetInteger mode)) =>
  UnifiedInteger (mode :: EvalModeTag)

instance UnifiedInteger 'Con

instance UnifiedInteger 'Sym
