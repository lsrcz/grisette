{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilyDependencies #-}

-- |
-- Module      :   Grisette.Unified.Internal.UnifiedAlgReal
-- Copyright   :   (c) Sirui Lu 2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Unified.Internal.UnifiedAlgReal (UnifiedAlgReal) where

import Grisette.Internal.Core.Data.Class.SafeFdiv (FdivOr)
import Grisette.Internal.SymPrim.AlgReal (AlgReal)
import Grisette.Internal.SymPrim.SymAlgReal (SymAlgReal)
import Grisette.Unified.Internal.BaseConstraint (BasicGrisetteType, ConSymConversion)
import Grisette.Unified.Internal.EvalModeTag (EvalModeTag (Con, Sym))
import Grisette.Unified.Internal.UnifiedConstraint (UnifiedPrimitive)

class
  ( BasicGrisetteType (GetAlgReal mode),
    ConSymConversion AlgReal SymAlgReal (GetAlgReal mode),
    Num (GetAlgReal mode),
    Fractional (GetAlgReal mode),
    UnifiedPrimitive mode (GetAlgReal mode),
    FdivOr (GetAlgReal mode)
  ) =>
  UnifiedAlgRealImpl (mode :: EvalModeTag) a
    | mode -> a
  where
  type GetAlgReal mode = real | real -> mode

instance UnifiedAlgRealImpl 'Con AlgReal where
  type GetAlgReal 'Con = AlgReal

instance UnifiedAlgRealImpl 'Sym SymAlgReal where
  type GetAlgReal 'Sym = SymAlgReal

class
  (UnifiedAlgRealImpl mode (GetAlgReal mode)) =>
  UnifiedAlgReal (mode :: EvalModeTag)

instance UnifiedAlgReal 'Con

instance UnifiedAlgReal 'Sym
