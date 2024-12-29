{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      :   Grisette.Internal.Internal.Decl.Unified.Class.UnifiedITEOp
-- Copyright   :   (c) Sirui Lu 2021-2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Internal.Internal.Decl.Unified.Class.UnifiedITEOp
  ( UnifiedITEOp (..),
  )
where

import Data.Kind (Constraint)
import Data.Type.Bool (If)
import Grisette.Internal.Core.Data.Class.ITEOp (ITEOp)
import Grisette.Internal.Unified.EvalModeTag (IsConMode)

-- | A class that provides unified equality comparison.
--
-- We use this type class to help resolve the constraints for `ITEOp`.
class UnifiedITEOp mode v where
  withBaseITEOp ::
    ((If (IsConMode mode) (() :: Constraint) (ITEOp v)) => r) -> r
