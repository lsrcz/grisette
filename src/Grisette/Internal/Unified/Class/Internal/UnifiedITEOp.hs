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

module Grisette.Internal.Unified.Class.Internal.UnifiedITEOp
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
