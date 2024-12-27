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

import Control.Monad.Identity (Identity (runIdentity))
import Data.Kind (Constraint)
import Data.Type.Bool (If)
import Grisette.Internal.Core.Control.Monad.Union (Union)
import Grisette.Internal.Core.Data.Class.ITEOp (ITEOp)
import qualified Grisette.Internal.Core.Data.Class.ITEOp
import Grisette.Internal.Core.Data.Class.Mergeable (Mergeable)
import qualified Grisette.Internal.Core.Data.Class.PlainUnion
import Grisette.Internal.Unified.BaseMonad (BaseMonad)
import Grisette.Internal.Unified.EvalModeTag (EvalModeTag (S), IsConMode)
import Grisette.Internal.Unified.UnifiedBool (UnifiedBool (GetBool))
import Grisette.Internal.Unified.Util (DecideEvalMode, withMode)

-- | A class that provides unified equality comparison.
--
-- We use this type class to help resolve the constraints for `ITEOp`.
class UnifiedITEOp mode v where
  withBaseITEOp ::
    ((If (IsConMode mode) (() :: Constraint) (ITEOp v)) => r) -> r
