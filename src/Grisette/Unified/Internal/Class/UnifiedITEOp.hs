{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Grisette.Unified.Internal.Class.UnifiedITEOp
  ( symIte,
    symIteMerge,
    UnifiedITEOp (..),
  )
where

import Control.Monad.Identity (Identity (runIdentity))
import Data.Kind (Constraint)
import Data.Type.Bool (If)
import Data.Typeable (Typeable)
import Grisette.Internal.Core.Data.Class.ITEOp (ITEOp)
import qualified Grisette.Internal.Core.Data.Class.ITEOp
import Grisette.Internal.Core.Data.Class.Mergeable (Mergeable)
import qualified Grisette.Internal.Core.Data.Class.PlainUnion
import Grisette.Unified.Internal.BaseMonad (BaseMonad)
import Grisette.Unified.Internal.EvaluationMode
  ( IsConMode,
  )
import Grisette.Unified.Internal.UnifiedBool (UnifiedBool (GetBool))
import Grisette.Unified.Internal.Util (withMode)

-- | Unified `Grisette.Internal.Core.Data.Class.ITEOp.symIte` operation.
--
-- This function isn't able to infer the mode of the boolean variable, so you
-- need to provide the mode explicitly. For example:
--
-- > symIte @mode (a .== b) ...
-- > symIte (a .== b :: SymBool) ...
-- > symIte (a .== b :: GetBool mode) ...
symIte ::
  forall mode v.
  (Typeable mode, UnifiedITEOp mode v) =>
  GetBool mode ->
  v ->
  v ->
  v
symIte c a b =
  withMode @mode
    (withBaseITEOp @mode @v $ if c then a else b)
    ( withBaseITEOp @mode @v $
        Grisette.Internal.Core.Data.Class.ITEOp.symIte c a b
    )

-- | Unified `Grisette.Internal.Core.Data.Class.PlainUnion.symIteMerge`
-- operation.
--
-- This function isn't able to infer the mode of the base monad from the result,
-- so you need to provide the mode explicitly. For example:
--
-- > symIteMerge @mode ...
-- > symIteMerge (... :: BaseMonad mode v) ...
symIteMerge ::
  forall mode v.
  (Typeable mode, UnifiedITEOp mode v, Mergeable v) =>
  BaseMonad mode v ->
  v
symIteMerge m =
  withMode @mode
    (withBaseITEOp @mode @v $ runIdentity m)
    ( withBaseITEOp @mode @v $
        Grisette.Internal.Core.Data.Class.PlainUnion.symIteMerge m
    )

-- | A class that provides unified equality comparison.
--
-- We use this type class to help resolve the constraints for `ITEOp`.
class UnifiedITEOp mode v where
  withBaseITEOp ::
    ((If (IsConMode mode) (() :: Constraint) (ITEOp v)) => r) -> r

instance
  ( Typeable mode,
    If (IsConMode mode) (() :: Constraint) (ITEOp a)
  ) =>
  UnifiedITEOp mode a
  where
  withBaseITEOp r = withMode @mode r r
  {-# INLINE withBaseITEOp #-}
