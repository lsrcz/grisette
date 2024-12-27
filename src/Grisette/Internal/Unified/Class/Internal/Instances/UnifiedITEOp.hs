{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Eta reduce" #-}

module Grisette.Internal.Unified.Class.Internal.Instances.UnifiedITEOp
  ( symIte,
    symIteMerge,
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
import Grisette.Internal.Unified.Class.Internal.UnifiedITEOp
  ( UnifiedITEOp (withBaseITEOp),
  )
import Grisette.Internal.Unified.EvalModeTag (EvalModeTag (S), IsConMode)
import Grisette.Internal.Unified.UnifiedBool (UnifiedBool (GetBool))
import Grisette.Internal.Unified.Util (DecideEvalMode, withMode)

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
  (DecideEvalMode mode, UnifiedITEOp mode v) =>
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
  (DecideEvalMode mode, UnifiedITEOp mode v, Mergeable v) =>
  BaseMonad mode v ->
  v
symIteMerge m =
  withMode @mode
    (withBaseITEOp @mode @v $ runIdentity m)
    ( withBaseITEOp @mode @v $
        Grisette.Internal.Core.Data.Class.PlainUnion.symIteMerge m
    )

instance
  {-# INCOHERENT #-}
  ( DecideEvalMode mode,
    If (IsConMode mode) (() :: Constraint) (ITEOp a)
  ) =>
  UnifiedITEOp mode a
  where
  withBaseITEOp r = withMode @mode r r
  {-# INLINE withBaseITEOp #-}

instance (UnifiedITEOp 'S v, Mergeable v) => UnifiedITEOp 'S (Union v) where
  withBaseITEOp r = withBaseITEOp @'S @v r
  {-# INLINE withBaseITEOp #-}
