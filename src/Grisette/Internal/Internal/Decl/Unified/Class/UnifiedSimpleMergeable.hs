{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      :   Grisette.Internal.Internal.Decl.Unified.Class.UnifiedSimpleMergeable
-- Copyright   :   (c) Sirui Lu 2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Internal.Internal.Decl.Unified.Class.UnifiedSimpleMergeable
  ( UnifiedBranching (..),
    UnifiedSimpleMergeable (..),
    UnifiedSimpleMergeable1 (..),
    UnifiedSimpleMergeable2 (..),
  )
where

import Data.Kind (Constraint)
import Data.Type.Bool (If)
import Grisette.Internal.Internal.Decl.Core.Data.Class.Mergeable
  ( Mergeable,
  )
import Grisette.Internal.Internal.Decl.Core.Data.Class.SimpleMergeable
  ( SimpleMergeable,
    SimpleMergeable1,
    SimpleMergeable2,
    SymBranching,
  )
import Grisette.Internal.Internal.Decl.Core.Data.Class.TryMerge
  ( TryMerge,
  )
import Grisette.Internal.Unified.EvalModeTag (EvalModeTag, IsConMode)
import Grisette.Internal.Unified.Util (DecideEvalMode)

-- | A class that provides a unified simple merging.
--
-- We use this type class to help resolve the constraints for `SimpleMergeable`.
class (Mergeable a) => UnifiedSimpleMergeable mode a where
  withBaseSimpleMergeable ::
    ((If (IsConMode mode) (() :: Constraint) (SimpleMergeable a)) => r) -> r

-- | A class that provides lifting of unified simple merging.
--
-- We use this type class to help resolve the constraints for
-- `SimpleMergeable1`.
class UnifiedSimpleMergeable1 mode f where
  withBaseSimpleMergeable1 ::
    ((If (IsConMode mode) (() :: Constraint) (SimpleMergeable1 f)) => r) -> r

-- | A class that provides lifting of unified simple merging.
--
-- We use this type class to help resolve the constraints for
-- `SimpleMergeable2`.
class UnifiedSimpleMergeable2 mode f where
  withBaseSimpleMergeable2 ::
    ((If (IsConMode mode) (() :: Constraint) (SimpleMergeable2 f)) => r) -> r

-- | A class that provides a unified branching operation.
--
-- We use this type class to help resolve the constraints for
-- `SymBranching`.
class
  (DecideEvalMode mode, TryMerge m) =>
  UnifiedBranching (mode :: EvalModeTag) m
  where
  withBaseBranching ::
    ((If (IsConMode mode) (TryMerge m) (SymBranching m)) => r) -> r
