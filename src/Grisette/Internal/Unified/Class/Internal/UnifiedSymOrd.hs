{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Eta reduce" #-}

module Grisette.Internal.Unified.Class.Internal.UnifiedSymOrd
  ( UnifiedSymOrd (..),
    UnifiedSymOrd1 (..),
    UnifiedSymOrd2 (..),
  )
where

-- import Control.Monad.Identity (Identity (runIdentity))
import Data.Functor.Classes
  ( Ord1 (liftCompare),
    Ord2 (liftCompare2),
    compare1,
    compare2,
  )
import Data.Type.Bool (If)
import Grisette.Internal.Core.Data.Class.Internal.SymOrd
  ( SymOrd,
    SymOrd1,
    SymOrd2,
  )
-- import Grisette.Internal.Core.Data.Class.Mergeable (Mergeable)
-- import Grisette.Internal.Core.Data.Class.TryMerge (tryMerge)
-- import Grisette.Internal.Unified.BaseMonad (BaseMonad)
-- import Grisette.Internal.Unified.Class.UnifiedITEOp
--   ( UnifiedITEOp (withBaseITEOp),
--   )
-- import Grisette.Internal.Unified.Class.UnifiedSimpleMergeable
--   ( UnifiedBranching (withBaseBranching),
--   )
import Grisette.Internal.Unified.EvalModeTag (IsConMode)

-- import Grisette.Internal.Unified.UnifiedBool (UnifiedBool (GetBool))
-- import Grisette.Internal.Unified.Util (DecideEvalMode, withMode)

-- | A class that provides unified comparison.
--
-- We use this type class to help resolve the constraints for `Ord` and
-- `SymOrd`.
class UnifiedSymOrd mode a where
  withBaseSymOrd :: (((If (IsConMode mode) (Ord a) (SymOrd a)) => r)) -> r

-- | A class that provides unified lifting of comparison.
--
-- We use this type class to help resolve the constraints for `Ord1` and
-- `SymOrd1`.
class UnifiedSymOrd1 mode f where
  withBaseSymOrd1 :: (((If (IsConMode mode) (Ord1 f) (SymOrd1 f)) => r)) -> r

-- | A class that provides unified lifting of comparison.
--
-- We use this type class to help resolve the constraints for `Ord2` and
-- `SymOrd2`.
class UnifiedSymOrd2 mode f where
  withBaseSymOrd2 :: (((If (IsConMode mode) (Ord2 f) (SymOrd2 f)) => r)) -> r
