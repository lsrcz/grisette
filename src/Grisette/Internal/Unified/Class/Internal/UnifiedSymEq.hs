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
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Eta reduce" #-}

module Grisette.Internal.Unified.Class.Internal.UnifiedSymEq
  ( UnifiedSymEq (..),
    UnifiedSymEq1 (..),
    UnifiedSymEq2 (..),
  )
where

import Data.Functor.Classes (Eq1 (liftEq), Eq2 (liftEq2), eq1, eq2)
import Data.Type.Bool (If)
import Grisette.Internal.Core.Data.Class.Internal.SymEq
  ( SymEq,
    SymEq1,
    SymEq2,
  )
import qualified Grisette.Internal.Core.Data.Class.Internal.SymEq as SymEq
import Grisette.Internal.Internal.Decl.Unified.UnifiedBool
  ( UnifiedBool (GetBool),
  )
import Grisette.Internal.Unified.EvalModeTag (IsConMode)
import Grisette.Internal.Unified.Util (DecideEvalMode, withMode)

-- | A class that provides unified equality comparison.
--
-- We use this type class to help resolve the constraints for `Eq` and `SymEq`.
class UnifiedSymEq mode a where
  withBaseSymEq :: ((If (IsConMode mode) (Eq a) (SymEq a)) => r) -> r

-- | A class that provides unified lifting of equality comparison.
--
-- We use this type class to help resolve the constraints for `Eq1` and
-- `SymEq1`.
class
  (forall a. (UnifiedSymEq mode a) => UnifiedSymEq mode (f a)) =>
  UnifiedSymEq1 mode f
  where
  withBaseSymEq1 :: ((If (IsConMode mode) (Eq1 f) (SymEq1 f)) => r) -> r

-- | A class that provides unified lifting of equality comparison.
--
-- We use this type class to help resolve the constraints for `Eq2` and
-- `SymEq2`.
class
  (forall a. (UnifiedSymEq mode a) => UnifiedSymEq1 mode (f a)) =>
  UnifiedSymEq2 mode f
  where
  withBaseSymEq2 :: ((If (IsConMode mode) (Eq2 f) (SymEq2 f)) => r) -> r
