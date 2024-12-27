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

module Grisette.Internal.Internal.Decl.Unified.Class.UnifiedSymEq
  ( UnifiedSymEq (..),
    UnifiedSymEq1 (..),
    UnifiedSymEq2 (..),
  )
where

import Data.Functor.Classes (Eq1, Eq2)
import Data.Type.Bool (If)
import Grisette.Internal.Internal.Decl.Core.Data.Class.SymEq
  ( SymEq,
    SymEq1,
    SymEq2,
  )
import Grisette.Internal.Unified.EvalModeTag (IsConMode)

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
