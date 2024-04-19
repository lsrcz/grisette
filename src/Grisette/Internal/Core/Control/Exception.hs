{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Trustworthy #-}

-- |
-- Module      :   Grisette.Internal.Core.Control.Exception
-- Copyright   :   (c) Sirui Lu 2021-2023
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Internal.Core.Control.Exception
  ( -- * Predefined exceptions
    AssertionError (..),
    VerificationConditions (..),
  )
where

import Control.DeepSeq (NFData)
import GHC.Generics (Generic)

-- $setup
-- >>> import Grisette.Core
-- >>> import Grisette.Lib.Base
-- >>> import Grisette.SymPrim
-- >>> import Control.Monad.Trans.Except

-- | Assertion error.
data AssertionError = AssertionError
  deriving (Show, Eq, Ord, Generic, NFData)

-- | Verification conditions.
-- A crashed program path can terminate with either assertion violation errors or assumption violation errors.
data VerificationConditions
  = AssertionViolation
  | AssumptionViolation
  deriving (Show, Eq, Ord, Generic, NFData)
