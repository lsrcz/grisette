{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilyDependencies #-}

-- |
-- Module      :   Grisette.Unified.Internal.EvalModeTag
-- Copyright   :   (c) Sirui Lu 2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Unified.Internal.EvalModeTag
  ( EvalModeTag (..),
    IsConMode,
  )
where

import Language.Haskell.TH.Syntax (Lift)

-- | Evaluation mode for unified types. 'C' means concrete evaluation, 'S'
-- means symbolic evaluation.
data EvalModeTag = C | S deriving (Lift, Show, Eq)

-- | Type family to check if a mode is 'C'.
type family IsConMode (mode :: EvalModeTag) = (r :: Bool) | r -> mode where
  IsConMode 'C = 'True
  IsConMode 'S = 'False
