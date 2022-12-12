{-# LANGUAGE Trustworthy #-}

-- |
-- Module      :   Grisette.Internal.Core
-- Copyright   :   (c) Sirui Lu 2021-2022
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Internal.Core
  ( UnionBase (..),
    ifWithLeftMost,
    ifWithStrategy,
    fullReconstruct,
    UnionMBase (..),
    underlyingUnion,
    isMerged,
  )
where

import Grisette.Core.Control.Monad.UnionMBase
import Grisette.Core.Data.UnionBase
