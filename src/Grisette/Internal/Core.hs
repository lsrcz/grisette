{-# LANGUAGE Trustworthy #-}

-- |
-- Module      :   Grisette.Internal.Core
-- Copyright   :   (c) Sirui Lu 2021-2023
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Internal.Core
  ( -- * The UnionBase type
    Union (..),
    ifWithLeftMost,
    ifWithStrategy,
    fullReconstruct,

    -- * The UnionMBase type
    UnionM (..),
    underlyingUnion,
    isMerged,
  )
where

import Grisette.Core.Control.Monad.UnionM
import Grisette.Core.Data.Union
