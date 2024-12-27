{-# OPTIONS_GHC -Wno-missing-import-lists #-}

-- |
-- Module      :   Grisette.Internal.Core.Data.Class.SymEq
-- Copyright   :   (c) Sirui Lu 2021-2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Internal.Core.Data.Class.SymEq
  ( -- * Symbolic equality
    SymEq (..),
    SymEq1 (..),
    symEq1,
    SymEq2 (..),
    symEq2,
    pairwiseSymDistinct,

    -- * More 'Eq' helper
    distinct,

    -- * Generic 'SymEq'
    SymEqArgs (..),
    GSymEq (..),
    genericSymEq,
    genericLiftSymEq,
  )
where

import Grisette.Internal.Core.Data.Class.Internal.Instances.SymEq ()
import Grisette.Internal.Core.Data.Class.Internal.SymEq
