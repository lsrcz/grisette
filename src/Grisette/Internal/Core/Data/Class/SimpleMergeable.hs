{-# OPTIONS_GHC -Wno-missing-import-lists #-}

-- |
-- Module      :   Grisette.Internal.Core.Data.Class.SimpleMergeable
-- Copyright   :   (c) Sirui Lu 2021-2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Internal.Core.Data.Class.SimpleMergeable
  ( -- * Simple mergeable types
    SimpleMergeable (..),
    SimpleMergeable1 (..),
    mrgIte1,
    SimpleMergeable2 (..),
    mrgIte2,

    -- * Generic 'SimpleMergeable'
    SimpleMergeableArgs (..),
    GSimpleMergeable (..),
    genericMrgIte,
    genericLiftMrgIte,

    -- * Symbolic branching
    SymBranching (..),
    mrgIf,
    mergeWithStrategy,
    merge,
  )
where

import Grisette.Internal.Internal.Decl.Core.Data.Class.SimpleMergeable
import Grisette.Internal.Internal.Impl.Core.Data.Class.SimpleMergeable ()
