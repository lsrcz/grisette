{-# OPTIONS_GHC -Wno-missing-import-lists #-}

-- |
-- Module      :   Grisette.Internal.Unified.Class.UnifiedSimpleMergeable
-- Copyright   :   (c) Sirui Lu 2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Internal.Unified.Class.UnifiedSimpleMergeable
  ( UnifiedBranching (..),
    UnifiedSimpleMergeable (..),
    UnifiedSimpleMergeable1 (..),
    UnifiedSimpleMergeable2 (..),
    mrgIf,
    liftBaseMonad,
    mrgIte,
    mrgIte1,
    liftMrgIte,
    mrgIte2,
    liftMrgIte2,
    simpleMerge,
  )
where

import Grisette.Internal.Internal.Decl.Unified.Class.UnifiedSimpleMergeable
import Grisette.Internal.Internal.Impl.Unified.Class.UnifiedSimpleMergeable
