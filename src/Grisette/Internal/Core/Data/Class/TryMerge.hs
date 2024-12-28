{-# OPTIONS_GHC -Wno-missing-import-lists #-}

-- |
-- Module      :   Grisette.Internal.Core.Data.Class.TryMerge
-- Copyright   :   (c) Sirui Lu 2023-2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Internal.Core.Data.Class.TryMerge
  ( TryMerge (..),
    tryMerge,
    MonadTryMerge,
    mrgSingle,
    mrgSingleWithStrategy,
    mrgToSym,
    toUnionSym,
  )
where

import Grisette.Internal.Internal.Decl.Core.Data.Class.TryMerge
import Grisette.Internal.Internal.Impl.Core.Data.Class.TryMerge ()
