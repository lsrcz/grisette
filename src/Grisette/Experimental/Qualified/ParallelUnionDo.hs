-- |
-- Module      :   Grisette.Qualified.ParallelUnionDo
-- Copyright   :   (c) Sirui Lu 2023
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Experimental.Qualified.ParallelUnionDo ((>>=), (>>)) where

import Control.Parallel.Strategies (NFData)
import Grisette.Experimental.MonadParallelUnion
  ( MonadParallelUnion (parBindUnion),
  )
import Grisette.Internal.Core.Data.Class.Mergeable (Mergeable)
import Prelude (const, ($))

(>>=) :: (MonadParallelUnion m, Mergeable b, NFData b) => m a -> (a -> m b) -> m b
(>>=) = parBindUnion

(>>) :: (MonadParallelUnion m, Mergeable b, NFData b) => m a -> m b -> m b
(>>) a b = parBindUnion a $ const b
