module Grisette.Qualified.ParallelUnionDo where

import Control.Parallel.Strategies
import Grisette.Core.Control.Monad.Class.MonadParallelUnion
import Grisette.Core.Data.Class.Mergeable

(>>=) :: (MonadParallelUnion m, Mergeable b, NFData b) => m a -> (a -> m b) -> m b
(>>=) = parBindUnion

(>>) :: (MonadParallelUnion m, Mergeable b, NFData b) => m a -> m b -> m b
(>>) a b = parBindUnion a $ const b
