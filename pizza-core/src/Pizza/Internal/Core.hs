module Pizza.Internal.Core
  ( UnionBase (..),
    ifWithLeftMost,
    ifWithStrategy,
    fullReconstruct,
    UnionMBase (..),
    underlyingUnion,
    isMerged,
  )
where

import Pizza.Core.Control.Monad.UnionMBase
import Pizza.Core.Data.UnionBase
