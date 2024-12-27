-- |
-- Module      :   Grisette.Internal.Core.Data.UnionBase
-- Copyright   :   (c) Sirui Lu 2021-2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Internal.Core.Data.UnionBase
  ( -- * The union data structure.

    -- | Please consider using 'Grisette.Core.Union' instead.
    UnionBase (..),
    ifWithLeftMost,
    ifWithStrategy,
    fullReconstruct,
  )
where

import Grisette.Internal.Internal.Decl.Core.Data.UnionBase
  ( UnionBase (UnionIf, UnionSingle),
    fullReconstruct,
    ifWithLeftMost,
    ifWithStrategy,
  )
import Grisette.Internal.Internal.Impl.Core.Data.UnionBase ()
