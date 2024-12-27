{-# OPTIONS_GHC -Wno-missing-import-lists #-}

-- |
-- Module      :   Grisette.Internal.Core.Data.Class.SymOrd
-- Copyright   :   (c) Sirui Lu 2021-2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Internal.Core.Data.Class.SymOrd
  ( -- * Symbolic total order relation
    SymOrd (..),
    SymOrd1 (..),
    symCompare1,
    SymOrd2 (..),
    symCompare2,

    -- * Min and max
    symMax,
    symMin,
    mrgMax,
    mrgMin,

    -- * Generic 'SymOrd'
    SymOrdArgs (..),
    GSymOrd (..),
    genericSymCompare,
    genericLiftSymCompare,
  )
where

import Grisette.Internal.Internal.Decl.Core.Data.Class.SymOrd
import Grisette.Internal.Internal.Impl.Core.Data.Class.SymOrd ()
