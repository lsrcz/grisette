{-# OPTIONS_GHC -Wno-missing-import-lists #-}

-- |
-- Module      :   Grisette.Internal.Core.Data.Class.ToCon
-- Copyright   :   (c) Sirui Lu 2021-2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Internal.Core.Data.Class.ToCon
  ( -- * Converting to concrete values
    ToCon (..),
    ToCon1 (..),
    toCon1,
    ToCon2 (..),
    toCon2,

    -- * Generic 'ToCon'
    ToConArgs (..),
    GToCon (..),
    genericToCon,
    genericLiftToCon,
  )
where

import Grisette.Internal.Internal.Decl.Core.Data.Class.ToCon
import Grisette.Internal.Internal.Impl.Core.Data.Class.ToCon ()
