{-# OPTIONS_GHC -Wno-missing-import-lists #-}

-- |
-- Module      :   Grisette.Internal.Core.Data.Class.SafeDiv
-- Copyright   :   (c) Sirui Lu 2021-2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Internal.Core.Data.Class.SafeDiv
  ( ArithException (..),
    SafeDiv (..),
    DivOr (..),
    divOrZero,
    modOrDividend,
    quotOrZero,
    remOrDividend,
    divModOrZeroDividend,
    quotRemOrZeroDividend,
  )
where

import Grisette.Internal.Internal.Decl.Core.Data.Class.SafeDiv
import Grisette.Internal.Internal.Impl.Core.Data.Class.SafeDiv ()
