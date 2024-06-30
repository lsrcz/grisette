{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module      :   Grisette.Lib.Data.Either
-- Copyright   :   (c) Sirui Lu 2021-2023
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Lib.Data.Either (mrgLeft, mrgRight) where

import Grisette.Internal.Core.Data.Class.TryMerge (mrgSingle)
import Grisette.Internal.TH.MergeConstructor
  ( mkMergeConstructor,
  )

mkMergeConstructor "mrg" ''Either
