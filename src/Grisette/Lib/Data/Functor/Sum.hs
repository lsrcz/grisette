{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module      :   Grisette.Lib.Data.Functor.Sum
-- Copyright   :   (c) Sirui Lu 2021-2023
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Lib.Data.Functor.Sum (mrgInR, mrgInL) where

import Data.Functor.Sum (Sum)
import Grisette.Internal.TH.Ctor.SmartConstructor
  ( makePrefixedSmartCtor,
  )

makePrefixedSmartCtor "mrg" ''Sum
