{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module      :   Grisette.Lib.Data.Bool
-- Copyright   :   (c) Sirui Lu 2021-2023
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Lib.Data.Bool (mrgTrue, mrgFalse) where

import Grisette.Internal.TH.Ctor.SmartConstructor
  ( makePrefixedSmartCtor,
  )

makePrefixedSmartCtor "mrg" ''Bool
