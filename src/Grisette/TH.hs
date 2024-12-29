{-# OPTIONS_GHC -Wno-missing-import-lists #-}

-- |
-- Module      :   Grisette.TH
-- Copyright   :   (c) Sirui Lu 2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.TH
  ( -- * Convenient derivation of all instances relating to Grisette
    deriveGADT,
    deriveGADTWith,
    allClasses0,
    allClasses0WithOrd,
    allClasses1,
    allClasses1WithOrd,
    allClasses2,
    allClasses2WithOrd,

    -- * Smart constructors that merges in a monad
    makePrefixedSmartCtor,
    makeNamedSmartCtor,
    makeSmartCtor,
    makeSmartCtorWith,

    -- * Smart constructors that are polymorphic in evaluation modes
    makePrefixedUnifiedCtor,
    makeNamedUnifiedCtor,
    makeUnifiedCtor,
    makeUnifiedCtorWith,
  )
where

import Grisette.Internal.TH.Ctor.SmartConstructor
  ( makeNamedSmartCtor,
    makePrefixedSmartCtor,
    makeSmartCtor,
    makeSmartCtorWith,
  )
import Grisette.Internal.TH.Ctor.UnifiedConstructor
  ( makeNamedUnifiedCtor,
    makePrefixedUnifiedCtor,
    makeUnifiedCtor,
    makeUnifiedCtorWith,
  )
import Grisette.Internal.TH.GADT.DeriveGADT
  ( allClasses0,
    allClasses0WithOrd,
    allClasses1,
    allClasses1WithOrd,
    allClasses2,
    allClasses2WithOrd,
    deriveGADT,
    deriveGADTWith,
  )
