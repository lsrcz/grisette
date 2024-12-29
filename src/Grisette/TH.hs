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
    EvalModeConfig (..),
    DeriveConfig (..),
    deriveGADT,
    deriveGADTWith,
    allClasses0,
    allClasses01,
    allClasses012,
    basicClasses0,
    noExistentialClasses0,
    ordClasses0,
    basicClasses1,
    noExistentialClasses1,
    ordClasses1,
    basicClasses2,
    noExistentialClasses2,
    ordClasses2,

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
    allClasses01,
    allClasses012,
    basicClasses0,
    basicClasses1,
    basicClasses2,
    deriveGADT,
    deriveGADTWith,
    noExistentialClasses0,
    noExistentialClasses1,
    noExistentialClasses2,
    ordClasses0,
    ordClasses1,
    ordClasses2,
  )
import Grisette.Internal.TH.GADT.Common (EvalModeConfig (..), DeriveConfig (..))
