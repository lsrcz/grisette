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
    derive,
    deriveAll,
    deriveAllExcept,

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

    -- * Tools for building more derivation procedures

    -- ** Type parameter handlers
    DeriveTypeParamHandler (..),
    IsFPBits (..),
    NatShouldBePositive (..),
    PrimaryConstraint (..),
    SomeDeriveTypeParamHandler (..),

    -- ** Instance providers
    DeriveInstanceProvider (..),
    Strategy (..),

    -- ** For unified interfaces
    TypeableMode (..),
    PrimaryUnifiedConstraint (..),
    UnifiedInstance (..),

    -- ** Other helpers
    deriveWithHandlers,
    derivePredefined,
    derivePredefinedMultipleClasses,
    deriveBuiltinExtra,
    deriveUnifiedInterfaceExtra,
    deriveUnifiedInterface1Extra,
    deriveFunctorArgUnifiedInterfaceExtra,
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
import Grisette.Internal.TH.DeriveBuiltin
  ( deriveBuiltinExtra,
  )
import Grisette.Internal.TH.DeriveInstanceProvider
  ( DeriveInstanceProvider (..),
    Strategy (..),
  )
import Grisette.Internal.TH.DerivePredefined
  ( derive,
    deriveAll,
    deriveAllExcept,
    derivePredefined,
    derivePredefinedMultipleClasses,
  )
import Grisette.Internal.TH.DeriveTypeParamHandler
  ( DeriveTypeParamHandler (..),
    IsFPBits (..),
    NatShouldBePositive (..),
    PrimaryConstraint (..),
    SomeDeriveTypeParamHandler (..),
  )
import Grisette.Internal.TH.DeriveUnifiedInterface
  ( PrimaryUnifiedConstraint (..),
    TypeableMode (..),
    UnifiedInstance (..),
    deriveFunctorArgUnifiedInterfaceExtra,
    deriveUnifiedInterface1Extra,
    deriveUnifiedInterfaceExtra,
  )
import Grisette.Internal.TH.DeriveWithHandlers (deriveWithHandlers)
