{-# OPTIONS_GHC -Wno-missing-import-lists #-}

module Grisette.TH
  ( -- * Convenient derivation of all instances relating to Grisette
    derive,
    deriveAll,
    deriveAllExcept,

    -- * Smart constructors that merges in a monad
    mkMergeConstructor,
    mkMergeConstructor',

    -- * Smart constructors that are polymorphic in evaluation modes
    mkUnifiedConstructor,
    mkUnifiedConstructor',

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
import Grisette.Internal.TH.MergeConstructor
  ( mkMergeConstructor,
    mkMergeConstructor',
  )
import Grisette.Internal.TH.UnifiedConstructor
  ( mkUnifiedConstructor,
    mkUnifiedConstructor',
  )
