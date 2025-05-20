-- |
-- Module      :   Grisette.Internal.TH.ADT
-- Copyright   :   (c) Sirui Lu 2025
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Internal.TH.ADT
  ( makeGrisetteADTWithClasses,
    makeGrisetteBasicADT,
    makeGrisetteBasicADT1,
    makeGrisetteBasicADT2,
    makeGrisetteBasicADTWith,
    makeGrisetteBasicADT1With,
    makeGrisetteBasicADT2With,
    makeGrisetteADT,
    makeGrisetteADT1,
    makeGrisetteADT2,
    makeGrisetteADTWith,
    makeGrisetteADT1With,
    makeGrisetteADT2With,
  )
where

import Grisette.Internal.TH.Ctor.SmartConstructor (makeSmartCtor)
import Grisette.Internal.TH.Derivation.Common (DeriveConfig)
import Grisette.Internal.TH.Derivation.Derive
  ( allClasses0,
    allClasses01,
    allClasses012,
    basicClasses0,
    basicClasses01,
    basicClasses012,
    deriveWith,
  )
import Language.Haskell.TH (Dec, Name, Q)

-- | Make an ADT compatible with Grisette.
--
-- This will generate instances for the ADT with the given classes,
-- and smart constructors for each constructor.
--
-- The derivation is done with the given configuration.
makeGrisetteADTWithClasses :: DeriveConfig -> Name -> [Name] -> Q [Dec]
makeGrisetteADTWithClasses config name classes = do
  instances <- deriveWith config [name] classes
  ctors <- makeSmartCtor name
  return (instances ++ ctors)

-- | Make an ADT compatible with Grisette.
--
-- This will generate most useful instances for the ADT,
-- and smart constructors for each constructor.
--
-- This does not include 'Ord' instances.
--
-- See 'derive', 'basicClasses0', and 'makeSmartCtor' for more details.
makeGrisetteBasicADT :: Name -> Q [Dec]
makeGrisetteBasicADT = makeGrisetteBasicADTWith mempty

-- | Make an ADT compatible with Grisette.
--
-- This will generate most useful instances for the ADT,
-- and smart constructors for each constructor.
--
-- This does not include 'Ord' instances.
--
-- The derivation is done with the given configuration.
--
-- See 'deriveWith', 'basicClasses0', and 'makeSmartCtor' for more details.
makeGrisetteBasicADTWith :: DeriveConfig -> Name -> Q [Dec]
makeGrisetteBasicADTWith config name =
  makeGrisetteADTWithClasses config name basicClasses0

-- | Make an ADT compatible with Grisette.
--
-- This will generate most useful instances for the ADT,
-- and smart constructors for each constructor.
--
-- This does not include 'Ord' instances.
--
-- See 'derive', 'basicClasses01', and 'makeSmartCtor' for more details.
makeGrisetteBasicADT1 :: Name -> Q [Dec]
makeGrisetteBasicADT1 name =
  makeGrisetteADTWithClasses mempty name basicClasses01

-- | Make an ADT compatible with Grisette.
--
-- This will generate most useful instances for the ADT,
-- and smart constructors for each constructor.
--
-- This does not include 'Ord' instances.
--
-- The derivation is done with the given configuration.
--
-- See 'deriveWith', 'basicClasses01', and 'makeSmartCtor' for more details.
makeGrisetteBasicADT1With :: DeriveConfig -> Name -> Q [Dec]
makeGrisetteBasicADT1With config name =
  makeGrisetteADTWithClasses config name basicClasses01

-- | Make an ADT compatible with Grisette.
--
-- This will generate most useful instances for the ADT,
-- and smart constructors for each constructor.
--
-- This does not include 'Ord' instances.
--
-- See 'derive', 'basicClasses012', and 'makeSmartCtor' for more details.
makeGrisetteBasicADT2 :: Name -> Q [Dec]
makeGrisetteBasicADT2 name =
  makeGrisetteADTWithClasses mempty name basicClasses012

-- | Make an ADT compatible with Grisette.
--
-- This will generate most useful instances for the ADT,
-- and smart constructors for each constructor.
--
-- This does not include 'Ord' instances.
--
-- The derivation is done with the given configuration.
--
-- See 'deriveWith', 'basicClasses012', and 'makeSmartCtor' for more details.
makeGrisetteBasicADT2With :: DeriveConfig -> Name -> Q [Dec]
makeGrisetteBasicADT2With config name =
  makeGrisetteADTWithClasses config name basicClasses012

-- | Make an ADT compatible with Grisette.
--
-- This will generate almost all useful instances for the ADT,
-- and smart constructors for each constructor.
--
-- This cannot be used for ADTs with existential type variables.
--
-- See 'derive', 'allClasses0', and 'makeSmartCtor' for more details.
makeGrisetteADT :: Name -> Q [Dec]
makeGrisetteADT = makeGrisetteADTWith mempty

-- | Make an ADT compatible with Grisette.
--
-- This will generate almost all useful instances for the ADT,
-- and smart constructors for each constructor.
--
-- This cannot be used for ADTs with existential type variables.
--
-- The derivation is done with the given configuration.
--
-- See 'deriveWith', 'allClasses0', and 'makeSmartCtor' for more details.
makeGrisetteADTWith :: DeriveConfig -> Name -> Q [Dec]
makeGrisetteADTWith config name =
  makeGrisetteADTWithClasses config name allClasses0

-- | Make an ADT compatible with Grisette.
--
-- This will generate almost all useful instances for the ADT,
-- and smart constructors for each constructor.
--
-- This cannot be used for ADTs with existential type variables.
--
-- See 'derive', 'allClasses01', and 'makeSmartCtor' for more details.
makeGrisetteADT1 :: Name -> Q [Dec]
makeGrisetteADT1 = makeGrisetteADT1With mempty

-- | Make an ADT compatible with Grisette.
--
-- This will generate almost all useful instances for the ADT,
-- and smart constructors for each constructor.
--
-- This cannot be used for ADTs with existential type variables.
--
-- The derivation is done with the given configuration.
--
-- See 'deriveWith', 'allClasses01', and 'makeSmartCtor' for more details.
makeGrisetteADT1With :: DeriveConfig -> Name -> Q [Dec]
makeGrisetteADT1With config name =
  makeGrisetteADTWithClasses config name allClasses01

-- | Make an ADT compatible with Grisette.
--
-- This will generate almost all useful instances for the ADT,
-- and smart constructors for each constructor.
--
-- This cannot be used for ADTs with existential type variables.
--
-- See 'derive', 'allClasses012', and 'makeSmartCtor' for more details.
makeGrisetteADT2 :: Name -> Q [Dec]
makeGrisetteADT2 = makeGrisetteADT2With mempty

-- | Make an ADT compatible with Grisette.
--
-- This will generate almost all useful instances for the ADT,
-- and smart constructors for each constructor.
--
-- This cannot be used for ADTs with existential type variables.
--
-- The derivation is done with the given configuration.
--
-- See 'deriveWith', 'allClasses012', and 'makeSmartCtor' for more details.
makeGrisetteADT2With :: DeriveConfig -> Name -> Q [Dec]
makeGrisetteADT2With config name =
  makeGrisetteADTWithClasses config name allClasses012
