-- |
-- Module      :   Grisette.Internal.TH.ADT
-- Copyright   :   (c) Sirui Lu 2025
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Internal.TH.ADT
  ( makeGrisetteBasicADT,
    makeGrisetteBasicADT1,
    makeGrisetteBasicADT2,
    makeGrisetteADT,
    makeGrisetteADT1,
    makeGrisetteADT2,
  )
where

import Grisette.Internal.TH.Ctor.SmartConstructor (makeSmartCtor)
import Grisette.Internal.TH.Derivation.Derive
  ( allClasses0,
    allClasses01,
    allClasses012,
    basicClasses0,
    basicClasses01,
    basicClasses012,
    derive,
  )
import Language.Haskell.TH (Dec, Name, Q)

-- | Make an ADT compatible with Grisette.
--
-- This will generate most useful instances for the ADT,
-- and smart constructors for each constructor.
--
-- This does not include 'Ord' instances.
--
-- See 'derive', 'basicClasses0', and 'makeSmartCtor' for more details.
makeGrisetteBasicADT :: Name -> Q [Dec]
makeGrisetteBasicADT name = do
  instances <- derive [name] basicClasses0
  ctors <- makeSmartCtor name
  return (instances ++ ctors)

-- | Make an ADT compatible with Grisette.
--
-- This will generate most useful instances for the ADT,
-- and smart constructors for each constructor.
--
-- This does not include 'Ord' instances.
--
-- See 'derive', 'basicClasses01', and 'makeSmartCtor' for more details.
makeGrisetteBasicADT1 :: Name -> Q [Dec]
makeGrisetteBasicADT1 name = do
  instances <- derive [name] basicClasses01
  ctors <- makeSmartCtor name
  return (instances ++ ctors)

-- | Make an ADT compatible with Grisette.
--
-- This will generate most useful instances for the ADT,
-- and smart constructors for each constructor.
--
-- This does not include 'Ord' instances.
--
-- See 'derive', 'basicClasses012', and 'makeSmartCtor' for more details.
makeGrisetteBasicADT2 :: Name -> Q [Dec]
makeGrisetteBasicADT2 name = do
  instances <- derive [name] basicClasses012
  ctors <- makeSmartCtor name
  return (instances ++ ctors)

-- | Make an ADT compatible with Grisette.
--
-- This will generate almost all useful instances for the ADT,
-- and smart constructors for each constructor.
--
-- This cannot be used for ADTs with existential type variables.
--
-- See 'derive', 'allClasses0', and 'makeSmartCtor' for more details.
makeGrisetteADT :: Name -> Q [Dec]
makeGrisetteADT name = do
  instances <- derive [name] allClasses0
  ctors <- makeSmartCtor name
  return (instances ++ ctors)

-- | Make an ADT compatible with Grisette.
--
-- This will generate almost all useful instances for the ADT,
-- and smart constructors for each constructor.
--
-- This cannot be used for ADTs with existential type variables.
--
-- See 'derive', 'allClasses01', and 'makeSmartCtor' for more details.
makeGrisetteADT1 :: Name -> Q [Dec]
makeGrisetteADT1 name = do
  instances <- derive [name] allClasses01
  ctors <- makeSmartCtor name
  return (instances ++ ctors)

-- | Make an ADT compatible with Grisette.
--
-- This will generate almost all useful instances for the ADT,
-- and smart constructors for each constructor.
--
-- This cannot be used for ADTs with existential type variables.
--
-- See 'derive', 'allClasses012', and 'makeSmartCtor' for more details.
makeGrisetteADT2 :: Name -> Q [Dec]
makeGrisetteADT2 name = do
  instances <- derive [name] allClasses012
  ctors <- makeSmartCtor name
  return (instances ++ ctors)
