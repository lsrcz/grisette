-- |
-- Module      :   Grisette.Internal.Unified.Theories
-- Copyright   :   (c) Sirui Lu 2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Internal.Unified.Theories (TheoryToUnify (..), isUFun) where

-- | This data type is used to represent the theories that is unified.
--
-- The 'UFun' constructor is used to represent a specific uninterpreted function
-- type. The type is uncurried.
data TheoryToUnify
  = UBool
  | UIntN
  | UWordN
  | UInteger
  | UAlgReal
  | UFP
  | UFun [TheoryToUnify]
  deriving (Eq, Show)

-- | Check if the theory is a uninterpreted function.
isUFun :: TheoryToUnify -> Bool
isUFun (UFun _) = True
isUFun _ = False
