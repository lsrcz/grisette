{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Eta reduce" #-}

module Grisette.Internal.SymPrim.Prim.Internal.Instances.PEvalFractionalTerm () where

import Grisette.Internal.SymPrim.FP (FP, ValidFP)
import Grisette.Internal.SymPrim.Prim.Internal.Instances.SupportedPrim ()
import Grisette.Internal.SymPrim.Prim.Internal.Term
  ( PEvalFractionalTerm (pevalFdivTerm, pevalRecipTerm, withSbvFractionalTermConstraint),
    SupportedPrim (withPrim),
    fdivTerm,
    recipTerm,
  )
import Grisette.Internal.SymPrim.Prim.Internal.Unfold
  ( generalBinaryUnfolded,
    generalUnaryUnfolded,
  )

instance (ValidFP eb sb) => PEvalFractionalTerm (FP eb sb) where
  pevalFdivTerm = generalBinaryUnfolded (/) fdivTerm
  pevalRecipTerm = generalUnaryUnfolded recip recipTerm
  withSbvFractionalTermConstraint p r = withPrim @(FP eb sb) p r
