{-# LANGUAGE ScopedTypeVariables #-}
{-# HLINT ignore "Eta reduce" #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Grisette.Internal.SymPrim.Prim.Internal.Instances.PEvalFloatingTerm () where

import Grisette.Internal.SymPrim.FP (FP, ValidFP)
import Grisette.Internal.SymPrim.Prim.Internal.Instances.SupportedPrim ()
import Grisette.Internal.SymPrim.Prim.Internal.Term
  ( PEvalFloatingTerm
      ( pevalSqrtTerm,
        withSbvFloatingTermConstraint
      ),
    SupportedPrim (withPrim),
    sqrtTerm,
  )
import Grisette.Internal.SymPrim.Prim.Internal.Unfold
  ( generalUnaryUnfolded,
  )

instance (ValidFP eb sb) => PEvalFloatingTerm (FP eb sb) where
  pevalSqrtTerm = generalUnaryUnfolded sqrt sqrtTerm
  withSbvFloatingTermConstraint p r = withPrim @(FP eb sb) p r
