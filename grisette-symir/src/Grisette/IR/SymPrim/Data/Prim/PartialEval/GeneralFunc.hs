{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- Module      :   Grisette.IR.SymPrim.Data.Prim.PartialEval.GeneralFunc
-- Copyright   :   (c) Sirui Lu 2021-2022
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.IR.SymPrim.Data.Prim.PartialEval.GeneralFunc (pevalGeneralFuncApplyTerm) where

import Grisette.Core.Data.Class.Function
import Grisette.IR.SymPrim.Data.Prim.InternedTerm.InternedCtors
import Grisette.IR.SymPrim.Data.Prim.InternedTerm.Term
import {-# SOURCE #-} Grisette.IR.SymPrim.Data.Prim.InternedTerm.TermSubstitution
import Grisette.IR.SymPrim.Data.Prim.PartialEval.PartialEval

pevalGeneralFuncApplyTerm :: (SupportedPrim a, SupportedPrim b) => Term (a --> b) -> Term a -> Term b
pevalGeneralFuncApplyTerm = totalize2 doPevalGeneralFuncApplyTerm generalFuncApplyTerm

doPevalGeneralFuncApplyTerm :: (SupportedPrim a, SupportedPrim b) => Term (a --> b) -> Term a -> Maybe (Term b)
doPevalGeneralFuncApplyTerm (ConTerm _ (GeneralFunc arg tm)) v = Just $ substTerm arg v tm
doPevalGeneralFuncApplyTerm _ _ = Nothing
