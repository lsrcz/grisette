{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- Module      :   Grisette.IR.SymPrim.Data.Prim.PartialEval.GeneralFun
-- Copyright   :   (c) Sirui Lu 2021-2023
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.IR.SymPrim.Data.Prim.PartialEval.GeneralFun
  ( pevalGeneralFunApplyTerm,
  )
where

import Grisette.IR.SymPrim.Data.Prim.InternedTerm.InternedCtors
  ( generalFunApplyTerm,
  )
import Grisette.IR.SymPrim.Data.Prim.InternedTerm.Term
  ( SupportedPrim,
    Term (ConTerm, ITETerm),
    type (-->) (GeneralFun),
  )
import {-# SOURCE #-} Grisette.IR.SymPrim.Data.Prim.InternedTerm.TermSubstitution
  ( substTerm,
  )
import Grisette.IR.SymPrim.Data.Prim.PartialEval.Bool (pevalITETerm)
import Grisette.IR.SymPrim.Data.Prim.PartialEval.PartialEval
  ( totalize2,
  )

pevalGeneralFunApplyTerm :: (SupportedPrim a, SupportedPrim b) => Term (a --> b) -> Term a -> Term b
pevalGeneralFunApplyTerm = totalize2 doPevalGeneralFunApplyTerm generalFunApplyTerm

doPevalGeneralFunApplyTerm :: (SupportedPrim a, SupportedPrim b) => Term (a --> b) -> Term a -> Maybe (Term b)
doPevalGeneralFunApplyTerm (ConTerm _ (GeneralFun arg tm)) v = Just $ substTerm arg v tm
doPevalGeneralFunApplyTerm (ITETerm _ c l r) v =
  return $ pevalITETerm c (pevalGeneralFunApplyTerm l v) (pevalGeneralFunApplyTerm r v)
doPevalGeneralFunApplyTerm _ _ = Nothing
