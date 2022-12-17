{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

-- |
-- Module      :   Grisette.IR.SymPrim.Data.Class.Substitute
-- Copyright   :   (c) Sirui Lu 2021-2022
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.IR.SymPrim.Data.Class.Substitute
  ( SubstituteSym,
    substituteSym,
  )
where

import Grisette.Core.Data.Class.Substitute
import Grisette.IR.SymPrim.Data.Prim.InternedTerm.Term
import Grisette.IR.SymPrim.Data.SymPrim

-- | 'GSubstituteSym' specialized with 'TypedSymbol' and 'Sym'
type SubstituteSym a = GSubstituteSym TypedSymbol Sym a

-- | 'gsubstituteSym' specialized with 'TypedSymbol' and 'Sym'
substituteSym :: (SubstituteSym a) => TypedSymbol b -> Sym b -> a -> a
substituteSym = gsubstituteSym
{-# INLINE substituteSym #-}
