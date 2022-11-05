{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Grisette.IR.SymPrim.Data.Class.Substitute
  ( SubstituteSym,
    substituteSym,
  )
where

import Grisette.Core.Data.Class.Substitute
import Grisette.IR.SymPrim.Data.Prim.InternedTerm.Term
import Grisette.IR.SymPrim.Data.SymPrim

type SubstituteSym a = GSubstituteSym TypedSymbol Sym a

substituteSym :: (SubstituteSym a) => TypedSymbol b -> Sym b -> a -> a
substituteSym = gsubstituteSym
{-# INLINE substituteSym #-}
