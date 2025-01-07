{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- Module      :   Grisette.Internal.SymPrim.SymPrim
-- Copyright   :   (c) Sirui Lu 2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Internal.SymPrim.SymPrim (Prim, SymPrim, BasicSymPrim) where

import Control.DeepSeq (NFData)
import Data.Binary (Binary)
import Data.Bytes.Serial (Serial)
import Data.Hashable (Hashable)
import Data.Serialize (Serialize)
import Grisette.Internal.Core.Data.Class.EvalSym (EvalSym)
import Grisette.Internal.Core.Data.Class.ExtractSym (ExtractSym)
import Grisette.Internal.Core.Data.Class.Function (Apply (FunType))
import Grisette.Internal.Core.Data.Class.GenSym (GenSymSimple)
import Grisette.Internal.Core.Data.Class.ITEOp (ITEOp)
import Grisette.Internal.Core.Data.Class.Mergeable (Mergeable)
import Grisette.Internal.Core.Data.Class.SimpleMergeable (SimpleMergeable)
import Grisette.Internal.Core.Data.Class.Solvable (Solvable)
import Grisette.Internal.Core.Data.Class.SubstSym (SubstSym)
import Grisette.Internal.Core.Data.Class.ToCon (ToCon)
import Grisette.Internal.Core.Data.Class.ToSym (ToSym)
import Grisette.Internal.Internal.Decl.Core.Data.Class.PPrint (PPrint)
import Grisette.Internal.Internal.Decl.Core.Data.Class.SymEq (SymEq)
import Grisette.Internal.Internal.Decl.Core.Data.Class.SymOrd (SymOrd)
import Grisette.Internal.SymPrim.AllSyms (AllSyms)
import Grisette.Internal.SymPrim.Prim.Internal.Term
  ( ConRep (ConType),
    LinkedRep,
    SupportedNonFuncPrim,
  )
import Language.Haskell.TH.Syntax (Lift)
import Type.Reflection (Typeable)

-- | A type that is used as a constraint for all the primitive types (including
-- concrete primitives) in Grisette.
type Prim a =
  ( Show a,
    Binary a,
    Serial a,
    Serialize a,
    NFData a,
    Eq a,
    EvalSym a,
    ExtractSym a,
    Mergeable a,
    PPrint a,
    SubstSym a,
    SymEq a,
    SymOrd a,
    AllSyms a,
    Hashable a,
    Lift a,
    Typeable a
  )

-- | A type that is used as a constraint for all the symbolic primitive types
-- in Grisette.
type SymPrim a =
  ( Prim a,
    ITEOp a,
    GenSymSimple a a
  )

-- | A type that is used as a constraint for all the basic symbolic primitive
-- types in Grisette.
--
-- 'Grisette.SymPrim.SomeSymWordN' is not considered as a basic symbolic
-- primitive type.
type BasicSymPrim a =
  ( SymPrim a,
    SimpleMergeable a,
    GenSymSimple () a,
    Solvable (ConType a) a,
    ConRep a,
    LinkedRep (ConType a) a,
    ToCon a (ConType a),
    ToSym (ConType a) a,
    Apply a,
    a ~ FunType a,
    SupportedNonFuncPrim (ConType a)
  )
