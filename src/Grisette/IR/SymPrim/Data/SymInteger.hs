{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      :   Grisette.IR.SymPrim.Data.SymInteger
-- Copyright   :   (c) Sirui Lu 2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.IR.SymPrim.Data.SymInteger (SymInteger (SymInteger)) where

import Control.DeepSeq (NFData)
import Data.Hashable (Hashable (hashWithSalt))
import Data.String (IsString (fromString))
import GHC.Generics (Generic)
import Grisette.Core.Data.Class.Function (Apply (FunType, apply))
import Grisette.Core.Data.Class.Solvable (Solvable (con, conView, ssym, sym))
import Grisette.IR.SymPrim.Data.AllSyms (AllSyms (allSymsS), SomeSym (SomeSym))
import Grisette.IR.SymPrim.Data.Prim.Term
  ( ConRep (ConType),
    LinkedRep (underlyingTerm, wrapTerm),
    PEvalNumTerm
      ( pevalAbsNumTerm,
        pevalAddNumTerm,
        pevalMulNumTerm,
        pevalNegNumTerm,
        pevalSignumNumTerm
      ),
    SymRep (SymType),
    Term (ConTerm),
    conTerm,
    pevalSubNumTerm,
    pformat,
    symTerm,
  )
import Language.Haskell.TH.Syntax (Lift)

-- $setup
-- >>> import Grisette.Core
-- >>> import Grisette.IR.SymPrim
-- >>> import Grisette.Backend.SBV
-- >>> import Data.Proxy

-- | Symbolic (unbounded, mathematical) integer type.
--
-- >>> "a" + 1 :: SymInteger
-- (+ 1 a)
--
-- More symbolic operations are available. Please refer to the documentation
-- for the type class instances.
newtype SymInteger = SymInteger {underlyingIntegerTerm :: Term Integer}
  deriving (Lift, NFData, Generic)

instance ConRep SymInteger where
  type ConType SymInteger = Integer

instance SymRep Integer where
  type SymType Integer = SymInteger

instance LinkedRep Integer SymInteger where
  underlyingTerm (SymInteger a) = a
  wrapTerm = SymInteger

instance Apply SymInteger where
  type FunType SymInteger = SymInteger
  apply = id

instance Num SymInteger where
  (SymInteger l) + (SymInteger r) = SymInteger $ pevalAddNumTerm l r
  (SymInteger l) - (SymInteger r) = SymInteger $ pevalSubNumTerm l r
  (SymInteger l) * (SymInteger r) = SymInteger $ pevalMulNumTerm l r
  negate (SymInteger v) = SymInteger $ pevalNegNumTerm v
  abs (SymInteger v) = SymInteger $ pevalAbsNumTerm v
  signum (SymInteger v) = SymInteger $ pevalSignumNumTerm v
  fromInteger = con

instance Eq SymInteger where
  SymInteger l == SymInteger r = l == r

instance Hashable SymInteger where
  hashWithSalt s (SymInteger v) = s `hashWithSalt` v

instance Solvable Integer SymInteger where
  con = SymInteger . conTerm
  sym = SymInteger . symTerm
  conView (SymInteger (ConTerm _ t)) = Just t
  conView _ = Nothing

instance IsString SymInteger where
  fromString = ssym . fromString

instance Show SymInteger where
  show (SymInteger t) = pformat t

instance AllSyms SymInteger where
  allSymsS v = (SomeSym v :)
