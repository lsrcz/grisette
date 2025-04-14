{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      :   Grisette.Internal.SymPrim.SymInteger
-- Copyright   :   (c) Sirui Lu 2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Internal.SymPrim.SymInteger (SymInteger (SymInteger)) where

import Control.DeepSeq (NFData)
import qualified Data.Binary as Binary
import Data.Bytes.Serial (Serial (deserialize, serialize))
import Data.Hashable (Hashable (hashWithSalt))
import qualified Data.Serialize as Cereal
import Data.String (IsString (fromString))
import GHC.Generics (Generic)
import Grisette.Internal.Core.Data.Class.Function (Apply (FunType, apply))
import Grisette.Internal.Core.Data.Class.Solvable
  ( Solvable (con, conView, ssym, sym),
  )
import Grisette.Internal.Internal.Decl.SymPrim.AllSyms
  ( AllSyms (allSymsS),
    SomeSym (SomeSym),
  )
import Grisette.Internal.SymPrim.Prim.Term
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
    Term,
    conTerm,
    pevalDivIntegralTerm,
    pevalITETerm,
    pevalLeOrdTerm,
    pevalModIntegralTerm,
    pevalQuotIntegralTerm,
    pevalRemIntegralTerm,
    pevalSubNumTerm,
    pformatTerm,
    symTerm,
    typedConstantSymbol,
    pattern ConTerm,
  )
import Language.Haskell.TH.Syntax (Lift)

-- $setup
-- >>> import Grisette.Core
-- >>> import Grisette.SymPrim
-- >>> import Grisette.Backend
-- >>> import Data.Proxy

-- | Symbolic (unbounded, mathematical) integer type.
--
-- >>> "a" + 1 :: SymInteger
-- (+ 1 a)
--
-- More operations are available. Please refer to "Grisette.Core#g:symops" for
-- more information.
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

{-# NOINLINE [1] enumDeltaSymInteger #-}
enumDeltaSymInteger :: SymInteger -> SymInteger -> [SymInteger]
enumDeltaSymInteger x d = x `seq` (x : enumDeltaSymInteger (d + x) d)

instance Enum SymInteger where
  succ x = x + 1
  pred x = x - 1
  toEnum = fromIntegral
  fromEnum = error "fromEnum: fromEnum isn't supported for SymInteger"
  enumFrom x = enumDeltaSymInteger x 1
  {-# INLINE enumFrom #-}
  enumFromThen x y = enumDeltaSymInteger x (y - x)
  {-# INLINE enumFromThen #-}
  enumFromTo = error "enumFromTo: enumFromTo isn't supported for SymInteger"
  enumFromThenTo =
    error "enumFromThenTo: enumFromThenTo isn't supported for SymInteger"

instance Ord SymInteger where
  (<) = error "Ord: < isn't supported for SymInteger. Consider using the symbolic comparison operators (.<)."
  (<=) = error "Ord: <= isn't supported for SymInteger. Consider using the symbolic comparison operators (.<=)."
  (>=) = error "Ord: >= isn't supported for SymInteger. Consider using the symbolic comparison operators (.>=)."
  (>) = error "Ord: > isn't supported for SymInteger. Consider using the symbolic comparison operators (.>)."
  max (SymInteger l) (SymInteger r) =
    SymInteger $ pevalITETerm (pevalLeOrdTerm l r) r l
  min (SymInteger l) (SymInteger r) =
    SymInteger $ pevalITETerm (pevalLeOrdTerm l r) l r
  compare _ _ =
    error "compare: compare isn't supported for SymInteger. Consider using the symbolic comparison operators (symCompare)."

instance Real SymInteger where
  toRational _ = error "toRational: toRational isn't supported for SymInteger"

-- | The functions are total and will not throw errors. The result is considered
-- undefined if the divisor is 0.
--
-- It is the responsibility of the caller to ensure that the divisor is not
-- zero with the symbolic constraints, or use the t'Grisette.Core.DivOr' or
-- t'Grisette.Core.SafeDiv' classes.
instance Integral SymInteger where
  toInteger = error "toInteger: toInteger isn't supported for SymInteger"
  div (SymInteger l) (SymInteger r) = SymInteger $ pevalDivIntegralTerm l r
  mod (SymInteger l) (SymInteger r) = SymInteger $ pevalModIntegralTerm l r
  quot (SymInteger l) (SymInteger r) = SymInteger $ pevalQuotIntegralTerm l r
  rem (SymInteger l) (SymInteger r) = SymInteger $ pevalRemIntegralTerm l r
  divMod (SymInteger l) (SymInteger r) =
    (SymInteger $ pevalDivIntegralTerm l r, SymInteger $ pevalModIntegralTerm l r)
  quotRem (SymInteger l) (SymInteger r) =
    (SymInteger $ pevalQuotIntegralTerm l r, SymInteger $ pevalRemIntegralTerm l r)

-- | Checks if two formulas are the same. Not building the actual symbolic
-- equality formula.
--
-- The reason why we choose this behavior is to allow symbolic variables to be
-- used as keys in hash maps, which can be useful for memoization.
--
-- Use with caution. Usually you should use t'Grisette.Core.SymEq' instead.
instance Eq SymInteger where
  SymInteger l == SymInteger r = l == r

instance Hashable SymInteger where
  hashWithSalt s (SymInteger v) = s `hashWithSalt` v

instance Solvable Integer SymInteger where
  con = SymInteger . conTerm
  sym = SymInteger . symTerm . typedConstantSymbol
  conView (SymInteger (ConTerm t)) = Just t
  conView _ = Nothing

instance IsString SymInteger where
  fromString = ssym . fromString

instance Show SymInteger where
  show (SymInteger t) = pformatTerm t

instance AllSyms SymInteger where
  allSymsS v = (SomeSym v :)

instance Serial SymInteger where
  serialize = serialize . underlyingIntegerTerm
  deserialize = SymInteger <$> deserialize

instance Cereal.Serialize SymInteger where
  put = serialize
  get = deserialize

instance Binary.Binary SymInteger where
  put = serialize
  get = deserialize
