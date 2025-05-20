{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      :   Grisette.Internal.SymPrim.SymAlgReal
-- Copyright   :   (c) Sirui Lu 2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Internal.SymPrim.SymAlgReal
  ( SymAlgReal (SymAlgReal),
    SymAlgRealKey,
  )
where

import Control.DeepSeq (NFData)
import qualified Data.Binary as Binary
import Data.Bytes.Serial (Serial (deserialize, serialize))
import Data.Hashable (Hashable (hashWithSalt))
import qualified Data.Serialize as Cereal
import Data.String (IsString (fromString))
import GHC.Generics (Generic)
import Grisette.Internal.Core.Data.Class.AsKey
  ( AsKey,
    KeyEq (keyEq),
    KeyHashable (keyHashWithSalt),
    shouldUseAsKeyHasSymbolicVersionError,
    shouldUseSymbolicVersionError,
  )
import Grisette.Internal.Core.Data.Class.Function (Apply (FunType, apply))
import Grisette.Internal.Core.Data.Class.Solvable
  ( Solvable (con, conView, ssym, sym),
  )
import Grisette.Internal.Internal.Decl.SymPrim.AllSyms
  ( AllSyms (allSymsS),
    SomeSym (SomeSym),
  )
import Grisette.Internal.SymPrim.AlgReal (AlgReal)
import Grisette.Internal.SymPrim.Prim.Internal.Term
  ( FloatingUnaryOp
      ( FloatingAcos,
        FloatingAsin,
        FloatingAtan,
        FloatingCos,
        FloatingCosh,
        FloatingExp,
        FloatingLog,
        FloatingSin,
        FloatingSinh,
        FloatingSqrt,
        FloatingTan,
        FloatingTanh
      ),
    PEvalFloatingTerm (pevalFloatingUnaryTerm, pevalPowerTerm),
    PEvalFractionalTerm (pevalFdivTerm, pevalRecipTerm),
    PEvalNumTerm
      ( pevalAbsNumTerm,
        pevalMulNumTerm,
        pevalNegNumTerm,
        pevalSignumNumTerm
      ),
    PEvalOrdTerm (pevalLeOrdTerm),
    SupportedPrim (pevalITETerm),
    pevalSubNumTerm,
    typedConstantSymbol,
  )
import Grisette.Internal.SymPrim.Prim.Term
  ( ConRep (ConType),
    LinkedRep (underlyingTerm, wrapTerm),
    PEvalNumTerm (pevalAddNumTerm),
    SymRep (SymType),
    Term,
    conTerm,
    pformatTerm,
    symTerm,
    pattern ConTerm,
  )
import Language.Haskell.TH.Syntax (Lift)

-- | Symbolic representation of algebraic real numbers.
newtype SymAlgReal = SymAlgReal {underlyingAlgRealTerm :: Term AlgReal}
  deriving (Lift, Generic)
  deriving anyclass (NFData)

-- | t'SymAlgReal' type with identity equality.
type SymAlgRealKey = AsKey SymAlgReal

instance ConRep SymAlgReal where
  type ConType SymAlgReal = AlgReal

instance SymRep AlgReal where
  type SymType AlgReal = SymAlgReal

instance LinkedRep AlgReal SymAlgReal where
  underlyingTerm = underlyingAlgRealTerm
  wrapTerm = SymAlgReal

instance Apply SymAlgReal where
  type FunType SymAlgReal = SymAlgReal
  apply = id

-- | This will crash the program.
--
-- 'SymAlgReal' cannot be compared concretely.
--
-- If you want to use the type as keys in hash maps based on term equality, say
-- memo table, you should use @'AsKey' 'SymAlgReal'@ instead.
--
-- If you want symbolic version of the equality operator, use
-- t'Grisette.Core.SymEq' instead.
instance Eq SymAlgReal where
  (==) = shouldUseAsKeyHasSymbolicVersionError "SymAlgReal" "(==)" "(.==)"

instance KeyEq SymAlgReal where
  keyEq (SymAlgReal l) (SymAlgReal r) = l == r

-- | This will crash the program.
--
-- 'SymAlgReal' cannot be compared concretely.
--
-- If you want symbolic version of the comparison operators, use
-- t'Grisette.Core.SymOrd' instead.
instance Ord SymAlgReal where
  (<) = shouldUseSymbolicVersionError "SymAlgReal" "(<)" "(.<)"
  (<=) = shouldUseSymbolicVersionError "SymAlgReal" "(<=)" "(.<=)"
  (>=) = shouldUseSymbolicVersionError "SymAlgReal" "(>=)" "(.>=)"
  (>) = shouldUseSymbolicVersionError "SymAlgReal" "(>)" "(.>)"
  max (SymAlgReal l) (SymAlgReal r) =
    SymAlgReal $ pevalITETerm (pevalLeOrdTerm l r) r l
  min (SymAlgReal l) (SymAlgReal r) =
    SymAlgReal $ pevalITETerm (pevalLeOrdTerm l r) l r
  compare = shouldUseSymbolicVersionError "SymAlgReal" "compare" "symCompare"

instance Real SymAlgReal where
  toRational = error "toRational: toRational isn't supported for SymAlgReal"

instance KeyHashable SymAlgReal where
  keyHashWithSalt s (SymAlgReal v) = s `hashWithSalt` v

instance IsString SymAlgReal where
  fromString = ssym . fromString

instance Solvable AlgReal SymAlgReal where
  con = SymAlgReal . conTerm
  sym = SymAlgReal . symTerm . typedConstantSymbol
  conView (SymAlgReal (ConTerm t)) = Just t
  conView _ = Nothing

instance Show SymAlgReal where
  show (SymAlgReal t) = pformatTerm t

instance AllSyms SymAlgReal where
  allSymsS v = (SomeSym v :)

instance Num SymAlgReal where
  (SymAlgReal l) + (SymAlgReal r) = SymAlgReal $ pevalAddNumTerm l r
  (SymAlgReal l) - (SymAlgReal r) = SymAlgReal $ pevalSubNumTerm l r
  (SymAlgReal l) * (SymAlgReal r) = SymAlgReal $ pevalMulNumTerm l r
  negate (SymAlgReal v) = SymAlgReal $ pevalNegNumTerm v
  abs (SymAlgReal v) = SymAlgReal $ pevalAbsNumTerm v
  signum (SymAlgReal v) = SymAlgReal $ pevalSignumNumTerm v
  fromInteger = con . fromInteger

-- | The function is total and will not throw errors. The result is considered
-- undefined if the divisor is 0.
--
-- It is the responsibility of the caller to ensure that the divisor is not
-- zero with the symbolic constraints, or use the t'Grisette.Core.FdivOr' or
-- t'Grisette.Core.SafeFdiv' classes.
instance Fractional SymAlgReal where
  fromRational = con . fromRational
  (SymAlgReal l) / (SymAlgReal r) = SymAlgReal $ pevalFdivTerm l r
  recip (SymAlgReal l) = SymAlgReal $ pevalRecipTerm l

-- | The functions are total and will not throw errors. The result for 'logBase'
-- is considered undefined if the base is 1.
--
-- It is the responsibility of the caller to ensure that the base is not 1
-- with the symbolic constraints, or use the t'Grisette.Core.LogBaseOr' or
-- t'Grisette.Core.SafeLogBase' classes.
instance Floating SymAlgReal where
  pi = fromRational $ toRational pi
  exp (SymAlgReal v) = SymAlgReal $ pevalFloatingUnaryTerm FloatingExp v
  log (SymAlgReal v) = SymAlgReal $ pevalFloatingUnaryTerm FloatingLog v
  sqrt (SymAlgReal v) = SymAlgReal $ pevalFloatingUnaryTerm FloatingSqrt v
  sin (SymAlgReal v) = SymAlgReal $ pevalFloatingUnaryTerm FloatingSin v
  cos (SymAlgReal v) = SymAlgReal $ pevalFloatingUnaryTerm FloatingCos v
  tan (SymAlgReal v) = SymAlgReal $ pevalFloatingUnaryTerm FloatingTan v
  sinh (SymAlgReal v) = SymAlgReal $ pevalFloatingUnaryTerm FloatingSinh v
  cosh (SymAlgReal v) = SymAlgReal $ pevalFloatingUnaryTerm FloatingCosh v
  tanh (SymAlgReal v) = SymAlgReal $ pevalFloatingUnaryTerm FloatingTanh v
  asin (SymAlgReal v) = SymAlgReal $ pevalFloatingUnaryTerm FloatingAsin v
  acos (SymAlgReal v) = SymAlgReal $ pevalFloatingUnaryTerm FloatingAcos v
  atan (SymAlgReal v) = SymAlgReal $ pevalFloatingUnaryTerm FloatingAtan v
  asinh = error "asinh isn't supported by the underlying sbv library"
  acosh = error "acosh isn't supported by the underlying sbv library"
  atanh = error "atanh isn't supported by the underlying sbv library"
  SymAlgReal l ** SymAlgReal r = SymAlgReal $ pevalPowerTerm l r
  logBase (SymAlgReal baset) (SymAlgReal at) =
    SymAlgReal $
      pevalFdivTerm
        (pevalFloatingUnaryTerm FloatingLog at)
        (pevalFloatingUnaryTerm FloatingLog baset)

instance Serial SymAlgReal where
  serialize = serialize . underlyingAlgRealTerm
  deserialize = SymAlgReal <$> deserialize

instance Cereal.Serialize SymAlgReal where
  put = serialize
  get = deserialize

instance Binary.Binary SymAlgReal where
  put = serialize
  get = deserialize
