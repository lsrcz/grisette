{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      :   Grisette.Internal.SymPrim.AlgReal
-- Copyright   :   (c) Sirui Lu 2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Internal.SymPrim.AlgReal
  ( AlgRealPoly (..),
    UnsupportedAlgRealOperation (..),
    toSBVAlgReal,
    fromSBVAlgReal,
    RealPoint (..),
    AlgReal (..),
  )
where

import Control.DeepSeq (NFData)
import Control.Exception (Exception, throw)
import Data.Hashable (Hashable)
import qualified Data.SBV as SBV
import qualified Data.SBV.Internals as SBV
import Data.Serialize (Serialize)
import GHC.Generics (Generic)
import Grisette.Internal.Core.Data.Class.Function (Apply (FunType, apply))
import Language.Haskell.TH.Syntax (Lift)
import Test.QuickCheck (Arbitrary)
import Test.QuickCheck.Arbitrary (Arbitrary (arbitrary))

-- | A univariate polynomial with integer coefficients.
--
-- For instance, @5x^3+2x-5@ is represented as
-- @v'AlgRealPoly' [(5, 3), (2, 1), (-5, 0)]@.
newtype AlgRealPoly = AlgRealPoly [(Integer, Integer)]
  deriving (Eq, Generic, Lift)
  deriving newtype (Hashable, NFData, Serialize)

-- | Boundary point for real intervals.
data RealPoint
  = -- | Open point.
    OpenPoint Rational
  | -- | Closed point.
    ClosedPoint Rational
  deriving (Eq, Generic, Lift)
  deriving anyclass (Hashable, NFData, Serialize)

toSBVRealPoint :: RealPoint -> SBV.RealPoint Rational
toSBVRealPoint (OpenPoint r) = SBV.OpenPoint r
toSBVRealPoint (ClosedPoint r) = SBV.ClosedPoint r

fromSBVRealPoint :: SBV.RealPoint Rational -> RealPoint
fromSBVRealPoint (SBV.OpenPoint r) = OpenPoint r
fromSBVRealPoint (SBV.ClosedPoint r) = ClosedPoint r

-- | Algebraic real numbers. The representation can be abstract for
-- roots-of-polynomials or intervals.
data AlgReal where
  -- | Exact rational number.
  AlgExactRational :: Rational -> AlgReal
  -- | Inexact rational numbers. SMT-solver return it with ? at the end.
  AlgInexactRational :: Rational -> AlgReal
  -- | Algebraic real number as a root of a polynomial.
  AlgPolyRoot ::
    -- | Which root is it?
    Integer ->
    -- | Polynomial defining equation.
    AlgRealPoly ->
    -- | Approximate decimal representation.
    Maybe String ->
    AlgReal
  -- | Interval with low and high bounds.
  AlgInterval ::
    -- | Lower bound.
    RealPoint ->
    -- | Upper bound.
    RealPoint ->
    AlgReal
  deriving (Generic, Lift)
  deriving anyclass (Hashable, NFData, Serialize)

-- | Convert algebraic real numbers to SBV's algebraic real numbers.
toSBVAlgReal :: AlgReal -> SBV.AlgReal
toSBVAlgReal (AlgExactRational r) = SBV.AlgRational True r
toSBVAlgReal (AlgInexactRational r) = SBV.AlgRational False r
toSBVAlgReal (AlgPolyRoot i (AlgRealPoly ps) approx) =
  SBV.AlgPolyRoot (i, SBV.AlgRealPoly ps) approx
toSBVAlgReal (AlgInterval l u) =
  SBV.AlgInterval (toSBVRealPoint l) (toSBVRealPoint u)

-- | Convert SBV's algebraic real numbers to algebraic real numbers.
fromSBVAlgReal :: SBV.AlgReal -> AlgReal
fromSBVAlgReal (SBV.AlgRational True r) = AlgExactRational r
fromSBVAlgReal (SBV.AlgRational False r) = AlgInexactRational r
fromSBVAlgReal (SBV.AlgPolyRoot (i, SBV.AlgRealPoly ps) approx) =
  AlgPolyRoot i (AlgRealPoly ps) approx
fromSBVAlgReal (SBV.AlgInterval l u) =
  AlgInterval (fromSBVRealPoint l) (fromSBVRealPoint u)

instance Show AlgReal where
  show r = show $ toSBVAlgReal r

-- | Exception for unsupported operations on algebraic real numbers.
--
-- We only support operations on exact rationals.
data UnsupportedAlgRealOperation = UnsupportedAlgRealOperation
  { op :: String,
    msg :: String
  }
  deriving anyclass (Exception)

instance Show UnsupportedAlgRealOperation where
  show (UnsupportedAlgRealOperation op msg) =
    "AlgReal."
      ++ op
      ++ ": unsupported operation on algebraic rationals, only support exact "
      ++ "rationals"
      ++ ": "
      ++ msg

op1 :: String -> (Rational -> Rational) -> AlgReal -> AlgReal
op1 _ f (AlgExactRational r) = AlgExactRational $ f r
op1 name _ r =
  throw
    UnsupportedAlgRealOperation {op = name, msg = show r}

op2 ::
  String ->
  (Rational -> Rational -> Rational) ->
  AlgReal ->
  AlgReal ->
  AlgReal
op2 _ f (AlgExactRational l) (AlgExactRational r) = AlgExactRational $ f l r
op2 name _ l r =
  throw
    UnsupportedAlgRealOperation
      { op = name,
        msg = show l <> " and " <> show r
      }

instance Eq AlgReal where
  (AlgExactRational l) == (AlgExactRational r) = l == r
  l == r =
    throw $
      UnsupportedAlgRealOperation "==" $
        show l <> " and " <> show r

instance Ord AlgReal where
  compare (AlgExactRational l) (AlgExactRational r) = compare l r
  compare l r =
    throw $
      UnsupportedAlgRealOperation "compare" $
        show l <> " and " <> show r

instance Num AlgReal where
  (+) = op2 "+" (+)
  (*) = op2 "*" (*)
  (-) = op2 "-" (-)
  negate = op1 "negate" negate
  abs = op1 "abs" abs
  signum = op1 "signum" signum
  fromInteger = AlgExactRational . fromInteger

-- | Unlike sbv, we throw the error when divided by zero happens
instance Fractional AlgReal where
  (/) = op2 "/" (/)
  fromRational = AlgExactRational

instance Real AlgReal where
  toRational (AlgExactRational r) = r
  toRational r =
    throw $
      UnsupportedAlgRealOperation "toRational" $
        show r

instance Arbitrary AlgReal where
  arbitrary = AlgExactRational <$> arbitrary

instance Apply AlgReal where
  type FunType AlgReal = AlgReal
  apply = id
