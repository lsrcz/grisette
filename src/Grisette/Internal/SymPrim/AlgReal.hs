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
    RealPoint (..),
    AlgReal (..),
  )
where

import qualified Data.SBV as SBV
import qualified Data.SBV.Internals as SBV

-- | A univariate polynomial with integer coefficients.
--
-- For instance, @5x^3+2x-5@ is represented as
-- @'AlgRealPoly' [(5, 3), (2, 1), (-5, 0)]@.
newtype AlgRealPoly = AlgRealPoly [(Integer, Integer)]

-- | Boundary point for real intervals.
data RealPoint
  = -- | Open point.
    OpenPoint Rational
  | -- | Closed point.
    ClosedPoint Rational

toSBVRealPoint :: RealPoint -> SBV.RealPoint Rational
toSBVRealPoint (OpenPoint r) = SBV.OpenPoint r
toSBVRealPoint (ClosedPoint r) = SBV.ClosedPoint r

-- | Algebraic real numbers. The representation can be abstract for
-- roots-of-polynomials or intervals.
data AlgReal
  = -- | Exact rational number.
    AlgExactRational Rational
  | -- | Inexact rational numbers. SMT-solver return it with ? at the end.
    AlgInexactRational Rational
  | -- | Algebraic real number as a root of a polynomial.
    AlgPolyRoot
      -- | Which root is it?
      Integer
      -- | Polynomial defining equation.
      AlgRealPoly
      -- | Approximate decimal representation.
      (Maybe String)
  | -- | Interval with low and high bounds.
    AlgInterval
      -- | Lower bound.
      RealPoint
      -- | Upper bound.
      RealPoint

instance Show AlgReal where
  show (AlgExactRational r) = show $ SBV.AlgRational True r
  show (AlgInexactRational r) = show $ SBV.AlgRational False r
  show (AlgPolyRoot i (AlgRealPoly ps) approx) =
    show $ SBV.AlgPolyRoot (i, SBV.AlgRealPoly ps) approx
  show (AlgInterval l u) =
    show $ SBV.AlgInterval (toSBVRealPoint l) (toSBVRealPoint u)

op1 :: String -> (Rational -> Rational) -> AlgReal -> AlgReal
op1 _ f (AlgExactRational r) = AlgExactRational $ f r
op1 name _ r =
  error $
    "AlgReal."
      <> name
      <> ": only support exact algebraic rationals, but got: "
      ++ show r

op2 ::
  String ->
  (Rational -> Rational -> Rational) ->
  AlgReal ->
  AlgReal ->
  AlgReal
op2 _ f (AlgExactRational l) (AlgExactRational r) = AlgExactRational $ f l r
op2 name _ l r =
  error $
    "AlgReal."
      <> name
      <> ": only support exact algebraic rationals, but got: "
      ++ show l
      ++ " and "
      ++ show r

instance Eq AlgReal where
  (AlgExactRational l) == (AlgExactRational r) = l == r
  l == r =
    error $
      "AlgReal.==: only support comparing exact algebraic rationals, but got: "
        ++ show l
        ++ " and "
        ++ show r

instance Ord AlgReal where
  compare (AlgExactRational l) (AlgExactRational r) = compare l r
  compare l r =
    error $
      "AlgReal.compare: only support comparing exact algebraic rationals, but "
        ++ "got: "
        ++ show l
        ++ " and "
        ++ show r

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
    error $
      "AlgReal.toRational: only support exact algebraic rationals, but got: "
        ++ show r
