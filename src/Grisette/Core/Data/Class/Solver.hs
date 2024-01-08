{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      :   Grisette.Core.Data.Class.Solver
-- Copyright   :   (c) Sirui Lu 2021-2023
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Core.Data.Class.Solver
  ( -- * Note for the examples

    --

    -- | The examples assumes a [z3](https://github.com/Z3Prover/z3) solver available in @PATH@.

    -- * Union with exceptions
    UnionWithExcept (..),

    -- * Solver interfaces
    MonadicIncrementalSolver (..),
    Solver (..),
    solveExcept,
    solveMultiExcept,
  )
where

import Control.DeepSeq (NFData)
import Control.Monad.Except (ExceptT, runExceptT)
import Data.Hashable (Hashable)
import GHC.Generics (Generic)
import Grisette.Core.Data.Class.SimpleMergeable
  ( UnionPrjOp,
    simpleMerge,
  )
import Grisette.IR.SymPrim.Data.Prim.Model (Model)
import Grisette.IR.SymPrim.Data.SymPrim (SymBool)
import Language.Haskell.TH.Syntax (Lift)

data SolveInternal = SolveInternal
  deriving (Eq, Show, Ord, Generic, Hashable, Lift, NFData)

-- $setup
-- >>> import Grisette.Core
-- >>> import Grisette.IR.SymPrim
-- >>> import Grisette.Backend.SBV
-- >>> :set -XOverloadedStrings

class MonadicIncrementalSolver m failure | m -> failure where
  solveNext :: SymBool -> m (Either failure Model)

-- | A solver interface.
class
  Solver config failure
    | config -> failure
  where
  -- | Solve a single formula. Find an assignment to it to make it true.
  --
  -- >>> solve (precise z3) ("a" .&& ("b" :: SymInteger) .== 1)
  -- Right (Model {a -> True :: Bool, b -> 1 :: Integer})
  -- >>> solve (precise z3) ("a" .&& symNot "a")
  -- Left Unsat
  solve ::
    -- | solver configuration
    config ->
    -- | formula to solve, the solver will try to make it true
    SymBool ->
    IO (Either failure Model)

  -- | Solve a single formula while returning multiple models to make it true.
  -- The maximum number of desired models are given.
  --
  -- > >>> solveMulti (precise z3) 4 ("a" .|| "b")
  -- > [Model {a -> True :: Bool, b -> False :: Bool},Model {a -> False :: Bool, b -> True :: Bool},Model {a -> True :: Bool, b -> True :: Bool}]
  solveMulti ::
    -- | solver configuration
    config ->
    -- | maximum number of models to return
    Int ->
    -- | formula to solve, the solver will try to make it true
    SymBool ->
    IO ([Model], failure)

  -- | Solve a single formula while returning multiple models to make it true.
  -- All models are returned.
  --
  -- > >>> solveAll (precise z3) ("a" .|| "b")
  -- > [Model {a -> True :: Bool, b -> False :: Bool},Model {a -> False :: Bool, b -> True :: Bool},Model {a -> True :: Bool, b -> True :: Bool}]
  solveAll ::
    -- | solver configuration
    config ->
    -- | formula to solve, the solver will try to make it true
    SymBool ->
    IO [Model]

-- | A class that abstracts the union-like structures that contains exceptions.
class UnionWithExcept t u e v | t -> u e v where
  -- | Extract a union of exceptions and values from the structure.
  extractUnionExcept :: t -> u (Either e v)

instance UnionWithExcept (ExceptT e u v) u e v where
  extractUnionExcept = runExceptT

-- |
-- Solver procedure for programs with error handling.
--
-- >>> :set -XLambdaCase
-- >>> import Control.Monad.Except
-- >>> let x = "x" :: SymInteger
-- >>> :{
--   res :: ExceptT AssertionError UnionM ()
--   res = do
--     symAssert $ x .> 0       -- constrain that x is positive
--     symAssert $ x .< 2       -- constrain that x is less than 2
-- :}
--
-- >>> :{
--   translate (Left _) = con False -- errors are not desirable
--   translate _ = con True         -- non-errors are desirable
-- :}
--
-- >>> solveExcept (precise z3) translate res
-- Right (Model {x -> 1 :: Integer})
solveExcept ::
  ( UnionWithExcept t u e v,
    UnionPrjOp u,
    Functor u,
    Solver config failure
  ) =>
  -- | solver configuration
  config ->
  -- | mapping the results to symbolic boolean formulas, the solver would try to find a model to make the formula true
  (Either e v -> SymBool) ->
  -- | the program to be solved, should be a union of exception and values
  t ->
  IO (Either failure Model)
solveExcept config f v = solve config (simpleMerge $ f <$> extractUnionExcept v)

-- |
-- Solver procedure for programs with error handling. Would return multiple
-- models if possible.
solveMultiExcept ::
  ( UnionWithExcept t u e v,
    UnionPrjOp u,
    Functor u,
    Solver config failure
  ) =>
  -- | solver configuration
  config ->
  -- | maximum number of models to return
  Int ->
  -- | mapping the results to symbolic boolean formulas, the solver would try to find a model to make the formula true
  (Either e v -> SymBool) ->
  -- | the program to be solved, should be a union of exception and values
  t ->
  IO ([Model], failure)
solveMultiExcept config n f v = solveMulti config n (simpleMerge $ f <$> extractUnionExcept v)
