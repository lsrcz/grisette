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
-- Copyright   :   (c) Sirui Lu 2021-2022
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Core.Data.Class.Solver
  ( -- * Note for the examples

    --

    -- | This module does not contain the implementation for solvable (see "Grisette.Core#solvable")
    -- types, and the examples in this module rely on the implementations in
    -- the [grisette-symir](https://hackage.haskell.org/package/grisette-symir/)
    -- and [grisette-backend-sbv](https://hackage.haskell.org/package/grisette-backend-sbv) packages,
    -- which provides the solvable type and the solver backend implementations,
    -- respectively.
    --
    -- The examples also assumes a [z3](https://github.com/Z3Prover/z3) solver available in @PATH@.

    -- * Union with exceptions
    UnionWithExcept (..),

    -- * Solver interfaces
    Solver (..),
    solveExcept,
    solveMultiExcept,
  )
where

import Control.DeepSeq
import Control.Monad.Except
import Data.Hashable
import Generics.Deriving
import Grisette.Core.Control.Exception
import Grisette.Core.Data.Class.Bool
import Grisette.Core.Data.Class.Evaluate
import Grisette.Core.Data.Class.ExtractSymbolics
import Grisette.Core.Data.Class.SimpleMergeable
import Grisette.Core.Data.Class.Solvable
import Language.Haskell.TH.Syntax

data SolveInternal = SolveInternal deriving (Eq, Show, Ord, Generic, Hashable, Lift, NFData)

-- $setup
-- >>> import Grisette.Core
-- >>> import Grisette.IR.SymPrim
-- >>> import Grisette.Backend.SBV
-- >>> :set -XOverloadedStrings

-- | A solver interface.
class
  (SymBoolOp bool, GEvaluateSym model bool) =>
  Solver config bool failure model
    | config -> bool failure model
  where
  -- | Solve a single formula. Find an assignment to it to make it true.
  --
  -- >>> solve (UnboundedReasoning z3) ("a" &&~ ("b" :: SymInteger) ==~ 1)
  -- Right (Model {a -> True :: Bool, b -> 1 :: Integer})
  -- >>> solve (UnboundedReasoning z3) ("a" &&~ nots "a")
  -- Left Unsat
  solve ::
    -- | solver configuration
    config ->
    -- | formula to solve, the solver will try to make it true
    bool ->
    IO (Either failure model)

  -- | Solve a single formula while returning multiple models to make it true.
  -- The maximum number of desired models are given.
  --
  -- > >>> solveMulti (UnboundedReasoning z3) 4 ("a" ||~ "b")
  -- > [Model {a -> True :: Bool, b -> False :: Bool},Model {a -> False :: Bool, b -> True :: Bool},Model {a -> True :: Bool, b -> True :: Bool}]
  solveMulti ::
    -- | solver configuration
    config ->
    -- | maximum number of models to return
    Int ->
    -- | formula to solve, the solver will try to make it true
    bool ->
    IO [model]

  -- | Solve a single formula while returning multiple models to make it true.
  -- All models are returned.
  --
  -- > >>> solveAll (UnboundedReasoning z3) ("a" ||~ "b")
  -- > [Model {a -> True :: Bool, b -> False :: Bool},Model {a -> False :: Bool, b -> True :: Bool},Model {a -> True :: Bool, b -> True :: Bool}]
  solveAll ::
    -- | solver configuration
    config ->
    -- | formula to solve, the solver will try to make it true
    bool ->
    IO [model]

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
--     symAssert $ x >~ 0       -- constrain that x is positive
--     symAssert $ x <~ 2       -- constrain that x is less than 2
-- :}
--
-- >>> :{
--   translate (Left _) = conc False -- errors are not desirable
--   translate _ = conc True         -- non-errors are desirable
-- :}
--
-- >>> solveExcept (UnboundedReasoning z3) translate res
-- Right (Model {x -> 1 :: Integer})
solveExcept ::
  ( UnionWithExcept t u e v,
    GUnionPrjOp bool u,
    Functor u,
    SymBoolOp bool,
    Solver config bool failure model
  ) =>
  -- | solver configuration
  config ->
  -- | mapping the results to symbolic boolean formulas, the solver would try to find a model to make the formula true
  (Either e v -> bool) ->
  -- | the program to be solved, should be a union of exception and values
  t ->
  IO (Either failure model)
solveExcept config f v = solve config (simpleMerge $ f <$> extractUnionExcept v)

-- |
-- Solver procedure for programs with error handling. Would return multiple
-- models if possible.
solveMultiExcept ::
  ( UnionWithExcept t u e v,
    GUnionPrjOp bool u,
    Functor u,
    SymBoolOp bool,
    Solver config bool failure model
  ) =>
  -- | solver configuration
  config ->
  -- | maximum number of models to return
  Int ->
  -- | mapping the results to symbolic boolean formulas, the solver would try to find a model to make the formula true
  (Either e v -> bool) ->
  -- | the program to be solved, should be a union of exception and values
  t ->
  IO [model]
solveMultiExcept config n f v = solveMulti config n (simpleMerge $ f <$> extractUnionExcept v)
