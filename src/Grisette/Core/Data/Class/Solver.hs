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
    MonadicSolver (..),
    ConfigurableSolver (..),
    SolvingFailure (..),
    SolverCommand (..),
    Solver (..),
    withSolver,
    solveExcept,
    solveMultiExcept,
    solve,
    solveMulti,
  )
where

import Control.DeepSeq (NFData)
import Control.Exception (SomeException, bracket)
import Control.Monad.Except (ExceptT, runExceptT)
import qualified Data.HashSet as S
import Data.Hashable (Hashable)
import Data.Maybe (fromJust)
import GHC.Generics (Generic)
import Grisette.Core.Data.Class.ExtractSymbolics
  ( ExtractSymbolics (extractSymbolics),
  )
import Grisette.Core.Data.Class.LogicalOp (LogicalOp (symNot, (.||)))
import Grisette.Core.Data.Class.SimpleMergeable
  ( UnionPrjOp,
    simpleMerge,
  )
import Grisette.Core.Data.Class.Solvable (Solvable (con))
import Grisette.IR.SymPrim.Data.Prim.InternedTerm.Term
  ( SomeTypedSymbol (SomeTypedSymbol),
  )
import Grisette.IR.SymPrim.Data.Prim.Model
  ( Model,
    SymbolSet (unSymbolSet),
    equation,
  )
import Grisette.IR.SymPrim.Data.SymPrim (SymBool (SymBool))
import Language.Haskell.TH.Syntax (Lift)

data SolveInternal = SolveInternal
  deriving (Eq, Show, Ord, Generic, Hashable, Lift, NFData)

-- $setup
-- >>> import Grisette.Core
-- >>> import Grisette.IR.SymPrim
-- >>> import Grisette.Backend.SBV
-- >>> :set -XOverloadedStrings

data SolvingFailure
  = DSat (Maybe String)
  | Unsat
  | Unk
  | ResultNumLimitReached
  | SolvingError SomeException
  | Terminated
  deriving (Show)

class MonadicSolver m where
  monadicSolverPush :: Int -> m ()
  monadicSolverPop :: Int -> m ()
  monadicSolverSolve :: SymBool -> m (Either SolvingFailure Model)

data SolverCommand
  = SolverSolve SymBool
  | SolverPush Int
  | SolverPop Int
  | SolverTerminate

class ConfigurableSolver config handle | config -> handle where
  newSolver :: config -> IO handle

class Solver handle where
  solverRunCommand ::
    (handle -> IO (Either SolvingFailure a)) ->
    handle ->
    SolverCommand ->
    IO (Either SolvingFailure a)
  solverSolve :: handle -> SymBool -> IO (Either SolvingFailure Model)
  solverPush :: handle -> Int -> IO (Either SolvingFailure ())
  solverPush handle n =
    solverRunCommand (const $ return $ Right ()) handle $ SolverPush n
  solverPop :: handle -> Int -> IO (Either SolvingFailure ())
  solverPop handle n =
    solverRunCommand (const $ return $ Right ()) handle $ SolverPop n
  solverTerminate :: handle -> IO ()
  solverForceTerminate :: handle -> IO ()

withSolver ::
  (ConfigurableSolver config handle, Solver handle) =>
  config ->
  (handle -> IO a) ->
  IO a
withSolver config = bracket (newSolver config) solverTerminate

-- | Solve a single formula. Find an assignment to it to make it true.
--
-- >>> solve (precise z3) ("a" .&& ("b" :: SymInteger) .== 1)
-- Right (Model {a -> True :: Bool, b -> 1 :: Integer})
-- >>> solve (precise z3) ("a" .&& symNot "a")
-- Left Unsat
solve ::
  (ConfigurableSolver config handle, Solver handle) =>
  -- | solver configuration
  config ->
  -- | formula to solve, the solver will try to make it true
  SymBool ->
  IO (Either SolvingFailure Model)
solve config formula = withSolver config (`solverSolve` formula)

-- | Solve a single formula while returning multiple models to make it true.
-- The maximum number of desired models are given.
--
-- > >>> solveMulti (precise z3) 4 ("a" .|| "b")
-- > [Model {a -> True :: Bool, b -> False :: Bool},Model {a -> False :: Bool, b -> True :: Bool},Model {a -> True :: Bool, b -> True :: Bool}]
solveMulti ::
  (ConfigurableSolver config handle, Solver handle) =>
  -- | solver configuration
  config ->
  -- | maximum number of models to return
  Int ->
  -- | formula to solve, the solver will try to make it true
  SymBool ->
  IO ([Model], SolvingFailure)
solveMulti config numOfModelRequested formula =
  withSolver config $ \solver -> do
    firstModel <- solverSolve solver formula
    case firstModel of
      Left err -> return ([], err)
      Right model -> do
        (models, err) <- go solver model numOfModelRequested
        return (model : models, err)
  where
    allSymbols = extractSymbolics formula :: SymbolSet
    go solver prevModel n
      | n <= 1 = return ([], ResultNumLimitReached)
      | otherwise = do
          let newFormula =
                S.foldl'
                  ( \acc (SomeTypedSymbol _ v) ->
                      acc
                        .|| (symNot (SymBool $ fromJust $ equation v prevModel))
                  )
                  (con False)
                  (unSymbolSet allSymbols)
          res <- solverSolve solver newFormula
          case res of
            Left err -> return ([], err)
            Right model -> do
              (models, err) <- go solver model (n - 1)
              return (model : models, err)

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
    ConfigurableSolver config handle,
    Solver handle
  ) =>
  -- | solver configuration
  config ->
  -- | mapping the results to symbolic boolean formulas, the solver would try to
  -- find a model to make the formula true
  (Either e v -> SymBool) ->
  -- | the program to be solved, should be a union of exception and values
  t ->
  IO (Either SolvingFailure Model)
solveExcept config f v = solve config (simpleMerge $ f <$> extractUnionExcept v)

-- |
-- Solver procedure for programs with error handling. Would return multiple
-- models if possible.
solveMultiExcept ::
  ( UnionWithExcept t u e v,
    UnionPrjOp u,
    Functor u,
    ConfigurableSolver config handle,
    Solver handle
  ) =>
  -- | solver configuration
  config ->
  -- | maximum number of models to return
  Int ->
  -- | mapping the results to symbolic boolean formulas, the solver would try to
  -- find a model to make the formula true
  (Either e v -> SymBool) ->
  -- | the program to be solved, should be a union of exception and values
  t ->
  IO ([Model], SolvingFailure)
solveMultiExcept config n f v = solveMulti config n (simpleMerge $ f <$> extractUnionExcept v)
