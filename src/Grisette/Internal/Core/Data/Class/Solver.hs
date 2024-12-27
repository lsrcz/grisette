{-# OPTIONS_GHC -Wno-missing-import-lists #-}

-- |
-- Module      :   Grisette.Internal.Core.Data.Class.Solver
-- Copyright   :   (c) Sirui Lu 2021-2023
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Internal.Core.Data.Class.Solver
  ( -- * Note for the examples

    --

    -- | The examples assumes that the [z3](https://github.com/Z3Prover/z3)
    -- solver is available in @PATH@.

    -- * Solver interfaces
    SolvingFailure (..),
    MonadicSolver (..),
    monadicSolverSolve,
    SolverCommand (..),
    ConfigurableSolver (..),
    Solver (..),
    solverSolve,
    withSolver,
    solve,
    solverSolveMulti,
    solveMulti,

    -- * Union with exceptions
    UnionWithExcept (..),
    solverSolveExcept,
    solveExcept,
    solverSolveMultiExcept,
    solveMultiExcept,
  )
where

import Grisette.Internal.Internal.Decl.Core.Data.Class.Solver
import Grisette.Internal.Internal.Impl.Core.Data.Class.Solver ()
