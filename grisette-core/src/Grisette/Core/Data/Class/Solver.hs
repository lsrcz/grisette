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

    -- | This module does not contain actual implementation for symbolic
    -- primitive types and solver backends, and the examples in this module
    -- cannot be executed solely with @grisette-core@ package. They rely on the
    -- implementation in @grisette-symir@ and @grisette-backend-sbv@ package.
    --
    -- The examples also assumes a z3 solver available in @PATH@.

    -- * Union with exceptions
    UnionWithExcept (..),

    -- * Solver interfaces
    Solver (..),
    solveExcept,
    solveMultiExcept,
  )
where

import Control.DeepSeq
-- import Control.Monad.Except

-- import Grisette.Core.Control.Monad.UnionMBase

-- import Grisette.Core.Data.Class.GenSym
-- import Grisette.Core.Data.Class.Mergeable

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
  solve :: config -> bool -> IO (Either failure model)

  -- | Solve a single formula while returning multiple models to make it true.
  -- The maximum number of desired models are given.
  --
  -- > >>> solveMulti (UnboundedReasoning z3) 4 ("a" ||~ "b")
  -- > [Model {a -> True :: Bool, b -> False :: Bool},Model {a -> False :: Bool, b -> True :: Bool},Model {a -> True :: Bool, b -> True :: Bool}]
  solveMulti :: config -> Int -> bool -> IO [model]

  -- | Solve a single formula while returning multiple models to make it true.
  -- All models are returned.
  --
  -- > >>> solveAll (UnboundedReasoning z3) ("a" ||~ "b")
  -- > [Model {a -> True :: Bool, b -> False :: Bool},Model {a -> False :: Bool, b -> True :: Bool},Model {a -> True :: Bool, b -> True :: Bool}]
  solveAll :: config -> Int -> bool -> IO [model]

class UnionWithExcept t u e v | t -> u e v where
  extractUnionExcept :: t -> u (Either e v)

instance UnionWithExcept (ExceptT e u v) u e v where
  extractUnionExcept = runExceptT

solveExcept ::
  ( UnionWithExcept t u e v,
    GUnionPrjOp bool u,
    Functor u,
    SymBoolOp bool,
    Solver config bool failure model
  ) =>
  config ->
  (Either e v -> bool) ->
  t ->
  IO (Either failure model)
solveExcept config f v = solve config (getSingle $ f <$> extractUnionExcept v)

solveMultiExcept ::
  ( UnionWithExcept t u e v,
    GUnionPrjOp bool u,
    Functor u,
    SymBoolOp bool,
    Solver config bool failure model
  ) =>
  config ->
  Int ->
  (Either e v -> bool) ->
  t ->
  IO [model]
solveMultiExcept config n f v = solveMulti config n (getSingle $ f <$> extractUnionExcept v)
