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
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Grisette.Core.Data.Class.Solver
  ( Solver (..),
    UnionWithExcept (..),
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
import Grisette.Core.Data.Class.PrimWrapper
import Grisette.Core.Data.Class.SimpleMergeable
import Language.Haskell.TH.Syntax

data SolveInternal = SolveInternal deriving (Eq, Show, Ord, Generic, Hashable, Lift, NFData)

class
  (SymBoolOp bool, GEvaluateSym model bool) =>
  Solver config bool failure model
    | config -> bool failure model
  where
  solveFormula :: config -> bool -> IO (Either failure model)
  solveFormulaMulti :: config -> Int -> bool -> IO [model]
  solveFormulaAll :: config -> Int -> bool -> IO [model]

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
solveExcept config f v = solveFormula config (getSingle $ f <$> extractUnionExcept v)

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
solveMultiExcept config n f v = solveFormulaMulti config n (getSingle $ f <$> extractUnionExcept v)
