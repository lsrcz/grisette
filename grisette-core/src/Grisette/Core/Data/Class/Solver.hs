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
    ExtractUnionEither (..),
    solveFallable,
    solveMultiFallable,
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

class ExtractUnionEither t u e v | t -> u e v where
  extractUnionEither :: t -> u (Either e v)

instance ExtractUnionEither (ExceptT e u v) u e v where
  extractUnionEither = runExceptT

solveFallable ::
  ( ExtractUnionEither t u e v,
    GUnionPrjOp bool u,
    Functor u,
    SymBoolOp bool,
    Solver config bool failure model
  ) =>
  config ->
  (Either e v -> bool) ->
  t ->
  IO (Either failure model)
solveFallable config f v = solveFormula config (getSingle $ f <$> extractUnionEither v)

solveMultiFallable ::
  ( ExtractUnionEither t u e v,
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
solveMultiFallable config n f v = solveFormulaMulti config n (getSingle $ f <$> extractUnionEither v)
