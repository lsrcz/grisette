{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Grisette.Core.Data.Class.Solver
  ( -- * Note for the examples

    --

    -- | This module does not contain actual implementation for symbolic
    -- primitive types and solver backends, and the examples in this module
    -- cannot be executed solely with @grisette-core@ package. They rely on the
    -- implementation in @grisette-symir@ and @grisette-backend-sbv@ package.
    --
    -- The examples also assumes a z3 solver available in @PATH@.

    -- * General error handling types
    ExtractUnionEither (..),

    -- * Solver interfaces
    Solver (..),
    solveFallable,
    solveMultiFallable,
    cegisFallable,
    cegisFallable',
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
import Grisette.Core.Data.Class.Solvable
import Grisette.Core.Data.Class.SimpleMergeable
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
  Solver config bool symbolSet failure model
    | config -> bool symbolSet failure model
  where
  -- | Solve a single formula. Find an assignment to it to make it true.
  --
  -- >>> solveFormula (UnboundedReasoning z3) ("a" &&~ ("b" :: SymInteger) ==~ 1)
  -- Right (Model {a -> True :: Bool, b -> 1 :: Integer})
  -- >>> solveFormula (UnboundedReasoning z3) ("a" &&~ nots "a")
  -- Left Unsat
  solveFormula :: config -> bool -> IO (Either failure model)
  -- | Solve a single formula while returning multiple models to make it true.
  -- The maximum number of desired models are given.
  --
  -- > >>> solveFormulaMulti (UnboundedReasoning z3) 4 ("a" ||~ "b")
  -- > [Model {a -> True :: Bool, b -> False :: Bool},Model {a -> False :: Bool, b -> True :: Bool},Model {a -> True :: Bool, b -> True :: Bool}]
  solveFormulaMulti :: config -> Int -> bool -> IO [model]
  -- | Solve a single formula while returning multiple models to make it true.
  -- All models are returned.
  --
  -- > >>> solveFormulaAll (UnboundedReasoning z3) ("a" ||~ "b")
  -- > [Model {a -> True :: Bool, b -> False :: Bool},Model {a -> False :: Bool, b -> True :: Bool},Model {a -> True :: Bool, b -> True :: Bool}]
  solveFormulaAll :: config -> Int -> bool -> IO [model]
  cegisFormula ::
    (GEvaluateSym model forallArg, GExtractSymbolics symbolSet forallArg) =>
    config ->
    forallArg ->
    bool ->
    IO (Either failure ([forallArg], model))
  cegisFormula config forallArg = cegisFormulas config forallArg (conc False)
  cegisFormulas ::
    (GEvaluateSym model forallArg, GExtractSymbolics symbolSet forallArg) =>
    config ->
    forallArg ->
    bool ->
    bool ->
    IO (Either failure ([forallArg], model))

-- | For those types that are capable to handle errors.
class ExtractUnionEither t u e v | t -> u e v where
  extractUnionEither :: t -> u (Either e v)

instance ExtractUnionEither (ExceptT e u v) u e v where
  extractUnionEither = runExceptT

solveFallable ::
  ( ExtractUnionEither t u e v,
    GUnionPrjOp bool u,
    Functor u,
    SymBoolOp bool,
    Solver config bool symbolSet failure model
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
    Solver config bool symbolSet failure model
  ) =>
  config ->
  Int ->
  (Either e v -> bool) ->
  t ->
  IO [model]
solveMultiFallable config n f v = solveFormulaMulti config n (getSingle $ f <$> extractUnionEither v)

cegisFallable ::
  ( ExtractUnionEither t u e v,
    GUnionPrjOp bool u,
    Functor u,
    SymBoolOp bool,
    GEvaluateSym model forallArgs,
    GExtractSymbolics symbolSet forallArgs,
    Solver config bool symbolSet failure model
  ) =>
  config ->
  forallArgs ->
  (Either e v -> (bool, bool)) ->
  t ->
  IO (Either failure ([forallArgs], model))
cegisFallable config args f v = uncurry (cegisFormulas config args) (getSingle $ f <$> extractUnionEither v)

cegisFallable' ::
  ( ExtractUnionEither t u e v,
    GUnionPrjOp bool u,
    Monad u,
    SymBoolOp bool,
    GEvaluateSym model forallArgs,
    GExtractSymbolics symbolSet forallArgs,
    Solver config bool symbolSet failure model
  ) =>
  config ->
  forallArgs ->
  (Either e v -> u (Either VerificationConditions ())) ->
  t ->
  IO (Either failure ([forallArgs], model))
cegisFallable' config args f v =
  uncurry
    (cegisFormulas config args)
    ( getSingle $
        ( \case
            Left AssumptionViolation -> (conc True, conc False)
            Left AssertionViolation -> (conc False, conc True)
            _ -> (conc False, conc False)
        )
          <$> (extractUnionEither v >>= f)
    )
