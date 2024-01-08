{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE Trustworthy #-}

-- |
-- Module      :   Grisette.Core.Data.Class.CEGISSolver
-- Copyright   :   (c) Sirui Lu 2021-2023
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Core.Data.Class.CEGISSolver
  ( -- * Note for the examples

    --

    -- | The examples assumes a [z3](https://github.com/Z3Prover/z3) solver available in @PATH@.

    -- * CEGIS solver interfaces
    CEGISCondition (..),
    cegisPostCond,
    cegisPrePost,
    cegis,
    cegisExcept,
    cegisExceptStdVC,
    cegisExceptVC,
    cegisExceptMultiInputs,
    cegisExceptStdVCMultiInputs,
    cegisExceptVCMultiInputs,
    SynthesisConstraintFun,
    VerifierResult (..),
    StatefulVerifierFun,
    CEGISResult (..),
    genericCEGIS,
    cegisMultiInputs,
  )
where

import Control.Monad (foldM, unless)
import Data.List (partition)
import GHC.Generics (Generic)
import Generics.Deriving (Default (Default))
import Grisette.Core.Control.Exception
  ( VerificationConditions (AssertionViolation, AssumptionViolation),
  )
import Grisette.Core.Data.Class.EvaluateSym (EvaluateSym, evaluateSym)
import Grisette.Core.Data.Class.ExtractSymbolics
  ( ExtractSymbolics,
    extractSymbolics,
  )
import Grisette.Core.Data.Class.LogicalOp (LogicalOp (symNot, (.&&)))
import Grisette.Core.Data.Class.Mergeable (Mergeable)
import Grisette.Core.Data.Class.ModelOps
  ( ModelOps (exact, exceptFor),
    SymbolSetOps (isEmptySet),
  )
import Grisette.Core.Data.Class.SEq (SEq)
import Grisette.Core.Data.Class.SimpleMergeable
  ( SimpleMergeable,
    UnionPrjOp,
    simpleMerge,
  )
import Grisette.Core.Data.Class.Solvable (Solvable (con))
import Grisette.Core.Data.Class.Solver
  ( ConfigurableSolver,
    Solver (solverSolve),
    SolvingFailure (Unsat),
    UnionWithExcept (extractUnionExcept),
    solve,
    withSolver,
  )
import Grisette.IR.SymPrim.Data.Prim.Model (Model)
import Grisette.IR.SymPrim.Data.SymPrim (SymBool)

-- $setup
-- >>> import Grisette.Core
-- >>> import Grisette.Lib.Base
-- >>> import Grisette.IR.SymPrim
-- >>> import Grisette.Backend.SBV

type SynthesisConstraintFun input = Int -> input -> IO SymBool

data VerifierResult input exception
  = FoundCex input
  | NoCex
  | VerifierFailure exception

type StatefulVerifierFun state input exception =
  state -> Model -> IO (state, VerifierResult input exception)

data CEGISResult exception
  = CEGISSuccess Model
  | CEGISVerifierFailure exception
  | CEGISSolverFailure SolvingFailure
  deriving (Show)

genericCEGIS ::
  (ConfigurableSolver config handle, Solver handle) =>
  config ->
  SymBool ->
  SynthesisConstraintFun input ->
  verifierState ->
  StatefulVerifierFun verifierState input exception ->
  IO ([input], CEGISResult exception)
genericCEGIS config initConstr synthConstr initVerifierState verifier =
  withSolver config $ \solver -> do
    firstResult <- solverSolve solver initConstr
    case firstResult of
      Left err -> return ([], CEGISSolverFailure err)
      Right model -> go solver model 0 initVerifierState
  where
    go solver prevModel iterNum verifierState = do
      (newVerifierState, verifierResult) <-
        verifier verifierState prevModel
      case verifierResult of
        FoundCex cex -> do
          newResult <- solverSolve solver =<< synthConstr iterNum cex
          case newResult of
            Left err -> return ([], CEGISSolverFailure err)
            Right model -> do
              (cexes, result) <- go solver model (iterNum + 1) newVerifierState
              return (cex : cexes, result)
        NoCex -> return ([], CEGISSuccess prevModel)
        VerifierFailure exception -> return ([], CEGISVerifierFailure exception)

data CEGISMultiInputsState input = CEGISMultiInputsState
  { _cegisMultiInputsRemainingSymInputs :: [input],
    _cegisMultiInputsPre :: SymBool,
    _cegisMultiInputsPost :: SymBool
  }

-- | The condition for CEGIS to solve.
--
-- The first argument is the pre-condition, and the second argument is the
-- post-condition.
--
-- The CEGIS procedures would try to find a model for the formula
--
-- \[
--   \forall P. (\exists I. \mathrm{pre}(P, I)) \wedge (\forall I. \mathrm{pre}(P, I)\implies \mathrm{post}(P, I))
-- \]
--
-- In program synthesis tasks, \(P\) is the symbolic constants in the symbolic
-- program, and \(I\) is the input. The pre-condition is used to restrict the
-- search space of the program. The procedure would only return programs that
-- meets the pre-conditions on every possible inputs, and there are at least
-- one possible input. The post-condition is used to specify the desired program
-- behaviors.
data CEGISCondition = CEGISCondition SymBool SymBool
  deriving (Generic)
  deriving (EvaluateSym) via (Default CEGISCondition)

-- | Construct a CEGIS condition with only a post-condition. The pre-condition
-- would be set to true, meaning that all programs in the program space are
-- allowed.
cegisPostCond :: SymBool -> CEGISCondition
cegisPostCond = CEGISCondition (con True)

-- | Construct a CEGIS condition with both pre- and post-conditions.
cegisPrePost :: SymBool -> SymBool -> CEGISCondition
cegisPrePost = CEGISCondition

deriving via (Default CEGISCondition) instance Mergeable CEGISCondition

deriving via (Default CEGISCondition) instance SimpleMergeable CEGISCondition

cegisMultiInputs ::
  ( EvaluateSym input,
    ExtractSymbolics input,
    ConfigurableSolver config handle,
    Solver handle
  ) =>
  config ->
  [input] ->
  (input -> CEGISCondition) ->
  IO ([input], CEGISResult SolvingFailure)
cegisMultiInputs config inputs toCEGISCondition = do
  initConstr <- cexesAssertFun conInputs
  genericCEGIS
    config
    initConstr
    synthConstr
    (CEGISMultiInputsState symInputs (con True) (con True))
    verifier
  where
    (conInputs, symInputs) = partition (isEmptySet . extractSymbolics) inputs
    forallSymbols = extractSymbolics symInputs
    cexAssertFun input = do
      unless (isEmptySet (extractSymbolics input)) $ error "BUG"
      CEGISCondition pre post <- return $ toCEGISCondition input
      return $ pre .&& post
    cexesAssertFun = foldM (\acc x -> (acc .&&) <$> cexAssertFun x) (con True)
    synthConstr _ = cexAssertFun
    verifier state@(CEGISMultiInputsState [] _ _) _ = return (state, NoCex)
    verifier (CEGISMultiInputsState (nextSymInput : symInputs) pre post) candidate = do
      CEGISCondition nextPre nextPost <- return $ toCEGISCondition nextSymInput
      let newPre = pre .&& nextPre
      let newPost = post .&& nextPost
      let evaluated =
            evaluateSym False (exceptFor forallSymbols candidate) $
              newPre .&& symNot newPost
      r <- solve config evaluated
      case r of
        Left Unsat ->
          verifier (CEGISMultiInputsState symInputs newPre newPost) candidate
        Left err -> return (CEGISMultiInputsState [] newPre newPost, VerifierFailure err)
        Right model ->
          return
            ( CEGISMultiInputsState (nextSymInput : symInputs) newPre newPost,
              FoundCex $
                evaluateSym False (exact forallSymbols model) nextSymInput
            )

-- -- | Counter-example guided inductive synthesis (CEGIS) solver interface.
-- class
--   CEGISSolver config failure
--     | config -> failure
--   where
--   -- |
--   -- CEGIS with multiple (possibly symbolic) inputs. Solves the following formula (see
--   -- 'CEGISCondition' for details).
--   --
--   -- \[
--   --   \forall P. (\exists I\in\mathrm{inputs}. \mathrm{pre}(P, I)) \wedge (\forall I\in\mathrm{inputs}. \mathrm{pre}(P, I)\implies \mathrm{post}(P, I))
--   -- \]
--   --
--   -- For simpler queries, where the inputs are representable by a single
--   -- symbolic value, you may want to use 'cegis' or 'cegisExcept' instead.
--   -- We have an example for the 'cegis' call.
--   cegisMultiInputs ::
--     (EvaluateSym inputs, ExtractSymbolics inputs) =>
--     -- | The configuration of the solver
--     config ->
--     -- | Some initial counter-examples. Providing some concrete
--     -- inputs may help the solver to find a model faster. Providing
--     -- symbolic inputs would cause the solver to find the program
--     -- that works on all the inputs representable by it (see
--     -- 'CEGISCondition').
--     [inputs] ->
--     -- | The function mapping the inputs to
--     -- the conditions for the solver to
--     -- solve.
--     (inputs -> CEGISCondition) ->
--     -- | The counter-examples generated
--     -- during the CEGIS loop, and the
--     -- model found by the solver.
--     IO ([inputs], Either failure Model)

-- |
-- CEGIS with a single symbolic input to represent a set of inputs.
--
-- The following example tries to find the value of @c@ such that for all
-- positive @x@, @x * c < 0 && c > -2@. The @c .> -2@ clause is used to make
-- the solution unique.
--
-- >>> :set -XOverloadedStrings
-- >>> let [x,c] = ["x","c"] :: [SymInteger]
-- >>> cegis (precise z3) x (\x -> cegisPrePost (x .> 0) (x * c .< 0 .&& c .> -2))
-- ([...],CEGISSuccess (Model {c -> -1 :: Integer}))
cegis ::
  ( ConfigurableSolver config handle,
    Solver handle,
    EvaluateSym inputs,
    ExtractSymbolics inputs,
    SEq inputs
  ) =>
  -- | The configuration of the solver
  config ->
  -- | Initial symbolic inputs. The solver will try to find a
  -- program that works on all the inputs representable by it (see
  -- 'CEGISCondition').
  inputs ->
  -- | The condition for the solver to solve. All the
  -- symbolic constants that are not in the inputs will
  -- be considered as part of the symbolic program.
  (inputs -> CEGISCondition) ->
  -- | The counter-examples generated
  -- during the CEGIS loop, and the
  -- model found by the solver.
  IO ([inputs], CEGISResult SolvingFailure)
cegis config inputs = cegisMultiInputs config [inputs]

-- |
-- CEGIS for symbolic programs with error handling, using multiple (possibly
-- symbolic) inputs to represent a set of inputs.
cegisExceptMultiInputs ::
  ( ConfigurableSolver config handle,
    Solver handle,
    EvaluateSym inputs,
    ExtractSymbolics inputs,
    UnionWithExcept t u e v,
    UnionPrjOp u,
    Monad u
  ) =>
  config ->
  [inputs] ->
  (Either e v -> CEGISCondition) ->
  (inputs -> t) ->
  IO ([inputs], CEGISResult SolvingFailure)
cegisExceptMultiInputs config cexes interpretFun f =
  cegisMultiInputs config cexes (simpleMerge . (interpretFun <$>) . extractUnionExcept . f)

-- |
-- CEGIS for symbolic programs with error handling, using multiple (possibly
-- symbolic) inputs to represent a set of inputs.
--
-- The errors should be translated to assertion or assumption violations.
cegisExceptVCMultiInputs ::
  ( ConfigurableSolver config handle,
    Solver handle,
    EvaluateSym inputs,
    ExtractSymbolics inputs,
    UnionWithExcept t u e v,
    UnionPrjOp u,
    Monad u
  ) =>
  config ->
  [inputs] ->
  (Either e v -> u (Either VerificationConditions ())) ->
  (inputs -> t) ->
  IO ([inputs], CEGISResult SolvingFailure)
cegisExceptVCMultiInputs config cexes interpretFun f =
  cegisMultiInputs
    config
    cexes
    ( \v ->
        simpleMerge
          ( ( \case
                Left AssumptionViolation -> cegisPrePost (con False) (con True)
                Left AssertionViolation -> cegisPostCond (con False)
                _ -> cegisPostCond (con True)
            )
              <$> (extractUnionExcept (f v) >>= interpretFun)
          )
    )

-- |
-- CEGIS for symbolic programs with error handling, using multiple (possibly
-- symbolic) inputs to represent a set of inputs. This function saves the
-- efforts to implement the translation function for the standard error type
-- 'VerificationConditions', and the standard result type '()'.
--
-- This function translates assumption violations to failed pre-conditions,
-- and translates assertion violations to failed post-conditions.
-- The '()' result will not fail any conditions.
cegisExceptStdVCMultiInputs ::
  ( ConfigurableSolver config handle,
    Solver handle,
    EvaluateSym inputs,
    ExtractSymbolics inputs,
    UnionWithExcept t u VerificationConditions (),
    UnionPrjOp u,
    Monad u
  ) =>
  config ->
  [inputs] ->
  (inputs -> t) ->
  IO ([inputs], CEGISResult SolvingFailure)
cegisExceptStdVCMultiInputs config cexes =
  cegisExceptVCMultiInputs config cexes return

-- |
-- CEGIS for symbolic programs with error handling, using a single symbolic
-- input to represent a set of inputs.
--
-- 'cegisExcept' is particularly useful when custom error types are used.
-- With 'cegisExcept', you define how the errors are interpreted to the
-- CEGIS conditions after the symbolic evaluation. This could increase the
-- readability and modularity of the code.
--
-- The following example tries to find the value of @c@ such that for all
-- positive @x@, @x * c < 0 && c > -2@. The @c .> -2@ assertion is used to make
-- the solution unique.
--
-- >>> :set -XOverloadedStrings
-- >>> let [x,c] = ["x","c"] :: [SymInteger]
-- >>> import Control.Monad.Except
-- >>> :{
--   res :: SymInteger -> ExceptT VerificationConditions UnionM ()
--   res x = do
--     symAssume $ x .> 0
--     symAssert $ x * c .< 0
--     symAssert $ c .> -2
-- :}
--
-- >>> :{
--   translation (Left AssumptionViolation) = cegisPrePost (con False) (con True)
--   translation (Left AssertionViolation) = cegisPostCond (con False)
--   translation _ = cegisPostCond (con True)
-- :}
--
-- >>> cegisExcept (precise z3) x translation res
-- ([...],CEGISSuccess (Model {c -> -1 :: Integer}))
cegisExcept ::
  ( UnionWithExcept t u e v,
    UnionPrjOp u,
    Functor u,
    EvaluateSym inputs,
    ExtractSymbolics inputs,
    ConfigurableSolver config handle,
    Solver handle,
    SEq inputs
  ) =>
  config ->
  inputs ->
  (Either e v -> CEGISCondition) ->
  (inputs -> t) ->
  IO ([inputs], CEGISResult SolvingFailure)
cegisExcept config inputs f v = cegis config inputs $ \i -> simpleMerge $ f <$> extractUnionExcept (v i)

-- |
-- CEGIS for symbolic programs with error handling, using a single symbolic
-- input to represent a set of inputs.
--
-- The errors should be translated to assertion or assumption violations.
cegisExceptVC ::
  ( UnionWithExcept t u e v,
    UnionPrjOp u,
    Monad u,
    EvaluateSym inputs,
    ExtractSymbolics inputs,
    ConfigurableSolver config handle,
    Solver handle,
    SEq inputs
  ) =>
  config ->
  inputs ->
  (Either e v -> u (Either VerificationConditions ())) ->
  (inputs -> t) ->
  IO ([inputs], CEGISResult SolvingFailure)
cegisExceptVC config inputs f v = do
  cegis config inputs $ \i ->
    simpleMerge $
      ( \case
          Left AssumptionViolation -> cegisPrePost (con False) (con True)
          Left AssertionViolation -> cegisPostCond (con False)
          _ -> cegisPostCond (con True)
      )
        <$> (extractUnionExcept (v i) >>= f)

-- |
-- CEGIS for symbolic programs with error handling, using a single symbolic
-- input to represent a set of inputs. This function saves the efforts to
-- implement the translation function for the standard error type
-- 'VerificationConditions', and the standard result type '()'.
--
-- This function translates assumption violations to failed pre-conditions,
-- and translates assertion violations to failed post-conditions.
-- The '()' result will not fail any conditions.
--
-- The following example tries to find the value of @c@ such that for all
-- positive @x@, @x * c < 0 && c > -2@. The @c .> -2@ assertion is used to make
-- the solution unique.
--
-- >>> :set -XOverloadedStrings
-- >>> let [x,c] = ["x","c"] :: [SymInteger]
-- >>> import Control.Monad.Except
-- >>> :{
--   res :: SymInteger -> ExceptT VerificationConditions UnionM ()
--   res x = do
--     symAssume $ x .> 0
--     symAssert $ x * c .< 0
--     symAssert $ c .> -2
-- :}
--
-- >>> cegisExceptStdVC (precise z3) x res
-- ([...],CEGISSuccess (Model {c -> -1 :: Integer}))
cegisExceptStdVC ::
  ( UnionWithExcept t u VerificationConditions (),
    UnionPrjOp u,
    Monad u,
    EvaluateSym inputs,
    ExtractSymbolics inputs,
    ConfigurableSolver config handle,
    Solver handle,
    SEq inputs
  ) =>
  config ->
  inputs ->
  (inputs -> t) ->
  IO ([inputs], CEGISResult SolvingFailure)
cegisExceptStdVC config inputs = cegisExceptVC config inputs return
