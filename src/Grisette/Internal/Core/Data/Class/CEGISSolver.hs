{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE Trustworthy #-}

-- |
-- Module      :   Grisette.Internal.Core.Data.Class.CEGISSolver
-- Copyright   :   (c) Sirui Lu 2021-2023
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Internal.Core.Data.Class.CEGISSolver
  ( -- * Note for the examples

    --

    -- | The examples assumes that the [z3](https://github.com/Z3Prover/z3)
    -- solver is available in @PATH@.

    -- * Generic CEGIS interface
    VerifierResult (..),
    SynthesisConstraintFun,
    VerifierFun,
    CEGISResult (..),
    solverGenericCEGIS,
    solverGenericCEGISWithRefinement,
    genericCEGIS,
    genericCEGISWithRefinement,

    -- * CEGIS interfaces with pre/post conditions
    CEGISCondition (..),
    cegisPostCond,
    cegisPrePost,
    solverCegisMultiInputs,
    solverCegis,
    solverCegisExcept,
    solverCegisExceptStdVC,
    solverCegisExceptVC,
    solverCegisExceptMultiInputs,
    solverCegisExceptStdVCMultiInputs,
    solverCegisExceptVCMultiInputs,
    solverCegisForAll,
    solverCegisForAllExcept,
    solverCegisForAllExceptStdVC,
    solverCegisForAllExceptVC,
    cegisMultiInputs,
    cegis,
    cegisExcept,
    cegisExceptStdVC,
    cegisExceptVC,
    cegisExceptMultiInputs,
    cegisExceptStdVCMultiInputs,
    cegisExceptVCMultiInputs,
    cegisForAll,
    cegisForAllExcept,
    cegisForAllExceptStdVC,
    cegisForAllExceptVC,
  )
where

#if MIN_VERSION_base(4,20,0)
import Data.List (partition)
#else
import Data.List (foldl', partition)
#endif

import Control.DeepSeq (NFData)
import qualified Data.Binary as Binary
import Data.Bytes.Serial (Serial (deserialize, serialize))
import Data.Hashable (Hashable)
import qualified Data.Serialize as Cereal
import GHC.Generics (Generic)
import Generics.Deriving (Default (Default))
import Grisette.Internal.Core.Control.Exception
  ( VerificationConditions (AssertionViolation, AssumptionViolation),
  )
import Grisette.Internal.Core.Data.Class.EvalSym (EvalSym, evalSym)
import Grisette.Internal.Core.Data.Class.ExtractSym
  ( ExtractSym,
    extractSym,
  )
import Grisette.Internal.Core.Data.Class.LogicalOp (LogicalOp (symNot, (.&&)))
import Grisette.Internal.Core.Data.Class.Mergeable (Mergeable)
import Grisette.Internal.Core.Data.Class.ModelOps
  ( ModelOps (exact, exceptFor),
    SymbolSetOps (isEmptySet),
  )
import Grisette.Internal.Core.Data.Class.PPrint (PPrint (pformat), (<+>))
import Grisette.Internal.Core.Data.Class.SimpleMergeable
  ( SimpleMergeable,
  )
import Grisette.Internal.Core.Data.Class.Solvable (Solvable (con))
import Grisette.Internal.Core.Data.Class.Solver
  ( ConfigurableSolver,
    Solver (solverResetAssertions),
    SolvingFailure (Unsat),
    UnionWithExcept (extractUnionExcept),
    solverSolve,
    withSolver,
  )
import Grisette.Internal.Core.Data.Class.SymEq (SymEq)
import Grisette.Internal.Core.Data.Class.UnionView
  ( UnionView,
    simpleMerge,
  )
import Grisette.Internal.SymPrim.Prim.Model (Model)
import Grisette.Internal.SymPrim.SymBool (SymBool)
import Language.Haskell.TH.Syntax (Lift)

-- $setup
-- >>> import Grisette.Core
-- >>> import Grisette.Lib.Base
-- >>> import Grisette.SymPrim
-- >>> import Grisette.Backend

-- | The response from a verifier.
data VerifierResult cex exception
  = CEGISVerifierFoundCex cex
  | -- | True indicates that the verifier is sure that there is no
    -- counter-example, while False indicates that the verifier is not sure,
    -- but it cannot find a counter-example.
    CEGISVerifierNoCex Bool
  | CEGISVerifierException exception
  deriving (Show, Eq, Generic, Lift)
  deriving anyclass (Hashable, NFData)

instance
  (PPrint cex, PPrint exception) =>
  PPrint (VerifierResult cex exception)
  where
  pformat = \case
    CEGISVerifierFoundCex cex -> "Found cex:" <+> pformat cex
    CEGISVerifierNoCex True -> "No cex"
    CEGISVerifierNoCex False -> "Maybe no cex"
    CEGISVerifierException e -> "Exception:" <+> pformat e

-- | Build the synthesizer constraint from the verfication result. The first
-- argument will be guaranteed to be distinct during each invocation of the
-- function in the CEGIS algorithm, so it can be used to instantiate the
-- identifiers for fresh variables.
type SynthesisConstraintFun cex = cex -> IO SymBool

-- | The verifier.
type VerifierFun cex exception = Model -> IO (VerifierResult cex exception)

type RefinementConditionFun = Model -> IO SymBool

-- | The result of the CEGIS procedure.
data CEGISResult exception
  = CEGISSuccess Model
  | CEGISVerifierFailure exception
  | CEGISSolverFailure SolvingFailure
  deriving (Show, Eq, Generic, Lift)
  deriving anyclass (NFData, Hashable, Serial)

instance (Serial exception) => Cereal.Serialize (CEGISResult exception) where
  put = serialize
  get = deserialize

instance (Serial exception) => Binary.Binary (CEGISResult exception) where
  put = serialize
  get = deserialize

-- | Generic CEGIS procedure. See 'genericCEGIS' for more details.
--
-- The difference from 'genericCEGIS' is that this function accepts a solver
-- handle for the synthesizer, instead of a solver configuration.
solverGenericCEGIS ::
  (Solver handle) =>
  -- | Configuration of the solver.
  handle ->
  -- | Whether we should rerun the passed verifiers if any other verifier found
  -- a counter-example.
  Bool ->
  -- | The initial synthesis constraint.
  SymBool ->
  -- | Synthesis constraint from counter-examples
  SynthesisConstraintFun input ->
  -- | The verifier functions.
  [VerifierFun input exception] ->
  IO ([input], CEGISResult exception)
solverGenericCEGIS solver rerun initConstr synthConstr verifiers = do
  firstResult <- solverSolve solver initConstr
  case firstResult of
    Left err -> return ([], CEGISSolverFailure err)
    Right model -> go model False numAllVerifiers 0 verifiers
  where
    numAllVerifiers = length verifiers
    go prevModel _ 0 _ (_ : _) = return ([], CEGISSuccess prevModel)
    go prevModel needRerun runBound nextBound (verifier : remainingVerifiers) = do
      verifierResult <- verifier prevModel
      case verifierResult of
        CEGISVerifierFoundCex cex -> do
          newResult <- solverSolve solver =<< synthConstr cex
          case newResult of
            Left err -> return ([cex], CEGISSolverFailure err)
            Right model -> do
              (cexes, result) <-
                go
                  model
                  (needRerun || rerun)
                  (length remainingVerifiers + 1)
                  (numAllVerifiers - length remainingVerifiers - 1)
                  $ verifier : remainingVerifiers
              return (cex : cexes, result)
        CEGISVerifierNoCex {} ->
          go prevModel needRerun (runBound - 1) nextBound remainingVerifiers
        CEGISVerifierException exception ->
          return ([], CEGISVerifierFailure exception)
    go prevModel False _ _ [] = return ([], CEGISSuccess prevModel)
    go prevModel True _runBound nextBound [] =
      go prevModel False nextBound 0 verifiers

-- | Generic CEGIS procedure with refinement. See 'genericCEGISWithRefinement'
-- for more details.
--
-- The difference from 'genericCEGISWithRefinement' is that this function
-- accepts a solver handle for the synthesizer, instead of a solver
-- configuration.
solverGenericCEGISWithRefinement ::
  (Solver handle) =>
  -- | Configuration of the solver.
  handle ->
  -- | Whether we should rerun the passed verifiers if any other verifier found
  -- a counter-example.
  Bool ->
  -- | The initial synthesis constraint.
  SymBool ->
  -- | Synthesis constraint from counter-examples
  SynthesisConstraintFun input ->
  -- | Refinement condition generator.
  Maybe RefinementConditionFun ->
  -- | The verifier functions.
  [VerifierFun input exception] ->
  IO ([input], CEGISResult exception)
solverGenericCEGISWithRefinement
  solver
  rerun
  initConstr
  synthConstr
  refineCond
  verifiers = do
    (input, r) <-
      solverGenericCEGIS solver rerun initConstr synthConstr verifiers
    case r of
      CEGISSuccess model -> refine solver input model
      _ -> return (input, r)
    where
      refine solver input model = case refineCond of
        Just f -> do
          cond <- f model
          newResult <-
            solverGenericCEGIS solver rerun cond synthConstr verifiers
          case newResult of
            (newInputs, CEGISSuccess model) ->
              refine solver (input ++ newInputs) model
            _ -> return (input, CEGISSuccess model)
        Nothing -> return (input, CEGISSuccess model)

-- | Generic CEGIS procedure.
--
-- The CEGIS procedure will try to find a model that satisfies the initial
-- synthesis constraint, and satisfies all the inputs generated by the verifier.
genericCEGIS ::
  (ConfigurableSolver config handle) =>
  -- | Configuration of the solver.
  config ->
  -- | Whether we should rerun the passed verifiers if any other verifier found
  -- a counter-example.
  Bool ->
  -- | The initial synthesis constraint.
  SymBool ->
  -- | Synthesis constraint from counter-examples
  SynthesisConstraintFun input ->
  -- | The verifier functions.
  [VerifierFun input exception] ->
  IO ([input], CEGISResult exception)
genericCEGIS config rerun initConstr synthConstr verifier =
  withSolver config $ \solver ->
    solverGenericCEGIS solver rerun initConstr synthConstr verifier

-- | Generic CEGIS procedure.
--
-- The CEGIS procedure will try to find a model that satisfies the initial
-- synthesis constraint, and satisfies all the inputs generated by the verifier.
genericCEGISWithRefinement ::
  (ConfigurableSolver config handle) =>
  -- | Configuration of the solver.
  config ->
  -- | Whether we should rerun the passed verifiers if any other verifier found
  -- a counter-example.
  Bool ->
  -- | The initial synthesis constraint.
  SymBool ->
  -- | Synthesis constraint from counter-examples
  SynthesisConstraintFun input ->
  -- | Refinement condition generator.
  Maybe RefinementConditionFun ->
  -- | The verifier functions.
  [VerifierFun input exception] ->
  IO ([input], CEGISResult exception)
genericCEGISWithRefinement
  config
  rerun
  initConstr
  synthConstr
  refineCond
  verifier =
    withSolver config $ \solver -> do
      solverGenericCEGISWithRefinement
        solver
        rerun
        initConstr
        synthConstr
        refineCond
        verifier

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
  deriving (EvalSym) via (Default CEGISCondition)

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

-- | CEGIS with multiple (possibly symbolic) inputs. See 'cegisMultiInputs' for
-- more details.
--
-- The difference from 'cegisMultiInputs' is that this function accepts two
-- solver handles, one for the synthesizer and one for the verifier.
--
-- The synthesizer solver will **not** be reset, while the verifier solver will
-- be reset after each iteration.
solverCegisMultiInputs ::
  ( EvalSym input,
    ExtractSym input,
    Solver handle
  ) =>
  -- The synthesizer solver handle
  handle ->
  -- The verifier solver handle
  handle ->
  -- | Initial symbolic inputs. The solver will try to find a
  -- program that works on all the inputs representable by these inputs (see
  -- t'CEGISCondition').
  [input] ->
  -- | The condition for the solver to solve. All the
  -- symbolic constants that are not in the inputs will
  -- be considered as part of the symbolic program.
  (input -> CEGISCondition) ->
  -- | The counter-examples generated
  -- during the CEGIS loop, and the
  -- model found by the solver.
  IO ([input], CEGISResult SolvingFailure)
solverCegisMultiInputs
  synthesizerSolver
  verifierSolver
  inputs
  toCEGISCondition = do
    solverGenericCEGIS
      synthesizerSolver
      True
      (foldl' (\acc v -> acc .&& cexAssertFun v) (con True) conInputs)
      (return . cexAssertFun)
      $ getVerifier <$> symInputs
    where
      cexAssertFun input =
        case toCEGISCondition input of
          CEGISCondition pre post -> pre .&& post
      getVerifier input md = do
        let CEGISCondition pre post = toCEGISCondition input
        let evaluated =
              evalSym False (exceptFor (extractSym input) md) $
                pre .&& symNot post
        solverResetAssertions verifierSolver
        r <- solverSolve verifierSolver evaluated
        case r of
          Left Unsat -> return $ CEGISVerifierNoCex True
          Left err -> return $ CEGISVerifierException err
          Right model -> do
            let newCexInput =
                  evalSym True (exact (extractSym input) model) input
            return $ CEGISVerifierFoundCex newCexInput
      (conInputs, symInputs) = partition (isEmptySet . extractSym) inputs

-- | CEGIS with a single symbolic input to represent a set of inputs. See
-- 'cegis' for more details.
--
-- The difference from 'cegis' is that this function accepts two solver handles,
-- one for the synthesizer and one for the verifier.
--
-- The synthesizer solver will **not** be reset, while the verifier solver will
-- be reset after each iteration.
solverCegis ::
  ( Solver handle,
    EvalSym inputs,
    ExtractSym inputs,
    SymEq inputs
  ) =>
  -- | The synthesizer solver handle
  handle ->
  -- | The verifier solver handle
  handle ->
  -- | Initial symbolic inputs. The solver will try to find a
  -- program that works on all the inputs representable by it (see
  -- t'CEGISCondition').
  inputs ->
  -- | The condition for the solver to solve. All the
  -- symbolic constants that are not in the inputs will
  -- be considered as part of the symbolic program.
  (inputs -> CEGISCondition) ->
  -- | The counter-examples generated
  -- during the CEGIS loop, and the
  -- model found by the solver.
  IO ([inputs], CEGISResult SolvingFailure)
solverCegis synthesizerSolver verifierSolver inputs =
  solverCegisMultiInputs synthesizerSolver verifierSolver [inputs]

-- |
-- CEGIS for symbolic programs with error handling, using multiple (possibly
-- symbolic) inputs to represent a set of inputs.
--
-- The difference from 'cegisExceptMultiInputs' is that this function accepts
-- two solver handles, one for the synthesizer and one for the verifier.
--
-- The synthesizer solver will **not** be reset, while the verifier solver will
-- be reset after each iteration.
solverCegisExceptMultiInputs ::
  ( Solver handle,
    EvalSym inputs,
    ExtractSym inputs,
    UnionWithExcept t u e v,
    UnionView u,
    Monad u
  ) =>
  handle ->
  handle ->
  [inputs] ->
  (Either e v -> CEGISCondition) ->
  (inputs -> t) ->
  IO ([inputs], CEGISResult SolvingFailure)
solverCegisExceptMultiInputs
  synthesizerSolver
  verifierSolver
  cexes
  interpretFun
  f =
    solverCegisMultiInputs
      synthesizerSolver
      verifierSolver
      cexes
      (simpleMerge . (interpretFun <$>) . extractUnionExcept . f)

-- |
-- CEGIS for symbolic programs with error handling, using multiple (possibly
-- symbolic) inputs to represent a set of inputs.
--
-- The errors should be translated to assertion or assumption violations.
--
-- The difference from 'cegisExceptVCMultiInputs' is that this function accepts
-- two solver handles, one for the synthesizer and one for the verifier.
--
-- The synthesizer solver will **not** be reset, while the verifier solver will
-- be reset after each iteration.
solverCegisExceptVCMultiInputs ::
  ( Solver handle,
    EvalSym inputs,
    ExtractSym inputs,
    UnionWithExcept t u e v,
    UnionView u,
    Monad u
  ) =>
  handle ->
  handle ->
  [inputs] ->
  (Either e v -> u (Either VerificationConditions ())) ->
  (inputs -> t) ->
  IO ([inputs], CEGISResult SolvingFailure)
solverCegisExceptVCMultiInputs
  synthesizerSolver
  verifierSolver
  cexes
  interpretFun
  f =
    solverCegisMultiInputs
      synthesizerSolver
      verifierSolver
      cexes
      ( \v ->
          simpleMerge
            ( ( \case
                  Left AssumptionViolation ->
                    cegisPrePost (con False) (con True)
                  Left AssertionViolation -> cegisPostCond (con False)
                  _ -> cegisPostCond (con True)
              )
                <$> (extractUnionExcept (f v) >>= interpretFun)
            )
      )

-- |
-- CEGIS for symbolic programs with error handling, using multiple (possibly
-- symbolic) inputs to represent a set of inputs. See
-- 'cegisExceptStdVCMultiInputs' for more details.
--
-- The difference from 'cegisExceptStdVCMultiInputs' is that this function
-- accepts two solver handles, one for the synthesizer and one for the verifier.
--
-- The synthesizer solver will **not** be reset, while the verifier solver will
-- be reset after each iteration.
solverCegisExceptStdVCMultiInputs ::
  ( Solver handle,
    EvalSym inputs,
    ExtractSym inputs,
    UnionWithExcept t u VerificationConditions (),
    UnionView u,
    Monad u
  ) =>
  handle ->
  handle ->
  [inputs] ->
  (inputs -> t) ->
  IO ([inputs], CEGISResult SolvingFailure)
solverCegisExceptStdVCMultiInputs synthesizerSolver verifierSolver cexes =
  solverCegisExceptVCMultiInputs synthesizerSolver verifierSolver cexes return

-- |
-- CEGIS for symbolic programs with error handling, using a single symbolic
-- input to represent a set of inputs. See 'cegisExcept' for more details.
--
-- The difference from 'cegisExcept' is that this function accepts two solver
-- handles, one for the synthesizer and one for the verifier.
--
-- The synthesizer solver will **not** be reset, while the verifier solver will
-- be reset after each iteration.
solverCegisExcept ::
  ( UnionWithExcept t u e v,
    UnionView u,
    Functor u,
    EvalSym inputs,
    ExtractSym inputs,
    Solver handle,
    SymEq inputs
  ) =>
  handle ->
  handle ->
  inputs ->
  (Either e v -> CEGISCondition) ->
  (inputs -> t) ->
  IO ([inputs], CEGISResult SolvingFailure)
solverCegisExcept synthesizerSolver verifierSolver inputs f v =
  solverCegis synthesizerSolver verifierSolver inputs $
    \i -> simpleMerge $ f <$> extractUnionExcept (v i)

-- |
-- CEGIS for symbolic programs with error handling, using a single symbolic
-- input to represent a set of inputs.
--
-- The errors should be translated to assertion or assumption violations.
--
-- The difference from 'cegisExceptVC' is that this function accepts two solver
-- handles, one for the synthesizer and one for the verifier.
--
-- The synthesizer solver will **not** be reset, while the verifier solver will
-- be reset after each iteration.
solverCegisExceptVC ::
  ( UnionWithExcept t u e v,
    UnionView u,
    Monad u,
    EvalSym inputs,
    ExtractSym inputs,
    Solver handle,
    SymEq inputs
  ) =>
  handle ->
  handle ->
  inputs ->
  (Either e v -> u (Either VerificationConditions ())) ->
  (inputs -> t) ->
  IO ([inputs], CEGISResult SolvingFailure)
solverCegisExceptVC synthesizerSolver verifierSolver inputs f v = do
  solverCegis synthesizerSolver verifierSolver inputs $ \i ->
    simpleMerge $
      ( \case
          Left AssumptionViolation -> cegisPrePost (con False) (con True)
          Left AssertionViolation -> cegisPostCond (con False)
          _ -> cegisPostCond (con True)
      )
        <$> (extractUnionExcept (v i) >>= f)

-- |
-- CEGIS for symbolic programs with error handling, using a single symbolic
-- input to represent a set of inputs. See 'cegisExceptStdVC' for more details.
--
-- The difference from 'cegisExceptStdVC' is that this function accepts two
-- solver handles, one for the synthesizer and one for the verifier.
--
-- The synthesizer solver will **not** be reset, while the verifier solver will
-- be reset after each iteration.
solverCegisExceptStdVC ::
  ( UnionWithExcept t u VerificationConditions (),
    UnionView u,
    Monad u,
    EvalSym inputs,
    ExtractSym inputs,
    Solver handle,
    SymEq inputs
  ) =>
  handle ->
  handle ->
  inputs ->
  (inputs -> t) ->
  IO ([inputs], CEGISResult SolvingFailure)
solverCegisExceptStdVC synthesizerSolver verifierSolver inputs =
  solverCegisExceptVC synthesizerSolver verifierSolver inputs return

-- |
-- CEGIS with a single symbolic input to represent a set of inputs. See
-- 'cegisForAll' for more details.
--
-- The difference from 'cegisForAll' is that this function accepts two solver
-- handles, one for the synthesizer and one for the verifier.
--
-- The synthesizer solver will **not** be reset, while the verifier solver will
-- be reset after each iteration.
solverCegisForAll ::
  ( ExtractSym forallInput,
    Solver handle
  ) =>
  handle ->
  handle ->
  -- | A symbolic value. All the symbolic constants in the value are treated as
  -- for-all variables.
  forallInput ->
  CEGISCondition ->
  -- | First output are the counter-examples for all the for-all variables, and
  -- the second output is the model for all other variables if CEGIS succeeds.
  IO ([Model], CEGISResult SolvingFailure)
solverCegisForAll
  synthesizerSolver
  verifierSolver
  input
  (CEGISCondition pre post) = do
    (models, result) <-
      solverGenericCEGIS
        synthesizerSolver
        False
        phi
        (\md -> return $ evalSym False md phi)
        [verifier]
    let exactResult = case result of
          CEGISSuccess model -> CEGISSuccess $ exceptFor forallSymbols model
          _ -> result
    return (models, exactResult)
    where
      phi = pre .&& post
      negphi = pre .&& symNot post
      forallSymbols = extractSym input
      verifier candidate = do
        let evaluated =
              evalSym False (exceptFor forallSymbols candidate) negphi
        solverResetAssertions verifierSolver
        r <- solverSolve verifierSolver evaluated
        case r of
          Left Unsat -> return $ CEGISVerifierNoCex True
          Left err -> return $ CEGISVerifierException err
          Right model ->
            return $ CEGISVerifierFoundCex (exact forallSymbols model)

-- |
-- CEGIS for symbolic programs with error handling, with a forall variable.
--
-- See 'cegisForAllExcept', 'cegisForAll' and 'cegisExcept'.
--
-- The difference from 'cegisForAllExcept' is that this function accepts two
-- solver handles, one for the synthesizer and one for the verifier.
--
-- The synthesizer solver will **not** be reset, while the verifier solver will
-- be reset after each iteration.
solverCegisForAllExcept ::
  ( UnionWithExcept t u e v,
    UnionView u,
    Functor u,
    EvalSym inputs,
    ExtractSym inputs,
    Solver handle,
    SymEq inputs
  ) =>
  handle ->
  handle ->
  inputs ->
  (Either e v -> CEGISCondition) ->
  t ->
  IO ([Model], CEGISResult SolvingFailure)
solverCegisForAllExcept synthesizerSolver verifierSolver inputs f v =
  solverCegisForAll synthesizerSolver verifierSolver inputs $
    simpleMerge $
      f <$> extractUnionExcept v

-- |
-- CEGIS for symbolic programs with error handling, with a forall variable.
--
-- See 'cegisForAllExceptVC' 'cegisForAll' and 'cegisExceptVC'.
--
-- The difference from 'cegisForAllExceptVC' is that this function accepts two
-- solver handles, one for the synthesizer and one for the verifier.
--
-- The synthesizer solver will **not** be reset, while the verifier solver will
-- be reset after each iteration.
solverCegisForAllExceptVC ::
  ( UnionWithExcept t u e v,
    UnionView u,
    Monad u,
    EvalSym inputs,
    ExtractSym inputs,
    Solver handle,
    SymEq inputs
  ) =>
  handle ->
  handle ->
  inputs ->
  (Either e v -> u (Either VerificationConditions ())) ->
  t ->
  IO ([Model], CEGISResult SolvingFailure)
solverCegisForAllExceptVC synthesizerSolver verifierSolver inputs f v = do
  solverCegisForAll synthesizerSolver verifierSolver inputs $
    simpleMerge $
      ( \case
          Left AssumptionViolation -> cegisPrePost (con False) (con True)
          Left AssertionViolation -> cegisPostCond (con False)
          _ -> cegisPostCond (con True)
      )
        <$> (extractUnionExcept v >>= f)

-- |
-- CEGIS for symbolic programs with error handling, with a forall variable.
--
-- See 'cegisForAllExceptStdVC' 'cegisForAll' and 'cegisExceptStdVC'.
--
-- The difference from 'cegisForAllExceptStdVC' is that this function accepts
-- two solver handles, one for the synthesizer and one for the verifier.
--
-- The synthesizer solver will **not** be reset, while the verifier solver will
-- be reset after each iteration.
solverCegisForAllExceptStdVC ::
  ( UnionWithExcept t u VerificationConditions (),
    UnionView u,
    Monad u,
    EvalSym inputs,
    ExtractSym inputs,
    Solver handle,
    SymEq inputs
  ) =>
  handle ->
  handle ->
  inputs ->
  t ->
  IO ([Model], CEGISResult SolvingFailure)
solverCegisForAllExceptStdVC synthesizerSolver verifierSolver inputs =
  solverCegisForAllExceptVC synthesizerSolver verifierSolver inputs return

-- |
-- CEGIS with multiple (possibly symbolic) inputs. Solves the following formula
-- (see t'CEGISCondition' for details).
--
-- \[
--   \forall P. (\exists I\in\mathrm{inputs}. \mathrm{pre}(P, I)) \wedge (\forall I\in\mathrm{inputs}. \mathrm{pre}(P, I)\implies \mathrm{post}(P, I))
-- \]
--
-- For simpler queries, where the inputs are representable by a single
-- symbolic value, you may want to use 'cegis' or 'cegisExcept' instead.
-- We have an example for the 'cegis' call.
cegisMultiInputs ::
  ( EvalSym input,
    ExtractSym input,
    ConfigurableSolver config handle
  ) =>
  -- | The configuration of the solver
  config ->
  -- | Initial symbolic inputs. The solver will try to find a
  -- program that works on all the inputs representable by these inputs (see
  -- t'CEGISCondition').
  [input] ->
  -- | The condition for the solver to solve. All the
  -- symbolic constants that are not in the inputs will
  -- be considered as part of the symbolic program.
  (input -> CEGISCondition) ->
  -- | The counter-examples generated
  -- during the CEGIS loop, and the
  -- model found by the solver.
  IO ([input], CEGISResult SolvingFailure)
cegisMultiInputs config inputs toCEGISCondition =
  withSolver config $ \synthesizerSolver ->
    withSolver config $ \verifierSolver ->
      solverCegisMultiInputs
        synthesizerSolver
        verifierSolver
        inputs
        toCEGISCondition

-- |
-- CEGIS with a single symbolic input to represent a set of inputs.
--
-- The following example tries to find the value of @c@ such that for all
-- positive @x@, @x * c < 0 && c > -2@. The @c .> -2@ clause is used to make
-- the solution unique.
--
-- >>> let [x,c] = ["x","c"] :: [SymInteger]
-- >>> cegis z3 x (\x -> cegisPrePost (x .> 0) (x * c .< 0 .&& c .> -2))
-- (...,CEGISSuccess (Model {c -> -1 :: Integer}))
cegis ::
  ( ConfigurableSolver config handle,
    EvalSym inputs,
    ExtractSym inputs,
    SymEq inputs
  ) =>
  -- | The configuration of the solver
  config ->
  -- | Initial symbolic inputs. The solver will try to find a
  -- program that works on all the inputs representable by it (see
  -- t'CEGISCondition').
  inputs ->
  -- | The condition for the solver to solve. All the
  -- symbolic constants that are not in the inputs will
  -- be considered as part of the symbolic program.
  (inputs -> CEGISCondition) ->
  -- | The counter-examples generated
  -- during the CEGIS loop, and the
  -- model found by the solver.
  IO ([inputs], CEGISResult SolvingFailure)
cegis config inputs condition =
  withSolver config $ \synthesizerSolver ->
    withSolver config $ \verifierSolver ->
      solverCegis synthesizerSolver verifierSolver inputs condition

-- |
-- CEGIS for symbolic programs with error handling, using multiple (possibly
-- symbolic) inputs to represent a set of inputs.
cegisExceptMultiInputs ::
  ( ConfigurableSolver config handle,
    EvalSym inputs,
    ExtractSym inputs,
    UnionWithExcept t u e v,
    UnionView u,
    Monad u
  ) =>
  config ->
  [inputs] ->
  (Either e v -> CEGISCondition) ->
  (inputs -> t) ->
  IO ([inputs], CEGISResult SolvingFailure)
cegisExceptMultiInputs config cexes interpretFun f =
  withSolver config $ \synthesizerSolver ->
    withSolver config $ \verifierSolver ->
      solverCegisExceptMultiInputs
        synthesizerSolver
        verifierSolver
        cexes
        interpretFun
        f

-- |
-- CEGIS for symbolic programs with error handling, using multiple (possibly
-- symbolic) inputs to represent a set of inputs.
--
-- The errors should be translated to assertion or assumption violations.
cegisExceptVCMultiInputs ::
  ( ConfigurableSolver config handle,
    EvalSym inputs,
    ExtractSym inputs,
    UnionWithExcept t u e v,
    UnionView u,
    Monad u
  ) =>
  config ->
  [inputs] ->
  (Either e v -> u (Either VerificationConditions ())) ->
  (inputs -> t) ->
  IO ([inputs], CEGISResult SolvingFailure)
cegisExceptVCMultiInputs config cexes interpretFun f =
  withSolver config $ \synthesizerSolver ->
    withSolver config $ \verifierSolver ->
      solverCegisExceptVCMultiInputs
        synthesizerSolver
        verifierSolver
        cexes
        interpretFun
        f

-- |
-- CEGIS for symbolic programs with error handling, using multiple (possibly
-- symbolic) inputs to represent a set of inputs. This function saves the
-- efforts to implement the translation function for the standard error type
-- 'VerificationConditions', and the standard result type @()@.
--
-- This function translates assumption violations to failed pre-conditions,
-- and translates assertion violations to failed post-conditions.
-- The @()@ result will not fail any conditions.
cegisExceptStdVCMultiInputs ::
  ( ConfigurableSolver config handle,
    EvalSym inputs,
    ExtractSym inputs,
    UnionWithExcept t u VerificationConditions (),
    UnionView u,
    Monad u
  ) =>
  config ->
  [inputs] ->
  (inputs -> t) ->
  IO ([inputs], CEGISResult SolvingFailure)
cegisExceptStdVCMultiInputs config cexes f =
  withSolver config $ \synthesizerSolver ->
    withSolver config $ \verifierSolver ->
      solverCegisExceptStdVCMultiInputs synthesizerSolver verifierSolver cexes f

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
-- >>> let [x,c] = ["x","c"] :: [SymInteger]
-- >>> import Control.Monad.Except
-- >>> :{
--   res :: SymInteger -> ExceptT VerificationConditions Union ()
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
-- >>> cegisExcept z3 x translation res
-- ([...],CEGISSuccess (Model {c -> -1 :: Integer}))
cegisExcept ::
  ( UnionWithExcept t u e v,
    UnionView u,
    Functor u,
    EvalSym inputs,
    ExtractSym inputs,
    ConfigurableSolver config handle,
    SymEq inputs
  ) =>
  config ->
  inputs ->
  (Either e v -> CEGISCondition) ->
  (inputs -> t) ->
  IO ([inputs], CEGISResult SolvingFailure)
cegisExcept config inputs f v =
  withSolver config $ \synthesizerSolver ->
    withSolver config $ \verifierSolver ->
      solverCegisExcept synthesizerSolver verifierSolver inputs f v

-- |
-- CEGIS for symbolic programs with error handling, using a single symbolic
-- input to represent a set of inputs.
--
-- The errors should be translated to assertion or assumption violations.
cegisExceptVC ::
  ( UnionWithExcept t u e v,
    UnionView u,
    Monad u,
    EvalSym inputs,
    ExtractSym inputs,
    ConfigurableSolver config handle,
    SymEq inputs
  ) =>
  config ->
  inputs ->
  (Either e v -> u (Either VerificationConditions ())) ->
  (inputs -> t) ->
  IO ([inputs], CEGISResult SolvingFailure)
cegisExceptVC config inputs f v =
  withSolver config $ \synthesizerSolver ->
    withSolver config $ \verifierSolver ->
      solverCegisExceptVC synthesizerSolver verifierSolver inputs f v

-- |
-- CEGIS for symbolic programs with error handling, using a single symbolic
-- input to represent a set of inputs. This function saves the efforts to
-- implement the translation function for the standard error type
-- 'VerificationConditions', and the standard result type @()@.
--
-- This function translates assumption violations to failed pre-conditions,
-- and translates assertion violations to failed post-conditions.
-- The @()@ result will not fail any conditions.
--
-- The following example tries to find the value of @c@ such that for all
-- positive @x@, @x * c < 0 && c > -2@. The @c .> -2@ assertion is used to make
-- the solution unique.
--
-- >>> let [x,c] = ["x","c"] :: [SymInteger]
-- >>> import Control.Monad.Except
-- >>> :{
--   res :: SymInteger -> ExceptT VerificationConditions Union ()
--   res x = do
--     symAssume $ x .> 0
--     symAssert $ x * c .< 0
--     symAssert $ c .> -2
-- :}
--
-- >>> cegisExceptStdVC z3 x res
-- ([...],CEGISSuccess (Model {c -> -1 :: Integer}))
cegisExceptStdVC ::
  ( UnionWithExcept t u VerificationConditions (),
    UnionView u,
    Monad u,
    EvalSym inputs,
    ExtractSym inputs,
    ConfigurableSolver config handle,
    SymEq inputs
  ) =>
  config ->
  inputs ->
  (inputs -> t) ->
  IO ([inputs], CEGISResult SolvingFailure)
cegisExceptStdVC config inputs f =
  withSolver config $ \synthesizerSolver ->
    withSolver config $ \verifierSolver ->
      solverCegisExceptStdVC synthesizerSolver verifierSolver inputs f

-- |
-- CEGIS with a single symbolic input to represent a set of inputs.
--
-- The following example tries to find the value of @c@ such that for all
-- positive @x@, @x * c < 0 && c > -2@. The @c .> -2@ clause is used to make
-- the solution unique.
--
-- >>> let [x,c] = ["x","c"] :: [SymInteger]
-- >>> cegisForAll z3 x $ cegisPrePost (x .> 0) (x * c .< 0 .&& c .> -2)
-- (...,CEGISSuccess (Model {c -> -1 :: Integer}))
cegisForAll ::
  ( ExtractSym forallInput,
    ConfigurableSolver config handle
  ) =>
  config ->
  -- | A symbolic value. All the symbolic constants in the value are treated as
  -- for-all variables.
  forallInput ->
  CEGISCondition ->
  -- | First output are the counter-examples for all the for-all variables, and
  -- the second output is the model for all other variables if CEGIS succeeds.
  IO ([Model], CEGISResult SolvingFailure)
cegisForAll config input (CEGISCondition pre post) =
  withSolver config $ \synthesizerSolver ->
    withSolver config $ \verifierSolver ->
      solverCegisForAll
        synthesizerSolver
        verifierSolver
        input
        (CEGISCondition pre post)

-- |
-- CEGIS for symbolic programs with error handling, with a forall variable.
--
-- See 'cegisForAll' and 'cegisExcept'.
cegisForAllExcept ::
  ( UnionWithExcept t u e v,
    UnionView u,
    Functor u,
    EvalSym inputs,
    ExtractSym inputs,
    ConfigurableSolver config handle,
    SymEq inputs
  ) =>
  config ->
  inputs ->
  (Either e v -> CEGISCondition) ->
  t ->
  IO ([Model], CEGISResult SolvingFailure)
cegisForAllExcept config inputs f v =
  withSolver config $ \synthesizerSolver ->
    withSolver config $ \verifierSolver ->
      solverCegisForAllExcept synthesizerSolver verifierSolver inputs f v

-- |
-- CEGIS for symbolic programs with error handling, with a forall variable.
--
-- See 'cegisForAll' and 'cegisExceptVC'.
cegisForAllExceptVC ::
  ( UnionWithExcept t u e v,
    UnionView u,
    Monad u,
    EvalSym inputs,
    ExtractSym inputs,
    ConfigurableSolver config handle,
    SymEq inputs
  ) =>
  config ->
  inputs ->
  (Either e v -> u (Either VerificationConditions ())) ->
  t ->
  IO ([Model], CEGISResult SolvingFailure)
cegisForAllExceptVC config inputs f v =
  withSolver config $ \synthesizerSolver ->
    withSolver config $ \verifierSolver ->
      solverCegisForAllExceptVC synthesizerSolver verifierSolver inputs f v

-- |
-- CEGIS for symbolic programs with error handling, with a forall variable.
--
-- See 'cegisForAll' and 'cegisExceptStdVC'.
cegisForAllExceptStdVC ::
  ( UnionWithExcept t u VerificationConditions (),
    UnionView u,
    Monad u,
    EvalSym inputs,
    ExtractSym inputs,
    ConfigurableSolver config handle,
    SymEq inputs
  ) =>
  config ->
  inputs ->
  t ->
  IO ([Model], CEGISResult SolvingFailure)
cegisForAllExceptStdVC config inputs u =
  withSolver config $ \synthesizerSolver ->
    withSolver config $ \verifierSolver ->
      solverCegisForAllExceptStdVC synthesizerSolver verifierSolver inputs u
