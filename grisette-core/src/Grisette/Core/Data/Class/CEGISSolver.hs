{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}

-- |
-- Module      :   Grisette.Core.Data.Class.CEGISSolver
-- Copyright   :   (c) Sirui Lu 2021-2022
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Core.Data.Class.CEGISSolver
  ( -- * Note for the examples

    --

    -- | This module does not contain the implementation for symbolic primitive
    -- types, and the examples in this module rely on the implementations in
    -- the [grisette-symir](https://hackage.haskell.org/package/grisette-symir/)
    -- and [grisette-backend-sbv](https://hackage.haskell.org/package/grisette-backend-sbv) packages,
    -- which provides the solvable type and the solver backend implementations,
    -- respectively.
    --
    -- The examples also assumes a z3 solver available in @PATH@.

    -- * CEGIS solver interfaces
    CEGISSolver (..),
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
  )
where

import GHC.Generics
import Generics.Deriving
import Grisette.Core.Control.Exception
import Grisette.Core.Data.Class.Bool
import Grisette.Core.Data.Class.Evaluate
import Grisette.Core.Data.Class.ExtractSymbolics
import Grisette.Core.Data.Class.Mergeable
import Grisette.Core.Data.Class.SimpleMergeable
import Grisette.Core.Data.Class.Solvable
import Grisette.Core.Data.Class.Solver

-- $setup
-- >>> import Grisette.Core
-- >>> import Grisette.Lib.Base
-- >>> import Grisette.IR.SymPrim
-- >>> import Grisette.Backend.SBV

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
data CEGISCondition bool = CEGISCondition bool bool deriving (Generic)

-- | Construct a CEGIS condition with only a post-condition. The pre-condition
-- would be set to true, meaning that all programs in the program space are
-- allowed.
cegisPostCond :: SymBoolOp bool => bool -> CEGISCondition bool
cegisPostCond = CEGISCondition (conc True)

-- | Construct a CEGIS condition with both pre- and post-conditions.
cegisPrePost :: SymBoolOp bool => bool -> bool -> CEGISCondition bool
cegisPrePost = CEGISCondition

deriving via (Default (CEGISCondition bool)) instance SymBoolOp bool => GMergeable bool (CEGISCondition bool)

deriving via (Default (CEGISCondition bool)) instance SymBoolOp bool => GSimpleMergeable bool (CEGISCondition bool)

-- | Counter-example guided inductive synthesis (CEGIS) solver interface.
class
  (SymBoolOp bool, GEvaluateSym model bool) =>
  CEGISSolver config bool symbolSet failure model
    | config -> bool symbolSet failure model
  where
  -- |
  -- CEGIS with multiple (possibly symbolic) inputs. Solves the following formula (see
  -- 'CEGISCondition' for details).
  --
  -- \[
  --   \forall P. (\exists I\in\mathrm{inputs}. \mathrm{pre}(P, I)) \wedge (\forall I\in\mathrm{inputs}. \mathrm{pre}(P, I)\implies \mathrm{post}(P, I))
  -- \]
  --
  -- For simpler queries, where the inputs are representable by a single
  -- symbolic value, you may want to use 'cegis' or 'cegisExcept' instead.
  -- We have an example for the 'cegis' call.
  cegisMultiInputs ::
    (GEvaluateSym model inputs, GExtractSymbolics symbolSet inputs) =>
    -- | The configuration of the solver
    config ->
    -- | Some initial counter-examples. Providing some concrete
    -- inputs may help the solver to find a model faster. Providing
    -- symbolic inputs would cause the solver to find the program
    -- that works on all the inputs representable by it (see
    -- 'CEGISCondition').
    [inputs] ->
    -- | The function mapping the inputs to
    -- the conditions for the solver to
    -- solve.
    (inputs -> CEGISCondition bool) ->
    -- | The counter-examples generated
    -- during the CEGIS loop, and the
    -- model found by the solver.
    IO (Either failure ([inputs], model))

-- |
-- CEGIS with a single symbolic input to represent a set of inputs.
--
-- The following example tries to find the value of @c@ such that for all
-- positive @x@, @x * c < 0 && c > -2@. The @c >~ -2@ clause is used to make
-- the solution unique.
--
-- >>> :set -XOverloadedStrings
-- >>> let [x,c] = ["x","c"] :: [SymInteger]
-- >>> cegis (UnboundedReasoning z3) x (cegisPrePost (x >~ 0) (x * c <~ 0 &&~ c >~ -2))
-- Right ([],Model {c -> -1 :: Integer})
cegis ::
  ( CEGISSolver config bool symbolSet failure model,
    GEvaluateSym model inputs,
    GExtractSymbolics symbolSet inputs
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
  CEGISCondition bool ->
  -- | The counter-examples generated
  -- during the CEGIS loop, and the
  -- model found by the solver.
  IO (Either failure ([inputs], model))
cegis config inputs cond = cegisMultiInputs config [inputs] (const cond)

-- |
-- CEGIS for symbolic programs with error handling, using multiple (possibly
-- symbolic) inputs to represent a set of inputs.
cegisExceptMultiInputs ::
  ( CEGISSolver config bool symbolSet failure model,
    GEvaluateSym model inputs,
    GExtractSymbolics symbolSet inputs,
    UnionWithExcept t u e v,
    GUnionPrjOp bool u,
    Monad u,
    SymBoolOp bool
  ) =>
  config ->
  [inputs] ->
  (Either e v -> CEGISCondition bool) ->
  (inputs -> t) ->
  IO (Either failure ([inputs], model))
cegisExceptMultiInputs config cexes interpretFunc f =
  cegisMultiInputs config cexes (getSingle . (interpretFunc <$>) . extractUnionExcept . f)

-- |
-- CEGIS for symbolic programs with error handling, using multiple (possibly
-- symbolic) inputs to represent a set of inputs.
--
-- The errors should be translated to assertion or assumption violations.
cegisExceptVCMultiInputs ::
  ( CEGISSolver config bool symbolSet failure model,
    GEvaluateSym model inputs,
    GExtractSymbolics symbolSet inputs,
    UnionWithExcept t u e v,
    GUnionPrjOp bool u,
    Monad u,
    SymBoolOp bool
  ) =>
  config ->
  [inputs] ->
  (Either e v -> u (Either VerificationConditions ())) ->
  (inputs -> t) ->
  IO (Either failure ([inputs], model))
cegisExceptVCMultiInputs config cexes interpretFunc f =
  cegisMultiInputs
    config
    cexes
    ( \v ->
        getSingle
          ( ( \case
                Left AssumptionViolation -> cegisPrePost (conc False) (conc True)
                Left AssertionViolation -> cegisPostCond (conc False)
                _ -> cegisPostCond (conc True)
            )
              <$> (extractUnionExcept (f v) >>= interpretFunc)
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
  ( CEGISSolver config bool symbolSet failure model,
    GEvaluateSym model inputs,
    GExtractSymbolics symbolSet inputs,
    UnionWithExcept t u VerificationConditions (),
    GUnionPrjOp bool u,
    Monad u,
    SymBoolOp bool
  ) =>
  config ->
  [inputs] ->
  (inputs -> t) ->
  IO (Either failure ([inputs], model))
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
-- positive @x@, @x * c < 0 && c > -2@. The @c >~ -2@ assertion is used to make
-- the solution unique.
--
-- >>> :set -XOverloadedStrings
-- >>> let [x,c] = ["x","c"] :: [SymInteger]
-- >>> import Control.Monad.Except
-- >>> :{
--   res :: ExceptT VerificationConditions UnionM ()
--   res = do
--     symAssume $ x >~ 0
--     symAssert $ x * c <~ 0
--     symAssert $ c >~ -2
-- :}
--
-- >>> :{
--   translation (Left AssumptionViolation) = cegisPrePost (conc False) (conc True)
--   translation (Left AssertionViolation) = cegisPostCond (conc False)
--   translation _ = cegisPostCond (conc True)
-- :}
--
-- >>> cegisExcept (UnboundedReasoning z3) x translation res
-- Right ([],Model {c -> -1 :: Integer})
cegisExcept ::
  ( UnionWithExcept t u e v,
    GUnionPrjOp bool u,
    Functor u,
    SymBoolOp bool,
    GEvaluateSym model inputs,
    GExtractSymbolics symbolSet inputs,
    CEGISSolver config bool symbolSet failure model
  ) =>
  config ->
  inputs ->
  (Either e v -> CEGISCondition bool) ->
  t ->
  IO (Either failure ([inputs], model))
cegisExcept config inputs f v = cegis config inputs $ getSingle $ f <$> extractUnionExcept v

-- |
-- CEGIS for symbolic programs with error handling, using a single symbolic
-- input to represent a set of inputs.
--
-- The errors should be translated to assertion or assumption violations.
cegisExceptVC ::
  ( UnionWithExcept t u e v,
    GUnionPrjOp bool u,
    Monad u,
    SymBoolOp bool,
    GEvaluateSym model inputs,
    GExtractSymbolics symbolSet inputs,
    CEGISSolver config bool symbolSet failure model
  ) =>
  config ->
  inputs ->
  (Either e v -> u (Either VerificationConditions ())) ->
  t ->
  IO (Either failure ([inputs], model))
cegisExceptVC config inputs f v =
  cegis config inputs $
    getSingle $
      ( \case
          Left AssumptionViolation -> cegisPrePost (conc False) (conc True)
          Left AssertionViolation -> cegisPostCond (conc False)
          _ -> cegisPostCond (conc True)
      )
        <$> (extractUnionExcept v >>= f)

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
-- positive @x@, @x * c < 0 && c > -2@. The @c >~ -2@ assertion is used to make
-- the solution unique.
--
-- >>> :set -XOverloadedStrings
-- >>> let [x,c] = ["x","c"] :: [SymInteger]
-- >>> import Control.Monad.Except
-- >>> :{
--   res :: ExceptT VerificationConditions UnionM ()
--   res = do
--     symAssume $ x >~ 0
--     symAssert $ x * c <~ 0
--     symAssert $ c >~ -2
-- :}
--
-- >>> cegisExceptStdVC (UnboundedReasoning z3) x res
-- Right ([],Model {c -> -1 :: Integer})
cegisExceptStdVC ::
  ( UnionWithExcept t u VerificationConditions (),
    GUnionPrjOp bool u,
    Monad u,
    SymBoolOp bool,
    GEvaluateSym model inputs,
    GExtractSymbolics symbolSet inputs,
    CEGISSolver config bool symbolSet failure model
  ) =>
  config ->
  inputs ->
  t ->
  IO (Either failure ([inputs], model))
cegisExceptStdVC config inputs = cegisExceptVC config inputs return
