{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}

module Grisette.Core.Data.Class.CEGISSolver
  ( CEGISSolver (..),
    CEGISCondition (..),
    cegisPostCond,
    cegisPrePost,
    cegis,
    cegisExcept,
    cegisExceptVC,
    cegisExceptMultiInputs,
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
import Grisette.Core.Data.Class.PrimWrapper
import Grisette.Core.Data.Class.SimpleMergeable
import Grisette.Core.Data.Class.Solver

data CEGISCondition bool = CEGISCondition bool bool deriving (Generic)

cegisPostCond :: SymBoolOp bool => bool -> CEGISCondition bool
cegisPostCond = CEGISCondition (conc True)

cegisPrePost :: SymBoolOp bool => bool -> bool -> CEGISCondition bool
cegisPrePost = CEGISCondition

deriving via (Default (CEGISCondition bool)) instance SymBoolOp bool => GMergeable bool (CEGISCondition bool)

deriving via (Default (CEGISCondition bool)) instance SymBoolOp bool => GSimpleMergeable bool (CEGISCondition bool)

class
  (SymBoolOp bool, GEvaluateSym model bool) =>
  CEGISSolver config bool symbolSet failure model
    | config -> bool symbolSet failure model
  where
  cegisMultiInputs ::
    (GEvaluateSym model inputs, GExtractSymbolics symbolSet inputs) =>
    config ->
    [inputs] ->
    (inputs -> CEGISCondition bool) ->
    IO (Either failure ([inputs], model))

cegis ::
  ( CEGISSolver config bool symbolSet failure model,
    GEvaluateSym model forallArg,
    GExtractSymbolics symbolSet forallArg
  ) =>
  config ->
  forallArg ->
  CEGISCondition bool ->
  IO (Either failure ([forallArg], model))
cegis config forallArg cond = cegisMultiInputs config [forallArg] (const cond)

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
  cegisMultiInputs config cexes (simpleMerge . (interpretFunc <$>) . extractUnionExcept . f)

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
        simpleMerge
          ( ( \case
                Left AssumptionViolation -> cegisPrePost (conc False) (conc True)
                Left AssertionViolation -> cegisPostCond (conc False)
                _ -> cegisPostCond (conc True)
            )
              <$> (extractUnionExcept (f v) >>= interpretFunc)
          )
    )

cegisExcept ::
  ( UnionWithExcept t u e v,
    GUnionPrjOp bool u,
    Functor u,
    SymBoolOp bool,
    GEvaluateSym model forallArgs,
    GExtractSymbolics symbolSet forallArgs,
    CEGISSolver config bool symbolSet failure model
  ) =>
  config ->
  forallArgs ->
  (Either e v -> CEGISCondition bool) ->
  t ->
  IO (Either failure ([forallArgs], model))
cegisExcept config args f v = cegis config args $ simpleMerge $ f <$> extractUnionExcept v

cegisExceptVC ::
  ( UnionWithExcept t u e v,
    GUnionPrjOp bool u,
    Monad u,
    SymBoolOp bool,
    GEvaluateSym model forallArgs,
    GExtractSymbolics symbolSet forallArgs,
    CEGISSolver config bool symbolSet failure model
  ) =>
  config ->
  forallArgs ->
  (Either e v -> u (Either VerificationConditions ())) ->
  t ->
  IO (Either failure ([forallArgs], model))
cegisExceptVC config args f v =
  cegis config args $
    simpleMerge $
      ( \case
          Left AssumptionViolation -> cegisPrePost (conc False) (conc True)
          Left AssertionViolation -> cegisPostCond (conc False)
          _ -> cegisPostCond (conc True)
      )
        <$> (extractUnionExcept v >>= f)
