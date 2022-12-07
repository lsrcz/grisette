{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}

module Grisette.Core.Data.Class.CEGISSolver
  ( CEGISSolver (..),
    cegisFallable,
    cegisFallable',
  )
where

import Grisette.Core.Control.Exception
import Grisette.Core.Data.Class.Bool
import Grisette.Core.Data.Class.Evaluate
import Grisette.Core.Data.Class.ExtractSymbolics
import Grisette.Core.Data.Class.PrimWrapper
import Grisette.Core.Data.Class.SimpleMergeable
import Grisette.Core.Data.Class.Solver

class
  (SymBoolOp bool, GEvaluateSym model bool) =>
  CEGISSolver config bool symbolSet failure model
    | config -> bool symbolSet failure model
  where
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

cegisFallable ::
  ( ExtractUnionEither t u e v,
    GUnionPrjOp bool u,
    Functor u,
    SymBoolOp bool,
    GEvaluateSym model forallArgs,
    GExtractSymbolics symbolSet forallArgs,
    CEGISSolver config bool symbolSet failure model
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
    CEGISSolver config bool symbolSet failure model
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
