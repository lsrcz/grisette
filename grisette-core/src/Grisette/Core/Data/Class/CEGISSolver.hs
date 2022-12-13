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
    cegisExceptGenInputs,
    cegisExceptVCGenInputs,
  )
where

import GHC.Generics
import Generics.Deriving
import Grisette.Core.Control.Exception
import Grisette.Core.Data.Class.Bool
import Grisette.Core.Data.Class.Evaluate
import Grisette.Core.Data.Class.ExtractSymbolics
import Grisette.Core.Data.Class.GenSym
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
  cegisGenInputs ::
    (GEvaluateSym model inputs, GExtractSymbolics symbolSet inputs, GenSymSimple spec inputs) =>
    config ->
    [spec] ->
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
cegis config forallArg formula = do
  r <-
    cegisGenInputs
      config
      [IdGen forallArg]
      []
      (\(_ :: IdGen forallArg) -> formula)
  case r of
    Left f -> return $ Left f
    Right (cexes, mo) -> return $ Right (runIdGen <$> cexes, mo)

cegisExceptGenInputs ::
  ( CEGISSolver config bool symbolSet failure model,
    GEvaluateSym model inputs,
    GExtractSymbolics symbolSet inputs,
    GenSymSimple spec inputs,
    UnionWithExcept t u e v,
    GUnionPrjOp bool u,
    Monad u,
    SymBoolOp bool
  ) =>
  config ->
  [spec] ->
  [inputs] ->
  (Either e v -> CEGISCondition bool) ->
  (inputs -> t) ->
  IO (Either failure ([inputs], model))
cegisExceptGenInputs config inputGen cexes interpretFunc f =
  cegisGenInputs config inputGen cexes (getSingle . (interpretFunc <$>) . extractUnionExcept . f)

cegisExceptVCGenInputs ::
  ( CEGISSolver config bool symbolSet failure model,
    GEvaluateSym model inputs,
    GExtractSymbolics symbolSet inputs,
    GenSymSimple spec inputs,
    UnionWithExcept t u e v,
    GUnionPrjOp bool u,
    Monad u,
    SymBoolOp bool
  ) =>
  config ->
  [spec] ->
  [inputs] ->
  (Either e v -> u (Either VerificationConditions ())) ->
  (inputs -> t) ->
  IO (Either failure ([inputs], model))
cegisExceptVCGenInputs config inputGen cexes interpretFunc f =
  cegisGenInputs
    config
    inputGen
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
cegisExcept config args f v = cegis config args $ getSingle $ f <$> extractUnionExcept v

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
    getSingle $
      ( \case
          Left AssumptionViolation -> cegisPrePost (conc False) (conc True)
          Left AssertionViolation -> cegisPostCond (conc False)
          _ -> cegisPostCond (conc True)
      )
        <$> (extractUnionExcept v >>= f)

newtype IdGen x = IdGen {runIdGen :: x}

instance GEvaluateSym model x => GEvaluateSym model (IdGen x) where
  gevaluateSym fillDefault model (IdGen v) = IdGen (gevaluateSym fillDefault model v)

instance GExtractSymbolics set x => GExtractSymbolics set (IdGen x) where
  gextractSymbolics (IdGen v) = gextractSymbolics v

instance GenSymSimple (IdGen x) (IdGen x) where
  simpleFresh = return
