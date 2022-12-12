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
    CEGISFormulas (..),
    cegisPostOnly,
    cegisPrePost,
    cegisFormulas,
    cegisFallable,
    cegisFallable',
    cegisFallableGenForalls,
    cegisFallableGenForalls',
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

data CEGISFormulas bool = CEGISFormulas bool bool deriving (Generic)

cegisPostOnly :: SymBoolOp bool => bool -> CEGISFormulas bool
cegisPostOnly = CEGISFormulas (conc True)

cegisPrePost :: SymBoolOp bool => bool -> bool -> CEGISFormulas bool
cegisPrePost = CEGISFormulas

deriving via (Default (CEGISFormulas bool)) instance SymBoolOp bool => GMergeable bool (CEGISFormulas bool)

deriving via (Default (CEGISFormulas bool)) instance SymBoolOp bool => GSimpleMergeable bool (CEGISFormulas bool)

class
  (SymBoolOp bool, GEvaluateSym model bool) =>
  CEGISSolver config bool symbolSet failure model
    | config -> bool symbolSet failure model
  where
  cegisFormulasGenForalls ::
    (GEvaluateSym model inputs, GExtractSymbolics symbolSet inputs, GenSymSimple spec inputs) =>
    config ->
    [spec] ->
    [inputs] ->
    (inputs -> CEGISFormulas bool) ->
    IO (Either failure ([inputs], model))

cegisFormulas ::
  ( CEGISSolver config bool symbolSet failure model,
    GEvaluateSym model forallArg,
    GExtractSymbolics symbolSet forallArg
  ) =>
  config ->
  forallArg ->
  CEGISFormulas bool ->
  IO (Either failure ([forallArg], model))
cegisFormulas config forallArg formula = do
  r <-
    cegisFormulasGenForalls
      config
      [IdGen forallArg]
      []
      (\(_ :: IdGen forallArg) -> formula)
  case r of
    Left f -> return $ Left f
    Right (cexes, mo) -> return $ Right (runIdGen <$> cexes, mo)

cegisFallableGenForalls ::
  ( CEGISSolver config bool symbolSet failure model,
    GEvaluateSym model inputs,
    GExtractSymbolics symbolSet inputs,
    GenSymSimple spec inputs,
    ExtractUnionEither t u e v,
    GUnionPrjOp bool u,
    Monad u,
    SymBoolOp bool
  ) =>
  config ->
  [spec] ->
  [inputs] ->
  (Either e v -> CEGISFormulas bool) ->
  (inputs -> t) ->
  IO (Either failure ([inputs], model))
cegisFallableGenForalls config inputGen cexes interpretFunc f =
  cegisFormulasGenForalls config inputGen cexes (getSingle . (interpretFunc <$>) . extractUnionEither . f)

cegisFallableGenForalls' ::
  ( CEGISSolver config bool symbolSet failure model,
    GEvaluateSym model inputs,
    GExtractSymbolics symbolSet inputs,
    GenSymSimple spec inputs,
    ExtractUnionEither t u e v,
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
cegisFallableGenForalls' config inputGen cexes interpretFunc f =
  cegisFormulasGenForalls
    config
    inputGen
    cexes
    ( \v ->
        getSingle
          ( ( \case
                Left AssumptionViolation -> cegisPrePost (conc False) (conc True)
                Left AssertionViolation -> cegisPostOnly (conc False)
                _ -> cegisPostOnly (conc True)
            )
              <$> (extractUnionEither (f v) >>= interpretFunc)
          )
    )

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
  (Either e v -> CEGISFormulas bool) ->
  t ->
  IO (Either failure ([forallArgs], model))
cegisFallable config args f v = cegisFormulas config args $ getSingle $ f <$> extractUnionEither v

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
  cegisFormulas config args $
    getSingle $
      ( \case
          Left AssumptionViolation -> cegisPrePost (conc False) (conc True)
          Left AssertionViolation -> cegisPostOnly (conc False)
          _ -> cegisPostOnly (conc True)
      )
        <$> (extractUnionEither v >>= f)

newtype IdGen x = IdGen {runIdGen :: x}

instance GEvaluateSym model x => GEvaluateSym model (IdGen x) where
  gevaluateSym fillDefault model (IdGen v) = IdGen (gevaluateSym fillDefault model v)

instance GExtractSymbolics set x => GExtractSymbolics set (IdGen x) where
  gextractSymbolics (IdGen v) = gextractSymbolics v

instance GenSymSimple (IdGen x) (IdGen x) where
  simpleFresh = return
