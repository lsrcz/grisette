{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module      :   Grisette.Internal.TH.GADT.UnifiedOpCommon
-- Copyright   :   (c) Sirui Lu 2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Internal.TH.GADT.UnifiedOpCommon
  ( UnaryOpUnifiedConfig (..),
    defaultUnaryOpUnifiedFun,
  )
where

import Grisette.Internal.TH.GADT.Common (DeriveConfig (evalModeConfig))
import Grisette.Internal.TH.GADT.UnaryOpCommon
  ( UnaryOpFunConfig (genUnaryOpFun),
  )
import Grisette.Internal.Unified.Util (withMode)
import Language.Haskell.TH
  ( Exp (VarE),
    Kind,
    Name,
    Q,
    Type (AppT, ArrowT, StarT, VarT),
    appE,
    clause,
    funD,
    newName,
    normalB,
    varE,
    varP,
  )

-- | Default implementation for the derivation rules for a unified operation.
defaultUnaryOpUnifiedFun :: [Name] -> Type -> (Type, Kind) -> Q (Maybe Exp)
defaultUnaryOpUnifiedFun funNames modeTy (ty, kind) =
  case kind of
    StarT ->
      Just
        <$> [|
          $(varE $ head funNames) @($(return modeTy))
            @($(return ty))
          |]
    AppT (AppT ArrowT StarT) StarT ->
      Just
        <$> [|
          $(varE $ funNames !! 1) @($(return modeTy))
            @($(return ty))
          |]
    AppT (AppT (AppT ArrowT StarT) StarT) StarT ->
      Just
        <$> [|
          $(varE $ funNames !! 2) @($(return modeTy))
            @($(return ty))
          |]
    _ -> return Nothing

-- | Configuration for the derivation rules for a unified operation.
newtype UnaryOpUnifiedConfig = UnaryOpUnifiedConfig
  {unifiedFun :: Type -> (Type, Kind) -> Q (Maybe Exp)}

instance UnaryOpFunConfig UnaryOpUnifiedConfig where
  genUnaryOpFun
    deriveConfig
    (UnaryOpUnifiedConfig {..})
    funNames
    n
    extraVars
    keptTypes
    _
    isVarUsedInFields
    _ = do
      modeTy <- case evalModeConfig deriveConfig of
        [] -> return $ fst $ head extraVars
        [(i, _)] -> return $ fst $ keptTypes !! i
        _ -> fail "Unified classes does not support multiple evaluation modes"
      let isTypeUsedInFields (VarT nm) = isVarUsedInFields nm
          isTypeUsedInFields _ = False
      exprs <-
        traverse (unifiedFun modeTy) $
          filter (isTypeUsedInFields . fst) keptTypes
      rVar <- newName "r"
      let rf =
            foldl
              ( \exp nextFun -> case nextFun of
                  Nothing -> exp
                  Just fun -> appE (return fun) exp
              )
              (return $ VarE rVar)
              exprs
      let instanceFunName = funNames !! n
      funD
        instanceFunName
        [ clause
            [varP rVar]
            ( normalB
                [|
                  withMode @($(return modeTy)) $(rf) $(rf)
                  |]
            )
            []
        ]
