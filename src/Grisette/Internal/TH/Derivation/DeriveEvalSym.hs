{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Unused LANGUAGE pragma" #-}

-- |
-- Module      :   Grisette.Internal.TH.Derivation.DeriveEvalSym
-- Copyright   :   (c) Sirui Lu 2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Internal.TH.Derivation.DeriveEvalSym
  ( deriveEvalSym,
    deriveEvalSym1,
    deriveEvalSym2,
  )
where

import Grisette.Internal.Internal.Decl.Core.Data.Class.EvalSym
  ( EvalSym (evalSym),
    EvalSym1 (liftEvalSym),
    EvalSym2 (liftEvalSym2),
  )
import Grisette.Internal.TH.Derivation.Common (DeriveConfig)
import Grisette.Internal.TH.Derivation.UnaryOpCommon
  ( UnaryOpClassConfig
      ( UnaryOpClassConfig,
        unaryOpAllowExistential,
        unaryOpConfigs,
        unaryOpContextNames,
        unaryOpExtraVars,
        unaryOpInstanceNames,
        unaryOpInstanceTypeFromConfig
      ),
    UnaryOpConfig (UnaryOpConfig),
    UnaryOpFieldConfig
      ( UnaryOpFieldConfig,
        extraLiftedPatNames,
        extraPatNames,
        fieldCombineFun,
        fieldFunExp,
        fieldResFun
      ),
    defaultFieldFunExp,
    defaultFieldResFun,
    defaultUnaryOpInstanceTypeFromConfig,
    genUnaryOpClass,
  )
import Language.Haskell.TH
  ( Dec,
    Exp (AppE, ConE),
    Name,
    Q,
  )

evalSymConfig :: UnaryOpClassConfig
evalSymConfig =
  UnaryOpClassConfig
    { unaryOpConfigs =
        [ UnaryOpConfig
            UnaryOpFieldConfig
              { extraPatNames = ["fillDefault", "model"],
                extraLiftedPatNames = const [],
                fieldResFun = defaultFieldResFun,
                fieldCombineFun = \_ _ _ con extraPat exp -> do
                  return (foldl AppE (ConE con) exp, False <$ extraPat),
                fieldFunExp =
                  defaultFieldFunExp ['evalSym, 'liftEvalSym, 'liftEvalSym2]
              }
            ['evalSym, 'liftEvalSym, 'liftEvalSym2]
        ],
      unaryOpInstanceNames =
        [''EvalSym, ''EvalSym1, ''EvalSym2],
      unaryOpExtraVars = const $ return [],
      unaryOpInstanceTypeFromConfig = defaultUnaryOpInstanceTypeFromConfig,
      unaryOpAllowExistential = True,
      unaryOpContextNames = Nothing
    }

-- | Derive 'EvalSym' instance for a data type.
deriveEvalSym :: DeriveConfig -> Name -> Q [Dec]
deriveEvalSym deriveConfig = genUnaryOpClass deriveConfig evalSymConfig 0

-- | Derive 'EvalSym1' instance for a data type.
deriveEvalSym1 :: DeriveConfig -> Name -> Q [Dec]
deriveEvalSym1 deriveConfig = genUnaryOpClass deriveConfig evalSymConfig 1

-- | Derive 'EvalSym2' instance for a data type.
deriveEvalSym2 :: DeriveConfig -> Name -> Q [Dec]
deriveEvalSym2 deriveConfig = genUnaryOpClass deriveConfig evalSymConfig 2
