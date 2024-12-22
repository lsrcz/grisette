{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Unused LANGUAGE pragma" #-}

-- |
-- Module      :   Grisette.Internal.TH.GADT.DeriveSubstSym
-- Copyright   :   (c) Sirui Lu 2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Internal.TH.GADT.DeriveSubstSym
  ( deriveGADTSubstSym,
    deriveGADTSubstSym1,
    deriveGADTSubstSym2,
  )
where

import Grisette.Internal.Core.Data.Class.SubstSym
  ( SubstSym (substSym),
    SubstSym1 (liftSubstSym),
    SubstSym2 (liftSubstSym2),
  )
import Grisette.Internal.TH.GADT.Common (DeriveConfig)
import Grisette.Internal.TH.GADT.UnaryOpCommon
  ( UnaryOpClassConfig
      ( UnaryOpClassConfig,
        unaryOpFieldConfigs,
        unaryOpInstanceNames
      ),
    UnaryOpFieldConfig
      ( UnaryOpFieldConfig,
        extraLiftedPatNames,
        extraPatNames,
        fieldCombineFun,
        fieldFunExp,
        fieldFunNames,
        fieldResFun
      ),
    defaultFieldFunExp,
    defaultFieldResFun,
    genUnaryOpClass,
  )
import Language.Haskell.TH (Dec, Exp (AppE, ConE), Name)
import Language.Haskell.TH.Syntax (Q)

substSymConfig :: UnaryOpClassConfig
substSymConfig =
  UnaryOpClassConfig
    { unaryOpFieldConfigs =
        [ UnaryOpFieldConfig
            { extraPatNames = ["symbol", "newVal"],
              extraLiftedPatNames = const [],
              fieldResFun = defaultFieldResFun,
              fieldCombineFun = \_ _ con extraPat exp ->
                return (foldl AppE (ConE con) exp, False <$ extraPat),
              fieldFunExp =
                defaultFieldFunExp
                  ['substSym, 'liftSubstSym, 'liftSubstSym2],
              fieldFunNames =
                ['substSym, 'liftSubstSym, 'liftSubstSym2]
            }
        ],
      unaryOpInstanceNames =
        [''SubstSym, ''SubstSym1, ''SubstSym2]
    }

-- | Derive 'SubstSym' instance for a GADT.
deriveGADTSubstSym :: DeriveConfig -> Name -> Q [Dec]
deriveGADTSubstSym deriveConfig = genUnaryOpClass deriveConfig substSymConfig 0

-- | Derive 'SubstSym1' instance for a GADT.
deriveGADTSubstSym1 :: DeriveConfig -> Name -> Q [Dec]
deriveGADTSubstSym1 deriveConfig = genUnaryOpClass deriveConfig substSymConfig 1

-- | Derive 'SubstSym2' instance for a GADT.
deriveGADTSubstSym2 :: DeriveConfig -> Name -> Q [Dec]
deriveGADTSubstSym2 deriveConfig = genUnaryOpClass deriveConfig substSymConfig 2