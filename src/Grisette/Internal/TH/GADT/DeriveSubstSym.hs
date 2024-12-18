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
import Grisette.Internal.TH.GADT.UnaryOpCommon
  ( UnaryOpClassConfig
      ( UnaryOpClassConfig,
        unaryOpFieldConfig,
        unaryOpFunNames,
        unaryOpInstanceNames
      ),
    UnaryOpFieldConfig (UnaryOpFieldConfig, extraPatNames, fieldCombineFun),
    genUnaryOpClass,
  )
import Language.Haskell.TH (Dec, Exp (AppE), Name)
import Language.Haskell.TH.Syntax (Q)

genSubstSym' :: Int -> Name -> Q [Dec]
genSubstSym' n typName = do
  genUnaryOpClass
    UnaryOpClassConfig
      { unaryOpFieldConfig =
          UnaryOpFieldConfig
            { extraPatNames = ["symbol", "newVal"],
              fieldCombineFun = \con exp -> return $ foldl AppE con exp
            },
        unaryOpInstanceNames =
          [''SubstSym, ''SubstSym1, ''SubstSym2],
        unaryOpFunNames =
          ['substSym, 'liftSubstSym, 'liftSubstSym2]
      }
    n
    typName

deriveGADTSubstSym :: Name -> Q [Dec]
deriveGADTSubstSym = genSubstSym' 0

deriveGADTSubstSym1 :: Name -> Q [Dec]
deriveGADTSubstSym1 = genSubstSym' 1

deriveGADTSubstSym2 :: Name -> Q [Dec]
deriveGADTSubstSym2 = genSubstSym' 2
