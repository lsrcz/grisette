{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Unused LANGUAGE pragma" #-}

-- |
-- Module      :   Grisette.Internal.TH.GADT.DeriveExtractSym
-- Copyright   :   (c) Sirui Lu 2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Internal.TH.GADT.DeriveExtractSym
  ( deriveGADTExtractSym,
    deriveGADTExtractSym1,
    deriveGADTExtractSym2,
  )
where

import Grisette.Internal.Core.Data.Class.ExtractSym
  ( ExtractSym (extractSymMaybe),
    ExtractSym1 (liftExtractSymMaybe),
    ExtractSym2 (liftExtractSymMaybe2),
  )
import Grisette.Internal.TH.GADT.UnaryOpCommon
  ( UnaryOpClassConfig
      ( UnaryOpClassConfig,
        unaryOpFieldConfig,
        unaryOpFunNames,
        unaryOpInstanceNames
      ),
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
    genUnaryOpClass,
  )
import Language.Haskell.TH
  ( Dec,
    Exp (AppE, ListE, VarE),
    Name,
    Q,
  )

genExtractSym' :: Int -> Name -> Q [Dec]
genExtractSym' n typName = do
  genUnaryOpClass
    UnaryOpClassConfig
      { unaryOpFieldConfig =
          UnaryOpFieldConfig
            { extraPatNames = [],
              extraLiftedPatNames = const [],
              fieldResFun = defaultFieldResFun,
              fieldCombineFun = \_ _ _ exp -> do
                return (AppE (VarE 'mconcat) $ ListE exp, False <$ exp),
              fieldFunExp =
                defaultFieldFunExp
                  ['extractSymMaybe, 'liftExtractSymMaybe, 'liftExtractSymMaybe2]
            },
        unaryOpInstanceNames =
          [''ExtractSym, ''ExtractSym1, ''ExtractSym2],
        unaryOpFunNames =
          ['extractSymMaybe, 'liftExtractSymMaybe, 'liftExtractSymMaybe2]
      }
    n
    typName

-- | Derive 'ExtractSym' instance for a GADT.
deriveGADTExtractSym :: Name -> Q [Dec]
deriveGADTExtractSym = genExtractSym' 0

-- | Derive 'ExtractSym1' instance for a GADT.
deriveGADTExtractSym1 :: Name -> Q [Dec]
deriveGADTExtractSym1 = genExtractSym' 1

-- | Derive 'ExtractSym2' instance for a GADT.
deriveGADTExtractSym2 :: Name -> Q [Dec]
deriveGADTExtractSym2 = genExtractSym' 2
