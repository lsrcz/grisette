{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

-- |
-- Module      :   Grisette.Internal.TH.GADT.DeriveEq
-- Copyright   :   (c) Sirui Lu 2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Internal.TH.GADT.DeriveEq
  ( deriveGADTEq,
    deriveGADTEq1,
    deriveGADTEq2,
  )
where

import Data.Functor.Classes (Eq1 (liftEq), Eq2 (liftEq2))
import Grisette.Internal.TH.GADT.BinaryOpCommon
  ( BinaryOpClassConfig
      ( BinaryOpClassConfig,
        binaryOpAllowSumType,
        binaryOpFieldConfigs,
        binaryOpInstanceNames
      ),
    BinaryOpFieldConfig
      ( BinaryOpFieldConfig,
        extraPatNames,
        fieldCombineFun,
        fieldDifferentExistentialFun,
        fieldFunExp,
        fieldFunNames,
        fieldLMatchResult,
        fieldRMatchResult,
        fieldResFun
      ),
    binaryOpAllowExistential,
    defaultFieldFunExp,
    genBinaryOpClass,
  )
import Grisette.Internal.TH.GADT.Common (DeriveConfig)
import Language.Haskell.TH (Dec, Exp (ListE), Q)
import Language.Haskell.TH.Syntax (Name)

eqConfig :: BinaryOpClassConfig
eqConfig =
  BinaryOpClassConfig
    { binaryOpFieldConfigs =
        [ BinaryOpFieldConfig
            { extraPatNames = [],
              fieldResFun = \_ (lhs, rhs) f ->
                (,[]) <$> [|$(return f) $(return lhs) $(return rhs)|],
              fieldCombineFun = \_ lst ->
                (,[]) <$> [|and $(return $ ListE lst)|],
              fieldDifferentExistentialFun = const [|False|],
              fieldFunExp = defaultFieldFunExp ['(==), 'liftEq, 'liftEq2],
              fieldFunNames = ['(==), 'liftEq, 'liftEq2],
              fieldLMatchResult = [|False|],
              fieldRMatchResult = [|False|]
            }
        ],
      binaryOpInstanceNames = [''Eq, ''Eq1, ''Eq2],
      binaryOpAllowSumType = True,
      binaryOpAllowExistential = True
    }

-- | Derive 'Eq' instance for a GADT.
deriveGADTEq :: DeriveConfig -> Name -> Q [Dec]
deriveGADTEq deriveConfig = genBinaryOpClass deriveConfig eqConfig 0

-- | Derive 'Eq1' instance for a GADT.
deriveGADTEq1 :: DeriveConfig -> Name -> Q [Dec]
deriveGADTEq1 deriveConfig = genBinaryOpClass deriveConfig eqConfig 1

-- | Derive 'Eq2' instance for a GADT.
deriveGADTEq2 :: DeriveConfig -> Name -> Q [Dec]
deriveGADTEq2 deriveConfig = genBinaryOpClass deriveConfig eqConfig 2