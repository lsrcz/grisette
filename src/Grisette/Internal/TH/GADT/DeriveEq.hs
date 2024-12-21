{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

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
        binaryOpFieldConfigs,
        binaryOpInstanceNames
      ),
    BinaryOpFieldConfig
      ( BinaryOpFieldConfig,
        fieldCombineFun,
        fieldDifferentExistentialFun,
        fieldFunExp,
        fieldFunNames,
        fieldLMatchResult,
        fieldRMatchResult,
        fieldResFun
      ),
    defaultFieldFunExp,
    genBinaryOpClass,
  )
import Language.Haskell.TH (Dec, Exp (ListE), Q)
import Language.Haskell.TH.Syntax (Name)

eqConfig :: BinaryOpClassConfig
eqConfig =
  BinaryOpClassConfig
    { binaryOpFieldConfigs =
        [ BinaryOpFieldConfig
            { fieldResFun = \(lhs, rhs) f ->
                [|$(return f) $(return lhs) $(return rhs)|],
              fieldCombineFun = \lst -> [|and $(return $ ListE lst)|],
              fieldDifferentExistentialFun = const [|False|],
              fieldFunExp = defaultFieldFunExp ['(==), 'liftEq, 'liftEq2],
              fieldFunNames = ['(==), 'liftEq, 'liftEq2],
              fieldLMatchResult = [|False|],
              fieldRMatchResult = [|False|]
            }
        ],
      binaryOpInstanceNames = [''Eq, ''Eq1, ''Eq2]
    }

-- | Derive 'Eq' instance for a GADT.
deriveGADTEq :: Name -> Q [Dec]
deriveGADTEq = genBinaryOpClass eqConfig 0

-- | Derive 'Eq1' instance for a GADT.
deriveGADTEq1 :: Name -> Q [Dec]
deriveGADTEq1 = genBinaryOpClass eqConfig 1

-- | Derive 'Eq2' instance for a GADT.
deriveGADTEq2 :: Name -> Q [Dec]
deriveGADTEq2 = genBinaryOpClass eqConfig 2
