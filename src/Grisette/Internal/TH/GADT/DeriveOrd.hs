{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module      :   Grisette.Internal.TH.GADT.DeriveOrd
-- Copyright   :   (c) Sirui Lu 2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Internal.TH.GADT.DeriveOrd
  ( deriveGADTOrd,
    deriveGADTOrd1,
    deriveGADTOrd2,
  )
where

import Data.Functor.Classes (Ord1 (liftCompare), Ord2 (liftCompare2))
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
import Language.Haskell.TH (Dec, Exp (ListE), Name, Q)

ordConfig :: BinaryOpClassConfig
ordConfig =
  BinaryOpClassConfig
    { binaryOpFieldConfigs =
        [ BinaryOpFieldConfig
            { fieldResFun = \(lhs, rhs) f ->
                [|$(return f) $(return lhs) $(return rhs)|],
              fieldCombineFun = \lst -> [|mconcat $(return $ ListE lst)|],
              fieldDifferentExistentialFun = return,
              fieldFunExp =
                defaultFieldFunExp ['compare, 'liftCompare, 'liftCompare2],
              fieldFunNames = ['compare, 'liftCompare, 'liftCompare2],
              fieldLMatchResult = [|LT|],
              fieldRMatchResult = [|GT|]
            }
        ],
      binaryOpInstanceNames = [''Ord, ''Ord1, ''Ord2]
    }

-- | Derive 'Ord' instance for a GADT.
deriveGADTOrd :: Name -> Q [Dec]
deriveGADTOrd = genBinaryOpClass ordConfig 0

-- | Derive 'Ord1' instance for a GADT.
deriveGADTOrd1 :: Name -> Q [Dec]
deriveGADTOrd1 = genBinaryOpClass ordConfig 1

-- | Derive 'Ord2' instance for a GADT.
deriveGADTOrd2 :: Name -> Q [Dec]
deriveGADTOrd2 = genBinaryOpClass ordConfig 2