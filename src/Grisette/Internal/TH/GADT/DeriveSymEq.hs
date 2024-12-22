{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module      :   Grisette.Internal.TH.GADT.DeriveSymEq
-- Copyright   :   (c) Sirui Lu 2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Internal.TH.GADT.DeriveSymEq
  ( deriveGADTSymEq,
    deriveGADTSymEq1,
    deriveGADTSymEq2,
  )
where

import Grisette.Internal.Core.Data.Class.LogicalOp
  ( LogicalOp (false, true, (.&&)),
  )
import Grisette.Internal.Core.Data.Class.SymEq
  ( SymEq ((.==)),
    SymEq1 (liftSymEq),
    SymEq2 (liftSymEq2),
  )
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
import Grisette.Internal.TH.GADT.Common (DeriveConfig)

symEqConfig :: BinaryOpClassConfig
symEqConfig =
  BinaryOpClassConfig
    { binaryOpFieldConfigs =
        [ BinaryOpFieldConfig
            { fieldResFun = \(lhs, rhs) f ->
                [|$(return f) $(return lhs) $(return rhs)|],
              fieldCombineFun =
                \lst -> [|foldl (.&&) true $(return $ ListE lst)|],
              fieldDifferentExistentialFun = const [|false|],
              fieldFunExp =
                defaultFieldFunExp ['(.==), 'liftSymEq, 'liftSymEq2],
              fieldFunNames = ['(.==), 'liftSymEq, 'liftSymEq2],
              fieldLMatchResult = [|false|],
              fieldRMatchResult = [|false|]
            }
        ],
      binaryOpInstanceNames = [''SymEq, ''SymEq1, ''SymEq2]
    }

-- | Derive 'SymEq' instance for a GADT.
deriveGADTSymEq :: DeriveConfig -> Name -> Q [Dec]
deriveGADTSymEq deriveConfig = genBinaryOpClass deriveConfig symEqConfig 0

-- | Derive 'SymEq1' instance for a GADT.
deriveGADTSymEq1 :: DeriveConfig -> Name -> Q [Dec]
deriveGADTSymEq1 deriveConfig = genBinaryOpClass deriveConfig symEqConfig 1

-- | Derive 'SymEq2' instance for a GADT.
deriveGADTSymEq2 :: DeriveConfig -> Name -> Q [Dec]
deriveGADTSymEq2 deriveConfig = genBinaryOpClass deriveConfig symEqConfig 2