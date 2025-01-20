{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

-- |
-- Module      :   Grisette.Internal.TH.Derivation.DeriveSymEq
-- Copyright   :   (c) Sirui Lu 2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Internal.TH.Derivation.DeriveSymEq
  ( deriveSymEq,
    deriveSymEq1,
    deriveSymEq2,
  )
where

import Grisette.Internal.Core.Data.Class.LogicalOp
  ( LogicalOp (false, true, (.&&)),
  )
import Grisette.Internal.Internal.Decl.Core.Data.Class.SymEq
  ( SymEq ((.==)),
    SymEq1 (liftSymEq),
    SymEq2 (liftSymEq2),
  )
import Grisette.Internal.TH.Derivation.BinaryOpCommon
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
import Grisette.Internal.TH.Derivation.Common (DeriveConfig)
import Language.Haskell.TH (Dec, Exp (ListE), Name, Q)

symEqConfig :: BinaryOpClassConfig
symEqConfig =
  BinaryOpClassConfig
    { binaryOpFieldConfigs =
        [ BinaryOpFieldConfig
            { extraPatNames = [],
              fieldResFun = \_ (lhs, rhs) f ->
                (,[]) <$> [|$(return f) $(return lhs) $(return rhs)|],
              fieldCombineFun =
                \_ lst -> (,[]) <$> [|foldl (.&&) true $(return $ ListE lst)|],
              fieldDifferentExistentialFun = const [|false|],
              fieldFunExp =
                defaultFieldFunExp ['(.==), 'liftSymEq, 'liftSymEq2],
              fieldFunNames = ['(.==), 'liftSymEq, 'liftSymEq2],
              fieldLMatchResult = [|false|],
              fieldRMatchResult = [|false|]
            }
        ],
      binaryOpInstanceNames = [''SymEq, ''SymEq1, ''SymEq2],
      binaryOpAllowSumType = True,
      binaryOpAllowExistential = True
    }

-- | Derive 'SymEq' instance for a data type.
deriveSymEq :: DeriveConfig -> Name -> Q [Dec]
deriveSymEq deriveConfig = genBinaryOpClass deriveConfig symEqConfig 0

-- | Derive 'SymEq1' instance for a data type.
deriveSymEq1 :: DeriveConfig -> Name -> Q [Dec]
deriveSymEq1 deriveConfig = genBinaryOpClass deriveConfig symEqConfig 1

-- | Derive 'SymEq2' instance for a data type.
deriveSymEq2 :: DeriveConfig -> Name -> Q [Dec]
deriveSymEq2 deriveConfig = genBinaryOpClass deriveConfig symEqConfig 2
