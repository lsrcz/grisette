{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module      :   Grisette.Internal.TH.GADT.DeriveHashable
-- Copyright   :   (c) Sirui Lu 2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Internal.TH.GADT.DeriveHashable
  ( deriveGADTHashable,
    deriveGADTHashable1,
    deriveGADTHashable2,
  )
where

import Data.Hashable (Hashable (hashWithSalt))
import Data.Hashable.Lifted
  ( Hashable1 (liftHashWithSalt),
    Hashable2 (liftHashWithSalt2),
  )
import Grisette.Internal.TH.GADT.Common (ExtraConstraint)
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
    genUnaryOpClass,
  )
import Language.Haskell.TH (Dec, Name, Q)

hashableConfig :: UnaryOpClassConfig
hashableConfig =
  UnaryOpClassConfig
    { unaryOpFieldConfigs =
        [ UnaryOpFieldConfig
            { extraPatNames = ["salt"],
              extraLiftedPatNames = const [],
              fieldCombineFun =
                \_ _ _ [salt] exp -> do
                  r <-
                    foldl
                      (\salt exp -> [|$(return exp) $salt|])
                      (return salt)
                      exp
                  return (r, [True]),
              fieldResFun = \_ _ _ _ fieldPat fieldFun -> do
                r <- [|\salt -> $(return fieldFun) salt $(return fieldPat)|]
                return (r, [False]),
              fieldFunExp =
                defaultFieldFunExp
                  ['hashWithSalt, 'liftHashWithSalt, 'liftHashWithSalt2],
              fieldFunNames =
                ['hashWithSalt, 'liftHashWithSalt, 'liftHashWithSalt2]
            }
        ],
      unaryOpInstanceNames =
        [''Hashable, ''Hashable1, ''Hashable2]
    }

-- | Derive 'Hashable' instance for a GADT.
deriveGADTHashable :: ExtraConstraint -> Name -> Q [Dec]
deriveGADTHashable extra = genUnaryOpClass extra hashableConfig 0

-- | Derive 'Hashable1' instance for a GADT.
deriveGADTHashable1 :: ExtraConstraint -> Name -> Q [Dec]
deriveGADTHashable1 extra = genUnaryOpClass extra hashableConfig 1

-- | Derive 'Hashable2' instance for a GADT.
deriveGADTHashable2 :: ExtraConstraint -> Name -> Q [Dec]
deriveGADTHashable2 extra = genUnaryOpClass extra hashableConfig 2
