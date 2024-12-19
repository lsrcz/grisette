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
    genUnaryOpClass,
  )
import Language.Haskell.TH (Dec, Name, Q)

hashableConfig :: UnaryOpClassConfig
hashableConfig =
  UnaryOpClassConfig
    { unaryOpFieldConfig =
        UnaryOpFieldConfig
          { extraPatNames = ["salt"],
            extraLiftedPatNames = const [],
            fieldCombineFun =
              \_ _ [salt] exp -> do
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
                ['hashWithSalt, 'liftHashWithSalt, 'liftHashWithSalt2]
          },
      unaryOpInstanceNames = [''Hashable, ''Hashable1, ''Hashable2],
      unaryOpFunNames = ['hashWithSalt, 'liftHashWithSalt, 'liftHashWithSalt2]
    }

-- | Derive 'Hashable' instance for a GADT.
deriveGADTHashable :: Name -> Q [Dec]
deriveGADTHashable = genUnaryOpClass hashableConfig 0

-- | Derive 'Hashable1' instance for a GADT.
deriveGADTHashable1 :: Name -> Q [Dec]
deriveGADTHashable1 = genUnaryOpClass hashableConfig 1

-- | Derive 'Hashable2' instance for a GADT.
deriveGADTHashable2 :: Name -> Q [Dec]
deriveGADTHashable2 = genUnaryOpClass hashableConfig 2
