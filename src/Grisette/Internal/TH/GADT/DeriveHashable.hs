{-# LANGUAGE TemplateHaskell #-}

module Grisette.Internal.TH.GADT.DeriveHashable
  ( deriveGADTHashable,
    deriveGADTHashable1,
    deriveGADTHashable2,
  )
where

import Control.Monad (zipWithM)
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
        extraPatNames,
        fieldCombineFun,
        fieldResFun
      ),
    genUnaryOpClass,
  )
import Language.Haskell.TH (Dec, Name, Q)

hashableConfig :: UnaryOpClassConfig
hashableConfig =
  UnaryOpClassConfig
    { unaryOpFieldConfig =
        UnaryOpFieldConfig
          { extraPatNames = ["salt"],
            fieldCombineFun =
              \_ [salt] exp ->
                foldl
                  (\salt exp -> [|$(return exp) $salt|])
                  (return salt)
                  exp,
            fieldResFun = \_ fieldFun fieldPat ->
              zipWithM
                (\ff fp -> [|\salt -> $(return ff) salt $(return fp)|])
                fieldFun
                fieldPat
          },
      unaryOpInstanceNames = [''Hashable, ''Hashable1, ''Hashable2],
      unaryOpFunNames = ['hashWithSalt, 'liftHashWithSalt, 'liftHashWithSalt2]
    }

deriveGADTHashable :: Name -> Q [Dec]
deriveGADTHashable = genUnaryOpClass hashableConfig 0

deriveGADTHashable1 :: Name -> Q [Dec]
deriveGADTHashable1 = genUnaryOpClass hashableConfig 1

deriveGADTHashable2 :: Name -> Q [Dec]
deriveGADTHashable2 = genUnaryOpClass hashableConfig 2
