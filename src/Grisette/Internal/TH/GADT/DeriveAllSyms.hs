{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Unused LANGUAGE pragma" #-}

module Grisette.Internal.TH.GADT.DeriveAllSyms
  ( deriveGADTAllSyms,
    deriveGADTAllSyms1,
    deriveGADTAllSyms2,
  )
where

import Grisette.Internal.SymPrim.AllSyms
  ( AllSyms (allSymsS),
    AllSyms1 (liftAllSymsS),
    AllSyms2 (liftAllSymsS2),
  )
import Grisette.Internal.TH.GADT.UnaryOpCommon
  ( UnaryOpClassConfig
      ( UnaryOpClassConfig,
        unaryOpAllowExistential,
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
    defaultFieldResFun,
    genUnaryOpClass,
  )
import Language.Haskell.TH (Dec, Exp (AppE, ListE, VarE), Name, Q)

allSymsConfig :: UnaryOpClassConfig
allSymsConfig =
  UnaryOpClassConfig
    { unaryOpFieldConfigs =
        [ UnaryOpFieldConfig
            { extraPatNames = [],
              extraLiftedPatNames = const [],
              fieldResFun = defaultFieldResFun,
              fieldCombineFun = \_ _ _ _ exp ->
                return (AppE (VarE 'mconcat) $ ListE exp, False <$ exp),
              fieldFunExp =
                defaultFieldFunExp
                  [ 'allSymsS,
                    'liftAllSymsS,
                    'liftAllSymsS2
                  ],
              fieldFunNames = ['allSymsS, 'liftAllSymsS, 'liftAllSymsS2]
            }
        ],
      unaryOpInstanceNames = [''AllSyms, ''AllSyms1, ''AllSyms2],
      unaryOpAllowExistential = True
    }

deriveGADTAllSyms :: Name -> Q [Dec]
deriveGADTAllSyms = genUnaryOpClass allSymsConfig 0

deriveGADTAllSyms1 :: Name -> Q [Dec]
deriveGADTAllSyms1 = genUnaryOpClass allSymsConfig 1

deriveGADTAllSyms2 :: Name -> Q [Dec]
deriveGADTAllSyms2 = genUnaryOpClass allSymsConfig 2
