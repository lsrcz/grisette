{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Unused LANGUAGE pragma" #-}

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
        extraPatNames,
        fieldCombineFun
      ),
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
              fieldCombineFun = \_ exp ->
                return $ AppE (VarE 'mconcat) $ ListE exp
            },
        unaryOpInstanceNames =
          [''ExtractSym, ''ExtractSym1, ''ExtractSym2],
        unaryOpFunNames =
          ['extractSymMaybe, 'liftExtractSymMaybe, 'liftExtractSymMaybe2]
      }
    n
    typName

deriveGADTExtractSym :: Name -> Q [Dec]
deriveGADTExtractSym = genExtractSym' 0

deriveGADTExtractSym1 :: Name -> Q [Dec]
deriveGADTExtractSym1 = genExtractSym' 1

deriveGADTExtractSym2 :: Name -> Q [Dec]
deriveGADTExtractSym2 = genExtractSym' 2
