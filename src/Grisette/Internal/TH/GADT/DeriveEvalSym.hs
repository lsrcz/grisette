{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Unused LANGUAGE pragma" #-}

module Grisette.Internal.TH.GADT.DeriveEvalSym
  ( deriveGADTEvalSym,
    deriveGADTEvalSym1,
    deriveGADTEvalSym2,
  )
where

import Grisette.Internal.Core.Data.Class.EvalSym
  ( EvalSym (evalSym),
    EvalSym1 (liftEvalSym),
    EvalSym2 (liftEvalSym2),
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
    Exp (AppE),
    Name,
    Q,
  )

genEvalSym' :: Int -> Name -> Q [Dec]
genEvalSym' n typName = do
  genUnaryOpClass
    UnaryOpClassConfig
      { unaryOpFieldConfig =
          UnaryOpFieldConfig
            { extraPatNames = ["fillDefault", "model"],
              fieldCombineFun = \con exp -> return $ foldl AppE con exp
            },
        unaryOpInstanceNames =
          [''EvalSym, ''EvalSym1, ''EvalSym2],
        unaryOpFunNames =
          ['evalSym, 'liftEvalSym, 'liftEvalSym2]
      }
    n
    typName

deriveGADTEvalSym :: Name -> Q [Dec]
deriveGADTEvalSym = genEvalSym' 0

deriveGADTEvalSym1 :: Name -> Q [Dec]
deriveGADTEvalSym1 = genEvalSym' 1

deriveGADTEvalSym2 :: Name -> Q [Dec]
deriveGADTEvalSym2 = genEvalSym' 2
