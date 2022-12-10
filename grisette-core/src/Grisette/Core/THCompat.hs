{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE Trustworthy #-}
{- ORMOLU_DISABLE -}

-- |
-- Module      :   Grisette.Core.THCompat
-- Copyright   :   (c) Sirui Lu 2021-2022
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only

module Grisette.Core.THCompat (augmentFinalType) where

import Data.Bifunctor
import Language.Haskell.TH.Syntax
import Grisette.Core.Control.Monad.Union
import Grisette.Core.Data.Class.Bool
import Grisette.Core.Data.Class.Mergeable

#if MIN_VERSION_template_haskell(2,17,0)
augmentFinalType :: Name -> Name -> Type -> Q (([TyVarBndr Specificity], [Pred]), Type)
#elif MIN_VERSION_template_haskell(2,16,0)
augmentFinalType :: Name -> Name -> Type -> Q (([TyVarBndr], [Pred]), Type)
#endif
augmentFinalType unionTypeName boolTypeName (AppT a@(AppT ArrowT _) t) = do
  tl <- augmentFinalType unionTypeName boolTypeName t
  return $ second (AppT a) tl
#if MIN_VERSION_template_haskell(2,17,0)
augmentFinalType unionTypeName boolTypeName (AppT (AppT (AppT MulArrowT _) var) t) = do
  tl <- augmentFinalType unionTypeName boolTypeName t
  return $ second (AppT (AppT ArrowT var)) tl
#endif
augmentFinalType unionTypeName boolTypeName t = do
  let boolType = VarT boolTypeName
  let unionType = VarT unionTypeName
  symBoolOp <- [t|SymBoolOp|]
  monad <- [t|Monad|]
  mergeable <- [t|GMergeable|]
  monadUnion <- [t|GMonadUnion|]
#if MIN_VERSION_template_haskell(2,17,0)
  return
    ( ( [ 
          KindedTV boolTypeName SpecifiedSpec StarT,
          KindedTV unionTypeName SpecifiedSpec (AppT (AppT ArrowT StarT) StarT)
      ],
        [ AppT symBoolOp boolType,
          AppT monad unionType,
          AppT (AppT mergeable boolType) t,
          AppT (AppT monadUnion boolType) unionType
        ]
      ),
      AppT unionType t
    )
#elif MIN_VERSION_template_haskell(2,16,0)
  return
    ( ( [ 
          KindedTV boolTypeName StarT,
          KindedTV unionTypeName (AppT (AppT ArrowT StarT) StarT)
      ],
        [ AppT symBoolOp boolType,
          AppT monad unionType,
          AppT (AppT mergeable boolType) t,
          AppT (AppT monadUnion boolType) unionType
        ]
      ),
      AppT unionType t
    )
#endif
