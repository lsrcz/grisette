{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE Trustworthy #-}
{- ORMOLU_DISABLE -}

-- |
-- Module      :   Grisette.Core.THCompat
-- Copyright   :   (c) Sirui Lu 2021-2023
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only

module Grisette.Core.THCompat (augmentFinalType) where

import Data.Bifunctor
import Language.Haskell.TH.Syntax
import Grisette.Core.Data.Class.Mergeable
import Grisette.Core.Control.Monad.UnionM

#if MIN_VERSION_template_haskell(2,17,0)
augmentFinalType :: Type -> Q (([TyVarBndr Specificity], [Pred]), Type)
#elif MIN_VERSION_template_haskell(2,16,0)
augmentFinalType :: Type -> Q (([TyVarBndr], [Pred]), Type)
#endif
augmentFinalType (AppT a@(AppT ArrowT _) t) = do
  tl <- augmentFinalType t
  return $ second (AppT a) tl
#if MIN_VERSION_template_haskell(2,17,0)
augmentFinalType (AppT (AppT (AppT MulArrowT _) var) t) = do
  tl <- augmentFinalType t
  return $ second (AppT (AppT ArrowT var)) tl
#endif
augmentFinalType t = do
  unionType <- [t|UnionM|]
  mergeable <- [t|Mergeable|]
#if MIN_VERSION_template_haskell(2,17,0)
  return
    ( ( [ ],
        [ AppT mergeable t
        ]
      ),
      AppT unionType t
    )
#elif MIN_VERSION_template_haskell(2,16,0)
  return
    ( ( [ ],
        [ AppT mergeable t
        ]
      ),
      AppT unionType t
    )
#endif
