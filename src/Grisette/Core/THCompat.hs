{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE Trustworthy #-}

-- |
-- Module      :   Grisette.Core.THCompat
-- Copyright   :   (c) Sirui Lu 2021-2023
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Core.THCompat (augmentFinalType) where

import Data.Bifunctor (Bifunctor (second))
import Grisette.Core.Control.Monad.UnionM (UnionM)
import Grisette.Core.Data.Class.Mergeable (Mergeable)
#if MIN_VERSION_template_haskell(2,17,0)
import Language.Haskell.TH.Syntax
  ( Pred,
    Q,
    Specificity,
    TyVarBndr,
    Type
      ( AppT,
        ArrowT,
        MulArrowT
      ),
  )
#else
import Language.Haskell.TH.Syntax
  ( Pred,
    Q,
    TyVarBndr,
    Type
      ( AppT,
        ArrowT
      ),
  )
#endif

#if MIN_VERSION_template_haskell(2,17,0)
augmentFinalType :: Type -> Q (([TyVarBndr Specificity], [Pred]), Type)
#else
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
