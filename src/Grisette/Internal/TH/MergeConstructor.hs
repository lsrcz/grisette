{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE Trustworthy #-}

-- |
-- Module      :   Grisette.Internal.TH.MergedConstructor
-- Copyright   :   (c) Sirui Lu 2021-2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Internal.TH.MergeConstructor
  ( mkMergeConstructor,
    mkMergeConstructor',
  )
where

import Control.Monad (join, replicateM, when, zipWithM)
import Data.Bifunctor (Bifunctor (second))
import Grisette.Internal.Core.Data.Class.Mergeable (Mergeable)
import Grisette.Internal.Core.Data.Class.TryMerge (TryMerge)
import Grisette.Internal.TH.Util (constructorInfoToType, occName)
import Language.Haskell.TH
  ( Body (NormalB),
    Clause (Clause),
    Dec (FunD, SigD),
    Exp (AppE, ConE, LamE, VarE),
    Name,
    Pat (VarP),
    Pred,
    Q,
    Type (AppT, ArrowT, ForallT, VarT),
    mkName,
    newName,
  )
import Language.Haskell.TH.Datatype
  ( ConstructorInfo
      ( constructorFields,
        constructorName
      ),
    DatatypeInfo (datatypeCons),
    reifyDatatype,
  )
import Language.Haskell.TH.Datatype.TyVarBndr
  ( Specificity (SpecifiedSpec),
    TyVarBndrSpec,
    plainTVFlag,
  )

-- | Generate constructor wrappers that wraps the result in a container with `TryMerge` with provided names.
--
-- > mkMergeConstructor' ["mrgTuple2"] ''(,)
--
-- generates
--
-- > mrgTuple2 :: (Mergeable (a, b), Applicative m, TryMerge m) => a -> b -> u (a, b)
-- > mrgTuple2 = \v1 v2 -> mrgSingle (v1, v2)
mkMergeConstructor' ::
  -- | Names for generated wrappers
  [String] ->
  -- | The type to generate the wrappers for
  Name ->
  Q [Dec]
mkMergeConstructor' names typName = do
  d <- reifyDatatype typName
  let constructors = datatypeCons d
  when (length names /= length constructors) $
    fail "Number of names does not match the number of constructors"
  ds <- zipWithM (mkSingleWrapper d) names constructors
  return $ join ds

-- | Generate constructor wrappers that wraps the result in a container with `TryMerge`.
--
-- > mkMergeConstructor "mrg" ''Maybe
--
-- generates
--
-- > mrgJust :: (Mergeable (Maybe a), Applicative m, TryMerge m) => m (Maybe a)
-- > mrgNothing = mrgSingle Nothing
-- > mrgJust :: (Mergeable (Maybe a), Applicative m, TryMerge m) => a -> m (Maybe a)
-- > mrgJust = \x -> mrgSingle (Just x)
mkMergeConstructor ::
  -- | Prefix for generated wrappers
  String ->
  -- | The type to generate the wrappers for
  Name ->
  Q [Dec]
mkMergeConstructor prefix typName = do
  d <- reifyDatatype typName
  let constructorNames = occName . constructorName <$> datatypeCons d
  mkMergeConstructor' ((prefix ++) <$> constructorNames) typName

augmentNormalCExpr :: Int -> Exp -> Q Exp
augmentNormalCExpr n f = do
  xs <- replicateM n (newName "x")
  let args = map VarP xs
  mrgSingleFun <- [|mrgSingle|]
  return $
    LamE
      args
      ( AppE mrgSingleFun $
          foldl AppE f (map VarE xs)
      )

augmentFinalType :: Type -> Q (([TyVarBndrSpec], [Pred]), Type)
augmentFinalType (AppT a@(AppT ArrowT _) t) = do
  tl <- augmentFinalType t
  return $ second (AppT a) tl
augmentFinalType t = do
  mName <- newName "m"
  let mTy = VarT mName
  mergeable <- [t|Mergeable|]
  applicative <- [t|Applicative|]
  tryMerge <- [t|TryMerge|]
  return
    ( ( [plainTVFlag mName SpecifiedSpec],
        [AppT mergeable t, AppT applicative mTy, AppT tryMerge mTy]
      ),
      AppT mTy t
    )

augmentConstructorType :: Type -> Q Type
augmentConstructorType (ForallT tybinders ctx ty1) = do
  ((bndrs, preds), augmentedTyp) <- augmentFinalType ty1
  return $ ForallT (tybinders ++ bndrs) (preds ++ ctx) augmentedTyp
augmentConstructorType t = do
  ((bndrs, preds), augmentedTyp) <- augmentFinalType t
  return $ ForallT bndrs preds augmentedTyp

mkSingleWrapper :: DatatypeInfo -> String -> ConstructorInfo -> Q [Dec]
mkSingleWrapper dataType name info = do
  constructorTyp <- constructorInfoToType dataType info
  augmentedTyp <- augmentConstructorType constructorTyp
  let oriName = constructorName info
  let retName = mkName name
  expr <- augmentNormalCExpr (length $ constructorFields info) (ConE oriName)
  return
    [ SigD retName augmentedTyp,
      FunD retName [Clause [] (NormalB expr) []]
    ]
