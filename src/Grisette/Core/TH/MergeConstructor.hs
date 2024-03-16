{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE Trustworthy #-}

-- |
-- Module      :   Grisette.Core.TH.MergedConstructor
-- Copyright   :   (c) Sirui Lu 2021-2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Core.TH.MergeConstructor
  ( mkMergeConstructor,
    mkMergeConstructor',
  )
where

import Control.Monad (join, replicateM, when, zipWithM)
import Data.Bifunctor (Bifunctor (second))
import Grisette.Core.Data.Class.Mergeable (Mergeable)
import Grisette.Core.Data.Class.TryMerge (TryMerge)
import Language.Haskell.TH
  ( Body (NormalB),
    Clause (Clause),
    Con (ForallC, GadtC, InfixC, NormalC, RecC, RecGadtC),
    Dec (DataD, FunD, NewtypeD, SigD),
    Exp (AppE, ConE, LamE, VarE),
    Info (DataConI, TyConI),
    Name,
    Pat (VarP),
    Pred,
    Q,
    TyVarBndr (PlainTV),
    Type (AppT, ArrowT, ForallT, VarT),
    mkName,
    newName,
    pprint,
    reify,
  )
#if MIN_VERSION_template_haskell(2,17,0)
import Language.Haskell.TH.Syntax
  ( Name (Name),
    OccName (OccName),
    Specificity (SpecifiedSpec),
    Type (MulArrowT),
  )
#else
import Language.Haskell.TH.Syntax (Name (Name), OccName (OccName))
#endif

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
  constructors <- getConstructors typName
  when (length names /= length constructors) $
    fail "Number of names does not match the number of constructors"
  ds <- zipWithM mkSingleWrapper names constructors
  return $ join ds

occName :: Name -> String
occName (Name (OccName name) _) = name

getConstructorName :: Con -> Q String
getConstructorName (NormalC name _) = return $ occName name
getConstructorName (RecC name _) = return $ occName name
getConstructorName InfixC {} =
  fail "You should use mkMergeConstructor' to manually provide the name for infix constructors"
getConstructorName (ForallC _ _ c) = getConstructorName c
getConstructorName (GadtC [name] _ _) = return $ occName name
getConstructorName (RecGadtC [name] _ _) = return $ occName name
getConstructorName c = fail $ "Unsupported constructor at this time: " ++ pprint c

getConstructors :: Name -> Q [Con]
getConstructors typName = do
  d <- reify typName
  case d of
    TyConI (DataD _ _ _ _ constructors _) -> return constructors
    TyConI (NewtypeD _ _ _ _ constructor _) -> return [constructor]
    _ -> fail $ "Unsupported declaration: " ++ pprint d

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
  constructors <- getConstructors typName
  constructorNames <- mapM getConstructorName constructors
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
  mName <- newName "m"
  let mTy = VarT mName
  mergeable <- [t|Mergeable|]
  applicative <- [t|Applicative|]
  tryMerge <- [t|TryMerge|]
#if MIN_VERSION_template_haskell(2,17,0)
  return
    ( ( [ PlainTV mName SpecifiedSpec ],
        [ AppT mergeable t, AppT applicative mTy, AppT tryMerge mTy]
      ),
      AppT mTy t
    )
#else
  return
    ( ( [ PlainTV mName ],
        [ AppT mergeable t, AppT applicative mTy, AppT tryMerge mTy]
      ),
      AppT mTy t
    )
#endif

augmentNormalCType :: Type -> Q Type
augmentNormalCType (ForallT tybinders ctx ty1) = do
  ((bndrs, preds), augmentedTyp) <- augmentFinalType ty1
  return $ ForallT (tybinders ++ bndrs) (preds ++ ctx) augmentedTyp
augmentNormalCType t = do
  ((bndrs, preds), augmentedTyp) <- augmentFinalType t
  return $ ForallT bndrs preds augmentedTyp

mkSingleWrapper :: String -> Con -> Q [Dec]
mkSingleWrapper name (NormalC oriName b) = do
  DataConI _ constructorTyp _ <- reify oriName
  augmentedTyp <- augmentNormalCType constructorTyp
  let retName = mkName name
  expr <- augmentNormalCExpr (length b) (ConE oriName)
  return
    [ SigD retName augmentedTyp,
      FunD retName [Clause [] (NormalB expr) []]
    ]
mkSingleWrapper name (RecC oriName b) = do
  DataConI _ constructorTyp _ <- reify oriName
  augmentedTyp <- augmentNormalCType constructorTyp
  let retName = mkName name
  expr <- augmentNormalCExpr (length b) (ConE oriName)
  return
    [ SigD retName augmentedTyp,
      FunD retName [Clause [] (NormalB expr) []]
    ]
mkSingleWrapper _ v = fail $ "Unsupported constructor" ++ pprint v
