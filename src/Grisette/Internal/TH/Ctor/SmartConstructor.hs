{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE Trustworthy #-}

-- |
-- Module      :   Grisette.Internal.TH.Ctor.SmartConstructor
-- Copyright   :   (c) Sirui Lu 2021-2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Internal.TH.Ctor.SmartConstructor
  ( makeSmartCtorWith,
    makePrefixedSmartCtor,
    makeNamedSmartCtor,
    makeSmartCtor,
  )
where

import Control.Monad (join, replicateM, when, zipWithM)
import Data.Bifunctor (Bifunctor (second))
import Grisette.Internal.Core.Data.Class.Mergeable (Mergeable)
import Grisette.Internal.Core.Data.Class.TryMerge (TryMerge, mrgSingle)
import Grisette.Internal.TH.Ctor.Common
  ( decapitalizeTransformer,
    prefixTransformer,
    withNameTransformer,
  )
import Grisette.Internal.TH.Util (constructorInfoToType, putHaddock)
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

-- | Generate constructor wrappers that wraps the result in a container with
-- `TryMerge` with provided name transformer.
--
-- > makeSmartCtorWith (\name -> "mrg" ++ name) ''Maybe
--
-- generates
--
-- > mrgNothing :: (Mergeable (Maybe a), Applicative m, TryMerge m) => m (Maybe a)
-- > mrgNothing = mrgSingle Nothing
makeSmartCtorWith :: (String -> String) -> Name -> Q [Dec]
makeSmartCtorWith = withNameTransformer makeNamedSmartCtor

-- | Generate constructor wrappers that wraps the result in a container with
-- `TryMerge`.
--
-- > makePrefixedSmartCtor "mrg" ''Maybe
--
-- generates
--
-- > mrgNothing :: (Mergeable (Maybe a), Applicative m, TryMerge m) => m (Maybe a)
-- > mrgNothing = mrgSingle Nothing
-- > mrgJust :: (Mergeable (Maybe a), Applicative m, TryMerge m) => a -> m (Maybe a)
-- > mrgJust = \x -> mrgSingle (Just x)
makePrefixedSmartCtor ::
  -- | Prefix for generated wrappers
  String ->
  -- | The type to generate the wrappers for
  Name ->
  Q [Dec]
makePrefixedSmartCtor = makeSmartCtorWith . prefixTransformer

-- | Generate constructor wrappers that wraps the result in a container with
-- `TryMerge`.
--
-- > makeSmartCtor ''Maybe
--
-- generates
--
-- > nothing :: (Mergeable (Maybe a), Applicative m, TryMerge m) => m (Maybe a)
-- > nothing = mrgSingle Nothing
-- > just :: (Mergeable (Maybe a), Applicative m, TryMerge m) => a -> m (Maybe a)
-- > just = \x -> mrgSingle (Just x)
makeSmartCtor ::
  -- | The type to generate the wrappers for
  Name ->
  Q [Dec]
makeSmartCtor = makeSmartCtorWith decapitalizeTransformer

-- | Generate constructor wrappers that wraps the result in a container with
-- `TryMerge` with provided names.
--
-- > makeNamedSmartCtor ["mrgTuple2"] ''(,)
--
-- generates
--
-- > mrgTuple2 :: (Mergeable (a, b), Applicative m, TryMerge m) => a -> b -> u (a, b)
-- > mrgTuple2 = \v1 v2 -> mrgSingle (v1, v2)
makeNamedSmartCtor ::
  -- | Names for generated wrappers
  [String] ->
  -- | The type to generate the wrappers for
  Name ->
  Q [Dec]
makeNamedSmartCtor names typName = do
  d <- reifyDatatype typName
  let constructors = datatypeCons d
  when (length names /= length constructors) $
    fail "Number of names does not match the number of constructors"
  ds <- zipWithM (mkSingleWrapper d) names constructors
  return $ join ds

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
  putHaddock retName $
    "Smart constructor for v'"
      <> show oriName
      <> "' to construct values wrapped and possibly merged in a container."
  return
    [ SigD retName augmentedTyp,
      FunD retName [Clause [] (NormalB expr) []]
    ]
