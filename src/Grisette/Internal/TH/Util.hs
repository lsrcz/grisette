{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Unused LANGUAGE pragma" #-}

-- |
-- Module      :   Grisette.Internal.TH.Util
-- Copyright   :   (c) Sirui Lu 2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Internal.TH.Util
  ( occName,
    constructorInfoToType,
    tvIsMode,
    tvIsNat,
    tvIsStar,
    tvIsStarToStar,
    substDataType,
    reifyDatatypeWithFreshNames,
    singleParamClassParamKind,
    binaryClassParamKind,
    getTypeWithMaybeSubst,
    dropLastTypeParam,
    dropNTypeParam,
    classParamKinds,
    allSameKind,
    classNumParam,
    kindNumParam,
    concatPreds,
    putHaddock,
  )
where

#if MIN_VERSION_template_haskell(2,18,0)
import Language.Haskell.TH.Syntax (addModFinalizer, putDoc, DocLoc(DeclDoc))
#endif

import Control.Monad (when)
import qualified Data.Map as M
import GHC.TypeNats (Nat)
import Grisette.Unified.Internal.EvalModeTag (EvalModeTag)
import Language.Haskell.TH
  ( Dec (ClassD),
    Info (ClassI),
    Kind,
    Name,
    Pred,
    Q,
    Type (AppT, ArrowT, ConT, ForallT, StarT, VarT),
    newName,
    pprint,
    reify,
    varT,
  )
import Language.Haskell.TH.Datatype
  ( ConstructorInfo (constructorContext, constructorFields, constructorVars),
    DatatypeInfo (datatypeCons, datatypeInstTypes, datatypeVars),
    TypeSubstitution (applySubstitution),
    datatypeType,
    reifyDatatype,
    tvName,
  )
import Language.Haskell.TH.Datatype.TyVarBndr
  ( Specificity (SpecifiedSpec),
    TyVarBndrUnit,
    TyVarBndr_,
    mapTVFlag,
    mapTVName,
    tvKind,
  )
import Language.Haskell.TH.Syntax (Name (Name), OccName (OccName))

-- | Get the unqualified name of a 'Name'.
occName :: Name -> String
occName (Name (OccName name) _) = name

-- | Convert a 'ConstructorInfo' to a 'Type' of the constructor.
constructorInfoToType :: DatatypeInfo -> ConstructorInfo -> Q Type
constructorInfoToType dataType info = do
  let binders =
        mapTVFlag (const SpecifiedSpec)
          <$> datatypeVars dataType ++ constructorVars info
  let ctx = constructorContext info
  let fields = constructorFields info
  let tyBody =
        foldr (AppT . AppT ArrowT) (datatypeType dataType) fields
  if null binders then return tyBody else return $ ForallT binders ctx tyBody

-- | Check if a type variable is of kind 'EvalModeTag'.
tvIsMode :: TyVarBndr_ flag -> Bool
tvIsMode = (== ConT ''EvalModeTag) . tvKind

-- | Check if a type variable is of kind 'Nat'.
tvIsNat :: TyVarBndr_ flag -> Bool
tvIsNat = (== ConT ''Nat) . tvKind

-- | Check if a type variable is of kind 'Data.Kind.Type'.
tvIsStar :: TyVarBndr_ flag -> Bool
tvIsStar = (== StarT) . tvKind

-- | Check if a type variable is of kind 'Data.Kind.Type -> Data.Kind.Type'.
tvIsStarToStar :: TyVarBndr_ flag -> Bool
tvIsStarToStar = (== (AppT (AppT ArrowT StarT) StarT)) . tvKind

-- | Substitute the type variables in a 'DatatypeInfo' with the given
-- substitution map.
substDataType :: DatatypeInfo -> M.Map Name Type -> DatatypeInfo
substDataType d substMap =
  d
    { datatypeInstTypes = applySubstitution substMap <$> datatypeInstTypes d,
      datatypeCons = applySubstitution substMap <$> datatypeCons d
    }

-- | Convert a 'DatatypeInfo' to a 'DatatypeInfo' with fresh type variable
-- names.
datatypeToFreshNames :: DatatypeInfo -> Q DatatypeInfo
datatypeToFreshNames d = do
  let vars = datatypeVars d
  let names = tvName <$> vars
  freshNames <- traverse (newName . show) names
  let newDTVars = zipWith (\v n -> mapTVName (const n) v) vars freshNames
  let substMap = M.fromList $ zip names (VarT <$> freshNames)
  return $ substDataType d {datatypeVars = newDTVars} substMap

-- | Reify a datatype with fresh type variable names.
reifyDatatypeWithFreshNames :: Name -> Q DatatypeInfo
reifyDatatypeWithFreshNames name = do
  d <- reifyDatatype name
  datatypeToFreshNames d

-- | Check if all type variables have the same kind.
allSameKind :: [TyVarBndrUnit] -> Bool
allSameKind [] = True
allSameKind (x : xs) = all ((== tvKind x) . tvKind) xs

-- | Get the kinds of the type parameters of a class.
classParamKinds :: Name -> Q [Kind]
classParamKinds className = do
  cls <- reify className
  case cls of
    ClassI (ClassD _ _ bndrs _ _) _ -> return $ tvKind <$> bndrs
    _ ->
      fail $
        "symmetricClassParamKind:" <> show className <> " is not a class"

-- | Get the number of type parameters of a class.
classNumParam :: Name -> Q Int
classNumParam className = do
  cls <- reify className
  case cls of
    ClassI (ClassD _ _ bndrs _ _) _ -> return $ length bndrs
    _ ->
      fail $
        "classNumParam:" <> show className <> " is not a class"

-- | Get the kind of the single type parameter of a class.
singleParamClassParamKind :: Name -> Q Kind
singleParamClassParamKind className = do
  cls <- reify className
  case cls of
    ClassI (ClassD _ _ bndrs _ _) _ ->
      case bndrs of
        [x] -> return $ tvKind x
        _ ->
          fail $
            "singleParamClassParamKind: only support classes with one type "
              <> "parameter, but "
              <> show className
              <> " has "
              <> show (length bndrs)
    _ ->
      fail $
        "singleParamClassParamKind:" <> show className <> " is not a class"

-- | Get the kind of the binary type parameter of a class.
binaryClassParamKind :: Name -> Q Kind
binaryClassParamKind className = do
  cls <- reify className
  case cls of
    ClassI (ClassD _ _ bndrs _ _) _ ->
      case bndrs of
        [x, y] -> do
          when (tvKind x /= tvKind y) $
            fail "binaryClassParamKind: type parameters have different kinds"
          return $ tvKind x
        _ ->
          fail $
            "binaryClassParamKind: only support classes with two type "
              <> "parameters, but "
              <> show className
              <> " has "
              <> show (length bndrs)
    _ ->
      fail $
        "binaryClassParamKind:" <> show className <> " is not a class"

-- | Get a type with a possible substitution.
getTypeWithMaybeSubst :: TyVarBndrUnit -> Maybe Type -> Q Type
getTypeWithMaybeSubst tv Nothing = varT $ tvName tv
getTypeWithMaybeSubst _ (Just t) = return t

-- | Drop the last instantiated type parameter of a type.
dropLastTypeParam :: Type -> Q Type
dropLastTypeParam (AppT c _) = return c
dropLastTypeParam v =
  fail $
    "dropLastTypeParam: have no type parameters: "
      <> pprint v
      <> " / "
      <> show v

-- | Drop the last N instantiated type parameters of a type.
dropNTypeParam :: Int -> Type -> Q Type
dropNTypeParam 0 t = return t
dropNTypeParam n t = dropLastTypeParam t >>= dropNTypeParam (n - 1)

-- | Get the number of type parameters of a kind.
kindNumParam :: Kind -> Q Int
kindNumParam (AppT (AppT ArrowT _) k) = (1 +) <$> kindNumParam k
kindNumParam _ = return 0

-- | Concatenate two 'Maybe [Pred]'.
concatPreds :: Maybe [Pred] -> Maybe [Pred] -> Maybe [Pred]
concatPreds Nothing Nothing = Nothing
concatPreds (Just ps) Nothing = Just ps
concatPreds Nothing (Just ps) = Just ps
concatPreds (Just ps1) (Just ps2) = Just $ ps1 ++ ps2

#if MIN_VERSION_template_haskell(2,18,0)
-- | Put a haddock comment on a declaration.
putHaddock :: Name -> String -> Q ()
putHaddock name = addModFinalizer . putDoc (DeclDoc name) 
#else
-- | Put a haddock comment on a declaration.
-- (No-op because compiling with GHC < 9.2)
putHaddock :: Name -> String -> Q ()
putHaddock name _ = return ()
#endif
