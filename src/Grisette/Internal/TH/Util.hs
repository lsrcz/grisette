{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Unused LANGUAGE pragma" #-}

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
  )
where

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

occName :: Name -> String
occName (Name (OccName name) _) = name

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

tvIsMode :: TyVarBndr_ flag -> Bool
tvIsMode = (== ConT ''EvalModeTag) . tvKind

tvIsNat :: TyVarBndr_ flag -> Bool
tvIsNat = (== ConT ''Nat) . tvKind

tvIsStar :: TyVarBndr_ flag -> Bool
tvIsStar = (== StarT) . tvKind

tvIsStarToStar :: TyVarBndr_ flag -> Bool
tvIsStarToStar = (== (AppT (AppT ArrowT StarT) StarT)) . tvKind

substDataType :: DatatypeInfo -> M.Map Name Type -> DatatypeInfo
substDataType d substMap =
  d
    { datatypeInstTypes = applySubstitution substMap <$> datatypeInstTypes d,
      datatypeCons = applySubstitution substMap <$> datatypeCons d
    }

datatypeToFreshNames :: DatatypeInfo -> Q DatatypeInfo
datatypeToFreshNames d = do
  let vars = datatypeVars d
  let names = tvName <$> vars
  freshNames <- traverse (newName . show) names
  let newDTVars = zipWith (\v n -> mapTVName (const n) v) vars freshNames
  let substMap = M.fromList $ zip names (VarT <$> freshNames)
  return $ substDataType d {datatypeVars = newDTVars} substMap

reifyDatatypeWithFreshNames :: Name -> Q DatatypeInfo
reifyDatatypeWithFreshNames name = do
  d <- reifyDatatype name
  datatypeToFreshNames d

allSameKind :: [TyVarBndrUnit] -> Bool
allSameKind [] = True
allSameKind (x : xs) = all ((== tvKind x) . tvKind) xs

classParamKinds :: Name -> Q [Kind]
classParamKinds className = do
  cls <- reify className
  case cls of
    ClassI (ClassD _ _ bndrs _ _) _ -> return $ tvKind <$> bndrs
    _ ->
      fail $
        "symmetricClassParamKind:" <> show className <> " is not a class"

classNumParam :: Name -> Q Int
classNumParam className = do
  cls <- reify className
  case cls of
    ClassI (ClassD _ _ bndrs _ _) _ -> return $ length bndrs
    _ ->
      fail $
        "classNumParam:" <> show className <> " is not a class"

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

getTypeWithMaybeSubst :: TyVarBndrUnit -> Maybe Type -> Q Type
getTypeWithMaybeSubst tv Nothing = varT $ tvName tv
getTypeWithMaybeSubst _ (Just t) = return t

dropLastTypeParam :: Type -> Q Type
dropLastTypeParam (AppT c _) = return c
dropLastTypeParam v =
  fail $
    "dropLastTypeParam: have no type parameters: "
      <> pprint v
      <> " / "
      <> show v

dropNTypeParam :: Int -> Type -> Q Type
dropNTypeParam 0 t = return t
dropNTypeParam n t = dropLastTypeParam t >>= dropNTypeParam (n - 1)

kindNumParam :: Kind -> Q Int
kindNumParam (AppT (AppT ArrowT _) k) = (1 +) <$> kindNumParam k
kindNumParam _ = return 0

concatPreds :: Maybe [Pred] -> Maybe [Pred] -> Maybe [Pred]
concatPreds Nothing Nothing = Nothing
concatPreds (Just ps) Nothing = Just ps
concatPreds Nothing (Just ps) = Just ps
concatPreds (Just ps1) (Just ps2) = Just $ ps1 ++ ps2
