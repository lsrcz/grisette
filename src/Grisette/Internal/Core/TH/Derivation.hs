{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TemplateHaskell #-}
{-# HLINT ignore "Use fewer imports" #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Grisette.Internal.Core.TH.Derivation
  ( deriveNewtype,
    deriveAnyclass,
    deriveStock,
    deriveViaDefault,
    deriveNewtypeWithMode,
    deriveAnyclassWithMode,
    deriveStockWithMode,
    deriveViaDefaultWithMode,
    deriveConversions,
    deriveGrisette,
    deriveAllGrisette,
    deriveAllGrisetteExcept,
    deriveUnifiedSEq,
  )
where

import Control.DeepSeq (NFData)
import Control.Monad (when, zipWithM)
import Data.Containers.ListUtils (nubOrd)
import Data.Hashable (Hashable)
import qualified Data.Map as M
import Data.Maybe (isNothing)
import Data.Typeable (Typeable)
import GHC.TypeNats (KnownNat, Nat, type (<=))
import Generics.Deriving (Default, Generic)
import Grisette.Internal.Core.Data.Class.EvaluateSym (EvaluateSym)
import Grisette.Internal.Core.Data.Class.ExtractSymbolics (ExtractSymbolics)
import Grisette.Internal.Core.Data.Class.GPretty (GPretty)
import Grisette.Internal.Core.Data.Class.Mergeable (Mergeable)
import Grisette.Internal.Core.Data.Class.SEq (SEq)
import Grisette.Internal.Core.Data.Class.SOrd (SOrd)
import Grisette.Internal.Core.Data.Class.SubstituteSym (SubstituteSym)
import Grisette.Internal.Core.Data.Class.ToCon (ToCon)
import Grisette.Internal.Core.Data.Class.ToSym (ToSym)
import Grisette.Internal.SymPrim.AllSyms (AllSyms)
import Grisette.Unified.Internal.Class.UnifiedSEq (UnifiedSEq (withBaseSEq))
import Grisette.Unified.Internal.EvaluationMode (EvaluationMode (Con, Sym))
import Grisette.Unified.Internal.IsMode (IsMode)
import Grisette.Unified.Internal.Util (withMode)
import Language.Haskell.TH
  ( Dec (StandaloneDerivD),
    DerivStrategy
      ( AnyclassStrategy,
        NewtypeStrategy,
        StockStrategy,
        ViaStrategy
      ),
    Exp,
    Name,
    Pred,
    Q,
    Type (AppT, ConT, PromotedT, StarT, VarT),
    instanceD,
    lam1E,
    normalB,
    pprint,
    valD,
    varE,
    varP,
  )
import Language.Haskell.TH.Datatype
  ( ConstructorInfo (constructorFields),
    DatatypeInfo
      ( datatypeCons,
        datatypeInstTypes,
        datatypeVariant,
        datatypeVars
      ),
    DatatypeVariant (Datatype, Newtype),
    TypeSubstitution (applySubstitution),
    datatypeType,
    reifyDatatype,
  )
import Language.Haskell.TH.Datatype.TyVarBndr
  ( TyVarBndrUnit,
    TyVarBndr_,
    mapTVName,
    tvKind,
    tvName,
  )
import Language.Haskell.TH.Syntax (Lift, newName)

#if MIN_VERSION_base(4,16,0)
#else
import Data.List (sort)
import Data.Containers.ListUtils (nubOrd)
import Language.Haskell.TH.Datatype (ConstructorInfo (constructorFields))
#endif

tvIsMode :: TyVarBndr_ flag -> Bool
tvIsMode = (== ConT ''EvaluationMode) . tvKind

tvIsNat :: TyVarBndr_ flag -> Bool
tvIsNat = (== ConT ''Nat) . tvKind

tvIsStar :: TyVarBndr_ flag -> Bool
tvIsStar = (== StarT) . tvKind

tvIsUnsupported :: TyVarBndr_ flag -> Bool
tvIsUnsupported bndr = not $ tvIsMode bndr || tvIsNat bndr || tvIsStar bndr

tvType :: TyVarBndr_ flag -> Type
tvType = VarT . tvName

data TyVarCategorized flag = TyVarCategorized
  { modeTyVars :: [Q Type],
    natTyVars :: [Q Type],
    starTyVars :: [Q Type],
    unsupportedTyVars :: [Q Type]
  }

categorizeTyVars :: [TyVarBndr_ flag] -> TyVarCategorized flag
categorizeTyVars = foldr categorize (TyVarCategorized [] [] [] [])
  where
    categorize bndr acc
      | tvIsMode bndr = acc {modeTyVars = return (tvType bndr) : modeTyVars acc}
      | tvIsNat bndr = acc {natTyVars = return (tvType bndr) : natTyVars acc}
      | tvIsStar bndr = acc {starTyVars = return (tvType bndr) : starTyVars acc}
      | tvIsUnsupported bndr =
          acc {unsupportedTyVars = return (tvType bndr) : unsupportedTyVars acc}
      | otherwise = acc

data Strategy = Stock | WithNewtype | Via | Anyclass | SpecialForGeneric
  deriving (Eq)

evModeSubstMap :: Maybe EvaluationMode -> [TyVarBndrUnit] -> M.Map Name Type
evModeSubstMap Nothing _ = M.empty
evModeSubstMap (Just mode) bndrs =
  M.fromList $ (\bndr -> (tvName bndr, promote mode)) <$> filter tvIsMode bndrs
  where
    promote :: EvaluationMode -> Type
    promote Con = PromotedT 'Con
    promote Sym = PromotedT 'Sym

#if MIN_VERSION_base(4,16,0)
fixInnerConstraints :: DatatypeInfo -> Name -> Q [Pred]
fixInnerConstraints _ _ = return []
#else
fixInnerConstraints :: DatatypeInfo -> Name -> Q [Pred]
fixInnerConstraints d cls = do
  let cons = datatypeCons d
  let allFields = nubOrd $ concatMap constructorFields cons
  traverse (\ty -> [t|$(return $ AppT (ConT cls) ty)|]) allFields
#endif

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

deriveWithMode :: Maybe EvaluationMode -> Strategy -> Name -> Name -> Q [Dec]
deriveWithMode evmode strategy name cls = do
  d <- reifyDatatype name
  let tyVars = datatypeVars d
  bndrConstraints <- concat <$> traverse genBndrConstraint tyVars
  let substMap = evModeSubstMap evmode tyVars
  let substedDataType = substDataType d substMap
  let substTy = datatypeType substedDataType
  innerConstraints <- fixInnerConstraints substedDataType cls
  deriveStrategy <- getStrategy substTy
  return
    [ StandaloneDerivD
        (Just deriveStrategy)
        ( if strategy == SpecialForGeneric
            then []
            else (bndrConstraints ++ innerConstraints)
        )
        (AppT (ConT cls) substTy)
    ]
  where
    getStrategy substTy =
      case strategy of
        Stock -> return StockStrategy
        SpecialForGeneric -> return StockStrategy
        WithNewtype -> return NewtypeStrategy
        Via ->
          ViaStrategy
            <$> [t|Default $(return substTy)|]
        Anyclass -> return AnyclassStrategy
    genBndrConstraint :: TyVarBndr_ flag -> Q [Type]
    genBndrConstraint bndr = do
      let name = tvName bndr
      let tv = return $ VarT name
      let kind = tvKind bndr
      case kind of
        StarT -> sequence [[t|$(return $ ConT cls) $tv|], [t|Mergeable $tv|]]
        (ConT nm)
          | nm == ''EvaluationMode && isNothing evmode ->
              (: []) <$> [t|IsMode $tv|]
        (ConT nm) | nm == ''EvaluationMode -> return []
        (ConT nm)
          | nm == ''Nat -> sequence [[t|KnownNat $tv|], [t|1 <= $tv|]]
        _ -> fail $ "Unsupported kind in type arguments: " ++ pprint kind

deriveInstances :: Strategy -> Name -> [Name] -> Q [Dec]
deriveInstances strategy name =
  fmap concat <$> traverse (deriveWithMode Nothing strategy name)

deriveInstancesWithMode ::
  Strategy -> EvaluationMode -> Name -> [Name] -> Q [Dec]
deriveInstancesWithMode strategy mode name =
  fmap concat <$> traverse (deriveWithMode (Just mode) strategy name)

deriveNewtype :: Name -> [Name] -> Q [Dec]
deriveNewtype = deriveInstances WithNewtype

deriveAnyclass :: Name -> [Name] -> Q [Dec]
deriveAnyclass = deriveInstances Anyclass

deriveStock :: Name -> [Name] -> Q [Dec]
deriveStock = deriveInstances Stock

deriveViaDefault :: Name -> [Name] -> Q [Dec]
deriveViaDefault = deriveInstances Via

deriveNewtypeWithMode :: EvaluationMode -> Name -> [Name] -> Q [Dec]
deriveNewtypeWithMode = deriveInstancesWithMode WithNewtype

deriveAnyclassWithMode :: EvaluationMode -> Name -> [Name] -> Q [Dec]
deriveAnyclassWithMode = deriveInstancesWithMode Anyclass

deriveStockWithMode :: EvaluationMode -> Name -> [Name] -> Q [Dec]
deriveStockWithMode = deriveInstancesWithMode Stock

deriveViaDefaultWithMode :: EvaluationMode -> Name -> [Name] -> Q [Dec]
deriveViaDefaultWithMode = deriveInstancesWithMode Via

fixConversionInnerConstraints ::
  DatatypeInfo -> DatatypeInfo -> Name -> Q [Pred]
fixConversionInnerConstraints dfrom dto cls = do
  let fromCons = datatypeCons dfrom
  let toCons = datatypeCons dto
  when (length fromCons /= length toCons) $
    fail "The number of constructors must be the same."
  let cons = zip fromCons toCons
  allFields <- nubOrd . concat <$> traverse (uncurry zipFields) cons
  traverse
    (\(fromty, toty) -> [t|$(return $ AppT (AppT (ConT cls) fromty) toty)|])
    allFields
  where
    zipFields from to = do
      let fromFields = constructorFields from
      let toFields = constructorFields to
      when (length fromFields /= length toFields) $
        fail "The number of fields must be the same."
      return $ zip fromFields toFields

deriveConversionWithMode :: Name -> Name -> Name -> Q [Dec]
deriveConversionWithMode from to cls = do
  dfrom <- reifyDatatype from >>= datatypeToFreshNames
  dto <- reifyDatatype to >>= datatypeToFreshNames
  let fromTyVars = datatypeVars dfrom
  let toTyVars = datatypeVars dto
  when (length fromTyVars /= length toTyVars) $
    fail "The number of type arguments must be the same."
  bndrConstraints <- concat <$> zipWithM genBndrConstraint fromTyVars toTyVars
  let fromSubstTy = datatypeType dfrom
  let toSubstTy = datatypeType dto
  innerConstraints <-
    fixConversionInnerConstraints dfrom dto cls
  strategy <- ViaStrategy <$> [t|Default $(return toSubstTy)|]
  return
    [ StandaloneDerivD
        (Just strategy)
        (bndrConstraints ++ innerConstraints)
        (AppT (AppT (ConT cls) fromSubstTy) toSubstTy)
    ]
  where
    genBndrConstraint :: TyVarBndr_ flag -> TyVarBndr_ flag -> Q [Type]
    genBndrConstraint bndrFrom bndrTo = do
      let nameFrom = tvName bndrFrom
      let nameTo = tvName bndrTo
      let tvFrom = return $ VarT nameFrom
      let tvTo = return $ VarT nameTo
      let kindFrom = tvKind bndrFrom
      let kindTo = tvKind bndrTo
      when (kindFrom /= kindTo) $
        fail "The kinds of the type arguments must be the aligned."
      case kindFrom of
        StarT ->
          sequence
            [ [t|$(return $ ConT cls) $tvFrom $tvTo|],
              [t|Mergeable $tvFrom|],
              [t|Mergeable $tvTo|]
            ]
        (ConT nm)
          | nm == ''EvaluationMode ->
              sequence
                [[t|IsMode $tvFrom|], [t|IsMode $tvTo {-, [t|$tvFrom ~ $tvTo|]-}|]]
        (ConT nm) | nm == ''EvaluationMode -> return []
        (ConT nm)
          | nm == ''Nat ->
              sequence
                [ [t|KnownNat $tvFrom|],
                  [t|1 <= $tvFrom|],
                  [t|$tvFrom ~ $tvTo|]
                ]
        _ ->
          fail $ "Unsupported kind in type arguments: " ++ pprint kindFrom

deriveConversions ::
  Name -> Name -> [Name] -> Q [Dec]
deriveConversions from to =
  fmap concat . traverse (deriveConversionWithMode from to)

newtypeDefaultStrategy :: Name -> Q Strategy
newtypeDefaultStrategy nm
  | nm == ''Generic = return SpecialForGeneric
  | nm == ''Show = return Stock
  | nm == ''Lift = return Stock
  | otherwise = return WithNewtype

dataDefaultStrategy :: Name -> Q Strategy
dataDefaultStrategy nm
  | nm == ''Generic = return SpecialForGeneric
  | nm == ''Show = return Stock
  | nm == ''Eq = return Stock
  | nm == ''Ord = return Stock
  | nm == ''Lift = return Stock
  | nm == ''NFData = return Anyclass
  | nm == ''Hashable = return Anyclass
  | nm == ''AllSyms = return Via
  | nm == ''EvaluateSym = return Via
  | nm == ''ExtractSymbolics = return Via
  | nm == ''GPretty = return Via
  | nm == ''Mergeable = return Via
  | nm == ''SEq = return Via
  | nm == ''SOrd = return Via
  | nm == ''SubstituteSym = return Via
  | otherwise = fail $ "Unsupported class: " <> show nm

validEvaluationMode :: Name -> Q (Maybe EvaluationMode)
validEvaluationMode nm
  | nm == ''Ord = return $ Just Con
  | otherwise = return Nothing

deriveGrisette :: Name -> [Name] -> Q [Dec]
deriveGrisette nm clss = do
  d <- reifyDatatype nm
  let conversions = filter (\cls -> cls == ''ToCon || cls == ''ToSym) clss
  let nonConversions = filter (\cls -> cls /= ''ToCon && cls /= ''ToSym) clss
  conversionDerivation <- deriveConversionWithDefaultStrategy' nm conversions
  nonConversionDerivation <-
    if
      | datatypeVariant d == Datatype ->
          deriveWithDefaultStrategy' dataDefaultStrategy nm nonConversions
      | datatypeVariant d == Newtype ->
          deriveWithDefaultStrategy' newtypeDefaultStrategy nm nonConversions
      | otherwise ->
          fail "Currently only non-GADTs data or newtype are supported."
  unifiedSEq <-
    if (elem ''Eq clss && elem ''SEq clss)
      then deriveUnifiedSEq nm
      else return []
  return $ conversionDerivation <> nonConversionDerivation <> unifiedSEq
  where
    deriveWithDefaultStrategy' ::
      (Name -> Q Strategy) -> Name -> [Name] -> Q [Dec]
    deriveWithDefaultStrategy' getStrategy nm clss = do
      strategies <- traverse getStrategy clss
      modes <- traverse validEvaluationMode clss
      fmap concat
        $ traverse
          ( \(strategy, mode, cls) ->
              deriveWithMode mode strategy nm cls
          )
        $ zip3 strategies modes clss
    deriveConversionWithDefaultStrategy' :: Name -> [Name] -> Q [Dec]
    deriveConversionWithDefaultStrategy' nm =
      deriveConversions nm nm

allGrisette :: [Name]
allGrisette =
  [ ''Generic,
    ''Show,
    ''Eq,
    ''Ord,
    ''Lift,
    ''NFData,
    ''Hashable,
    ''AllSyms,
    ''EvaluateSym,
    ''ExtractSymbolics,
    ''GPretty,
    ''Mergeable,
    ''SEq,
    ''SOrd,
    ''SubstituteSym,
    ''ToCon,
    ''ToSym
  ]

deriveAllGrisette :: Name -> Q [Dec]
deriveAllGrisette nm = deriveGrisette nm allGrisette

deriveAllGrisetteExcept :: Name -> [Name] -> Q [Dec]
deriveAllGrisetteExcept nm clss = do
  deriveGrisette nm $ filter (`notElem` clss) allGrisette

deriveUnifiedSEq :: Name -> Q [Dec]
deriveUnifiedSEq name = do
  d <- reifyDatatype name
  let tyVars = datatypeVars d
  let categorizedTyVars = categorizeTyVars tyVars
  mode <- case modeTyVars categorizedTyVars of
    [var] -> return var
    _ ->
      fail "The number of mode type arguments must be 1."
  bndrConstraints <-
    concat <$> mapM (genBndrConstraint mode) tyVars
  sequence
    [ instanceD
        (return bndrConstraints)
        [t|UnifiedSEq $mode $(return $ datatypeType d)|]
        [body mode $ starTyVars categorizedTyVars]
    ]
  where
    genBndrConstraint :: Q Type -> TyVarBndr_ flag -> Q [Type]
    genBndrConstraint mode bndr = do
      let name = tvName bndr
      let tv = return $ VarT name
      let kind = tvKind bndr
      case kind of
        StarT -> sequence [[t|UnifiedSEq $mode $tv|], [t|Mergeable $tv|]]
        (ConT nm) | nm == ''EvaluationMode -> (: []) <$> [t|Typeable $tv|]
        (ConT nm)
          | nm == ''Nat -> sequence [[t|KnownNat $tv|], [t|1 <= $tv|]]
        _ -> fail $ "Unsupported kind in type arguments: " ++ pprint kind
    applyWithBaseSEq :: Q Type -> Q Type -> Q Exp -> Q Exp
    applyWithBaseSEq mode var exp = [|withBaseSEq @($mode) @($var) $exp|]
    body :: Q Type -> [Q Type] -> Q Dec
    body mode starVars = do
      var <- newName "r"
      let arg = varP var
      let branch = foldr (applyWithBaseSEq mode) (varE var) starVars
      let exp = lam1E arg [|withMode @($mode) $branch $branch|]
      valD (varP 'withBaseSEq) (normalB exp) []
