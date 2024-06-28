{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Grisette.Internal.TH.DerivePredefined
  ( derivePredefined,
    derivePredefinedMultipleClasses,
    deriveAll,
    deriveAllExcept,
  )
where

import Control.DeepSeq (NFData, NFData1)
import Data.Functor.Classes (Eq1, Ord1, Show1)
import Data.Hashable (Hashable)
import Data.Hashable.Lifted (Hashable1)
import GHC.Generics (Generic)
import Grisette.Internal.Core.Data.Class.EvaluateSym (EvaluateSym, EvaluateSym1)
import Grisette.Internal.Core.Data.Class.ExtractSymbolics
  ( ExtractSymbolics,
    ExtractSymbolics1,
  )
import Grisette.Internal.Core.Data.Class.GPretty (GPretty, GPretty1)
import Grisette.Internal.Core.Data.Class.Mergeable (Mergeable, Mergeable1)
import Grisette.Internal.Core.Data.Class.SEq (SEq, SEq1)
import Grisette.Internal.Core.Data.Class.SOrd (SOrd, SOrd1)
import Grisette.Internal.Core.Data.Class.SubstituteSym (SubstituteSym)
import Grisette.Internal.Core.Data.Class.ToCon (ToCon, ToCon1)
import Grisette.Internal.Core.Data.Class.ToSym (ToSym, ToSym1)
import Grisette.Internal.SymPrim.AllSyms (AllSyms, AllSyms1)
import Grisette.Internal.TH.DeriveBuiltin
  ( deriveBuiltinExtra,
    deriveWithHandlers,
  )
import Grisette.Internal.TH.DeriveInstanceProvider
  ( Strategy (Anyclass, Stock, ViaDefault, WithNewtype),
  )
import Grisette.Internal.TH.DeriveTypeParamHandler
  ( DeriveTypeParamHandler (handleBody, handleTypeParams),
    PrimaryConstraint (PrimaryConstraint),
    SomeDeriveTypeParamHandler (SomeDeriveTypeParamHandler),
  )
import Grisette.Internal.TH.DeriveUnifiedInterface
  ( deriveFunctorArgUnifiedInterfaceExtra,
  )
import Grisette.Internal.TH.Util (classParamKinds, concatPreds)
import Grisette.Unified.Internal.Class.UnifiedSEq
  ( UnifiedSEq (withBaseSEq),
    UnifiedSEq1 (withBaseSEq1),
  )
import Grisette.Unified.Internal.Class.UnifiedSOrd
  ( UnifiedSOrd (withBaseSOrd),
    UnifiedSOrd1 (withBaseSOrd1),
  )
import Grisette.Unified.Internal.EvaluationMode
  ( EvaluationMode (Con, Sym),
  )
import Grisette.Unified.Internal.IsMode (IsMode)
import Language.Haskell.TH
  ( Dec,
    Kind,
    Name,
    Pred,
    Q,
    Type (ConT, PromotedT),
    appT,
    conT,
    varT,
  )
import Language.Haskell.TH.Datatype
  ( DatatypeInfo (datatypeVariant),
    DatatypeVariant (Datatype, Newtype),
    reifyDatatype,
    tvKind,
    tvName,
  )
import Language.Haskell.TH.Datatype.TyVarBndr (TyVarBndrUnit)
import Language.Haskell.TH.Syntax (Lift)

newtypeDefaultStrategy :: Name -> Q Strategy
newtypeDefaultStrategy nm
  | nm == ''Show = return $ Stock nm
  | nm == ''GPretty = return $ ViaDefault nm
  | nm == ''Lift = return $ Stock nm
  | nm == ''ToCon = return $ ViaDefault nm
  | nm == ''ToSym = return $ ViaDefault nm
  | otherwise = return $ WithNewtype nm

dataDefaultStrategy :: Name -> Q Strategy
dataDefaultStrategy nm
  | nm == ''Show = return $ Stock nm
  | nm == ''Eq = return $ Stock nm
  | nm == ''Ord = return $ Stock nm
  | nm == ''Lift = return $ Stock nm
  | nm == ''NFData = return $ Anyclass nm
  | nm == ''Hashable = return $ Anyclass nm
  | nm == ''ToCon = return $ ViaDefault nm
  | nm == ''ToSym = return $ ViaDefault nm
  | nm == ''AllSyms = return $ ViaDefault nm
  | nm == ''EvaluateSym = return $ ViaDefault nm
  | nm == ''ExtractSymbolics = return $ ViaDefault nm
  | nm == ''GPretty = return $ ViaDefault nm
  | nm == ''Mergeable = return $ ViaDefault nm
  | nm == ''SEq = return $ ViaDefault nm
  | nm == ''SOrd = return $ ViaDefault nm
  | nm == ''SubstituteSym = return $ ViaDefault nm
  | otherwise = fail $ "Unsupported class: " <> show nm

allNeededConstraints :: Name -> [Name]
allNeededConstraints nm
  | nm == ''Show = [''Show, ''Show1]
  | nm == ''Eq = [''Eq, ''Eq1]
  | nm == ''Ord = [''Ord, ''Ord1]
  | nm == ''Lift = [''Lift]
  | nm == ''NFData = [''NFData, ''NFData1]
  | nm == ''Hashable = [''Hashable, ''Hashable1]
  | nm == ''AllSyms = [''AllSyms, ''AllSyms1]
  | nm == ''EvaluateSym =
      [''EvaluateSym, ''EvaluateSym1, ''Mergeable, ''Mergeable1]
  | nm == ''ExtractSymbolics = [''ExtractSymbolics, ''ExtractSymbolics1]
  | nm == ''GPretty = [''GPretty, ''GPretty1]
  | nm == ''Mergeable = [''Mergeable1, ''Mergeable1]
  | nm == ''ToCon = [''ToCon, ''ToCon1]
  | nm == ''ToSym = [''ToSym, ''ToSym1]
  | nm == ''SEq = [''SEq, ''SEq1, ''Mergeable, ''Mergeable1]
  | nm == ''SOrd = [''SOrd, ''SOrd1, ''Mergeable, ''Mergeable1]
  | nm == ''SubstituteSym =
      [''SubstituteSym, ''SubstituteSym, ''Mergeable, ''Mergeable1]
  | otherwise = []

newtype ModeTypeParamHandler = ModeTypeParamHandler
  { mode :: Maybe EvaluationMode
  }

instance DeriveTypeParamHandler ModeTypeParamHandler where
  handleTypeParams _ ModeTypeParamHandler {..} tys = do
    mapM (uncurry handle) tys
    where
      handle ::
        [(TyVarBndrUnit, Maybe Type)] ->
        Maybe [Pred] ->
        Q ([(TyVarBndrUnit, Maybe Type)], Maybe [Pred])
      handle [(ty, substTy)] preds | tvKind ty == ConT ''EvaluationMode =
        case (mode, substTy) of
          (_, Just {}) -> return ([(ty, substTy)], preds)
          (Just Con, _) -> return ([(ty, Just $ PromotedT 'Con)], preds)
          (Just Sym, _) -> return ([(ty, Just $ PromotedT 'Sym)], preds)
          (Nothing, _) -> do
            isMode <- [t|IsMode $(varT $ tvName ty)|]
            return ([(ty, substTy)], concatPreds (Just [isMode]) preds)
      handle tys preds = return (tys, preds)
  handleBody _ _ = return []

newtype FixInnerConstraints = FixInnerConstraints {cls :: Name}

instance DeriveTypeParamHandler FixInnerConstraints where
  handleTypeParams _ _ = return
  handleBody FixInnerConstraints {..} types = do
    kinds <- classParamKinds cls
    concat <$> mapM (handle kinds) types
    where
      handle :: [Kind] -> [Type] -> Q [Pred]
      handle k tys
        | length k /= length tys =
            fail "FixInnerConstraints: kind and type length mismatch"
        | otherwise = do
            constr <- foldl appT (conT cls) $ return <$> tys
            return [constr]

derivePredefined :: Maybe EvaluationMode -> Name -> Name -> Q [Dec]
derivePredefined _ cls name
  | cls == ''Generic =
      deriveWithHandlers [] (Stock ''Generic) True 0 [name]
derivePredefined _ cls name
  | cls == ''UnifiedSEq =
      deriveFunctorArgUnifiedInterfaceExtra
        [ SomeDeriveTypeParamHandler $ PrimaryConstraint ''Mergeable False,
          SomeDeriveTypeParamHandler $ PrimaryConstraint ''Mergeable1 False
        ]
        ''UnifiedSEq
        'withBaseSEq
        ''UnifiedSEq1
        'withBaseSEq1
        name
derivePredefined _ cls name
  | cls == ''UnifiedSOrd =
      deriveFunctorArgUnifiedInterfaceExtra
        [ SomeDeriveTypeParamHandler $ PrimaryConstraint ''Mergeable False,
          SomeDeriveTypeParamHandler $ PrimaryConstraint ''Mergeable1 False
        ]
        ''UnifiedSOrd
        'withBaseSOrd
        ''UnifiedSOrd1
        'withBaseSOrd1
        name
derivePredefined evmode cls name = do
  d <- reifyDatatype name
  strategy <-
    if
      | datatypeVariant d == Datatype -> dataDefaultStrategy cls
      | datatypeVariant d == Newtype -> newtypeDefaultStrategy cls
      | otherwise ->
          fail "Currently only non-GADTs data or newtype are supported."
  deriveBuiltinExtra
    [ SomeDeriveTypeParamHandler $ ModeTypeParamHandler evmode,
      SomeDeriveTypeParamHandler $ FixInnerConstraints cls
    ]
    False
    strategy
    (allNeededConstraints cls)
    name

derivePredefinedMultipleClasses ::
  Maybe EvaluationMode -> [Name] -> Name -> Q [Dec]
derivePredefinedMultipleClasses evmode clss name =
  concat <$> traverse (\cls -> derivePredefined evmode cls name) clss

allGrisetteClasses :: [Name]
allGrisetteClasses =
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
    ''ToSym,
    ''UnifiedSEq,
    ''UnifiedSOrd
  ]

deriveAll :: Name -> Q [Dec]
deriveAll = derivePredefinedMultipleClasses Nothing allGrisetteClasses

deriveAllExcept :: Name -> [Name] -> Q [Dec]
deriveAllExcept nm clss =
  derivePredefinedMultipleClasses
    Nothing
    (filter (`notElem` clss) allGrisetteClasses)
    nm
