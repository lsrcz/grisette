{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module      :   Grisette.Internal.TH.DerivePredefined
-- Copyright   :   (c) Sirui Lu 2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Internal.TH.DerivePredefined
  ( derivePredefined,
    derivePredefinedMultipleClasses,
    derive,
    deriveAll,
    deriveAllExcept,
  )
where

import Control.DeepSeq (NFData, NFData1)
import Data.Functor.Classes (Eq1, Ord1, Show1)
import Data.Hashable (Hashable)
import Data.Hashable.Lifted (Hashable1)
import GHC.Generics (Generic)
import Grisette.Internal.Core.Data.Class.EvalSym (EvalSym, EvalSym1)
import Grisette.Internal.Core.Data.Class.ExtractSym
  ( ExtractSym,
    ExtractSym1,
  )
import Grisette.Internal.Core.Data.Class.Format (Format, Format1)
import Grisette.Internal.Core.Data.Class.Mergeable (Mergeable, Mergeable1)
import Grisette.Internal.Core.Data.Class.SubstSym (SubstSym)
import Grisette.Internal.Core.Data.Class.SymEq (SymEq, SymEq1)
import Grisette.Internal.Core.Data.Class.SymOrd (SymOrd, SymOrd1)
import Grisette.Internal.Core.Data.Class.ToCon (ToCon, ToCon1)
import Grisette.Internal.Core.Data.Class.ToSym (ToSym, ToSym1)
import Grisette.Internal.SymPrim.AllSyms (AllSyms, AllSyms1)
import Grisette.Internal.TH.DeriveBuiltin
  ( deriveBuiltinExtra,
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
import Grisette.Internal.TH.DeriveWithHandlers (deriveWithHandlers)
import Grisette.Internal.TH.Util (classParamKinds, concatPreds)
import Grisette.Unified.Internal.Class.UnifiedSymEq
  ( UnifiedSymEq (withBaseSymEq),
    UnifiedSymEq1 (withBaseSymEq1),
  )
import Grisette.Unified.Internal.Class.UnifiedSymOrd
  ( UnifiedSymOrd (withBaseSymOrd),
    UnifiedSymOrd1 (withBaseSymOrd1),
  )
import Grisette.Unified.Internal.EvalMode (EvalMode)
import Grisette.Unified.Internal.EvalModeTag
  ( EvalModeTag (Con, Sym),
  )
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
  | nm == ''Format = return $ ViaDefault nm
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
  | nm == ''EvalSym = return $ ViaDefault nm
  | nm == ''ExtractSym = return $ ViaDefault nm
  | nm == ''Format = return $ ViaDefault nm
  | nm == ''Mergeable = return $ ViaDefault nm
  | nm == ''SymEq = return $ ViaDefault nm
  | nm == ''SymOrd = return $ ViaDefault nm
  | nm == ''SubstSym = return $ ViaDefault nm
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
  | nm == ''EvalSym =
      [''EvalSym, ''EvalSym1, ''Mergeable, ''Mergeable1]
  | nm == ''ExtractSym = [''ExtractSym, ''ExtractSym1]
  | nm == ''Format = [''Format, ''Format1]
  | nm == ''Mergeable = [''Mergeable1, ''Mergeable1]
  | nm == ''ToCon = [''ToCon, ''ToCon1]
  | nm == ''ToSym = [''ToSym, ''ToSym1]
  | nm == ''SymEq = [''SymEq, ''SymEq1, ''Mergeable, ''Mergeable1]
  | nm == ''SymOrd = [''SymOrd, ''SymOrd1, ''Mergeable, ''Mergeable1]
  | nm == ''SubstSym =
      [''SubstSym, ''SubstSym, ''Mergeable, ''Mergeable1]
  | otherwise = []

newtype ModeTypeParamHandler = ModeTypeParamHandler
  { mode :: Maybe EvalModeTag
  }

instance DeriveTypeParamHandler ModeTypeParamHandler where
  handleTypeParams _ ModeTypeParamHandler {..} tys = do
    mapM (uncurry handle) tys
    where
      handle ::
        [(TyVarBndrUnit, Maybe Type)] ->
        Maybe [Pred] ->
        Q ([(TyVarBndrUnit, Maybe Type)], Maybe [Pred])
      handle [(ty, substTy)] preds | tvKind ty == ConT ''EvalModeTag =
        case (mode, substTy) of
          (_, Just {}) -> return ([(ty, substTy)], preds)
          (Just Con, _) -> return ([(ty, Just $ PromotedT 'Con)], preds)
          (Just Sym, _) -> return ([(ty, Just $ PromotedT 'Sym)], preds)
          (Nothing, _) -> do
            evalMode <- [t|EvalMode $(varT $ tvName ty)|]
            return ([(ty, substTy)], concatPreds (Just [evalMode]) preds)
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

-- | Derive instances for a type with the given name, with the predefined
-- strategy.
derivePredefined :: Maybe EvalModeTag -> Name -> Name -> Q [Dec]
derivePredefined _ cls name
  | cls == ''Generic =
      deriveWithHandlers [] (Stock ''Generic) True 0 [name]
derivePredefined _ cls name
  | cls == ''UnifiedSymEq =
      deriveFunctorArgUnifiedInterfaceExtra
        [ SomeDeriveTypeParamHandler $ PrimaryConstraint ''Mergeable False,
          SomeDeriveTypeParamHandler $ PrimaryConstraint ''Mergeable1 False
        ]
        ''UnifiedSymEq
        'withBaseSymEq
        ''UnifiedSymEq1
        'withBaseSymEq1
        name
derivePredefined _ cls name
  | cls == ''UnifiedSymOrd =
      deriveFunctorArgUnifiedInterfaceExtra
        [ SomeDeriveTypeParamHandler $ PrimaryConstraint ''Mergeable False,
          SomeDeriveTypeParamHandler $ PrimaryConstraint ''Mergeable1 False
        ]
        ''UnifiedSymOrd
        'withBaseSymOrd
        ''UnifiedSymOrd1
        'withBaseSymOrd1
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

-- | Derive instances for a type with the given name, with the predefined
-- strategy.
--
-- Multiple classes can be derived at once.
derivePredefinedMultipleClasses ::
  Maybe EvalModeTag -> [Name] -> Name -> Q [Dec]
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
    ''EvalSym,
    ''ExtractSym,
    ''Format,
    ''Mergeable,
    ''SymEq,
    ''SymOrd,
    ''SubstSym,
    ''ToCon,
    ''ToSym,
    ''UnifiedSymEq,
    ''UnifiedSymOrd
  ]

-- | Derive specified classes for a type with the given name.
--
-- Support the same set of classes as 'deriveAll'.
derive :: Name -> [Name] -> Q [Dec]
derive = flip (derivePredefinedMultipleClasses Nothing)

-- | Derive all classes related to Grisette for a type with the given name.
--
-- Classes that are be derived by this procedure are:
--
-- * 'Generic'
-- * 'Show'
-- * 'Eq'
-- * 'Ord'
-- * 'Lift'
-- * 'NFData'
-- * 'Hashable'
-- * 'AllSyms'
-- * 'EvalSym'
-- * 'ExtractSym'
-- * 'Format'
-- * 'Mergeable'
-- * 'SymEq'
-- * 'SymOrd'
-- * 'SubstSym'
-- * 'ToCon'
-- * 'ToSym'
-- * 'UnifiedSymEq'
-- * 'UnifiedSymOrd'
--
-- 'Ord' isn't valid for all types (symbolic-only types), so it may be necessary
-- to exclude it.
--
-- 'deriveAll' needs the following language extensions:
--
-- * DeriveAnyClass
-- * DeriveGeneric
-- * DeriveLift
-- * DerivingVia
-- * FlexibleContexts
-- * FlexibleInstances
-- * MonoLocalBinds
-- * MultiParamTypeClasses
-- * ScopedTypeVariables
-- * StandaloneDeriving
-- * TemplateHaskell
-- * TypeApplications
-- * UndecidableInstances
--
-- Deriving for a newtype may also need
--
-- * GeneralizedNewtypeDeriving
--
-- You may get warnings if you don't have the following extensions:
--
-- * TypeOperators
--
-- It also requires that the v'Generics.Deriving.Default.Default' data
-- constructor is visible.
-- You may get strange errors if you only import
-- v'Generics.Deriving.Default.Default' type but not the data constructor.
deriveAll :: Name -> Q [Dec]
deriveAll = derivePredefinedMultipleClasses Nothing allGrisetteClasses

-- | Derive all classes related to Grisette for a type with the given name,
-- except for the given classes.
--
-- Excluding 'Ord' or 'SymOrd' will also exclude 'UnifiedSymOrd'.
-- Excluding 'Eq' or 'SymEq' will also exclude 'UnifiedSymEq'.
deriveAllExcept :: Name -> [Name] -> Q [Dec]
deriveAllExcept nm clss =
  derivePredefinedMultipleClasses
    Nothing
    (filter (`notElem` allExcluded) allGrisetteClasses)
    nm
  where
    allExcluded =
      ([''UnifiedSymEq | ''Eq `elem` clss || ''SymEq `elem` clss])
        <> ([''UnifiedSymOrd | ''Ord `elem` clss || ''SymOrd `elem` clss])
        <> clss
