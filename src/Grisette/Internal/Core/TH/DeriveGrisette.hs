{-# LANGUAGE CPP #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Unused LANGUAGE pragma" #-}

module Grisette.Internal.Core.TH.DeriveGrisette
  ( deriveGrisette,
    deriveAllGrisette,
    deriveAllGrisetteExcept,
    deriveUnifiedSEq,
  )
where

import Control.DeepSeq (NFData)
import Data.Hashable (Hashable)
import Generics.Deriving (Generic)
import Grisette.Internal.Core.Data.Class.EvaluateSym (EvaluateSym)
import Grisette.Internal.Core.Data.Class.ExtractSymbolics (ExtractSymbolics)
import Grisette.Internal.Core.Data.Class.GPretty (GPretty)
import Grisette.Internal.Core.Data.Class.Mergeable (Mergeable)
import Grisette.Internal.Core.Data.Class.SEq (SEq)
import Grisette.Internal.Core.Data.Class.SOrd (SOrd)
import Grisette.Internal.Core.Data.Class.SubstituteSym (SubstituteSym)
import Grisette.Internal.Core.Data.Class.ToCon (ToCon)
import Grisette.Internal.Core.Data.Class.ToSym (ToSym)
import Grisette.Internal.Core.TH.Derivation
  ( Strategy (Anyclass, SpecialForGeneric, Stock, Via, WithNewtype),
    deriveConversions,
    deriveWithMode,
  )
import Grisette.Internal.Core.TH.DeriveUnifiedInterface
  ( deriveUnifiedInterface,
  )
import Grisette.Internal.SymPrim.AllSyms (AllSyms)
import Grisette.Unified.Internal.Class.UnifiedSEq (UnifiedSEq (withBaseSEq))
import Grisette.Unified.Internal.EvaluationMode (EvaluationMode (Con))
import Language.Haskell.TH
  ( Dec,
    Name,
    Q,
  )
import Language.Haskell.TH.Datatype
  ( DatatypeInfo (datatypeVariant),
    DatatypeVariant (Datatype, Newtype),
    reifyDatatype,
  )
import Language.Haskell.TH.Syntax (Lift)

#if MIN_VERSION_base(4,16,0)
#else
import Data.List (sort)
import Data.Containers.ListUtils (nubOrd)
#endif

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
deriveUnifiedSEq = deriveUnifiedInterface ''UnifiedSEq 'withBaseSEq
