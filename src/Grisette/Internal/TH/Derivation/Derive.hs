{-# LANGUAGE LambdaCase #-}
{-# HLINT ignore "Unused LANGUAGE pragma" #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

-- |
-- Module      :   Grisette.Internal.TH.Derivation.Derive
-- Copyright   :   (c) Sirui Lu 2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Internal.TH.Derivation.Derive
  ( derive,
    deriveWith,
    allClasses0,
    allClasses01,
    allClasses012,
    basicClasses0,
    noExistentialClasses0,
    concreteOrdClasses0,
    basicClasses1,
    noExistentialClasses1,
    concreteOrdClasses1,
    basicClasses2,
    noExistentialClasses2,
    concreteOrdClasses2,
    showClasses,
    pprintClasses,
    evalSymClasses,
    extractSymClasses,
    substSymClasses,
    allSymsClasses,
    eqClasses,
    ordClasses,
    symOrdClasses,
    symEqClasses,
    unifiedSymOrdClasses,
    unifiedSymEqClasses,
    mergeableClasses,
    nfDataClasses,
    hashableClasses,
    toSymClasses,
    toConClasses,
    serialClasses,
    simpleMergeableClasses,
    unifiedSimpleMergeableClasses,
    filterExactNumArgs,
    filterLeqNumArgs,
  )
where

import Control.Arrow (Arrow (second))
import Control.DeepSeq (NFData, NFData1, NFData2)
import Data.Binary (Binary)
import Data.Bytes.Serial (Serial, Serial1, Serial2)
import Data.Functor.Classes (Eq1, Eq2, Ord1, Ord2, Show1, Show2)
import Data.Hashable (Hashable)
import Data.Hashable.Lifted (Hashable1, Hashable2)
import qualified Data.Map as M
import Data.Serialize (Serialize)
import qualified Data.Set as S
import Grisette.Internal.Internal.Decl.Core.Data.Class.EvalSym
  ( EvalSym,
    EvalSym1,
    EvalSym2,
  )
import Grisette.Internal.Internal.Decl.Core.Data.Class.ExtractSym
  ( ExtractSym,
    ExtractSym1,
    ExtractSym2,
  )
import Grisette.Internal.Internal.Decl.Core.Data.Class.Mergeable
  ( Mergeable,
    Mergeable1,
    Mergeable2,
    Mergeable3,
  )
import Grisette.Internal.Internal.Decl.Core.Data.Class.PPrint
  ( PPrint,
    PPrint1,
    PPrint2,
  )
import Grisette.Internal.Internal.Decl.Core.Data.Class.SimpleMergeable
  ( SimpleMergeable,
    SimpleMergeable1,
    SimpleMergeable2,
  )
import Grisette.Internal.Internal.Decl.Core.Data.Class.SubstSym
  ( SubstSym,
    SubstSym1,
    SubstSym2,
  )
import Grisette.Internal.Internal.Decl.Core.Data.Class.SymEq
  ( SymEq,
    SymEq1,
    SymEq2,
  )
import Grisette.Internal.Internal.Decl.Core.Data.Class.SymOrd
  ( SymOrd,
    SymOrd1,
    SymOrd2,
  )
import Grisette.Internal.Internal.Decl.Core.Data.Class.ToCon
  ( ToCon,
    ToCon1,
    ToCon2,
  )
import Grisette.Internal.Internal.Decl.Core.Data.Class.ToSym
  ( ToSym,
    ToSym1,
    ToSym2,
  )
import Grisette.Internal.Internal.Decl.SymPrim.AllSyms
  ( AllSyms,
    AllSyms1,
    AllSyms2,
  )
import Grisette.Internal.Internal.Decl.Unified.Class.UnifiedSimpleMergeable
  ( UnifiedSimpleMergeable,
    UnifiedSimpleMergeable1,
    UnifiedSimpleMergeable2,
  )
import Grisette.Internal.Internal.Decl.Unified.Class.UnifiedSymEq
  ( UnifiedSymEq,
    UnifiedSymEq1,
    UnifiedSymEq2,
  )
import Grisette.Internal.Internal.Decl.Unified.Class.UnifiedSymOrd
  ( UnifiedSymOrd,
    UnifiedSymOrd1,
    UnifiedSymOrd2,
  )
import Grisette.Internal.TH.Derivation.Common
  ( DeriveConfig
      ( evalModeConfig,
        needExtraMergeableUnderEvalMode,
        needExtraMergeableWithConcretizedEvalMode
      ),
    EvalModeConfig (EvalModeConstraints, EvalModeSpecified),
  )
import Grisette.Internal.TH.Derivation.DeriveAllSyms
  ( deriveAllSyms,
    deriveAllSyms1,
    deriveAllSyms2,
  )
import Grisette.Internal.TH.Derivation.DeriveBinary (deriveBinary)
import Grisette.Internal.TH.Derivation.DeriveCereal (deriveCereal)
import Grisette.Internal.TH.Derivation.DeriveEq
  ( deriveEq,
    deriveEq1,
    deriveEq2,
  )
import Grisette.Internal.TH.Derivation.DeriveEvalSym
  ( deriveEvalSym,
    deriveEvalSym1,
    deriveEvalSym2,
  )
import Grisette.Internal.TH.Derivation.DeriveExtractSym
  ( deriveExtractSym,
    deriveExtractSym1,
    deriveExtractSym2,
  )
import Grisette.Internal.TH.Derivation.DeriveHashable
  ( deriveHashable,
    deriveHashable1,
    deriveHashable2,
  )
import Grisette.Internal.TH.Derivation.DeriveMergeable (genMergeableList)
import Grisette.Internal.TH.Derivation.DeriveNFData
  ( deriveNFData,
    deriveNFData1,
    deriveNFData2,
  )
import Grisette.Internal.TH.Derivation.DeriveOrd
  ( deriveOrd,
    deriveOrd1,
    deriveOrd2,
  )
import Grisette.Internal.TH.Derivation.DerivePPrint
  ( derivePPrint,
    derivePPrint1,
    derivePPrint2,
  )
import Grisette.Internal.TH.Derivation.DeriveSerial
  ( deriveSerial,
    deriveSerial1,
    deriveSerial2,
  )
import Grisette.Internal.TH.Derivation.DeriveShow
  ( deriveShow,
    deriveShow1,
    deriveShow2,
  )
import Grisette.Internal.TH.Derivation.DeriveSimpleMergeable
  ( deriveSimpleMergeable,
    deriveSimpleMergeable1,
    deriveSimpleMergeable2,
  )
import Grisette.Internal.TH.Derivation.DeriveSubstSym
  ( deriveSubstSym,
    deriveSubstSym1,
    deriveSubstSym2,
  )
import Grisette.Internal.TH.Derivation.DeriveSymEq
  ( deriveSymEq,
    deriveSymEq1,
    deriveSymEq2,
  )
import Grisette.Internal.TH.Derivation.DeriveSymOrd
  ( deriveSymOrd,
    deriveSymOrd1,
    deriveSymOrd2,
  )
import Grisette.Internal.TH.Derivation.DeriveToCon
  ( deriveToCon,
    deriveToCon1,
    deriveToCon2,
  )
import Grisette.Internal.TH.Derivation.DeriveToSym
  ( deriveToSym,
    deriveToSym1,
    deriveToSym2,
  )
import Grisette.Internal.TH.Derivation.DeriveUnifiedSimpleMergeable
  ( deriveUnifiedSimpleMergeable,
    deriveUnifiedSimpleMergeable1,
    deriveUnifiedSimpleMergeable2,
  )
import Grisette.Internal.TH.Derivation.DeriveUnifiedSymEq
  ( deriveUnifiedSymEq,
    deriveUnifiedSymEq1,
    deriveUnifiedSymEq2,
  )
import Grisette.Internal.TH.Derivation.DeriveUnifiedSymOrd
  ( deriveUnifiedSymOrd,
    deriveUnifiedSymOrd1,
    deriveUnifiedSymOrd2,
  )
import Grisette.Internal.Unified.EvalModeTag (EvalModeTag (C, S))
import Language.Haskell.TH (Dec, Name, Q)

deriveProcedureMap :: M.Map Name (DeriveConfig -> Name -> Q [Dec])
deriveProcedureMap =
  M.fromList
    [ (''EvalSym, deriveEvalSym),
      (''EvalSym1, deriveEvalSym1),
      (''EvalSym2, deriveEvalSym2),
      (''ExtractSym, deriveExtractSym),
      (''ExtractSym1, deriveExtractSym1),
      (''ExtractSym2, deriveExtractSym2),
      (''SubstSym, deriveSubstSym),
      (''SubstSym1, deriveSubstSym1),
      (''SubstSym2, deriveSubstSym2),
      (''NFData, deriveNFData),
      (''NFData1, deriveNFData1),
      (''NFData2, deriveNFData2),
      (''Hashable, deriveHashable),
      (''Hashable1, deriveHashable1),
      (''Hashable2, deriveHashable2),
      (''Show, deriveShow),
      (''Show1, deriveShow1),
      (''Show2, deriveShow2),
      (''PPrint, derivePPrint),
      (''PPrint1, derivePPrint1),
      (''PPrint2, derivePPrint2),
      (''AllSyms, deriveAllSyms),
      (''AllSyms1, deriveAllSyms1),
      (''AllSyms2, deriveAllSyms2),
      (''Eq, deriveEq),
      (''Eq1, deriveEq1),
      (''Eq2, deriveEq2),
      (''Ord, deriveOrd),
      (''Ord1, deriveOrd1),
      (''Ord2, deriveOrd2),
      (''SymOrd, deriveSymOrd),
      (''SymOrd1, deriveSymOrd1),
      (''SymOrd2, deriveSymOrd2),
      (''SymEq, deriveSymEq),
      (''SymEq1, deriveSymEq1),
      (''SymEq2, deriveSymEq2),
      (''UnifiedSymEq, deriveUnifiedSymEq),
      (''UnifiedSymEq1, deriveUnifiedSymEq1),
      (''UnifiedSymEq2, deriveUnifiedSymEq2),
      (''UnifiedSymOrd, deriveUnifiedSymOrd),
      (''UnifiedSymOrd1, deriveUnifiedSymOrd1),
      (''UnifiedSymOrd2, deriveUnifiedSymOrd2),
      (''ToSym, deriveToSym),
      (''ToSym1, deriveToSym1),
      (''ToSym2, deriveToSym2),
      (''ToCon, deriveToCon),
      (''ToCon1, deriveToCon1),
      (''ToCon2, deriveToCon2),
      (''Serial, deriveSerial),
      (''Serial1, deriveSerial1),
      (''Serial2, deriveSerial2),
      (''SimpleMergeable, deriveSimpleMergeable),
      (''SimpleMergeable1, deriveSimpleMergeable1),
      (''SimpleMergeable2, deriveSimpleMergeable2),
      (''UnifiedSimpleMergeable, deriveUnifiedSimpleMergeable),
      (''UnifiedSimpleMergeable1, deriveUnifiedSimpleMergeable1),
      (''UnifiedSimpleMergeable2, deriveUnifiedSimpleMergeable2),
      (''Binary, deriveBinary),
      (''Serialize, deriveCereal)
    ]

deriveSingle :: DeriveConfig -> Name -> Name -> Q [Dec]
deriveSingle deriveConfig typName className = do
  let newExtra
        | className
            `elem` [ ''Eq,
                     ''Eq1,
                     ''Eq2,
                     ''SymEq,
                     ''SymEq1,
                     ''SymEq2,
                     ''SymOrd,
                     ''SymOrd1,
                     ''SymOrd2,
                     ''UnifiedSymEq,
                     ''UnifiedSymEq1,
                     ''UnifiedSymEq2,
                     ''UnifiedSymOrd,
                     ''UnifiedSymOrd1,
                     ''UnifiedSymOrd2,
                     ''UnifiedSimpleMergeable,
                     ''UnifiedSimpleMergeable1,
                     ''UnifiedSimpleMergeable2
                   ] =
            deriveConfig
              { needExtraMergeableUnderEvalMode = False,
                needExtraMergeableWithConcretizedEvalMode = False
              }
        | className
            `elem` [''SimpleMergeable, ''SimpleMergeable1, ''SimpleMergeable2] =
            deriveConfig
              { evalModeConfig =
                  second
                    ( \case
                        EvalModeConstraints _ -> EvalModeSpecified S
                        EvalModeSpecified tag -> EvalModeSpecified tag
                    )
                    <$> evalModeConfig deriveConfig,
                needExtraMergeableUnderEvalMode = False,
                needExtraMergeableWithConcretizedEvalMode = False
              }
        | className `elem` [''Ord, ''Ord1, ''Ord2] =
            deriveConfig
              { evalModeConfig =
                  second
                    ( \case
                        EvalModeConstraints _ -> EvalModeSpecified C
                        EvalModeSpecified tag -> EvalModeSpecified tag
                    )
                    <$> evalModeConfig deriveConfig,
                needExtraMergeableUnderEvalMode = False,
                needExtraMergeableWithConcretizedEvalMode = False
              }
        | otherwise = deriveConfig
  case M.lookup className deriveProcedureMap of
    Just procedure -> procedure newExtra typName
    Nothing ->
      fail $ "No derivation available for class " ++ show className

deriveWith' :: DeriveConfig -> Name -> [Name] -> Q [Dec]
deriveWith' deriveConfig typName classNameList = do
  let classNames = S.fromList classNameList
  let (ns, ms) = splitMergeable $ S.toList classNames
  decs <- mapM (deriveSingle deriveConfig typName) ns
  decMergeables <- deriveMergeables ms
  return $ concat decs ++ decMergeables
  where
    configWithOutExtraMergeable :: DeriveConfig
    configWithOutExtraMergeable =
      deriveConfig {needExtraMergeableUnderEvalMode = False}
    deriveMergeables :: [Int] -> Q [Dec]
    deriveMergeables = genMergeableList configWithOutExtraMergeable typName
    splitMergeable :: [Name] -> ([Name], [Int])
    splitMergeable [] = ([], [])
    splitMergeable (x : xs) =
      let (ns, is) = splitMergeable xs
       in if
            | x == ''Mergeable -> (ns, 0 : is)
            | x == ''Mergeable1 -> (ns, 1 : is)
            | x == ''Mergeable2 -> (ns, 2 : is)
            | x == ''Mergeable3 -> (ns, 3 : is)
            | otherwise -> (x : ns, is)

-- | Derive the specified classes for a data type with the given name.
--
-- Support the following classes for both vanilla data types and GADTs.
--
-- * 'Mergeable'
-- * 'Mergeable1'
-- * 'Mergeable2'
-- * 'Mergeable3'
-- * 'EvalSym'
-- * 'EvalSym1'
-- * 'EvalSym2'
-- * 'ExtractSym'
-- * 'ExtractSym1'
-- * 'ExtractSym2'
-- * 'SubstSym'
-- * 'SubstSym1'
-- * 'SubstSym2'
-- * 'NFData'
-- * 'NFData1'
-- * 'NFData2'
-- * 'Hashable'
-- * 'Hashable1'
-- * 'Hashable2'
-- * 'Show'
-- * 'Show1'
-- * 'Show2'
-- * 'PPrint'
-- * 'PPrint1'
-- * 'PPrint2'
-- * 'AllSyms'
-- * 'AllSyms1'
-- * 'AllSyms2'
-- * 'Eq'
-- * 'Eq1'
-- * 'Eq2'
-- * 'Ord'
-- * 'Ord1'
-- * 'Ord2'
-- * 'SymOrd'
-- * 'SymOrd1'
-- * 'SymOrd2'
-- * 'SymEq'
-- * 'SymEq1'
-- * 'SymEq2'
-- * 'UnifiedSymEq'
-- * 'UnifiedSymEq1'
-- * 'UnifiedSymEq2'
-- * 'UnifiedSymOrd'
-- * 'UnifiedSymOrd1'
-- * 'UnifiedSymOrd2'
-- * 'ToSym'
-- * 'ToSym1'
-- * 'ToSym2'
-- * 'ToCon'
-- * 'ToCon1'
-- * 'ToCon2'
-- * 'Serial'
-- * 'Serial1'
-- * 'Serial2'
-- * 'SimpleMergeable'
-- * 'SimpleMergeable1'
-- * 'SimpleMergeable2'
-- * 'Binary'
-- * 'Serialize'
--
-- Note that the following type classes cannot be derived for GADTs with
-- existential type variables.
--
-- * 'ToCon'
-- * 'ToCon1'
-- * 'ToCon2'
-- * 'ToSym'
-- * 'ToSym1'
-- * 'ToSym2'
-- * 'Serial'
-- * 'Serial1'
-- * 'Serial2'
-- * 'Binary'
-- * 'Serialize'
deriveWith :: DeriveConfig -> [Name] -> [Name] -> Q [Dec]
deriveWith deriveConfig typeNameList classNameList = do
  let typeNames = S.toList $ S.fromList typeNameList
  concat
    <$> traverse
      (\typeName -> deriveWith' deriveConfig typeName classNameList)
      typeNames

-- | Derive the specified classes for a data type with the given name.
--
-- See 'deriveWith' for more details.
derive :: [Name] -> [Name] -> Q [Dec]
derive = deriveWith mempty

-- | All the classes that can be derived for GADTs.
--
-- This includes:
--
-- * 'Mergeable'
-- * 'EvalSym'
-- * 'ExtractSym'
-- * 'SubstSym'
-- * 'NFData'
-- * 'Hashable'
-- * 'Show'
-- * 'PPrint'
-- * 'AllSyms'
-- * 'Eq'
-- * 'SymEq'
-- * 'SymOrd'
-- * 'UnifiedSymEq'
-- * 'Ord'
-- * 'UnifiedSymOrd'
-- * 'Serial'
-- * 'ToCon'
-- * 'ToSym'
allClasses0 :: [Name]
allClasses0 = basicClasses0 ++ concreteOrdClasses0 ++ noExistentialClasses0

-- | All the @*1@ classes that can be derived for GADT functors.
--
-- This includes:
--
-- * 'Mergeable1'
-- * 'EvalSym1'
-- * 'ExtractSym1'
-- * 'SubstSym1'
-- * 'NFData1'
-- * 'Hashable1'
-- * 'Show1'
-- * 'PPrint1'
-- * 'AllSyms1'
-- * 'Eq1'
-- * 'SymEq1'
-- * 'SymOrd1'
-- * 'UnifiedSymEq1'
-- * 'Ord1'
-- * 'UnifiedSymOrd1'
-- * 'Serial1'
-- * 'ToCon1'
-- * 'ToSym1'
allClasses1 :: [Name]
allClasses1 = basicClasses1 ++ concreteOrdClasses1 ++ noExistentialClasses1

-- | All the classes that can be derived for GADT functors.
--
-- This includes all the classes in 'allClasses0' and 'allClasses1'.
allClasses01 :: [Name]
allClasses01 = allClasses0 ++ allClasses1

-- | All the @*2@ classes that can be derived for GADT functors.
--
-- This includes:
--
-- * 'Mergeable2'
-- * 'EvalSym2'
-- * 'ExtractSym2'
-- * 'SubstSym2'
-- * 'NFData2'
-- * 'Hashable2'
-- * 'Show2'
-- * 'PPrint2'
-- * 'AllSyms2'
-- * 'Eq2'
-- * 'SymEq2'
-- * 'SymOrd2'
-- * 'UnifiedSymEq2'
-- * 'Ord2'
-- * 'UnifiedSymOrd2'
-- * 'Serial2'
-- * 'ToCon2'
-- * 'ToSym2'
allClasses2 :: [Name]
allClasses2 = basicClasses2 ++ concreteOrdClasses2 ++ noExistentialClasses2

-- | All the classes that can be derived for GADT functors.
--
-- This includes all the classes in 'allClasses0', 'allClasses1',
-- and 'allClasses2'.
allClasses012 :: [Name]
allClasses012 = allClasses0 ++ allClasses1 ++ allClasses2

-- | Basic classes for GADTs.
--
-- This includes:
--
-- * 'Mergeable'
-- * 'EvalSym'
-- * 'ExtractSym'
-- * 'SubstSym'
-- * 'NFData'
-- * 'Hashable'
-- * 'Show'
-- * 'PPrint'
-- * 'AllSyms'
-- * 'Eq'
-- * 'SymEq'
-- * 'SymOrd'
-- * 'UnifiedSymEq'
--
-- These classes can be derived for most GADTs.
basicClasses0 :: [Name]
basicClasses0 =
  [ ''Mergeable,
    ''EvalSym,
    ''ExtractSym,
    ''SubstSym,
    ''NFData,
    ''Hashable,
    ''Show,
    ''PPrint,
    ''AllSyms,
    ''Eq,
    ''SymEq,
    ''SymOrd,
    ''UnifiedSymEq
  ]

-- | Classes that can only be derived for GADTs without existential type
-- variables.
--
-- This includes:
--
-- * 'Serial'
-- * 'Serialize'
-- * 'Binary'
-- * 'ToCon'
-- * 'ToSym'
noExistentialClasses0 :: [Name]
noExistentialClasses0 = [''Serial, ''ToCon, ''ToSym, ''Serialize, ''Binary]

-- | Concrete ordered classes that can be derived for GADTs that
--
-- * uses unified evaluation mode, or
-- * does not contain any symbolic variables.
--
-- This includes:
--
-- * 'Ord'
-- * 'UnifiedSymOrd'
concreteOrdClasses0 :: [Name]
concreteOrdClasses0 = [''Ord, ''UnifiedSymOrd]

-- | Basic classes for GADT functors.
--
-- This includes:
--
-- * 'Mergeable1'
-- * 'EvalSym1'
-- * 'ExtractSym1'
-- * 'SubstSym1'
-- * 'NFData1'
-- * 'Hashable1'
-- * 'Show1'
-- * 'PPrint1'
-- * 'AllSyms1'
-- * 'Eq1'
-- * 'SymEq1'
-- * 'SymOrd1'
-- * 'UnifiedSymEq1'
basicClasses1 :: [Name]
basicClasses1 =
  [ ''Mergeable1,
    ''EvalSym1,
    ''ExtractSym1,
    ''SubstSym1,
    ''NFData1,
    ''Hashable1,
    ''Show1,
    ''PPrint1,
    ''AllSyms1,
    ''Eq1,
    ''SymEq1,
    ''SymOrd1,
    ''UnifiedSymEq1
  ]

-- | @*1@ classes that can only be derived for GADT functors without existential
-- type variables.
--
-- This includes:
--
-- * 'Serial1'
-- * 'ToCon1'
-- * 'ToSym1'
noExistentialClasses1 :: [Name]
noExistentialClasses1 = [''Serial1, ''ToCon1, ''ToSym1]

-- | @*1@ concrete ordered classes that can be derived for GADT functors that
--
-- * uses unified evaluation mode, or
-- * does not contain any symbolic variables.
--
-- This includes:
--
-- * 'Ord1'
-- * 'UnifiedSymOrd1'
concreteOrdClasses1 :: [Name]
concreteOrdClasses1 = [''Ord1, ''UnifiedSymOrd1]

-- | Basic classes for GADT functors.
--
-- This includes:
--
-- * 'Mergeable2'
-- * 'EvalSym2'
-- * 'ExtractSym2'
-- * 'SubstSym2'
-- * 'NFData2'
-- * 'Hashable2'
-- * 'Show2'
-- * 'PPrint2'
-- * 'AllSyms2'
-- * 'Eq2'
-- * 'SymEq2'
-- * 'SymOrd2'
-- * 'UnifiedSymEq2'
basicClasses2 :: [Name]
basicClasses2 =
  [ ''Mergeable2,
    ''EvalSym2,
    ''ExtractSym2,
    ''SubstSym2,
    ''NFData2,
    ''Hashable2,
    ''Show2,
    ''PPrint2,
    ''AllSyms2,
    ''Eq2,
    ''SymEq2,
    ''SymOrd2,
    ''UnifiedSymEq2
  ]

-- | @*2@ classes that can only be derived for GADT functors without existential
-- type variables.
--
-- This includes:
--
-- * 'Serial2'
-- * 'ToCon2'
-- * 'ToSym2'
noExistentialClasses2 :: [Name]
noExistentialClasses2 = [''Serial2, ''ToCon2, ''ToSym2]

-- | @*2@ concrete ordered classes that can be derived for GADT functors that
--
-- * uses unified evaluation mode, or
-- * does not contain any symbolic variables.
--
-- This includes:
--
-- * 'Ord2'
-- * 'UnifiedSymOrd2'
concreteOrdClasses2 :: [Name]
concreteOrdClasses2 = [''Ord2, ''UnifiedSymOrd2]

-- | 'Show' classes that can be derived for GADTs.
--
-- This includes:
--
-- * 'Show'
-- * 'Show1'
-- * 'Show2'
showClasses :: [Name]
showClasses = [''Show, ''Show1, ''Show2]

-- | 'PPrint' classes that can be derived for GADTs.
--
-- This includes:
--
-- * 'PPrint'
-- * 'PPrint1'
-- * 'PPrint2'
pprintClasses :: [Name]
pprintClasses = [''PPrint, ''PPrint1, ''PPrint2]

-- | 'EvalSym' classes that can be derived for GADTs.
--
-- This includes:
--
-- * 'EvalSym'
-- * 'EvalSym1'
-- * 'EvalSym2'
evalSymClasses :: [Name]
evalSymClasses = [''EvalSym, ''EvalSym1, ''EvalSym2]

-- | 'ExtractSym' classes that can be derived for GADTs.
--
-- This includes:
--
-- * 'ExtractSym'
-- * 'ExtractSym1'
-- * 'ExtractSym2'
extractSymClasses :: [Name]
extractSymClasses = [''ExtractSym, ''ExtractSym1, ''ExtractSym2]

-- | 'SubstSym' classes that can be derived for GADTs.
--
-- This includes:
--
-- * 'SubstSym'
-- * 'SubstSym1'
-- * 'SubstSym2'
substSymClasses :: [Name]
substSymClasses = [''SubstSym, ''SubstSym1, ''SubstSym2]

-- | 'AllSyms' classes that can be derived for GADTs.
--
-- This includes:
--
-- * 'AllSyms'
-- * 'AllSyms1'
-- * 'AllSyms2'
allSymsClasses :: [Name]
allSymsClasses = [''AllSyms, ''AllSyms1, ''AllSyms2]

-- | 'Eq' classes that can be derived for GADTs.
--
-- This includes:
--
-- * 'Eq'
-- * 'Eq1'
-- * 'Eq2'
eqClasses :: [Name]
eqClasses = [''Eq, ''Eq1, ''Eq2]

-- | 'SymEq' classes that can be derived for GADTs.
--
-- This includes:
--
-- * 'SymEq'
-- * 'SymEq1'
-- * 'SymEq2'
symEqClasses :: [Name]
symEqClasses = [''SymEq, ''SymEq1, ''SymEq2]

-- | 'UnifiedSymEq' classes that can be derived for GADTs.
--
-- This includes:
--
-- * 'UnifiedSymEq'
-- * 'UnifiedSymEq1'
-- * 'UnifiedSymEq2'
unifiedSymEqClasses :: [Name]
unifiedSymEqClasses = [''UnifiedSymEq, ''UnifiedSymEq1, ''UnifiedSymEq2]

-- | 'Ord' classes that can be derived for GADTs.
--
-- This includes:
--
-- * 'Ord'
-- * 'Ord1'
-- * 'Ord2'
ordClasses :: [Name]
ordClasses = [''Ord, ''Ord1, ''Ord2]

-- | 'SymOrd' classes that can be derived for GADTs.
--
-- This includes:
--
-- * 'SymOrd'
-- * 'SymOrd1'
-- * 'SymOrd2'
symOrdClasses :: [Name]
symOrdClasses = [''SymOrd, ''SymOrd1, ''SymOrd2]

-- | 'UnifiedSymOrd' classes that can be derived for GADTs.
--
-- This includes:
--
-- * 'UnifiedSymOrd'
-- * 'UnifiedSymOrd1'
-- * 'UnifiedSymOrd2'
unifiedSymOrdClasses :: [Name]
unifiedSymOrdClasses = [''UnifiedSymOrd, ''UnifiedSymOrd1, ''UnifiedSymOrd2]

-- | 'Mergeable' classes that can be derived for GADTs.
--
-- This includes:
--
-- * 'Mergeable'
-- * 'Mergeable1'
-- * 'Mergeable2'
-- * 'Mergeable3'
mergeableClasses :: [Name]
mergeableClasses = [''Mergeable, ''Mergeable1, ''Mergeable2, ''Mergeable3]

-- | 'NFData' classes that can be derived for GADTs.
--
-- This includes:
--
-- * 'NFData'
-- * 'NFData1'
-- * 'NFData2'
nfDataClasses :: [Name]
nfDataClasses = [''NFData, ''NFData1, ''NFData2]

-- | 'Hashable' classes that can be derived for GADTs.
--
-- This includes:
--
-- * 'Hashable'
-- * 'Hashable1'
-- * 'Hashable2'
hashableClasses :: [Name]
hashableClasses = [''Hashable, ''Hashable1, ''Hashable2]

-- | 'ToSym' classes that can be derived for GADTs.
--
-- This includes:
--
-- * 'ToSym'
-- * 'ToSym1'
-- * 'ToSym2'
toSymClasses :: [Name]
toSymClasses = [''ToSym, ''ToSym1, ''ToSym2]

-- | 'ToCon' classes that can be derived for GADTs.
--
-- This includes:
--
-- * 'ToCon'
-- * 'ToCon1'
-- * 'ToCon2'
toConClasses :: [Name]
toConClasses = [''ToCon, ''ToCon1, ''ToCon2]

-- | 'Serial' classes that can be derived for GADTs.
--
-- This includes:
--
-- * 'Serial'
-- * 'Serial1'
-- * 'Serial2'
serialClasses :: [Name]
serialClasses = [''Serial, ''Serial1, ''Serial2]

-- | 'SimpleMergeable' classes that can be derived for GADTs.
--
-- This includes:
--
-- * 'SimpleMergeable'
-- * 'SimpleMergeable1'
-- * 'SimpleMergeable2'
simpleMergeableClasses :: [Name]
simpleMergeableClasses =
  [''SimpleMergeable, ''SimpleMergeable1, ''SimpleMergeable2]

-- | 'UnifiedSimpleMergeable' classes that can be derived for GADTs.
--
-- This includes:
--
-- * 'UnifiedSimpleMergeable'
-- * 'UnifiedSimpleMergeable1'
-- * 'UnifiedSimpleMergeable2'
unifiedSimpleMergeableClasses :: [Name]
unifiedSimpleMergeableClasses =
  [ ''UnifiedSimpleMergeable,
    ''UnifiedSimpleMergeable1,
    ''UnifiedSimpleMergeable2
  ]

clsArgNumArgs :: Name -> Int
clsArgNumArgs cls =
  if
    | cls `elem` allClasses0 -> 0
    | cls `elem` allClasses1 -> 1
    | cls `elem` allClasses2 -> 2
    | cls == ''Mergeable3 -> 3
    | otherwise -> error $ "clsArgNumArgs: unknown class: " ++ show cls

-- | Filter classes that accepts type constructors with exactly @n@ arguments.
filterExactNumArgs :: Int -> [Name] -> [Name]
filterExactNumArgs n = filter (\cls -> clsArgNumArgs cls == n)

-- | Filter classes that accepts type constructors with at most @n@ arguments.
filterLeqNumArgs :: Int -> [Name] -> [Name]
filterLeqNumArgs n = filter (\cls -> clsArgNumArgs cls <= n)
