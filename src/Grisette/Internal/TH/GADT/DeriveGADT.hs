{-# LANGUAGE LambdaCase #-}
{-# HLINT ignore "Unused LANGUAGE pragma" #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

-- |
-- Module      :   Grisette.Internal.TH.GADT.DeriveGADT
-- Copyright   :   (c) Sirui Lu 2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Internal.TH.GADT.DeriveGADT
  ( deriveGADT,
    deriveGADTWith,
    allClasses0,
    allClasses0WithOrd,
    allClasses1,
    allClasses1WithOrd,
    allClasses2,
    allClasses2WithOrd,
  )
where

import Control.Arrow (Arrow (second))
import Control.DeepSeq (NFData, NFData1, NFData2)
import Data.Bytes.Serial (Serial, Serial1, Serial2)
import Data.Functor.Classes (Eq1, Eq2, Ord1, Ord2, Show1, Show2)
import Data.Hashable (Hashable)
import Data.Hashable.Lifted (Hashable1, Hashable2)
import qualified Data.Map as M
import qualified Data.Set as S
import Grisette.Internal.Core.Data.Class.EvalSym
  ( EvalSym,
    EvalSym1,
    EvalSym2,
  )
import Grisette.Internal.Core.Data.Class.ExtractSym
  ( ExtractSym,
    ExtractSym1,
    ExtractSym2,
  )
import Grisette.Internal.Core.Data.Class.Mergeable
  ( Mergeable,
    Mergeable1,
    Mergeable2,
    Mergeable3,
  )
import Grisette.Internal.Core.Data.Class.PPrint (PPrint, PPrint1, PPrint2)
import Grisette.Internal.Core.Data.Class.SimpleMergeable
  ( SimpleMergeable,
    SimpleMergeable1,
    SimpleMergeable2,
  )
import Grisette.Internal.Core.Data.Class.SubstSym
  ( SubstSym,
    SubstSym1,
    SubstSym2,
  )
import Grisette.Internal.Core.Data.Class.SymEq (SymEq, SymEq1, SymEq2)
import Grisette.Internal.Core.Data.Class.SymOrd (SymOrd, SymOrd1, SymOrd2)
import Grisette.Internal.Core.Data.Class.ToCon (ToCon, ToCon1, ToCon2)
import Grisette.Internal.Core.Data.Class.ToSym (ToSym, ToSym1, ToSym2)
import Grisette.Internal.SymPrim.AllSyms (AllSyms, AllSyms1, AllSyms2)
import Grisette.Internal.TH.GADT.Common
  ( DeriveConfig
      ( evalModeConfig,
        needExtraMergeableUnderEvalMode,
        needExtraMergeableWithConcretizedEvalMode
      ),
    EvalModeConfig (EvalModeConstraints, EvalModeSpecified),
  )
import Grisette.Internal.TH.GADT.DeriveAllSyms
  ( deriveGADTAllSyms,
    deriveGADTAllSyms1,
    deriveGADTAllSyms2,
  )
import Grisette.Internal.TH.GADT.DeriveEq
  ( deriveGADTEq,
    deriveGADTEq1,
    deriveGADTEq2,
  )
import Grisette.Internal.TH.GADT.DeriveEvalSym
  ( deriveGADTEvalSym,
    deriveGADTEvalSym1,
    deriveGADTEvalSym2,
  )
import Grisette.Internal.TH.GADT.DeriveExtractSym
  ( deriveGADTExtractSym,
    deriveGADTExtractSym1,
    deriveGADTExtractSym2,
  )
import Grisette.Internal.TH.GADT.DeriveHashable
  ( deriveGADTHashable,
    deriveGADTHashable1,
    deriveGADTHashable2,
  )
import Grisette.Internal.TH.GADT.DeriveMergeable
  ( genMergeable,
    genMergeable',
    genMergeableAndGetMergingInfoResult,
  )
import Grisette.Internal.TH.GADT.DeriveNFData
  ( deriveGADTNFData,
    deriveGADTNFData1,
    deriveGADTNFData2,
  )
import Grisette.Internal.TH.GADT.DeriveOrd
  ( deriveGADTOrd,
    deriveGADTOrd1,
    deriveGADTOrd2,
  )
import Grisette.Internal.TH.GADT.DerivePPrint
  ( deriveGADTPPrint,
    deriveGADTPPrint1,
    deriveGADTPPrint2,
  )
import Grisette.Internal.TH.GADT.DeriveSerial
  ( deriveGADTSerial,
    deriveGADTSerial1,
    deriveGADTSerial2,
  )
import Grisette.Internal.TH.GADT.DeriveShow
  ( deriveGADTShow,
    deriveGADTShow1,
    deriveGADTShow2,
  )
import Grisette.Internal.TH.GADT.DeriveSimpleMergeable
  ( deriveGADTSimpleMergeable,
    deriveGADTSimpleMergeable1,
    deriveGADTSimpleMergeable2,
  )
import Grisette.Internal.TH.GADT.DeriveSubstSym
  ( deriveGADTSubstSym,
    deriveGADTSubstSym1,
    deriveGADTSubstSym2,
  )
import Grisette.Internal.TH.GADT.DeriveSymEq
  ( deriveGADTSymEq,
    deriveGADTSymEq1,
    deriveGADTSymEq2,
  )
import Grisette.Internal.TH.GADT.DeriveSymOrd
  ( deriveGADTSymOrd,
    deriveGADTSymOrd1,
    deriveGADTSymOrd2,
  )
import Grisette.Internal.TH.GADT.DeriveToCon
  ( deriveGADTToCon,
    deriveGADTToCon1,
    deriveGADTToCon2,
  )
import Grisette.Internal.TH.GADT.DeriveToSym
  ( deriveGADTToSym,
    deriveGADTToSym1,
    deriveGADTToSym2,
  )
import Grisette.Internal.TH.GADT.DeriveUnifiedSimpleMergeable (deriveGADTUnifiedSimpleMergeable, deriveGADTUnifiedSimpleMergeable1, deriveGADTUnifiedSimpleMergeable2)
import Grisette.Internal.TH.GADT.DeriveUnifiedSymEq
  ( deriveGADTUnifiedSymEq,
    deriveGADTUnifiedSymEq1,
    deriveGADTUnifiedSymEq2,
  )
import Grisette.Internal.TH.GADT.DeriveUnifiedSymOrd
  ( deriveGADTUnifiedSymOrd,
    deriveGADTUnifiedSymOrd1,
    deriveGADTUnifiedSymOrd2,
  )
import Grisette.Unified.Internal.Class.UnifiedSimpleMergeable (UnifiedSimpleMergeable, UnifiedSimpleMergeable1, UnifiedSimpleMergeable2)
import Grisette.Unified.Internal.Class.UnifiedSymEq
  ( UnifiedSymEq,
    UnifiedSymEq1,
    UnifiedSymEq2,
  )
import Grisette.Unified.Internal.Class.UnifiedSymOrd
  ( UnifiedSymOrd,
    UnifiedSymOrd1,
    UnifiedSymOrd2,
  )
import Grisette.Unified.Internal.EvalModeTag (EvalModeTag (C, S))
import Language.Haskell.TH (Dec, Name, Q)

deriveProcedureMap :: M.Map Name (DeriveConfig -> Name -> Q [Dec])
deriveProcedureMap =
  M.fromList
    [ (''EvalSym, deriveGADTEvalSym),
      (''EvalSym1, deriveGADTEvalSym1),
      (''EvalSym2, deriveGADTEvalSym2),
      (''ExtractSym, deriveGADTExtractSym),
      (''ExtractSym1, deriveGADTExtractSym1),
      (''ExtractSym2, deriveGADTExtractSym2),
      (''SubstSym, deriveGADTSubstSym),
      (''SubstSym1, deriveGADTSubstSym1),
      (''SubstSym2, deriveGADTSubstSym2),
      (''NFData, deriveGADTNFData),
      (''NFData1, deriveGADTNFData1),
      (''NFData2, deriveGADTNFData2),
      (''Hashable, deriveGADTHashable),
      (''Hashable1, deriveGADTHashable1),
      (''Hashable2, deriveGADTHashable2),
      (''Show, deriveGADTShow),
      (''Show1, deriveGADTShow1),
      (''Show2, deriveGADTShow2),
      (''PPrint, deriveGADTPPrint),
      (''PPrint1, deriveGADTPPrint1),
      (''PPrint2, deriveGADTPPrint2),
      (''AllSyms, deriveGADTAllSyms),
      (''AllSyms1, deriveGADTAllSyms1),
      (''AllSyms2, deriveGADTAllSyms2),
      (''Eq, deriveGADTEq),
      (''Eq1, deriveGADTEq1),
      (''Eq2, deriveGADTEq2),
      (''Ord, deriveGADTOrd),
      (''Ord1, deriveGADTOrd1),
      (''Ord2, deriveGADTOrd2),
      (''SymOrd, deriveGADTSymOrd),
      (''SymOrd1, deriveGADTSymOrd1),
      (''SymOrd2, deriveGADTSymOrd2),
      (''SymEq, deriveGADTSymEq),
      (''SymEq1, deriveGADTSymEq1),
      (''SymEq2, deriveGADTSymEq2),
      (''UnifiedSymEq, deriveGADTUnifiedSymEq),
      (''UnifiedSymEq1, deriveGADTUnifiedSymEq1),
      (''UnifiedSymEq2, deriveGADTUnifiedSymEq2),
      (''UnifiedSymOrd, deriveGADTUnifiedSymOrd),
      (''UnifiedSymOrd1, deriveGADTUnifiedSymOrd1),
      (''UnifiedSymOrd2, deriveGADTUnifiedSymOrd2),
      (''ToSym, deriveGADTToSym),
      (''ToSym1, deriveGADTToSym1),
      (''ToSym2, deriveGADTToSym2),
      (''ToCon, deriveGADTToCon),
      (''ToCon1, deriveGADTToCon1),
      (''ToCon2, deriveGADTToCon2),
      (''Serial, deriveGADTSerial),
      (''Serial1, deriveGADTSerial1),
      (''Serial2, deriveGADTSerial2),
      (''SimpleMergeable, deriveGADTSimpleMergeable),
      (''SimpleMergeable1, deriveGADTSimpleMergeable1),
      (''SimpleMergeable2, deriveGADTSimpleMergeable2),
      (''UnifiedSimpleMergeable, deriveGADTUnifiedSimpleMergeable),
      (''UnifiedSimpleMergeable1, deriveGADTUnifiedSimpleMergeable1),
      (''UnifiedSimpleMergeable2, deriveGADTUnifiedSimpleMergeable2)
    ]

deriveSingleGADT :: DeriveConfig -> Name -> Name -> Q [Dec]
deriveSingleGADT deriveConfig typName className = do
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

-- | Derive the specified classes for a GADT with the given name.
--
-- Support the following classes.
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
--
-- Note that the following type classes cannot be derived for GADTs with
-- existential type variables.
--
-- * 'Eq1'
-- * 'Eq2'
-- * 'SymEq1'
-- * 'SymEq2'
-- * 'Ord1'
-- * 'Ord2'
-- * 'SymOrd1'
-- * 'SymOrd2'
deriveGADTWith :: DeriveConfig -> Name -> S.Set Name -> Q [Dec]
deriveGADTWith deriveConfig typName classNames = do
  let (ns, ms) = splitMergeable $ S.toList classNames
  decs <- mapM (deriveSingleGADT deriveConfig typName) ns
  decMergeables <- deriveMergeables ms
  return $ concat decs ++ decMergeables
  where
    configWithOutExtraMergeable :: DeriveConfig
    configWithOutExtraMergeable =
      deriveConfig {needExtraMergeableUnderEvalMode = False}
    deriveMergeables :: [Int] -> Q [Dec]
    deriveMergeables [] = return []
    deriveMergeables [n] = genMergeable configWithOutExtraMergeable typName n
    deriveMergeables (n : ns) = do
      (info, dn) <-
        genMergeableAndGetMergingInfoResult
          configWithOutExtraMergeable
          typName
          n
      dns <-
        traverse (genMergeable' configWithOutExtraMergeable info typName) ns
      return $ dn ++ concatMap snd dns
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

-- | Derive the specified classes for a GADT with the given name.
--
-- See 'deriveGADTWith' for more details.
deriveGADT :: Name -> S.Set Name -> Q [Dec]
deriveGADT = deriveGADTWith mempty

-- | The set of all classes (excluding concrete 'Ord') that can be derived by
-- 'deriveGADT'.
allClasses0 :: S.Set Name
allClasses0 =
  S.fromList
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
      ''UnifiedSymEq,
      ''ToCon,
      ''ToSym,
      ''Serial
    ]

allOrdClasses0 :: S.Set Name
allOrdClasses0 = S.fromList [''Ord, ''UnifiedSymOrd]

-- | The set of all classes (including concrete 'Ord') that can be derived by
-- 'deriveGADT'.
allClasses0WithOrd :: S.Set Name
allClasses0WithOrd = S.union allOrdClasses0 allClasses0

-- | The set of all classes for functors (excluding concrete 'Ord') that can be
-- derived by 'deriveGADT'.
allClasses1 :: S.Set Name
allClasses1 =
  S.union allClasses0 $
    S.fromList
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
        ''ToCon1,
        ''ToSym1,
        ''Serial1
      ]

allOrdClasses1 :: S.Set Name
allOrdClasses1 = S.union allOrdClasses0 $ S.fromList [''Ord1, ''UnifiedSymOrd1]

-- | The set of all classes for functors (including concrete 'Ord') that can be
-- derived by 'deriveGADT'.
allClasses1WithOrd :: S.Set Name
allClasses1WithOrd = S.union allClasses1 allOrdClasses1

-- | The set of all classes for bifunctors (excluding concrete 'Ord') that can
-- be derived by 'deriveGADT'.
allClasses2 :: S.Set Name
allClasses2 =
  S.union allClasses1 $
    S.fromList
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
        ''ToCon2,
        ''ToSym2,
        ''Serial2
      ]

allOrdClasses2 :: S.Set Name
allOrdClasses2 = S.union allOrdClasses1 $ S.fromList [''Ord2, ''UnifiedSymOrd2]

-- | The set of all classes for bifunctors (including concrete 'Ord') that can
-- be derived by 'deriveGADT'.
allClasses2WithOrd :: S.Set Name
allClasses2WithOrd = S.union allClasses2 allOrdClasses2

{-
-- | Derive all (non-functor) classes related to Grisette for a GADT with the
-- given name.
--
-- Extra classes are derived in addition to the classes derived by
-- 'deriveGADTAllWith'.
deriveGADTAllExtraWith :: DeriveConfig -> Name -> [Name] -> Q [Dec]
deriveGADTAllExtraWith deriveConfig typName extraClassNames = do
  deriveGADTWith deriveConfig typName $
    extraClassNames
      ++ [ ''Mergeable,
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
           ''SymOrd
         ]

-- | Derive all (non-functor) classes related to Grisette for a GADT with the
-- given name.
--
-- See 'deriveGADTAllExtraWith' for more details.
deriveGADTAllExtra :: Name -> [Name] -> Q [Dec]
deriveGADTAllExtra = deriveGADTAllExtraWith mempty

-- | Derive all (non-functor) classes related to Grisette for a GADT with the
-- given name.
--
-- Classes that are derived by this procedure are:
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
--
-- Note that it is okay to derive for non-GADT types using this procedure, and
-- it will be slightly more efficient.
deriveGADTAllWith :: DeriveConfig -> Name -> Q [Dec]
deriveGADTAllWith deriveConfig typName =
  deriveGADTAllExtraWith deriveConfig typName []

-- | Derive all (non-functor) classes related to Grisette for a GADT with the
-- given name.
--
-- See 'deriveGADTAllWith' for more details.
deriveGADTAll :: Name -> Q [Dec]
deriveGADTAll = deriveGADTAllWith mempty

-- | Derive all (non-functor) classes related to Grisette for a GADT with the
-- given name except the specified classes.
deriveGADTAllExceptWith :: DeriveConfig -> Name -> [Name] -> Q [Dec]
deriveGADTAllExceptWith deriveConfig typName classNames = do
  deriveGADTWith
    deriveConfig
    typName
    $ S.toList
    $ S.fromList
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
        ''SymOrd
      ]
      S.\\ S.fromList classNames

-- | Derive all (non-functor) classes related to Grisette for a GADT with the
-- given name except the specified classes.
--
-- See 'deriveGADTAllExceptWith' for more details.
deriveGADTAllExcept :: Name -> [Name] -> Q [Dec]
deriveGADTAllExcept = deriveGADTAllExceptWith mempty
-}