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
    deriveGADTAll,
    deriveGADTAllExcept,
  )
where

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
import Grisette.Internal.TH.GADT.DeriveMergeable (genMergeable, genMergeable', genMergeableAndGetMergingInfoResult)
import Language.Haskell.TH (Dec, Name, Q)

deriveProcedureMap :: M.Map Name (Name -> Q [Dec])
deriveProcedureMap =
  M.fromList
    [ -- (''Mergeable, deriveGADTMergeable),
      -- (''Mergeable1, deriveGADTMergeable1),
      -- (''Mergeable2, deriveGADTMergeable2),
      -- (''Mergeable3, deriveGADTMergeable3),
      (''EvalSym, deriveGADTEvalSym),
      (''EvalSym1, deriveGADTEvalSym1),
      (''EvalSym2, deriveGADTEvalSym2),
      (''ExtractSym, deriveGADTExtractSym),
      (''ExtractSym1, deriveGADTExtractSym1),
      (''ExtractSym2, deriveGADTExtractSym2)
    ]

deriveSingleGADT :: Name -> Name -> Q [Dec]
deriveSingleGADT typName className = do
  case M.lookup className deriveProcedureMap of
    Just procedure -> procedure typName
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
deriveGADT :: Name -> [Name] -> Q [Dec]
deriveGADT typName classNames = do
  let allClassNames = S.toList $ S.fromList classNames
  let (ns, ms) = splitMergeable allClassNames
  decs <- mapM (deriveSingleGADT typName) ns
  decMergeables <- deriveMergeables ms
  return $ concat decs ++ decMergeables
  where
    deriveMergeables :: [Int] -> Q [Dec]
    deriveMergeables [] = return []
    deriveMergeables [n] = genMergeable typName n
    deriveMergeables (n : ns) = do
      (info, dn) <- genMergeableAndGetMergingInfoResult typName n
      dns <- traverse (genMergeable' info typName) ns
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

-- | Derive all (non-functor) classes related to Grisette for a GADT with the
-- given name.
--
-- Classes that are derived by this procedure are:
--
-- * 'Mergeable'
-- * 'EvalSym'
-- * 'ExtractSym'
--
-- Note that it is okay to derive for non-GADT types using this procedure, and
-- it will be slightly more efficient.
deriveGADTAll :: Name -> Q [Dec]
deriveGADTAll typName =
  deriveGADT typName [''Mergeable, ''EvalSym, ''ExtractSym]

-- | Derive all (non-functor) classes related to Grisette for a GADT with the
-- given name except the specified classes.
deriveGADTAllExcept :: Name -> [Name] -> Q [Dec]
deriveGADTAllExcept typName classNames = do
  deriveGADT typName $
    S.toList $
      S.fromList [''Mergeable, ''EvalSym, ''ExtractSym]
        S.\\ S.fromList classNames
