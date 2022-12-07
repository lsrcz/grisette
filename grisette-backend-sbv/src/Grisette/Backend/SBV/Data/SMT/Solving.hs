{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Grisette.Backend.SBV.Data.SMT.Solving () where

import Control.Monad.Except
import qualified Data.HashSet as S
import Data.Maybe
import qualified Data.SBV as SBV
import Data.SBV.Control (Query)
import qualified Data.SBV.Control as SBVC
import Grisette.Backend.SBV.Data.SMT.Config
import Grisette.Backend.SBV.Data.SMT.Lowering
import Grisette.Core.Data.Class.Bool
import Grisette.Core.Data.Class.CEGISSolver
import Grisette.Core.Data.Class.Evaluate
import Grisette.Core.Data.Class.ExtractSymbolics
import Grisette.Core.Data.Class.ModelOps
import Grisette.Core.Data.Class.Solver
import Grisette.IR.SymPrim.Data.Prim.InternedTerm.InternedCtors
import Grisette.IR.SymPrim.Data.Prim.InternedTerm.Term
import Grisette.IR.SymPrim.Data.Prim.Model as PM
import Grisette.IR.SymPrim.Data.Prim.PartialEval.Bool
import Grisette.IR.SymPrim.Data.SymPrim

solveTermWith ::
  forall integerBitWidth.
  GrisetteSMTConfig integerBitWidth ->
  Term Bool ->
  IO (SymBiMap, Either SBVC.CheckSatResult PM.Model)
solveTermWith config term = SBV.runSMTWith (sbvConfig config) $ do
  (m, a) <- lowerSinglePrim config term
  SBVC.query $ do
    SBV.constrain a
    r <- SBVC.checkSat
    case r of
      SBVC.Sat -> do
        md <- SBVC.getModel
        return (m, Right $ parseModel config md m)
      _ -> return (m, Left r)

instance Solver (GrisetteSMTConfig n) SymBool SBVC.CheckSatResult PM.Model where
  solveFormula config (Sym t) = snd <$> solveTermWith config t
  solveFormulaMulti config n s@(Sym t)
    | n > 0 = SBV.runSMTWith (sbvConfig config) $ do
        (newm, a) <- lowerSinglePrim config t
        SBVC.query $ do
          SBV.constrain a
          r <- SBVC.checkSat
          case r of
            SBVC.Sat -> do
              md <- SBVC.getModel
              let model = parseModel config md newm
              remainingModels n model newm
            _ -> return []
    | otherwise = return []
    where
      allSymbols = gextractSymbolics s :: SymbolSet
      next :: PM.Model -> SymBiMap -> Query (SymBiMap, Either SBVC.CheckSatResult PM.Model)
      next md origm = do
        let newtm =
              S.foldl'
                (\acc (SomeTypedSymbol _ v) -> pevalOrTerm acc (pevalNotTerm (fromJust $ equation v md)))
                (concTerm False)
                (unSymbolSet allSymbols)
        let (lowered, newm) = lowerSinglePrim' config newtm origm
        SBV.constrain lowered
        r <- SBVC.checkSat
        case r of
          SBVC.Sat -> do
            md1 <- SBVC.getModel
            let model = parseModel config md1 newm
            return (newm, Right model)
          _ -> return (newm, Left r)
      remainingModels :: Int -> PM.Model -> SymBiMap -> Query [PM.Model]
      remainingModels n1 md origm
        | n1 > 1 = do
            (newm, r) <- next md origm
            case r of
              Left _ -> return [md]
              Right mo -> do
                rmmd <- remainingModels (n1 - 1) mo newm
                return $ md : rmmd
        | otherwise = return [md]
  solveFormulaAll = undefined

instance CEGISSolver (GrisetteSMTConfig n) SymBool SymbolSet SBVC.CheckSatResult PM.Model where
  cegisFormulas ::
    forall forallArg.
    (GExtractSymbolics SymbolSet forallArg, GEvaluateSym PM.Model forallArg) =>
    GrisetteSMTConfig n ->
    forallArg ->
    SymBool ->
    SymBool ->
    IO (Either SBVC.CheckSatResult ([forallArg], PM.Model))
  cegisFormulas config foralls assumption assertion = SBV.runSMTWith (sbvConfig config) $ do
    let Sym t = phi
    (newm, a) <- lowerSinglePrim config t
    SBVC.query $
      snd <$> do
        SBV.constrain a
        r <- SBVC.checkSat
        mr <- case r of
          SBVC.Sat -> do
            md <- SBVC.getModel
            return $ Right $ parseModel config md newm
          _ -> return $ Left r
        loop (exceptFor forallSymbols <$> mr) [] newm
    where
      forallSymbols :: SymbolSet
      forallSymbols = gextractSymbolics foralls
      phi = nots assertion &&~ nots assumption
      negphi = assertion &&~ nots assumption
      check :: Model -> IO (Either SBVC.CheckSatResult (forallArg, PM.Model))
      check candidate = do
        let evaluated = gevaluateSym False candidate negphi
        r <- solveFormula config evaluated
        return $ do
          m <- r
          let newm = exact forallSymbols m
          return (gevaluateSym False newm foralls, newm)
      guess :: Model -> SymBiMap -> Query (SymBiMap, Either SBVC.CheckSatResult PM.Model)
      guess candidate origm = do
        let Sym evaluated = gevaluateSym False candidate phi
        let (lowered, newm) = lowerSinglePrim' config evaluated origm
        SBV.constrain lowered
        r <- SBVC.checkSat
        case r of
          SBVC.Sat -> do
            md <- SBVC.getModel
            let model = parseModel config md newm
            return (newm, Right $ exceptFor forallSymbols model)
          _ -> return (newm, Left r)
      loop ::
        Either SBVC.CheckSatResult PM.Model ->
        [forallArg] ->
        SymBiMap ->
        Query (SymBiMap, Either SBVC.CheckSatResult ([forallArg], PM.Model))
      loop (Right mo) cexs origm = do
        r <- liftIO $ check mo
        case r of
          Left SBVC.Unsat -> return (origm, Right (cexs, mo))
          Left v -> return (origm, Left v)
          Right (cex, cexm) -> do
            (newm, res) <- guess cexm origm
            loop res (cex : cexs) newm
      loop (Left v) _ origm = return (origm, Left v)
