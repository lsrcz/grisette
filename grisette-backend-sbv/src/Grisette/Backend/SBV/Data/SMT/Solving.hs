{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Grisette.Backend.SBV.Data.SMT.Solving
  ( GrisetteSMTConfig (..),
    sbvConfig,
    TermTy,
  )
where

import Control.DeepSeq
import Control.Monad.Except
import qualified Data.HashSet as S
import Data.Hashable
import Data.Kind
import Data.List (partition)
import Data.Maybe
import qualified Data.SBV as SBV
import Data.SBV.Control (Query)
import qualified Data.SBV.Control as SBVC
import GHC.TypeNats
import Grisette.Backend.SBV.Data.SMT.Lowering
import Grisette.Core.Data.Class.Bool
import Grisette.Core.Data.Class.CEGISSolver
import Grisette.Core.Data.Class.Evaluate
import Grisette.Core.Data.Class.ExtractSymbolics
import Grisette.Core.Data.Class.GenSym
import Grisette.Core.Data.Class.ModelOps
import Grisette.Core.Data.Class.Solvable
import Grisette.Core.Data.Class.Solver
import Grisette.IR.SymPrim.Data.BV
import Grisette.IR.SymPrim.Data.Class.ExtractSymbolics
import Grisette.IR.SymPrim.Data.Prim.InternedTerm.InternedCtors
import Grisette.IR.SymPrim.Data.Prim.InternedTerm.Term
import Grisette.IR.SymPrim.Data.Prim.Model as PM
import Grisette.IR.SymPrim.Data.Prim.PartialEval.Bool
import Grisette.IR.SymPrim.Data.SymPrim
import Grisette.IR.SymPrim.Data.TabularFunc
import Language.Haskell.TH.Syntax (Lift)

type Aux :: Bool -> Nat -> Type
type family Aux o n where
  Aux 'True n = SBV.SInteger
  Aux 'False n = SBV.SInt n

type IsZero :: Nat -> Bool
type family IsZero n where
  IsZero 0 = 'True
  IsZero _ = 'False

type TermTy :: Nat -> Type -> Type
type family TermTy bitWidth b where
  TermTy _ Bool = SBV.SBool
  TermTy n Integer = Aux (IsZero n) n
  TermTy n (IntN x) = SBV.SBV (SBV.IntN x)
  TermTy n (WordN x) = SBV.SBV (SBV.WordN x)
  TermTy n (a =-> b) = TermTy n a -> TermTy n b
  TermTy n (a --> b) = TermTy n a -> TermTy n b
  TermTy _ v = v

data GrisetteSMTConfig (integerBitWidth :: Nat) where
  UnboundedReasoning :: SBV.SMTConfig -> GrisetteSMTConfig 0
  BoundedReasoning ::
    (KnownNat integerBitWidth, IsZero integerBitWidth ~ 'False, SBV.BVIsNonZero integerBitWidth) =>
    SBV.SMTConfig ->
    GrisetteSMTConfig integerBitWidth

sbvConfig :: forall integerBitWidth. GrisetteSMTConfig integerBitWidth -> SBV.SMTConfig
sbvConfig (UnboundedReasoning config) = config
sbvConfig (BoundedReasoning config) = config

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
  solve config (Sym t) = snd <$> solveTermWith config t
  solveMulti config n s@(Sym t)
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
  solveAll = undefined

instance CEGISSolver (GrisetteSMTConfig n) SymBool SymbolSet SBVC.CheckSatResult PM.Model where
  cegisMultiInputs ::
    forall inputs spec.
    (GExtractSymbolics SymbolSet inputs, GEvaluateSym PM.Model inputs) =>
    GrisetteSMTConfig n ->
    [inputs] ->
    (inputs -> CEGISCondition SymBool) ->
    IO (Either SBVC.CheckSatResult ([inputs], PM.Model))
  cegisMultiInputs config inputs func =
    go1 (cexesAssertFunc conInputs) conInputs (error "Should have at least one gen") [] (conc True) (conc True) symInputs
    where
      (conInputs, symInputs) = partition (isEmptySet . extractSymbolics) inputs
      go1 cexFormula cexes previousModel inputs pre post remainingSymInputs = do
        case remainingSymInputs of
          [] -> return $ Right (cexes, previousModel)
          newInput : vs -> do
            let CEGISCondition nextPre nextPost = func newInput
            let finalPre = pre &&~ nextPre
            let finalPost = post &&~ nextPost
            r <- go cexFormula newInput (newInput : inputs) finalPre finalPost
            case r of
              Left failure -> return $ Left failure
              Right (newCexes, mo) -> do
                go1
                  (cexFormula &&~ cexesAssertFunc newCexes)
                  (cexes ++ newCexes)
                  mo
                  (newInput : inputs)
                  finalPre
                  finalPost
                  vs
      cexAssertFunc input =
        let CEGISCondition pre post = func input in pre &&~ post
      cexesAssertFunc :: [inputs] -> SymBool
      cexesAssertFunc = foldl (\acc x -> acc &&~ cexAssertFunc x) (conc True)
      go ::
        SymBool ->
        inputs ->
        [inputs] ->
        SymBool ->
        SymBool ->
        IO (Either SBVC.CheckSatResult ([inputs], PM.Model))
      go cexFormula inputs allInputs pre post =
        SBV.runSMTWith (sbvConfig config) $ do
          let Sym t = phi &&~ cexFormula
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
              loop ((forallSymbols `exceptFor`) <$> mr) [] newm
        where
          forallSymbols :: SymbolSet
          forallSymbols = gextractSymbolics allInputs
          phi = pre &&~ post
          negphi = pre &&~ nots post
          check :: Model -> IO (Either SBVC.CheckSatResult (inputs, PM.Model))
          check candidate = do
            let evaluated = gevaluateSym False candidate negphi
            r <- solve config evaluated
            return $ do
              m <- r
              let newm = exact forallSymbols m
              return (gevaluateSym False newm inputs, newm)
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
            [inputs] ->
            SymBiMap ->
            Query (SymBiMap, Either SBVC.CheckSatResult ([inputs], PM.Model))
          loop (Right mo) cexs origm = do
            r <- liftIO $ check mo
            case r of
              Left SBVC.Unsat -> return (origm, Right (cexs, mo))
              Left v -> return (origm, Left v)
              Right (cex, cexm) -> do
                (newm, res) <- guess cexm origm
                loop res (cex : cexs) newm
          loop (Left v) _ origm = return (origm, Left v)

newtype CegisInternal = CegisInternal Int
  deriving (Eq, Show, Ord, Lift)
  deriving newtype (Hashable, NFData)
