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

-- |
-- Module      :   Grisette.Backend.SBV.Data.SMT.Solving
-- Copyright   :   (c) Sirui Lu 2021-2022
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
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
import Grisette.IR.SymPrim.Data.TabularFun
import Language.Haskell.TH.Syntax (Lift)

-- $setup
-- >>> import Grisette.Core
-- >>> import Grisette.IR.SymPrim
-- >>> import Grisette.Backend.SBV

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

-- | Solver configuration for the Grisette SBV backend.
-- A Grisette solver configuration consists of a SBV solver configuration and
-- the reasoning precision.
--
-- Integers can be unbounded (mathematical integer) or bounded (machine
-- integer/bit vector). The two types of integers have their own use cases,
-- and should be used to model different systems.
-- However, the solvers are known to have bad performance on some unbounded
-- integer operations, for example, when reason about non-linear integer
-- algebraic (e.g., multiplication or division),
-- the solver may not be able to get a result in a reasonable time.
-- In contrast, reasoning about bounded integers is usually more efficient.
--
-- To bridge the performance gap between the two types of integers, Grisette
-- allows to model the system with unbounded integers, and evaluate them with
-- infinite precision during the symbolic evaluation, but when solving the
-- queries, they are translated to bit vectors for better performance.
--
-- For example, the Grisette term @5 * "a" :: SymInteger@ should be translated
-- to the following SMT with the unbounded reasoning configuration (the term
-- is @t1@):
--
-- > (declare-fun a () Int)           ; declare symbolic constant a
-- > (define-fun c1 () Int 5)         ; define the concrete value 5
-- > (define-fun t1 () Int (* c1 a))  ; define the term
--
-- While with reasoning precision 4, it would be translated to the following
-- SMT (the term is @t1@):
--
-- > ; declare symbolic constant a, the type is a bit vector with bit width 4
-- > (declare-fun a () (_ BitVec 4))
-- > ; define the concrete value 1, translated to the bit vector #x1
-- > (define-fun c1 () (_ BitVec 4) #x5)
-- > ; define the term, using bit vector addition rather than integer addition
-- > (define-fun t1 () (_ BitVec 4) (bvmul c1 a))
--
-- This bounded translation can usually be solved faster than the unbounded
-- one, and should work well when no overflow is possible, in which case the
-- performance can be improved with almost no cost.
--
-- We must note that the bounded translation is an approximation and is __/not/__
-- __/sound/__. As the approximation happens only during the final translation,
-- the symbolic evaluation may aggressively optimize the term based on the
-- properties of mathematical integer arithmetic. This may cause the solver yield
-- results that is incorrect under both unbounded or bounded semantics.
--
-- The following is an example that is correct under bounded semantics, while is
-- incorrect under the unbounded semantics:
--
-- >>> :set -XTypeApplications -XOverloadedStrings -XDataKinds
-- >>> let a = "a" :: SymInteger
-- >>> solve (UnboundedReasoning z3) $ a >~ 7 &&~ a <~ 9
-- Right (Model {a -> 8 :: Integer})
-- >>> solve (BoundedReasoning @4 z3) $ a >~ 7 &&~ a <~ 9
-- Left Unsat
--
-- This should be avoided by setting an large enough reasoning precision to prevent
-- overflows.
data GrisetteSMTConfig (integerBitWidth :: Nat) where
  UnboundedReasoning :: SBV.SMTConfig -> GrisetteSMTConfig 0
  BoundedReasoning ::
    (KnownNat integerBitWidth, IsZero integerBitWidth ~ 'False, SBV.BVIsNonZero integerBitWidth) =>
    SBV.SMTConfig ->
    GrisetteSMTConfig integerBitWidth

-- | Extract the SBV solver configuration from the Grisette solver configuration.
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
                (conTerm False)
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
    go1 (cexesAssertFun conInputs) conInputs (error "Should have at least one gen") [] (con True) (con True) symInputs
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
                  (cexFormula &&~ cexesAssertFun newCexes)
                  (cexes ++ newCexes)
                  mo
                  (newInput : inputs)
                  finalPre
                  finalPost
                  vs
      cexAssertFun input =
        let CEGISCondition pre post = func input in pre &&~ post
      cexesAssertFun :: [inputs] -> SymBool
      cexesAssertFun = foldl (\acc x -> acc &&~ cexAssertFun x) (con True)
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
