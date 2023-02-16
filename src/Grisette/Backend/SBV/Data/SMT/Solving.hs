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
-- Copyright   :   (c) Sirui Lu 2021-2023
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Backend.SBV.Data.SMT.Solving
  ( ApproximationConfig (..),
    ExtraConfig (..),
    precise,
    approx,
    withTimeout,
    clearTimeout,
    withApprox,
    clearApprox,
    GrisetteSMTConfig (..),
    SolvingFailure (..),
    TermTy,
  )
where

import Control.DeepSeq
import Control.Exception
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
import Grisette.Core.Data.BV
import Grisette.Core.Data.Class.Bool
import Grisette.Core.Data.Class.CEGISSolver
import Grisette.Core.Data.Class.Evaluate
import Grisette.Core.Data.Class.ExtractSymbolics
import Grisette.Core.Data.Class.GenSym
import Grisette.Core.Data.Class.ModelOps
import Grisette.Core.Data.Class.Solvable
import Grisette.Core.Data.Class.Solver
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
-- >>> import Data.Proxy

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

-- | Configures how to approximate unbounded values.
--
-- For example, if we use @'Approx' ('Data.Proxy' :: 'Data.Proxy' 4)@ to approximate the
-- following unbounded integer:
--
-- > (+ a 9)
--
-- We will get
--
-- > (bvadd a #x9)
--
-- Here the value 9 will be approximated to a 4-bit bit vector, and the
-- operation `bvadd` will be used instead of `+`.
--
-- Note that this approximation may not be sound. See 'GrisetteSMTConfig' for
-- more details.
data ApproximationConfig (n :: Nat) where
  NoApprox :: ApproximationConfig 0
  Approx :: (KnownNat n, IsZero n ~ 'False, SBV.BVIsNonZero n) => p n -> ApproximationConfig n

data ExtraConfig (i :: Nat) = ExtraConfig
  { -- | Timeout in milliseconds for each solver call. CEGIS may call the
    -- solver multiple times and each call has its own timeout.
    timeout :: Maybe Int,
    -- | Configures how to approximate unbounded integer values.
    integerApprox :: ApproximationConfig i
  }

preciseExtraConfig :: ExtraConfig 0
preciseExtraConfig =
  ExtraConfig
    { timeout = Nothing,
      integerApprox = NoApprox
    }

approximateExtraConfig ::
  (KnownNat n, IsZero n ~ 'False, SBV.BVIsNonZero n) =>
  p n ->
  ExtraConfig n
approximateExtraConfig p =
  ExtraConfig
    { timeout = Nothing,
      integerApprox = Approx p
    }

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
-- For example, the Grisette term @5 * "a" :: 'SymInteger'@ should be translated
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
-- >>> solve (precise z3) $ a >~ 7 &&~ a <~ 9
-- Right (Model {a -> 8 :: Integer})
-- >>> solve (approx (Proxy @4) z3) $ a >~ 7 &&~ a <~ 9
-- Left Unsat
--
-- This may be avoided by setting an large enough reasoning precision to prevent
-- overflows.
data GrisetteSMTConfig (i :: Nat) = GrisetteSMTConfig {sbvConfig :: SBV.SMTConfig, extraConfig :: ExtraConfig i}

-- | A precise reasoning configuration with the given SBV solver configuration.
precise :: SBV.SMTConfig -> GrisetteSMTConfig 0
precise config = GrisetteSMTConfig config preciseExtraConfig

-- | An approximate reasoning configuration with the given SBV solver configuration.
approx ::
  forall p n.
  (KnownNat n, IsZero n ~ 'False, SBV.BVIsNonZero n) =>
  p n ->
  SBV.SMTConfig ->
  GrisetteSMTConfig n
approx p config = GrisetteSMTConfig config (approximateExtraConfig p)

-- | Set the timeout for the solver configuration.
withTimeout :: Int -> GrisetteSMTConfig i -> GrisetteSMTConfig i
withTimeout t config = config {extraConfig = (extraConfig config) {timeout = Just t}}

-- | Clear the timeout for the solver configuration.
clearTimeout :: GrisetteSMTConfig i -> GrisetteSMTConfig i
clearTimeout config = config {extraConfig = (extraConfig config) {timeout = Nothing}}

-- | Set the reasoning precision for the solver configuration.
withApprox :: (KnownNat n, IsZero n ~ 'False, SBV.BVIsNonZero n) => p n -> GrisetteSMTConfig i -> GrisetteSMTConfig n
withApprox p config = config {extraConfig = (extraConfig config) {integerApprox = Approx p}}

-- | Clear the reasoning precision and perform precise reasoning with the
-- solver configuration.
clearApprox :: GrisetteSMTConfig i -> GrisetteSMTConfig 0
clearApprox config = config {extraConfig = (extraConfig config) {integerApprox = NoApprox}}

data SolvingFailure
  = DSat (Maybe String)
  | Unsat
  | Unk
  | ResultNumLimitReached
  | SolvingError SBV.SBVException
  deriving (Show)

sbvCheckSatResult :: SBVC.CheckSatResult -> SolvingFailure
sbvCheckSatResult SBVC.Sat = error "Should not happen"
sbvCheckSatResult (SBVC.DSat msg) = DSat msg
sbvCheckSatResult SBVC.Unsat = Unsat
sbvCheckSatResult SBVC.Unk = Unk

applyTimeout :: GrisetteSMTConfig i -> Query a -> Query a
applyTimeout config q = case timeout (extraConfig config) of
  Nothing -> q
  Just t -> SBVC.timeout t q

solveTermWith ::
  forall integerBitWidth.
  GrisetteSMTConfig integerBitWidth ->
  Term Bool ->
  IO (Either SolvingFailure PM.Model)
solveTermWith config term =
  handle (return . Left . SolvingError) $
    SBV.runSMTWith (sbvConfig config) $ do
      (m, a) <- lowerSinglePrim config term
      SBVC.query $ applyTimeout config $ do
        SBV.constrain a
        r <- SBVC.checkSat
        case r of
          SBVC.Sat -> do
            md <- SBVC.getModel
            return (Right $ parseModel config md m)
          _ -> return (Left $ sbvCheckSatResult r)

instance Solver (GrisetteSMTConfig n) SolvingFailure where
  solve config (SymBool t) = solveTermWith config t
  solveMulti config n s@(SymBool t)
    | n > 0 =
        handle
          ( \(x :: SBV.SBVException) -> do
              print "An SBV Exception occurred:"
              print x
              print $
                "Warning: Note that solveMulti do not fully support "
                  ++ "timeouts, and will return an empty list if the solver"
                  ++ "timeouts in any iteration."
              return ([], SolvingError x)
          )
          $ SBV.runSMTWith (sbvConfig config)
          $ do
            (newm, a) <- lowerSinglePrim config t
            SBVC.query $ applyTimeout config $ do
              SBV.constrain a
              r <- SBVC.checkSat
              case r of
                SBVC.Sat -> do
                  md <- SBVC.getModel
                  let model = parseModel config md newm
                  remainingModels n model newm
                _ -> return ([], sbvCheckSatResult r)
    | otherwise = return ([], ResultNumLimitReached)
    where
      allSymbols = extractSymbolics s :: SymbolSet
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
      remainingModels :: Int -> PM.Model -> SymBiMap -> Query ([PM.Model], SolvingFailure)
      remainingModels n1 md origm
        | n1 > 1 = do
            (newm, r) <- next md origm
            case r of
              Left r -> return ([md], sbvCheckSatResult r)
              Right mo -> do
                (rmmd, e) <- remainingModels (n1 - 1) mo newm
                return (md : rmmd, e)
        | otherwise = return ([md], ResultNumLimitReached)
  solveAll = undefined

instance CEGISSolver (GrisetteSMTConfig n) SolvingFailure where
  cegisMultiInputs ::
    forall inputs spec.
    (ExtractSymbolics inputs, EvaluateSym inputs) =>
    GrisetteSMTConfig n ->
    [inputs] ->
    (inputs -> CEGISCondition) ->
    IO ([inputs], Either SolvingFailure PM.Model)
  cegisMultiInputs config inputs func =
    case symInputs of
      [] -> do
        m <- solve config (cexesAssertFun conInputs)
        return (conInputs, m)
      _ ->
        handle
          ( \(x :: SBV.SBVException) -> do
              print "An SBV Exception occurred:"
              print x
              print $
                "Warning: Note that CEGIS procedures do not fully support "
                  ++ "timeouts, and will return an empty counter example list if "
                  ++ "the solver timeouts during guessing phase."
              return ([], Left $ SolvingError x)
          )
          $ go1 (cexesAssertFun conInputs) conInputs (error "Should have at least one gen") [] (con True) (con True) symInputs
    where
      (conInputs, symInputs) = partition (isEmptySet . extractSymbolics) inputs
      go1 :: SymBool -> [inputs] -> PM.Model -> [inputs] -> SymBool -> SymBool -> [inputs] -> IO ([inputs], Either SolvingFailure PM.Model)
      go1 cexFormula cexes previousModel inputs pre post remainingSymInputs = do
        case remainingSymInputs of
          [] -> return (cexes, Right previousModel)
          newInput : vs -> do
            let CEGISCondition nextPre nextPost = func newInput
            let finalPre = pre &&~ nextPre
            let finalPost = post &&~ nextPost
            r <- go cexFormula newInput (newInput : inputs) finalPre finalPost
            case r of
              (newCexes, Left failure) -> return (cexes ++ newCexes, Left failure)
              (newCexes, Right mo) -> do
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
        IO ([inputs], Either SolvingFailure PM.Model)
      go cexFormula inputs allInputs pre post =
        SBV.runSMTWith (sbvConfig config) $ do
          let SymBool t = phi &&~ cexFormula
          (newm, a) <- lowerSinglePrim config t
          SBVC.query $
            applyTimeout config $
              snd <$> do
                SBV.constrain a
                r <- SBVC.checkSat
                mr <- case r of
                  SBVC.Sat -> do
                    md <- SBVC.getModel
                    return $ Right $ parseModel config md newm
                  _ -> return $ Left $ sbvCheckSatResult r
                loop ((forallSymbols `exceptFor`) <$> mr) [] newm
        where
          forallSymbols :: SymbolSet
          forallSymbols = extractSymbolics allInputs
          phi = pre &&~ post
          negphi = pre &&~ nots post
          check :: Model -> IO (Either SolvingFailure (inputs, PM.Model))
          check candidate = do
            let evaluated = evaluateSym False candidate negphi
            r <- solve config evaluated
            return $ do
              m <- r
              let newm = exact forallSymbols m
              return (evaluateSym False newm inputs, newm)
          guess :: Model -> SymBiMap -> Query (SymBiMap, Either SolvingFailure PM.Model)
          guess candidate origm = do
            let SymBool evaluated = evaluateSym False candidate phi
            let (lowered, newm) = lowerSinglePrim' config evaluated origm
            SBV.constrain lowered
            r <- SBVC.checkSat
            case r of
              SBVC.Sat -> do
                md <- SBVC.getModel
                let model = parseModel config md newm
                return (newm, Right $ exceptFor forallSymbols model)
              _ -> return (newm, Left $ sbvCheckSatResult r)
          loop ::
            Either SolvingFailure PM.Model ->
            [inputs] ->
            SymBiMap ->
            Query (SymBiMap, ([inputs], Either SolvingFailure PM.Model))
          loop (Right mo) cexes origm = do
            r <- liftIO $ check mo
            case r of
              Left Unsat -> return (origm, (cexes, Right mo))
              Left v -> return (origm, (cexes, Left v))
              Right (cex, cexm) -> do
                (newm, res) <- guess cexm origm
                loop res (cex : cexes) newm
          loop (Left v) cexes origm = return (origm, (cexes, Left v))

newtype CegisInternal = CegisInternal Int
  deriving (Eq, Show, Ord, Lift)
  deriving newtype (Hashable, NFData)
