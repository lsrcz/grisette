{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      :   Grisette.Backend.Solving
-- Copyright   :   (c) Sirui Lu 2021-2023
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Backend.Solving
  ( -- * SBV backend configuration
    ApproximationConfig (..),
    ExtraConfig (..),
    precise,
    approx,
    withTimeout,
    clearTimeout,
    withApprox,
    clearApprox,
    GrisetteSMTConfig (..),

    -- * SBV monadic solver interface
    SBVIncrementalT,
    SBVIncremental,
    runSBVIncrementalT,
    runSBVIncremental,

    -- * SBV solver handle
    SBVSolverHandle,

    -- * Internal lowering functions
    lowerSinglePrimCached,
    lowerSinglePrim,
    parseModel,
  )
where

import Control.Concurrent.Async (Async (asyncThreadId), async, wait)
import Control.Concurrent.STM
  ( TMVar,
    atomically,
    newTMVarIO,
    putTMVar,
    takeTMVar,
    tryReadTMVar,
    tryTakeTMVar,
  )
import Control.Concurrent.STM.TChan (TChan, newTChan, readTChan, writeTChan)
import Control.Exception (handle, throwTo)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader
  ( MonadReader (ask),
    MonadTrans (lift),
    ReaderT (runReaderT),
  )
import Control.Monad.STM (STM)
import Control.Monad.State (MonadState (get, put), StateT, evalStateT)
import Data.Dynamic (fromDyn, toDyn)
import Data.Proxy (Proxy (Proxy))
import qualified Data.SBV as SBV
import qualified Data.SBV.Control as SBVC
import qualified Data.SBV.Dynamic as SBVD
import qualified Data.SBV.Internals as SBVI
import qualified Data.SBV.Trans as SBVT
import qualified Data.SBV.Trans.Control as SBVTC
import GHC.IO.Exception (ExitCode (ExitSuccess))
import GHC.Stack (HasCallStack)
import GHC.TypeNats (KnownNat, Nat)
import Grisette.Backend.SymBiMap
  ( SymBiMap,
    addBiMap,
    addBiMapIntermediate,
    emptySymBiMap,
    findStringToSymbol,
    lookupTerm,
    sizeBiMap,
  )
import Grisette.Core.Data.Class.ModelOps (ModelOps (emptyModel, insertValue))
import Grisette.Core.Data.Class.Solver
  ( ConfigurableSolver (newSolver),
    MonadicSolver
      ( monadicSolverPop,
        monadicSolverPush,
        monadicSolverSolve
      ),
    Solver
      ( solverForceTerminate,
        solverRunCommand,
        solverSolve,
        solverTerminate
      ),
    SolverCommand (SolverPop, SolverPush, SolverSolve, SolverTerminate),
    SolvingFailure (SolvingError, Terminated, Unk, Unsat),
  )
import Grisette.SymPrim.Prim.Internal.IsZero (KnownIsZero)
import Grisette.SymPrim.Prim.Internal.Term
  ( PEvalApplyTerm (sbvApplyTerm),
    PEvalBVSignConversionTerm (sbvToSigned, sbvToUnsigned),
    PEvalBVTerm (sbvBVConcatTerm, sbvBVExtendTerm, sbvBVSelectTerm),
    PEvalBitwiseTerm
      ( sbvAndBitsTerm,
        sbvComplementBitsTerm,
        sbvOrBitsTerm,
        sbvXorBitsTerm
      ),
    PEvalDivModIntegralTerm
      ( sbvDivIntegralTerm,
        sbvModIntegralTerm,
        sbvQuotIntegralTerm,
        sbvRemIntegralTerm
      ),
    PEvalNumTerm
      ( sbvAbsNumTerm,
        sbvAddNumTerm,
        sbvMulNumTerm,
        sbvNegNumTerm,
        sbvSignumNumTerm
      ),
    PEvalOrdTerm (sbvLeOrdTerm, sbvLtOrdTerm),
    PEvalRotateTerm (sbvRotateLeftTerm, sbvRotateRightTerm),
    PEvalShiftTerm (sbvShiftLeftTerm, sbvShiftRightTerm),
    SBVFreshMonad,
    SBVRep (SBVType),
    SomeTypedSymbol (SomeTypedSymbol),
    SupportedPrim
      ( conSBVTerm,
        parseSMTModelResult,
        sbvEq,
        sbvIte,
        symSBVName,
        symSBVTerm,
        withPrim
      ),
    Term
      ( AbsNumTerm,
        AddNumTerm,
        AndBitsTerm,
        AndTerm,
        ApplyTerm,
        BVConcatTerm,
        BVExtendTerm,
        BVSelectTerm,
        ComplementBitsTerm,
        ConTerm,
        DivIntegralTerm,
        EqTerm,
        ITETerm,
        LeOrdTerm,
        LtOrdTerm,
        ModIntegralTerm,
        MulNumTerm,
        NegNumTerm,
        NotTerm,
        OrBitsTerm,
        OrTerm,
        QuotIntegralTerm,
        RemIntegralTerm,
        RotateLeftTerm,
        RotateRightTerm,
        ShiftLeftTerm,
        ShiftRightTerm,
        SignumNumTerm,
        SymTerm,
        ToSignedTerm,
        ToUnsignedTerm,
        XorBitsTerm
      ),
    introSupportedPrimConstraint,
    someTypedSymbol,
    withSymbolSupported,
  )
import Grisette.SymPrim.Prim.Model as PM
  ( Model,
  )
import Grisette.SymPrim.Prim.SomeTerm (SomeTerm (SomeTerm))
import Grisette.SymPrim.SymBool (SymBool (SymBool))

-- $setup
-- >>> import Grisette.Core
-- >>> import Grisette.SymPrim
-- >>> import Grisette.Backend
-- >>> import Data.Proxy

-- | Configures how to approximate unbounded values.
--
-- For example, if we use @'Approx' ('Data.Proxy' :: 'Data.Proxy' 4)@ to
-- approximate the following unbounded integer:
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
  Approx ::
    (KnownNat n, SBV.BVIsNonZero n, KnownIsZero n) =>
    p n ->
    ApproximationConfig n

-- | Grisette specific extra configurations for the SBV backend.
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
  (KnownNat n, SBV.BVIsNonZero n, KnownIsZero n) =>
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
-- We must note that the bounded translation is an approximation and is
-- __/not sound/__. As the approximation happens only during the final
-- translation, the symbolic evaluation may aggressively optimize the term based
-- on the properties of mathematical integer arithmetic. This may cause the
-- solver yield results that is incorrect under both unbounded or bounded
-- semantics.
--
-- The following is an example that is correct under bounded semantics, while is
-- incorrect under the unbounded semantics:
--
-- >>> :set -XTypeApplications -XOverloadedStrings -XDataKinds
-- >>> let a = "a" :: SymInteger
-- >>> solve (precise z3) $ a .> 7 .&& a .< 9
-- Right (Model {a -> 8 :: Integer})
-- >>> solve (approx (Proxy @4) z3) $ a .> 7 .&& a .< 9
-- Left Unsat
--
-- This may be avoided by setting an large enough reasoning precision to prevent
-- overflows.
data GrisetteSMTConfig (i :: Nat) = GrisetteSMTConfig
  { sbvConfig :: SBV.SMTConfig,
    extraConfig :: ExtraConfig i
  }

-- | A precise reasoning configuration with the given SBV solver configuration.
precise :: SBV.SMTConfig -> GrisetteSMTConfig 0
precise config = GrisetteSMTConfig config preciseExtraConfig

-- | An approximate reasoning configuration with the given SBV solver
-- configuration.
approx ::
  forall p n.
  (KnownNat n, SBV.BVIsNonZero n, KnownIsZero n) =>
  p n ->
  SBV.SMTConfig ->
  GrisetteSMTConfig n
approx p config = GrisetteSMTConfig config (approximateExtraConfig p)

-- | Set the timeout for the solver configuration.
withTimeout :: Int -> GrisetteSMTConfig i -> GrisetteSMTConfig i
withTimeout t config =
  config {extraConfig = (extraConfig config) {timeout = Just t}}

-- | Clear the timeout for the solver configuration.
clearTimeout :: GrisetteSMTConfig i -> GrisetteSMTConfig i
clearTimeout config =
  config {extraConfig = (extraConfig config) {timeout = Nothing}}

-- | Set the reasoning precision for the solver configuration.
withApprox ::
  (KnownNat n, SBV.BVIsNonZero n, KnownIsZero n) =>
  p n ->
  GrisetteSMTConfig i ->
  GrisetteSMTConfig n
withApprox p config =
  config {extraConfig = (extraConfig config) {integerApprox = Approx p}}

-- | Clear the reasoning precision and perform precise reasoning with the
-- solver configuration.
clearApprox :: GrisetteSMTConfig i -> GrisetteSMTConfig 0
clearApprox config =
  config {extraConfig = (extraConfig config) {integerApprox = NoApprox}}

sbvCheckSatResult :: SBVC.CheckSatResult -> SolvingFailure
sbvCheckSatResult SBVC.Sat = error "Should not happen"
sbvCheckSatResult (SBVC.DSat _) = error "DSat is currently not supported"
sbvCheckSatResult SBVC.Unsat = Unsat
sbvCheckSatResult SBVC.Unk = Unk

-- | Apply the timeout to the configuration.
applyTimeout ::
  (MonadIO m, SBVTC.MonadQuery m) => GrisetteSMTConfig i -> m a -> m a
applyTimeout config q = case timeout (extraConfig config) of
  Nothing -> q
  Just t -> SBVTC.timeout t q

-- | Incremental solver monad transformer with the SBV backend.
type SBVIncrementalT n m =
  ReaderT (GrisetteSMTConfig n) (StateT SymBiMap (SBVTC.QueryT m))

-- | Incremental solver monad with the SBV backend.
type SBVIncremental n = SBVIncrementalT n IO

-- | Run the incremental solver monad with a given configuration.
runSBVIncremental :: GrisetteSMTConfig n -> SBVIncremental n a -> IO a
runSBVIncremental = runSBVIncrementalT

-- | Run the incremental solver monad transformer with a given configuration.
runSBVIncrementalT ::
  (SBVTC.ExtractIO m) =>
  GrisetteSMTConfig n ->
  SBVIncrementalT n m a ->
  m a
runSBVIncrementalT config sbvIncrementalT =
  SBVT.runSMTWith (sbvConfig config) $
    SBVTC.query $
      applyTimeout config $
        flip evalStateT emptySymBiMap $
          runReaderT sbvIncrementalT config

instance (MonadIO m) => MonadicSolver (SBVIncrementalT n m) where
  monadicSolverSolve (SymBool formula) = do
    symBiMap <- get
    config <- ask
    (newSymBiMap, lowered) <- lowerSinglePrimCached config formula symBiMap
    lift $ lift $ SBV.constrain lowered
    put newSymBiMap
    checkSatResult <- SBVTC.checkSat
    case checkSatResult of
      SBVC.Sat -> do
        sbvModel <- SBVTC.getModel
        let model = parseModel config sbvModel newSymBiMap
        return $ Right model
      r -> return $ Left $ sbvCheckSatResult r
  monadicSolverPush = SBVTC.push
  monadicSolverPop = SBVTC.pop

data SBVSolverStatus = SBVSolverNormal | SBVSolverTerminated

-- | The handle type for the SBV solver.
--
-- See 'ConfigurableSolver' and 'Solver' for the interfaces.
data SBVSolverHandle = SBVSolverHandle
  { sbvSolverHandleMonad :: Async (),
    sbvSolverHandleStatus :: TMVar SBVSolverStatus,
    sbvSolverHandleInChan :: TChan SolverCommand,
    sbvSolverHandleOutChan :: TChan (Either SolvingFailure Model)
  }

setTerminated :: TMVar SBVSolverStatus -> STM ()
setTerminated status = do
  _ <- tryTakeTMVar status
  putTMVar status SBVSolverTerminated

instance ConfigurableSolver (GrisetteSMTConfig n) SBVSolverHandle where
  newSolver config = do
    sbvSolverHandleInChan <- atomically newTChan
    sbvSolverHandleOutChan <- atomically newTChan
    sbvSolverHandleStatus <- newTMVarIO SBVSolverNormal
    sbvSolverHandleMonad <- async $ do
      let handler e =
            liftIO $
              atomically $ do
                setTerminated sbvSolverHandleStatus
                writeTChan sbvSolverHandleOutChan (Left (SolvingError e))
      handle handler $ runSBVIncremental config $ do
        let loop = do
              nextFormula <- liftIO $ atomically $ readTChan sbvSolverHandleInChan
              case nextFormula of
                SolverPush n -> monadicSolverPush n >> loop
                SolverPop n -> monadicSolverPop n >> loop
                SolverTerminate -> return ()
                SolverSolve formula -> do
                  r <- monadicSolverSolve formula
                  liftIO $ atomically $ writeTChan sbvSolverHandleOutChan r
                  loop
        loop
        liftIO $ atomically $ do
          setTerminated sbvSolverHandleStatus
          writeTChan sbvSolverHandleOutChan $ Left Terminated
    return $ SBVSolverHandle {..}

instance Solver SBVSolverHandle where
  solverRunCommand f handle@(SBVSolverHandle _ status inChan _) command = do
    st <- liftIO $ atomically $ takeTMVar status
    case st of
      SBVSolverNormal -> do
        liftIO $ atomically $ writeTChan inChan command
        r <- f handle
        liftIO $ atomically $ do
          currStatus <- tryReadTMVar status
          case currStatus of
            Nothing -> putTMVar status SBVSolverNormal
            Just _ -> return ()
        return r
      SBVSolverTerminated -> do
        liftIO $ atomically $ setTerminated status
        return $ Left Terminated
  solverSolve handle nextFormula =
    solverRunCommand
      ( \(SBVSolverHandle _ _ _ outChan) ->
          liftIO $ atomically $ readTChan outChan
      )
      handle
      $ SolverSolve nextFormula
  solverTerminate (SBVSolverHandle thread status inChan _) = do
    liftIO $ atomically $ do
      setTerminated status
      writeTChan inChan SolverTerminate
    wait thread
  solverForceTerminate (SBVSolverHandle thread status _ outChan) = do
    liftIO $ atomically $ do
      setTerminated status
      writeTChan outChan (Left Terminated)
    throwTo (asyncThreadId thread) ExitSuccess
    wait thread

configIntroKnownIsZero :: GrisetteSMTConfig n -> ((KnownIsZero n) => r) -> r
configIntroKnownIsZero (GrisetteSMTConfig _ (ExtraConfig _ (Approx _))) r = r
configIntroKnownIsZero (GrisetteSMTConfig _ (ExtraConfig _ NoApprox)) r = r

lowerSinglePrimCached ::
  forall integerBitWidth a m.
  (HasCallStack, SBVFreshMonad m) =>
  GrisetteSMTConfig integerBitWidth ->
  Term a ->
  SymBiMap ->
  m (SymBiMap, SBVType integerBitWidth a)
lowerSinglePrimCached config t m =
  introSupportedPrimConstraint t $
    configIntroKnownIsZero config $
      case lookupTerm (SomeTerm t) m of
        Just x ->
          return
            ( m,
              withPrim @a (Proxy @integerBitWidth) $ fromDyn x undefined
            )
        Nothing -> lowerSinglePrimImpl config t m

lowerSinglePrim ::
  forall integerBitWidth a m.
  (HasCallStack, SBVFreshMonad m) =>
  GrisetteSMTConfig integerBitWidth ->
  Term a ->
  m (SymBiMap, SBVType integerBitWidth a)
lowerSinglePrim config t = lowerSinglePrimCached config t emptySymBiMap

lowerSinglePrimImpl ::
  forall integerBitWidth a m.
  (HasCallStack, SBVFreshMonad m, KnownIsZero integerBitWidth) =>
  GrisetteSMTConfig integerBitWidth ->
  Term a ->
  SymBiMap ->
  m (SymBiMap, SBVType integerBitWidth a)
lowerSinglePrimImpl config (ConTerm _ v) m =
  return (m, conSBVTerm config v)
lowerSinglePrimImpl config t@(SymTerm _ ts) m =
  withPrim @a config $ do
    let name = symSBVName ts (sizeBiMap m)
    g <- symSBVTerm @a config name
    return (addBiMap (SomeTerm t) (toDyn g) name (someTypedSymbol ts) m, g)
lowerSinglePrimImpl config t m =
  introSupportedPrimConstraint t $
    withPrim @a config $ do
      (m, r) <- lowerSinglePrimIntermediate config t m
      return (addBiMapIntermediate (SomeTerm t) (toDyn r) m, r)

lowerSinglePrimIntermediate ::
  forall integerBitWidth a m.
  (HasCallStack, SBVFreshMonad m, KnownIsZero integerBitWidth) =>
  GrisetteSMTConfig integerBitWidth ->
  Term a ->
  SymBiMap ->
  m (SymBiMap, SBVType integerBitWidth a)
lowerSinglePrimIntermediate config (NotTerm _ a) m = do
  (m1, a') <- lowerSinglePrimCached config a m
  return (m1, SBV.sNot a')
lowerSinglePrimIntermediate config (OrTerm _ a b) m = do
  (m1, a') <- lowerSinglePrimCached config a m
  (m2, b') <- lowerSinglePrimCached config b m1
  return (m2, a' SBV..|| b')
lowerSinglePrimIntermediate config (AndTerm _ a b) m = do
  (m1, a') <- lowerSinglePrimCached config a m
  (m2, b') <- lowerSinglePrimCached config b m1
  return (m2, a' SBV..&& b')
lowerSinglePrimIntermediate config (EqTerm _ (a :: Term v) b) m = do
  (m1, a') <- lowerSinglePrimCached config a m
  (m2, b') <- lowerSinglePrimCached config b m1
  return (m2, sbvEq @v config a' b')
lowerSinglePrimIntermediate config (ITETerm _ c a b) m = do
  (m1, c') <- lowerSinglePrimCached config c m
  (m2, a') <- lowerSinglePrimCached config a m1
  (m3, b') <- lowerSinglePrimCached config b m2
  return (m3, sbvIte @a config c' a' b')
lowerSinglePrimIntermediate config (AddNumTerm _ a b) m = do
  (m1, a') <- lowerSinglePrimCached config a m
  (m2, b') <- lowerSinglePrimCached config b m1
  return (m2, sbvAddNumTerm @a config a' b')
lowerSinglePrimIntermediate config (NegNumTerm _ a) m = do
  (m1, a') <- lowerSinglePrimCached config a m
  return (m1, sbvNegNumTerm @a config a')
lowerSinglePrimIntermediate config (MulNumTerm _ a b) m = do
  (m1, a') <- lowerSinglePrimCached config a m
  (m2, b') <- lowerSinglePrimCached config b m1
  return (m2, sbvMulNumTerm @a config a' b')
lowerSinglePrimIntermediate config (AbsNumTerm _ a) m = do
  (m1, a') <- lowerSinglePrimCached config a m
  return (m1, sbvAbsNumTerm @a config a')
lowerSinglePrimIntermediate config (SignumNumTerm _ a) m = do
  (m1, a') <- lowerSinglePrimCached config a m
  return (m1, sbvSignumNumTerm @a config a')
lowerSinglePrimIntermediate config (LtOrdTerm _ (a :: Term v) b) m = do
  (m1, a') <- lowerSinglePrimCached config a m
  (m2, b') <- lowerSinglePrimCached config b m1
  return (m2, sbvLtOrdTerm @v config a' b')
lowerSinglePrimIntermediate config (LeOrdTerm _ (a :: Term v) b) m = do
  (m1, a') <- lowerSinglePrimCached config a m
  (m2, b') <- lowerSinglePrimCached config b m1
  return (m2, sbvLeOrdTerm @v config a' b')
lowerSinglePrimIntermediate config (AndBitsTerm _ a b) m = do
  (m1, a') <- lowerSinglePrimCached config a m
  (m2, b') <- lowerSinglePrimCached config b m1
  return (m2, sbvAndBitsTerm @a config a' b')
lowerSinglePrimIntermediate config (OrBitsTerm _ a b) m = do
  (m1, a') <- lowerSinglePrimCached config a m
  (m2, b') <- lowerSinglePrimCached config b m1
  return (m2, sbvOrBitsTerm @a config a' b')
lowerSinglePrimIntermediate config (XorBitsTerm _ a b) m = do
  (m1, a') <- lowerSinglePrimCached config a m
  (m2, b') <- lowerSinglePrimCached config b m1
  return (m2, sbvXorBitsTerm @a config a' b')
lowerSinglePrimIntermediate config (ComplementBitsTerm _ a) m = do
  (m1, a') <- lowerSinglePrimCached config a m
  return (m1, sbvComplementBitsTerm @a config a')
lowerSinglePrimIntermediate config (ShiftLeftTerm _ a b) m = do
  (m1, a') <- lowerSinglePrimCached config a m
  (m2, b') <- lowerSinglePrimCached config b m1
  return (m2, sbvShiftLeftTerm @a config a' b')
lowerSinglePrimIntermediate config (ShiftRightTerm _ a b) m = do
  (m1, a') <- lowerSinglePrimCached config a m
  (m2, b') <- lowerSinglePrimCached config b m1
  return (m2, sbvShiftRightTerm @a config a' b')
lowerSinglePrimIntermediate config (RotateLeftTerm _ a b) m = do
  (m1, a') <- lowerSinglePrimCached config a m
  (m2, b') <- lowerSinglePrimCached config b m1
  return (m2, sbvRotateLeftTerm @a config a' b')
lowerSinglePrimIntermediate config (RotateRightTerm _ a b) m = do
  (m1, a') <- lowerSinglePrimCached config a m
  (m2, b') <- lowerSinglePrimCached config b m1
  return (m2, sbvRotateRightTerm @a config a' b')
lowerSinglePrimIntermediate config (ApplyTerm _ (f :: Term f) a) m = do
  (m1, l1) <- lowerSinglePrimCached config f m
  (m2, l2) <- lowerSinglePrimCached config a m1
  return (m2, sbvApplyTerm @f config l1 l2)
lowerSinglePrimIntermediate config (ToSignedTerm _ (a :: Term (u n))) m = do
  (m1, a') <- lowerSinglePrimCached config a m
  return (m1, sbvToSigned (Proxy @u) (Proxy @n) config a')
lowerSinglePrimIntermediate config (ToUnsignedTerm _ (a :: Term (s n))) m = do
  (m1, a') <- lowerSinglePrimCached config a m
  return (m1, sbvToUnsigned (Proxy @s) (Proxy @n) config a')
lowerSinglePrimIntermediate
  config
  (BVConcatTerm _ (a :: Term (bv l)) (b :: Term (bv r)))
  m = do
    (m1, a') <- lowerSinglePrimCached config a m
    (m2, b') <- lowerSinglePrimCached config b m1
    return (m2, sbvBVConcatTerm @bv config (Proxy @l) (Proxy @r) a' b')
lowerSinglePrimIntermediate
  config
  (BVExtendTerm _ signed (pr :: p r) (a :: Term (bv l)))
  m = do
    (m1, a') <- lowerSinglePrimCached config a m
    return (m1, sbvBVExtendTerm @bv config (Proxy @l) pr signed a')
lowerSinglePrimIntermediate
  config
  (BVSelectTerm _ (pix :: p ix) (pw :: q w) (a :: Term (bv n)))
  m = do
    (m1, a') <- lowerSinglePrimCached config a m
    return (m1, sbvBVSelectTerm @bv config pix pw (Proxy @n) a')
lowerSinglePrimIntermediate config (DivIntegralTerm _ a b) m = do
  (m1, a') <- lowerSinglePrimCached config a m
  (m2, b') <- lowerSinglePrimCached config b m1
  return (m2, sbvDivIntegralTerm @a config a' b')
lowerSinglePrimIntermediate config (ModIntegralTerm _ a b) m = do
  (m1, a') <- lowerSinglePrimCached config a m
  (m2, b') <- lowerSinglePrimCached config b m1
  return (m2, sbvModIntegralTerm @a config a' b')
lowerSinglePrimIntermediate config (QuotIntegralTerm _ a b) m = do
  (m1, a') <- lowerSinglePrimCached config a m
  (m2, b') <- lowerSinglePrimCached config b m1
  return (m2, sbvQuotIntegralTerm @a config a' b')
lowerSinglePrimIntermediate config (RemIntegralTerm _ a b) m = do
  (m1, a') <- lowerSinglePrimCached config a m
  (m2, b') <- lowerSinglePrimCached config b m1
  return (m2, sbvRemIntegralTerm @a config a' b')
lowerSinglePrimIntermediate _ _ _ = undefined

#if MIN_VERSION_sbv(10,3,0)
preprocessUIFuncs ::
  [(String, (Bool, ty, Either String ([([SBVD.CV], SBVD.CV)], SBVD.CV)))] ->
  Maybe [(String, ([([SBVD.CV], SBVD.CV)], SBVD.CV))]
preprocessUIFuncs =
  traverse
    (\v -> case v of
      (a, (_, _, Right c)) -> Just (a, c)
      _ -> Nothing)
#elif MIN_VERSION_sbv(10,0,0)
preprocessUIFuncs ::
  [(String, (ty, Either String ([([SBVD.CV], SBVD.CV)], SBVD.CV)))] ->
  Maybe [(String, ([([SBVD.CV], SBVD.CV)], SBVD.CV))]
preprocessUIFuncs =
  traverse
    (\v -> case v of
      (a, (_, Right c)) -> Just (a, c)
      _ -> Nothing)
#else
preprocessUIFuncs ::
  [(String, (ty, ([([SBVD.CV], SBVD.CV)], SBVD.CV)))] ->
  Maybe [(String, ([([SBVD.CV], SBVD.CV)], SBVD.CV))]
preprocessUIFuncs = Just . fmap (\(a, (_, c)) -> (a, c))
#endif

parseModel ::
  forall integerBitWidth.
  GrisetteSMTConfig integerBitWidth ->
  SBVI.SMTModel ->
  SymBiMap ->
  PM.Model
parseModel _ (SBVI.SMTModel _ _ assoc origFuncs) mp =
  case preprocessUIFuncs origFuncs of
    Just funcs -> foldr goSingle emptyModel $ funcs ++ assocFuncs
    _ -> error "SBV Failed to parse model"
  where
    assocFuncs = (\(s, v) -> (s, ([([], v)], v))) <$> assoc
    goSingle :: (String, ([([SBVD.CV], SBVD.CV)], SBVD.CV)) -> PM.Model -> PM.Model
    goSingle (name, cv) m = case findStringToSymbol name mp of
      Just (SomeTypedSymbol (_ :: p r) s) ->
        withSymbolSupported s $
          insertValue s (parseSMTModelResult 0 cv :: r) m
      Nothing ->
        error $
          "BUG: Please send a bug report. The model is not consistent with the "
            <> "list of symbols that have been defined."
