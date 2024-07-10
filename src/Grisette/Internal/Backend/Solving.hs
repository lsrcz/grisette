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
-- Module      :   Grisette.Internal.Backend.Solving
-- Copyright   :   (c) Sirui Lu 2021-2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Internal.Backend.Solving
  ( -- * SBV backend configuration
    GrisetteSMTConfig (..),
    boolector,
    bitwuzla,
    cvc4,
    cvc5,
    yices,
    dReal,
    z3,
    mathSAT,
    abc,

    -- * Changing the extra configurations
    ApproximationConfig (..),
    ExtraConfig (..),
    precise,
    approximate,
    withTimeout,
    clearTimeout,

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
import Grisette.Internal.Backend.QuantifiedStack
  ( QuantifiedStack,
    QuantifiedSymbols,
    addQuantified,
    addQuantifiedSymbol,
    emptyQuantifiedStack,
    emptyQuantifiedSymbols,
    isQuantifiedSymbol,
    lookupQuantified,
  )
import Grisette.Internal.Backend.SymBiMap
  ( SymBiMap,
    addBiMap,
    addBiMapIntermediate,
    attachNextQuantifiedSymbolInfo,
    emptySymBiMap,
    findStringToSymbol,
    lookupTerm,
    sizeBiMap,
  )
import Grisette.Internal.Core.Data.Class.ModelOps
  ( ModelOps (emptyModel, insertValue),
  )
import Grisette.Internal.Core.Data.Class.Solver
  ( ConfigurableSolver (newSolver),
    MonadicSolver
      ( monadicSolverAssert,
        monadicSolverCheckSat,
        monadicSolverPop,
        monadicSolverPush,
        monadicSolverResetAssertions
      ),
    Solver
      ( solverCheckSat,
        solverForceTerminate,
        solverRunCommand,
        solverTerminate
      ),
    SolverCommand
      ( SolverAssert,
        SolverCheckSat,
        SolverPop,
        SolverPush,
        SolverResetAssertions,
        SolverTerminate
      ),
    SolvingFailure (SolvingError, Terminated, Unk, Unsat),
  )
import Grisette.Internal.SymPrim.GeneralFun (substTerm)
import Grisette.Internal.SymPrim.Prim.Internal.Instances.PEvalFP
  ( sbvFPBinaryTerm,
    sbvFPFMATerm,
    sbvFPRoundingBinaryTerm,
    sbvFPRoundingUnaryTerm,
    sbvFPTraitTerm,
    sbvFPUnaryTerm,
  )
import Grisette.Internal.SymPrim.Prim.Internal.IsZero (KnownIsZero)
import Grisette.Internal.SymPrim.Prim.Internal.Term
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
    PEvalFloatingTerm (sbvFloatingUnaryTerm, sbvPowerTerm),
    PEvalFractionalTerm (sbvFdivTerm, sbvRecipTerm),
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
    SupportedNonFuncPrim (withNonFuncPrim),
    SupportedPrim
      ( conSBVTerm,
        parseSMTModelResult,
        sbvEq,
        sbvIte,
        symSBVName,
        symSBVTerm,
        withPrim
      ),
    SymbolKind (NonFuncSymbol),
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
        ExistsTerm,
        FPBinaryTerm,
        FPFMATerm,
        FPRoundingBinaryTerm,
        FPRoundingUnaryTerm,
        FPTraitTerm,
        FPUnaryTerm,
        FdivTerm,
        FloatingUnaryTerm,
        ForallTerm,
        ITETerm,
        LeOrdTerm,
        LtOrdTerm,
        ModIntegralTerm,
        MulNumTerm,
        NegNumTerm,
        NotTerm,
        OrBitsTerm,
        OrTerm,
        PowerTerm,
        QuotIntegralTerm,
        RecipTerm,
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
    TypedSymbol (TypedSymbol),
    introSupportedPrimConstraint,
    someTypedSymbol,
    symTerm,
    withSymbolSupported,
  )
import Grisette.Internal.SymPrim.Prim.Model as PM
  ( Model,
  )
import Grisette.Internal.SymPrim.Prim.SomeTerm (SomeTerm (SomeTerm))
import Grisette.Internal.SymPrim.SymBool (SymBool (SymBool))

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
-- operation @bvadd@ will be used instead of @+@.
--
-- Note that this approximation may not be sound, and usually you should not use
-- this feature. See 'approximate' for more details.
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

-- | Solver configuration for the Grisette SBV backend.
--
-- A Grisette solver configuration consists of a SBV solver configuration and
-- some extra configurations.
--
-- You should start with the predefined configurations.
data GrisetteSMTConfig (i :: Nat) = GrisetteSMTConfig
  { sbvConfig :: SBV.SMTConfig,
    extraConfig :: ExtraConfig i
  }

preciseExtraConfig :: ExtraConfig 0
preciseExtraConfig =
  ExtraConfig
    { timeout = Nothing,
      integerApprox = NoApprox
    }

-- | Solver configuration for Boolector. <https://boolector.github.io/>
boolector :: GrisetteSMTConfig 0
boolector = GrisetteSMTConfig SBV.boolector preciseExtraConfig

-- | Solver configuration for Bitwuzla. <https://bitwuzla.github.io/>
bitwuzla :: GrisetteSMTConfig 0
bitwuzla = GrisetteSMTConfig SBV.bitwuzla preciseExtraConfig

-- | Solver configuration for CVC4. <https://cvc4.github.io/>
cvc4 :: GrisetteSMTConfig 0
cvc4 = GrisetteSMTConfig SBV.cvc4 preciseExtraConfig

-- | Solver configuration for CVC5. <https://cvc5.github.io/>
cvc5 :: GrisetteSMTConfig 0
cvc5 = GrisetteSMTConfig SBV.cvc5 preciseExtraConfig

-- | Solver configuration for Yices. <https://yices.csl.sri.com/>
yices :: GrisetteSMTConfig 0
yices = GrisetteSMTConfig SBV.yices preciseExtraConfig

-- | Solver configuration for DReal. <http://dreal.github.io/>
dReal :: GrisetteSMTConfig 0
dReal = GrisetteSMTConfig SBV.dReal preciseExtraConfig

-- | Solver configuration for Z3. <https://github.com/Z3Prover/z3/>
z3 :: GrisetteSMTConfig 0
z3 = GrisetteSMTConfig SBV.z3 preciseExtraConfig

-- | Solver configuration for MathSAT. <http://mathsat.fbk.eu/>
mathSAT :: GrisetteSMTConfig 0
mathSAT = GrisetteSMTConfig SBV.mathSAT preciseExtraConfig

-- | Solver configuration for ABC. <http://www.eecs.berkeley.edu/~alanmi/abc/>
abc :: GrisetteSMTConfig 0
abc = GrisetteSMTConfig SBV.abc preciseExtraConfig

-- | Set to perform precise reasoning with the solver configuration.
precise :: GrisetteSMTConfig n -> GrisetteSMTConfig 0
precise config =
  config {extraConfig = (extraConfig config) {integerApprox = NoApprox}}

-- | Set to perform approximate reasoning with the solver configuration.
--
-- __Note:__ This isn't the preferred way to control the reasoning precision.
-- A better way is to write your symbolic evaluation code in a generic way, and
-- control the evaluation with the types.
--
-- >>> f :: (Num a) => a -> a -> a; f x y = x + y
-- >>> solve z3 $ f "a" 5 .== (2 :: SymInteger)
-- Right (Model {a -> -3 :: Integer})
-- >>> solve z3 $ f "a" 5 .== (2 :: SymWordN 4)
-- Right (Model {a -> 0xd :: WordN 4})
--
-- __Description:__
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
-- For example, the Grisette term @5 * "a" :: 'Grisette.SymPrim.SymInteger'@
-- should be translated to the following SMT with the unbounded reasoning
-- configuration (the term is @t1@):
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
-- >>> solve z3 $ a .> 7 .&& a .< 9
-- Right (Model {a -> 8 :: Integer})
-- >>> solve (approximate (Proxy @4) z3) $ a .> 7 .&& a .< 9
-- Left Unsat
--
-- This may be avoided by setting an large enough reasoning precision to prevent
-- overflows.
approximate ::
  forall p m n.
  (KnownNat n, SBV.BVIsNonZero n, KnownIsZero n) =>
  p n ->
  GrisetteSMTConfig m ->
  GrisetteSMTConfig n
approximate p config =
  config {extraConfig = (extraConfig config) {integerApprox = Approx p}}

-- | Set the timeout for the solver configuration.
withTimeout :: Int -> GrisetteSMTConfig i -> GrisetteSMTConfig i
withTimeout t config =
  config {extraConfig = (extraConfig config) {timeout = Just t}}

-- | Clear the timeout for the solver configuration.
clearTimeout :: GrisetteSMTConfig i -> GrisetteSMTConfig i
clearTimeout config =
  config {extraConfig = (extraConfig config) {timeout = Nothing}}

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
  monadicSolverAssert (SymBool formula) = do
    symBiMap <- get
    config <- ask
    (newSymBiMap, lowered) <-
      lowerSinglePrimCached config formula emptyQuantifiedSymbols symBiMap
    lift $ lift $ SBV.constrain (lowered emptyQuantifiedStack)
    put newSymBiMap
  monadicSolverCheckSat = do
    checkSatResult <- SBVTC.checkSat
    config <- ask
    symBiMap <- get
    case checkSatResult of
      SBVC.Sat -> do
        sbvModel <- SBVTC.getModel
        let model = parseModel config sbvModel symBiMap
        return $ Right model
      r -> return $ Left $ sbvCheckSatResult r
  monadicSolverResetAssertions = SBVTC.resetAssertions
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
                SolverResetAssertions -> monadicSolverResetAssertions >> loop
                SolverAssert formula -> do
                  monadicSolverAssert formula
                  loop
                SolverCheckSat -> do
                  r <- monadicSolverCheckSat
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
  solverCheckSat handle =
    solverRunCommand
      ( \(SBVSolverHandle _ _ _ outChan) ->
          liftIO $ atomically $ readTChan outChan
      )
      handle
      SolverCheckSat
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

-- | Lower a single primitive term to SBV. With an explicitly provided
-- 'SymBiMap' cache.
lowerSinglePrimCached ::
  forall integerBitWidth a m.
  (HasCallStack, SBVFreshMonad m) =>
  GrisetteSMTConfig integerBitWidth ->
  Term a ->
  QuantifiedSymbols ->
  SymBiMap ->
  m (SymBiMap, QuantifiedStack -> SBVType integerBitWidth a)
lowerSinglePrimCached config t qs m =
  introSupportedPrimConstraint t $
    configIntroKnownIsZero config $
      case lookupTerm (SomeTerm t) m of
        Just x ->
          return
            ( m,
              ( \qst ->
                  withPrim @a (Proxy @integerBitWidth) $
                    fromDyn (x qst) undefined
              )
            )
        Nothing -> lowerSinglePrimImpl config t qs m

-- | Lower a single primitive term to SBV.
lowerSinglePrim ::
  forall integerBitWidth a m.
  (HasCallStack, SBVFreshMonad m) =>
  GrisetteSMTConfig integerBitWidth ->
  Term a ->
  m (SymBiMap, QuantifiedStack -> SBVType integerBitWidth a)
lowerSinglePrim config t =
  lowerSinglePrimCached config t emptyQuantifiedSymbols emptySymBiMap

lowerSinglePrimImpl ::
  forall integerBitWidth a m.
  (HasCallStack, SBVFreshMonad m, KnownIsZero integerBitWidth) =>
  GrisetteSMTConfig integerBitWidth ->
  Term a ->
  QuantifiedSymbols ->
  SymBiMap ->
  m (SymBiMap, QuantifiedStack -> SBVType integerBitWidth a)
lowerSinglePrimImpl config (ConTerm _ v) _ m =
  return (m, const $ conSBVTerm config v)
lowerSinglePrimImpl config t@(SymTerm _ ts) qs m
  | isQuantifiedSymbol ts qs = withPrim @a config $ do
      let retDyn qst =
            case lookupQuantified (someTypedSymbol ts) qst of
              Just v -> v
              Nothing -> error "BUG: Symbol not found in the quantified stack"
      return
        ( addBiMapIntermediate (SomeTerm t) retDyn m,
          \x ->
            fromDyn
              (retDyn x)
              (error "BUG: Symbol not found in the quantified stack")
        )
lowerSinglePrimImpl config t@(SymTerm _ ts) _ m = withPrim @a config $
  withPrim @a config $ do
    let name = symSBVName ts (sizeBiMap m)
    g <- symSBVTerm @a config name
    return
      (addBiMap (SomeTerm t) (toDyn g) name (someTypedSymbol ts) m, const g)
#if MIN_VERSION_sbv(10,1,0)
lowerSinglePrimImpl config t@(ForallTerm _ (ts :: TypedSymbol 'NonFuncSymbol t1) v) qs m =
  withNonFuncPrim @t1 config $ do
    do
      let (newm, sb@(TypedSymbol sbs)) = attachNextQuantifiedSymbolInfo m ts
      let substedTerm = substTerm ts (symTerm sbs) v
      (nextm, r) <-
        lowerSinglePrimCached
          config
          substedTerm
          (addQuantifiedSymbol sb qs)
          newm
      let ret qst = SBV.quantifiedBool $
            \(SBV.Forall (a :: SBVType integerBitWidth t1)) ->
              r $ addQuantified sb (toDyn a) qst
      return (addBiMapIntermediate (SomeTerm t) (toDyn . ret) nextm, ret)
lowerSinglePrimImpl config t@(ExistsTerm _ (ts :: TypedSymbol 'NonFuncSymbol t1) v) qs m =
  withNonFuncPrim @t1 config $ do
    do
      let (newm, sb@(TypedSymbol sbs)) = attachNextQuantifiedSymbolInfo m ts
      let substedTerm = substTerm ts (symTerm sbs) v
      (nextm, r) <-
        lowerSinglePrimCached
          config
          substedTerm
          (addQuantifiedSymbol sb qs)
          newm
      let ret qst = SBV.quantifiedBool $
            \(SBV.Exists (a :: SBVType integerBitWidth t1)) ->
              r $ addQuantified sb (toDyn a) qst
      return (addBiMapIntermediate (SomeTerm t) (toDyn . ret) nextm, ret)
#else
lowerSinglePrimImpl _ ForallTerm {} _ _ =
  error "Quantifiers are only available when you build with SBV 10.1.0 or later"
lowerSinglePrimImpl _ ExistsTerm {} _ _ =
  error "Quantifiers are only available when you build with SBV 10.1.0 or later"
#endif
lowerSinglePrimImpl config t qs m =
  introSupportedPrimConstraint t $
    withPrim @a config $ do
      (m, r) <- lowerSinglePrimIntermediate config t qs m
      return (addBiMapIntermediate (SomeTerm t) (toDyn . r) m, r)

lowerSinglePrimIntermediate ::
  forall integerBitWidth a m.
  (HasCallStack, SBVFreshMonad m, KnownIsZero integerBitWidth) =>
  GrisetteSMTConfig integerBitWidth ->
  Term a ->
  QuantifiedSymbols ->
  SymBiMap ->
  m (SymBiMap, QuantifiedStack -> SBVType integerBitWidth a)
lowerSinglePrimIntermediate config (NotTerm _ a) qs m = do
  (m1, a') <- lowerSinglePrimCached config a qs m
  return (m1, SBV.sNot . a')
lowerSinglePrimIntermediate config (OrTerm _ a b) qs m = do
  (m1, a') <- lowerSinglePrimCached config a qs m
  (m2, b') <- lowerSinglePrimCached config b qs m1
  return (m2, \qst -> a' qst SBV..|| b' qst)
lowerSinglePrimIntermediate config (AndTerm _ a b) qs m = do
  (m1, a') <- lowerSinglePrimCached config a qs m
  (m2, b') <- lowerSinglePrimCached config b qs m1
  return (m2, \qst -> a' qst SBV..&& b' qst)
lowerSinglePrimIntermediate config (EqTerm _ (a :: Term v) b) qs m = do
  (m1, a') <- lowerSinglePrimCached config a qs m
  (m2, b') <- lowerSinglePrimCached config b qs m1
  return (m2, \qst -> sbvEq @v config (a' qst) (b' qst))
lowerSinglePrimIntermediate config (ITETerm _ c a b) qs m = do
  (m1, c') <- lowerSinglePrimCached config c qs m
  (m2, a') <- lowerSinglePrimCached config a qs m1
  (m3, b') <- lowerSinglePrimCached config b qs m2
  return (m3, \qst -> sbvIte @a config (c' qst) (a' qst) (b' qst))
lowerSinglePrimIntermediate config (AddNumTerm _ a b) qs m = do
  (m1, a') <- lowerSinglePrimCached config a qs m
  (m2, b') <- lowerSinglePrimCached config b qs m1
  return (m2, \qst -> sbvAddNumTerm @a config (a' qst) (b' qst))
lowerSinglePrimIntermediate config (NegNumTerm _ a) qs m = do
  (m1, a') <- lowerSinglePrimCached config a qs m
  return (m1, sbvNegNumTerm @a config . a')
lowerSinglePrimIntermediate config (MulNumTerm _ a b) qs m = do
  (m1, a') <- lowerSinglePrimCached config a qs m
  (m2, b') <- lowerSinglePrimCached config b qs m1
  return (m2, \qst -> sbvMulNumTerm @a config (a' qst) (b' qst))
lowerSinglePrimIntermediate config (AbsNumTerm _ a) qs m = do
  (m1, a') <- lowerSinglePrimCached config a qs m
  return (m1, sbvAbsNumTerm @a config . a')
lowerSinglePrimIntermediate config (SignumNumTerm _ a) qs m = do
  (m1, a') <- lowerSinglePrimCached config a qs m
  return (m1, sbvSignumNumTerm @a config . a')
lowerSinglePrimIntermediate config (LtOrdTerm _ (a :: Term v) b) qs m = do
  (m1, a') <- lowerSinglePrimCached config a qs m
  (m2, b') <- lowerSinglePrimCached config b qs m1
  return (m2, \qst -> sbvLtOrdTerm @v config (a' qst) (b' qst))
lowerSinglePrimIntermediate config (LeOrdTerm _ (a :: Term v) b) qs m = do
  (m1, a') <- lowerSinglePrimCached config a qs m
  (m2, b') <- lowerSinglePrimCached config b qs m1
  return (m2, \qst -> sbvLeOrdTerm @v config (a' qst) (b' qst))
lowerSinglePrimIntermediate config (AndBitsTerm _ a b) qs m = do
  (m1, a') <- lowerSinglePrimCached config a qs m
  (m2, b') <- lowerSinglePrimCached config b qs m1
  return (m2, \qst -> sbvAndBitsTerm @a config (a' qst) (b' qst))
lowerSinglePrimIntermediate config (OrBitsTerm _ a b) qs m = do
  (m1, a') <- lowerSinglePrimCached config a qs m
  (m2, b') <- lowerSinglePrimCached config b qs m1
  return (m2, \qst -> sbvOrBitsTerm @a config (a' qst) (b' qst))
lowerSinglePrimIntermediate config (XorBitsTerm _ a b) qs m = do
  (m1, a') <- lowerSinglePrimCached config a qs m
  (m2, b') <- lowerSinglePrimCached config b qs m1
  return (m2, \qst -> sbvXorBitsTerm @a config (a' qst) (b' qst))
lowerSinglePrimIntermediate config (ComplementBitsTerm _ a) qs m = do
  (m1, a') <- lowerSinglePrimCached config a qs m
  return (m1, sbvComplementBitsTerm @a config . a')
lowerSinglePrimIntermediate config (ShiftLeftTerm _ a b) qs m = do
  (m1, a') <- lowerSinglePrimCached config a qs m
  (m2, b') <- lowerSinglePrimCached config b qs m1
  return (m2, \qst -> sbvShiftLeftTerm @a config (a' qst) (b' qst))
lowerSinglePrimIntermediate config (ShiftRightTerm _ a b) qs m = do
  (m1, a') <- lowerSinglePrimCached config a qs m
  (m2, b') <- lowerSinglePrimCached config b qs m1
  return (m2, \qst -> sbvShiftRightTerm @a config (a' qst) (b' qst))
lowerSinglePrimIntermediate config (RotateLeftTerm _ a b) qs m = do
  (m1, a') <- lowerSinglePrimCached config a qs m
  (m2, b') <- lowerSinglePrimCached config b qs m1
  return (m2, \qst -> sbvRotateLeftTerm @a config (a' qst) (b' qst))
lowerSinglePrimIntermediate config (RotateRightTerm _ a b) qs m = do
  (m1, a') <- lowerSinglePrimCached config a qs m
  (m2, b') <- lowerSinglePrimCached config b qs m1
  return (m2, \qst -> sbvRotateRightTerm @a config (a' qst) (b' qst))
lowerSinglePrimIntermediate config (ApplyTerm _ (f :: Term f) a) qs m = do
  (m1, l1) <- lowerSinglePrimCached config f qs m
  (m2, l2) <- lowerSinglePrimCached config a qs m1
  return (m2, \qst -> sbvApplyTerm @f config (l1 qst) (l2 qst))
lowerSinglePrimIntermediate config (ToSignedTerm _ (a :: Term (u n))) qs m = do
  (m1, a') <- lowerSinglePrimCached config a qs m
  return (m1, sbvToSigned (Proxy @u) (Proxy @n) config . a')
lowerSinglePrimIntermediate config (ToUnsignedTerm _ (a :: Term (s n))) qs m = do
  (m1, a') <- lowerSinglePrimCached config a qs m
  return (m1, sbvToUnsigned (Proxy @s) (Proxy @n) config . a')
lowerSinglePrimIntermediate
  config
  (BVConcatTerm _ (a :: Term (bv l)) (b :: Term (bv r)))
  qs
  m = do
    (m1, a') <- lowerSinglePrimCached config a qs m
    (m2, b') <- lowerSinglePrimCached config b qs m1
    return (m2, \qst -> sbvBVConcatTerm @bv config (Proxy @l) (Proxy @r) (a' qst) (b' qst))
lowerSinglePrimIntermediate
  config
  (BVExtendTerm _ signed (pr :: p r) (a :: Term (bv l)))
  qs
  m = do
    (m1, a') <- lowerSinglePrimCached config a qs m
    return (m1, sbvBVExtendTerm @bv config (Proxy @l) pr signed . a')
lowerSinglePrimIntermediate
  config
  (BVSelectTerm _ (pix :: p ix) (pw :: q w) (a :: Term (bv n)))
  qs
  m = do
    (m1, a') <- lowerSinglePrimCached config a qs m
    return (m1, sbvBVSelectTerm @bv config pix pw (Proxy @n) . a')
lowerSinglePrimIntermediate config (DivIntegralTerm _ a b) qs m = do
  (m1, a') <- lowerSinglePrimCached config a qs m
  (m2, b') <- lowerSinglePrimCached config b qs m1
  return (m2, \qst -> sbvDivIntegralTerm @a config (a' qst) (b' qst))
lowerSinglePrimIntermediate config (ModIntegralTerm _ a b) qs m = do
  (m1, a') <- lowerSinglePrimCached config a qs m
  (m2, b') <- lowerSinglePrimCached config b qs m1
  return (m2, \qst -> sbvModIntegralTerm @a config (a' qst) (b' qst))
lowerSinglePrimIntermediate config (QuotIntegralTerm _ a b) qs m = do
  (m1, a') <- lowerSinglePrimCached config a qs m
  (m2, b') <- lowerSinglePrimCached config b qs m1
  return (m2, \qst -> sbvQuotIntegralTerm @a config (a' qst) (b' qst))
lowerSinglePrimIntermediate config (RemIntegralTerm _ a b) qs m = do
  (m1, a') <- lowerSinglePrimCached config a qs m
  (m2, b') <- lowerSinglePrimCached config b qs m1
  return (m2, \qst -> sbvRemIntegralTerm @a config (a' qst) (b' qst))
lowerSinglePrimIntermediate config (FPTraitTerm _ trait a) qs m = do
  (m, a') <- lowerSinglePrimCached config a qs m
  return (m, sbvFPTraitTerm trait . a')
lowerSinglePrimIntermediate config (FdivTerm _ a b) qs m = do
  (m, a) <- lowerSinglePrimCached config a qs m
  (m, b) <- lowerSinglePrimCached config b qs m
  return (m, \qst -> sbvFdivTerm @a config (a qst) (b qst))
lowerSinglePrimIntermediate config (RecipTerm _ a) qs m = do
  (m, a) <- lowerSinglePrimCached config a qs m
  return (m, sbvRecipTerm @a config . a)
lowerSinglePrimIntermediate config (FloatingUnaryTerm _ op a) qs m = do
  (m, a) <- lowerSinglePrimCached config a qs m
  return (m, sbvFloatingUnaryTerm @a config op . a)
lowerSinglePrimIntermediate config (PowerTerm _ a b) qs m = do
  (m, a) <- lowerSinglePrimCached config a qs m
  (m, b) <- lowerSinglePrimCached config b qs m
  return (m, \qst -> sbvPowerTerm @a config (a qst) (b qst))
lowerSinglePrimIntermediate config (FPUnaryTerm _ op a) qs m = do
  (m, a) <- lowerSinglePrimCached config a qs m
  return (m, sbvFPUnaryTerm op . a)
lowerSinglePrimIntermediate config (FPBinaryTerm _ op a b) qs m = do
  (m, a) <- lowerSinglePrimCached config a qs m
  (m, b) <- lowerSinglePrimCached config b qs m
  return (m, \qst -> sbvFPBinaryTerm op (a qst) (b qst))
lowerSinglePrimIntermediate config (FPRoundingUnaryTerm _ op round a) qs m = do
  (m, round) <- lowerSinglePrimCached config round qs m
  (m, a) <- lowerSinglePrimCached config a qs m
  return (m, \qst -> sbvFPRoundingUnaryTerm op (round qst) (a qst))
lowerSinglePrimIntermediate config (FPRoundingBinaryTerm _ op round a b) qs m = do
  (m, round) <- lowerSinglePrimCached config round qs m
  (m, a) <- lowerSinglePrimCached config a qs m
  (m, b) <- lowerSinglePrimCached config b qs m
  return (m, \qst -> sbvFPRoundingBinaryTerm op (round qst) (a qst) (b qst))
lowerSinglePrimIntermediate config (FPFMATerm _ round a b c) qs m = do
  (m, round) <- lowerSinglePrimCached config round qs m
  (m, a) <- lowerSinglePrimCached config a qs m
  (m, b) <- lowerSinglePrimCached config b qs m
  (m, c) <- lowerSinglePrimCached config c qs m
  return (m, \qst -> sbvFPFMATerm (round qst) (a qst) (b qst) (c qst))
lowerSinglePrimIntermediate _ _ _ _ = undefined

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

-- | Parse an SBV model to a Grisette model.
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
    assocFuncs = (\(s, v) -> (s, ([], v))) <$> assoc
    goSingle :: (String, ([([SBVD.CV], SBVD.CV)], SBVD.CV)) -> PM.Model -> PM.Model
    goSingle (name, cv) m = case findStringToSymbol name mp of
      Just (SomeTypedSymbol (_ :: p r) s) ->
        withSymbolSupported s $
          insertValue s (parseSMTModelResult 0 cv :: r) m
      Nothing ->
        error $
          "BUG: Please send a bug report. The model is not consistent with the "
            <> "list of symbols that have been defined."
