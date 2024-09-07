{-# LANGUAGE BangPatterns #-}
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
{-# LANGUAGE Strict #-}
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
    ExtraConfig (..),
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
import Control.Monad.RWS.Strict (RWST (runRWST))
import Control.Monad.Reader
  ( MonadReader (ask),
    MonadTrans (lift),
    ReaderT (runReaderT),
    ask,
    local,
  )
import Control.Monad.STM (STM)
import Control.Monad.State.Strict
  ( MonadState (get, put),
    StateT,
    evalStateT,
    modify,
  )
import Control.Monad.Writer (tell)
import Data.Dynamic (fromDyn, toDyn)
import qualified Data.HashSet as HS
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Proxy (Proxy (Proxy))
import qualified Data.SBV as SBV
import qualified Data.SBV.Control as SBVC
import qualified Data.SBV.Dynamic as SBVD
import qualified Data.SBV.Internals as SBVI
import qualified Data.SBV.Trans as SBVT
import qualified Data.SBV.Trans.Control as SBVTC
import GHC.IO.Exception (ExitCode (ExitSuccess))
import GHC.Stack (HasCallStack)
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
import Grisette.Internal.SymPrim.Prim.Internal.Term
  ( PEvalApplyTerm (sbvApplyTerm),
    PEvalBVTerm (sbvBVConcatTerm, sbvBVExtendTerm, sbvBVSelectTerm),
    PEvalBitCastOrTerm (sbvBitCastOr),
    PEvalBitCastTerm (sbvBitCast),
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
    PEvalFromIntegralTerm (sbvFromIntegralTerm),
    PEvalIEEEFPConvertibleTerm (sbvFromFPOrTerm, sbvToFPTerm),
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
        funcDummyConstraint,
        parseSMTModelResult,
        sbvDistinct,
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
        BitCastOrTerm,
        BitCastTerm,
        ComplementBitsTerm,
        ConTerm,
        DistinctTerm,
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
        FromFPOrTerm,
        FromIntegralTerm,
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
        ToFPTerm,
        XorBitsTerm
      ),
    TypedConstantSymbol,
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

-- | Grisette specific extra configurations for the SBV backend.
newtype ExtraConfig = ExtraConfig
  { -- | Timeout in microseconds for each solver call. CEGIS may call the
    -- solver multiple times and each call has its own timeout.
    timeout :: Maybe Int
  }

-- | Solver configuration for the Grisette SBV backend.
--
-- A Grisette solver configuration consists of a SBV solver configuration and
-- some extra configurations.
--
-- You should start with the predefined configurations.
data GrisetteSMTConfig = GrisetteSMTConfig
  { sbvConfig :: SBV.SMTConfig,
    extraConfig :: ExtraConfig
  }

preciseExtraConfig :: ExtraConfig
preciseExtraConfig = ExtraConfig {timeout = Nothing}

-- | Solver configuration for Boolector. <https://boolector.github.io/>
boolector :: GrisetteSMTConfig
boolector = GrisetteSMTConfig SBV.boolector preciseExtraConfig

-- | Solver configuration for Bitwuzla. <https://bitwuzla.github.io/>
bitwuzla :: GrisetteSMTConfig
bitwuzla = GrisetteSMTConfig SBV.bitwuzla preciseExtraConfig

-- | Solver configuration for CVC4. <https://cvc4.github.io/>
cvc4 :: GrisetteSMTConfig
cvc4 = GrisetteSMTConfig SBV.cvc4 preciseExtraConfig

-- | Solver configuration for CVC5. <https://cvc5.github.io/>
cvc5 :: GrisetteSMTConfig
cvc5 = GrisetteSMTConfig SBV.cvc5 preciseExtraConfig

-- | Solver configuration for Yices. <https://yices.csl.sri.com/>
yices :: GrisetteSMTConfig
yices = GrisetteSMTConfig SBV.yices preciseExtraConfig

-- | Solver configuration for DReal. <http://dreal.github.io/>
dReal :: GrisetteSMTConfig
dReal = GrisetteSMTConfig SBV.dReal preciseExtraConfig

-- | Solver configuration for Z3. <https://github.com/Z3Prover/z3/>
z3 :: GrisetteSMTConfig
z3 = GrisetteSMTConfig SBV.z3 preciseExtraConfig

-- | Solver configuration for MathSAT. <http://mathsat.fbk.eu/>
mathSAT :: GrisetteSMTConfig
mathSAT = GrisetteSMTConfig SBV.mathSAT preciseExtraConfig

-- | Solver configuration for ABC. <http://www.eecs.berkeley.edu/~alanmi/abc/>
abc :: GrisetteSMTConfig
abc = GrisetteSMTConfig SBV.abc preciseExtraConfig

-- | Set the timeout for the solver configuration.
--
-- The timeout is in microseconds (1e-6 seconds). The timeout is applied to each
-- individual solver query.
withTimeout :: Int -> GrisetteSMTConfig -> GrisetteSMTConfig
withTimeout t config =
  config {extraConfig = (extraConfig config) {timeout = Just t}}

-- | Clear the timeout for the solver configuration.
clearTimeout :: GrisetteSMTConfig -> GrisetteSMTConfig
clearTimeout config =
  config {extraConfig = (extraConfig config) {timeout = Nothing}}

sbvCheckSatResult :: SBVC.CheckSatResult -> SolvingFailure
sbvCheckSatResult SBVC.Sat = error "Should not happen"
sbvCheckSatResult (SBVC.DSat _) = error "DSat is currently not supported"
sbvCheckSatResult SBVC.Unsat = Unsat
sbvCheckSatResult SBVC.Unk = Unk

-- | Apply the timeout to the configuration.
applyTimeout ::
  (MonadIO m, SBVTC.MonadQuery m) => GrisetteSMTConfig -> m a -> m a
applyTimeout config q = case timeout (extraConfig config) of
  Nothing -> q
  Just t -> SBVTC.timeout t q

-- | Incremental solver monad transformer with the SBV backend.
type SBVIncrementalT m =
  ReaderT GrisetteSMTConfig (StateT SymBiMap (SBVTC.QueryT m))

-- | Incremental solver monad with the SBV backend.
type SBVIncremental = SBVIncrementalT IO

-- | Run the incremental solver monad with a given configuration.
runSBVIncremental :: GrisetteSMTConfig -> SBVIncremental a -> IO a
runSBVIncremental = runSBVIncrementalT

-- | Run the incremental solver monad transformer with a given configuration.
runSBVIncrementalT ::
  (SBVTC.ExtractIO m) =>
  GrisetteSMTConfig ->
  SBVIncrementalT m a ->
  m a
runSBVIncrementalT config sbvIncrementalT =
  SBVT.runSMTWith (sbvConfig config) $
    SBVTC.query $
      applyTimeout config $
        flip evalStateT emptySymBiMap $
          runReaderT sbvIncrementalT config

instance (MonadIO m) => MonadicSolver (SBVIncrementalT m) where
  monadicSolverAssert (SymBool formula) = do
    symBiMap <- get
    config <- ask
    (newSymBiMap, lowered, dummyConstraint) <-
      lowerSinglePrimCached config formula symBiMap
    lift $ lift $ SBV.constrain dummyConstraint
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

instance ConfigurableSolver GrisetteSMTConfig SBVSolverHandle where
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
              nextFormula <-
                liftIO $ atomically $ readTChan sbvSolverHandleInChan
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
  solverRunCommand f handle@(SBVSolverHandle _ status inChan _) !command = do
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

newtype TermAll = TermAll SBV.SBool

instance Semigroup TermAll where
  TermAll a <> TermAll b = TermAll (a SBV..&& b)

instance Monoid TermAll where
  mempty = TermAll SBV.sTrue

-- | Lower a single primitive term to SBV. With an explicitly provided
-- 'SymBiMap' cache.
lowerSinglePrimCached ::
  forall a m.
  (HasCallStack, SBVFreshMonad m) =>
  GrisetteSMTConfig ->
  Term a ->
  SymBiMap ->
  m (SymBiMap, QuantifiedStack -> SBVType a, SBV.SBool)
lowerSinglePrimCached config t m = do
  -- (_, newm, dummy) <- declareAllUFuncsImpl config t HS.empty m
  (r, finalm, TermAll dummy) <-
    runRWST (lowerSinglePrimCached' config t) emptyQuantifiedSymbols m
  return (finalm, r, dummy)

-- | Lower a single primitive term to SBV.
lowerSinglePrim ::
  forall a m.
  (HasCallStack, SBVFreshMonad m) =>
  GrisetteSMTConfig ->
  Term a ->
  m (SymBiMap, QuantifiedStack -> SBVType a, SBV.SBool)
lowerSinglePrim config t =
  lowerSinglePrimCached config t emptySymBiMap

lowerSinglePrimCached' ::
  forall a m.
  (HasCallStack, SBVFreshMonad m) =>
  GrisetteSMTConfig ->
  Term a ->
  RWST
    QuantifiedSymbols
    TermAll
    SymBiMap
    m
    (QuantifiedStack -> SBVType a)
lowerSinglePrimCached' config t = do
  m <- get
  introSupportedPrimConstraint t $
    case lookupTerm (SomeTerm t) m of
      Just x ->
        return
          ( \qst ->
              withPrim @a $
                fromDyn (x qst) undefined
          )
      Nothing -> do
        lowerSinglePrimImpl config t

lowerSinglePrimImpl ::
  forall a m.
  (HasCallStack, SBVFreshMonad m) =>
  GrisetteSMTConfig ->
  Term a ->
  RWST
    QuantifiedSymbols
    TermAll
    SymBiMap
    m
    (QuantifiedStack -> SBVType a)
lowerSinglePrimImpl _ (ConTerm _ _ _ v) =
  return $ const $ conSBVTerm v
lowerSinglePrimImpl _ t@(SymTerm _ _ _ ts) = do
  qs <- ask
  if isQuantifiedSymbol ts qs
    then withPrim @a $ do
      let retDyn qst =
            case lookupQuantified (someTypedSymbol ts) qst of
              Just v -> v
              Nothing -> error "BUG: Symbol not found in the quantified stack"
      modify $ \m -> addBiMapIntermediate (SomeTerm t) retDyn m
      return $
        \x ->
          fromDyn
            (retDyn x)
            (error "BUG: Symbol not found in the quantified stack")
    else withPrim @a $ do
      m <- get
      let name = symSBVName ts (sizeBiMap m)
      g <- symSBVTerm @a name
      tell $ TermAll $ funcDummyConstraint @a g
      put $ addBiMap (SomeTerm t) (toDyn g) name (someTypedSymbol ts) m
      return $ const g
#if MIN_VERSION_sbv(10,1,0)
lowerSinglePrimImpl config t@(ForallTerm _ _ _ (ts :: TypedConstantSymbol t1) v) =
  withNonFuncPrim @t1 $ do
    do
      m <- get
      let (newm, sb@(TypedSymbol sbs)) = attachNextQuantifiedSymbolInfo m ts
      put newm
      let substedTerm = substTerm ts (symTerm sbs) HS.empty v
      r <-
        local (addQuantifiedSymbol sb) $
          lowerSinglePrimCached'
            config
            substedTerm
      let ret qst = SBV.quantifiedBool $
            \(SBV.Forall (a :: SBVType t1)) ->
              r $ addQuantified sb (toDyn a) qst
      modify $ addBiMapIntermediate (SomeTerm t) (toDyn . ret)
      return ret
lowerSinglePrimImpl config t@(ExistsTerm _ _ _ (ts :: TypedConstantSymbol t1) v) =
  withNonFuncPrim @t1 $ do
    do
      m <- get
      let (newm, sb@(TypedSymbol sbs)) = attachNextQuantifiedSymbolInfo m ts
      put newm
      let substedTerm = substTerm ts (symTerm sbs) HS.empty v
      r <-
        local (addQuantifiedSymbol sb) $
          lowerSinglePrimCached'
            config
            substedTerm
      let ret qst = SBV.quantifiedBool $
            \(SBV.Exists (a :: SBVType t1)) ->
              r $ addQuantified sb (toDyn a) qst
      modify $ addBiMapIntermediate (SomeTerm t) (toDyn . ret)
      return ret
#else
lowerSinglePrimImpl _ ForallTerm {} =
  error "Quantifiers are only available when you build with SBV 10.1.0 or later"
lowerSinglePrimImpl _ ExistsTerm {} =
  error "Quantifiers are only available when you build with SBV 10.1.0 or later"
#endif
lowerSinglePrimImpl config t =
  introSupportedPrimConstraint t $
    withPrim @a $ do
      r <- lowerSinglePrimIntermediate config t
      modify $ addBiMapIntermediate (SomeTerm t) (toDyn . r)
      return r

lowerSinglePrimIntermediate ::
  forall a m.
  (HasCallStack, SBVFreshMonad m) =>
  GrisetteSMTConfig ->
  Term a ->
  RWST
    QuantifiedSymbols
    TermAll
    SymBiMap
    m
    (QuantifiedStack -> SBVType a)
lowerSinglePrimIntermediate config (NotTerm _ _ _ a) = do
  a' <- lowerSinglePrimCached' config a
  return $ SBV.sNot . a'
lowerSinglePrimIntermediate config (OrTerm _ _ _ a b) = do
  a' <- lowerSinglePrimCached' config a
  b' <- lowerSinglePrimCached' config b
  return $ \qst -> a' qst SBV..|| b' qst
lowerSinglePrimIntermediate config (AndTerm _ _ _ a b) = do
  a' <- lowerSinglePrimCached' config a
  b' <- lowerSinglePrimCached' config b
  return $ \qst -> a' qst SBV..&& b' qst
lowerSinglePrimIntermediate config (EqTerm _ _ _ (a :: Term v) b) = do
  a' <- lowerSinglePrimCached' config a
  b' <- lowerSinglePrimCached' config b
  return $ introSupportedPrimConstraint a $ \qst -> sbvEq @v (a' qst) (b' qst)
lowerSinglePrimIntermediate config (DistinctTerm _ _ _ (args@(arg1 :| _) :: NonEmpty (Term t))) = do
  args' <- traverse (lowerSinglePrimCached' config) args
  return $ introSupportedPrimConstraint arg1 $ \qst -> sbvDistinct @t (fmap ($ qst) args')
lowerSinglePrimIntermediate config (ITETerm _ _ _ c a b) = do
  c' <- lowerSinglePrimCached' config c
  a' <- lowerSinglePrimCached' config a
  b' <- lowerSinglePrimCached' config b
  return $ \qst -> sbvIte @a (c' qst) (a' qst) (b' qst)
lowerSinglePrimIntermediate config (AddNumTerm _ _ _ a b) = do
  a' <- lowerSinglePrimCached' config a
  b' <- lowerSinglePrimCached' config b
  return $ \qst -> sbvAddNumTerm @a (a' qst) (b' qst)
lowerSinglePrimIntermediate config (NegNumTerm _ _ _ a) = do
  a' <- lowerSinglePrimCached' config a
  return $ sbvNegNumTerm @a . a'
lowerSinglePrimIntermediate config (MulNumTerm _ _ _ a b) = do
  a' <- lowerSinglePrimCached' config a
  b' <- lowerSinglePrimCached' config b
  return $ \qst -> sbvMulNumTerm @a (a' qst) (b' qst)
lowerSinglePrimIntermediate config (AbsNumTerm _ _ _ a) = do
  a' <- lowerSinglePrimCached' config a
  return $ sbvAbsNumTerm @a . a'
lowerSinglePrimIntermediate config (SignumNumTerm _ _ _ a) = do
  a' <- lowerSinglePrimCached' config a
  return $ sbvSignumNumTerm @a . a'
lowerSinglePrimIntermediate config (LtOrdTerm _ _ _ (a :: Term v) b) = do
  a' <- lowerSinglePrimCached' config a
  b' <- lowerSinglePrimCached' config b
  return $ \qst -> sbvLtOrdTerm @v (a' qst) (b' qst)
lowerSinglePrimIntermediate config (LeOrdTerm _ _ _ (a :: Term v) b) = do
  a' <- lowerSinglePrimCached' config a
  b' <- lowerSinglePrimCached' config b
  return $ \qst -> sbvLeOrdTerm @v (a' qst) (b' qst)
lowerSinglePrimIntermediate config (AndBitsTerm _ _ _ a b) = do
  a' <- lowerSinglePrimCached' config a
  b' <- lowerSinglePrimCached' config b
  return $ \qst -> sbvAndBitsTerm @a (a' qst) (b' qst)
lowerSinglePrimIntermediate config (OrBitsTerm _ _ _ a b) = do
  a' <- lowerSinglePrimCached' config a
  b' <- lowerSinglePrimCached' config b
  return $ \qst -> sbvOrBitsTerm @a (a' qst) (b' qst)
lowerSinglePrimIntermediate config (XorBitsTerm _ _ _ a b) = do
  a' <- lowerSinglePrimCached' config a
  b' <- lowerSinglePrimCached' config b
  return $ \qst -> sbvXorBitsTerm @a (a' qst) (b' qst)
lowerSinglePrimIntermediate config (ComplementBitsTerm _ _ _ a) = do
  a' <- lowerSinglePrimCached' config a
  return $ sbvComplementBitsTerm @a . a'
lowerSinglePrimIntermediate config (ShiftLeftTerm _ _ _ a b) = do
  a' <- lowerSinglePrimCached' config a
  b' <- lowerSinglePrimCached' config b
  return $ \qst -> sbvShiftLeftTerm @a (a' qst) (b' qst)
lowerSinglePrimIntermediate config (ShiftRightTerm _ _ _ a b) = do
  a' <- lowerSinglePrimCached' config a
  b' <- lowerSinglePrimCached' config b
  return $ \qst -> sbvShiftRightTerm @a (a' qst) (b' qst)
lowerSinglePrimIntermediate config (RotateLeftTerm _ _ _ a b) = do
  a' <- lowerSinglePrimCached' config a
  b' <- lowerSinglePrimCached' config b
  return $ \qst -> sbvRotateLeftTerm @a (a' qst) (b' qst)
lowerSinglePrimIntermediate config (RotateRightTerm _ _ _ a b) = do
  a' <- lowerSinglePrimCached' config a
  b' <- lowerSinglePrimCached' config b
  return $ \qst -> sbvRotateRightTerm @a (a' qst) (b' qst)
lowerSinglePrimIntermediate config (ApplyTerm _ _ _ (f :: Term f) a) = do
  l1 <- lowerSinglePrimCached' config f
  l2 <- lowerSinglePrimCached' config a
  return $ \qst -> sbvApplyTerm @f (l1 qst) (l2 qst)
lowerSinglePrimIntermediate config (BitCastTerm _ _ _ (a :: Term x)) = do
  a' <- lowerSinglePrimCached' config a
  return $ sbvBitCast @x @a . a'
lowerSinglePrimIntermediate
  config
  (BitCastOrTerm _ _ _ (d :: Term a) (a :: Term x)) = do
    d' <- lowerSinglePrimCached' config d
    a' <- lowerSinglePrimCached' config a
    return $ \qst -> sbvBitCastOr @x @a (d' qst) (a' qst)
lowerSinglePrimIntermediate
  config
  (BVConcatTerm _ _ _ (a :: Term (bv l)) (b :: Term (bv r))) =
    do
      a' <- lowerSinglePrimCached' config a
      b' <- lowerSinglePrimCached' config b
      return $ \qst -> sbvBVConcatTerm @bv (Proxy @l) (Proxy @r) (a' qst) (b' qst)
lowerSinglePrimIntermediate
  config
  (BVExtendTerm _ _ _ signed (pr :: p r) (a :: Term (bv l))) =
    do
      a' <- lowerSinglePrimCached' config a
      return $ sbvBVExtendTerm @bv (Proxy @l) pr signed . a'
lowerSinglePrimIntermediate
  config
  (BVSelectTerm _ _ _ (pix :: p ix) (pw :: q w) (a :: Term (bv n))) =
    do
      a' <- lowerSinglePrimCached' config a
      return $ sbvBVSelectTerm @bv pix pw (Proxy @n) . a'
lowerSinglePrimIntermediate config (DivIntegralTerm _ _ _ a b) = do
  a' <- lowerSinglePrimCached' config a
  b' <- lowerSinglePrimCached' config b
  return $ \qst -> sbvDivIntegralTerm @a (a' qst) (b' qst)
lowerSinglePrimIntermediate config (ModIntegralTerm _ _ _ a b) = do
  a' <- lowerSinglePrimCached' config a
  b' <- lowerSinglePrimCached' config b
  return $ \qst -> sbvModIntegralTerm @a (a' qst) (b' qst)
lowerSinglePrimIntermediate config (QuotIntegralTerm _ _ _ a b) = do
  a' <- lowerSinglePrimCached' config a
  b' <- lowerSinglePrimCached' config b
  return $ \qst -> sbvQuotIntegralTerm @a (a' qst) (b' qst)
lowerSinglePrimIntermediate config (RemIntegralTerm _ _ _ a b) = do
  a' <- lowerSinglePrimCached' config a
  b' <- lowerSinglePrimCached' config b
  return $ \qst -> sbvRemIntegralTerm @a (a' qst) (b' qst)
lowerSinglePrimIntermediate config (FPTraitTerm _ _ _ trait a) = do
  a' <- lowerSinglePrimCached' config a
  return $ sbvFPTraitTerm trait . a'
lowerSinglePrimIntermediate config (FdivTerm _ _ _ a b) = do
  a <- lowerSinglePrimCached' config a
  b <- lowerSinglePrimCached' config b
  return $ \qst -> sbvFdivTerm @a (a qst) (b qst)
lowerSinglePrimIntermediate config (RecipTerm _ _ _ a) = do
  a <- lowerSinglePrimCached' config a
  return $ sbvRecipTerm @a . a
lowerSinglePrimIntermediate config (FloatingUnaryTerm _ _ _ op a) = do
  a <- lowerSinglePrimCached' config a
  return $ sbvFloatingUnaryTerm @a op . a
lowerSinglePrimIntermediate config (PowerTerm _ _ _ a b) = do
  a <- lowerSinglePrimCached' config a
  b <- lowerSinglePrimCached' config b
  return $ \qst -> sbvPowerTerm @a (a qst) (b qst)
lowerSinglePrimIntermediate config (FPUnaryTerm _ _ _ op a) = do
  a <- lowerSinglePrimCached' config a
  return $ sbvFPUnaryTerm op . a
lowerSinglePrimIntermediate config (FPBinaryTerm _ _ _ op a b) = do
  a <- lowerSinglePrimCached' config a
  b <- lowerSinglePrimCached' config b
  return $ \qst -> sbvFPBinaryTerm op (a qst) (b qst)
lowerSinglePrimIntermediate config (FPRoundingUnaryTerm _ _ _ op round a) = do
  round <- lowerSinglePrimCached' config round
  a <- lowerSinglePrimCached' config a
  return $ \qst -> sbvFPRoundingUnaryTerm op (round qst) (a qst)
lowerSinglePrimIntermediate config (FPRoundingBinaryTerm _ _ _ op round a b) = do
  round <- lowerSinglePrimCached' config round
  a <- lowerSinglePrimCached' config a
  b <- lowerSinglePrimCached' config b
  return $ \qst -> sbvFPRoundingBinaryTerm op (round qst) (a qst) (b qst)
lowerSinglePrimIntermediate config (FPFMATerm _ _ _ round a b c) = do
  round <- lowerSinglePrimCached' config round
  a <- lowerSinglePrimCached' config a
  b <- lowerSinglePrimCached' config b
  c <- lowerSinglePrimCached' config c
  return $ \qst -> sbvFPFMATerm (round qst) (a qst) (b qst) (c qst)
lowerSinglePrimIntermediate config (FromIntegralTerm _ _ _ (b :: Term b)) = do
  b <- lowerSinglePrimCached' config b
  return $ sbvFromIntegralTerm @b @a . b
lowerSinglePrimIntermediate config (FromFPOrTerm _ _ _ d mode arg) = do
  d <- lowerSinglePrimCached' config d
  mode <- lowerSinglePrimCached' config mode
  arg <- lowerSinglePrimCached' config arg
  return $ \qst -> sbvFromFPOrTerm @a (d qst) (mode qst) (arg qst)
lowerSinglePrimIntermediate config (ToFPTerm _ _ _ mode (arg :: Term b) _ _) = do
  mode <- lowerSinglePrimCached' config mode
  arg <- lowerSinglePrimCached' config arg
  return $ \qst -> sbvToFPTerm @b (mode qst) (arg qst)
lowerSinglePrimIntermediate _ ConTerm {} = error "Should not happen"
lowerSinglePrimIntermediate _ SymTerm {} = error "Should not happen"
lowerSinglePrimIntermediate _ ForallTerm {} = error "Should not happen"
lowerSinglePrimIntermediate _ ExistsTerm {} = error "Should not happen"

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
  GrisetteSMTConfig ->
  SBVI.SMTModel ->
  SymBiMap ->
  PM.Model
parseModel _ model@(SBVI.SMTModel _ _ assoc origFuncs) mp =
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
            <> "list of symbols that have been defined. The map is "
            <> show mp
            <> ". The model is "
            <> show model
