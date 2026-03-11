{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
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
import Control.Exception
  ( Exception (displayException),
    SomeException,
    handle,
    throwTo,
  )
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader
  ( MonadReader (ask),
    MonadTrans (lift),
    ReaderT (runReaderT),
    ask,
  )
import Control.Monad.STM (STM)
import Control.Monad.State.Strict
  ( MonadState (get, put),
    StateT,
    evalStateT,
  )
import Data.Dynamic (fromDyn, toDyn)
import qualified Data.HashSet as HS
import Data.IORef (modifyIORef', newIORef, readIORef, writeIORef)
import Data.List.NonEmpty (NonEmpty)
import Data.Proxy (Proxy (Proxy))
import qualified Data.SBV as SBV
import qualified Data.SBV.Control as SBVC
import qualified Data.SBV.Dynamic as SBVD
import qualified Data.SBV.Internals as SBVI
import qualified Data.SBV.Trans as SBVT
import qualified Data.SBV.Trans.Control as SBVTC
import qualified Data.Text as T
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
import Grisette.Internal.Core.Data.MemoUtils (htmemo)
import Grisette.Internal.SymPrim.GeneralFun (substTerm)
import Grisette.Internal.SymPrim.Prim.Model as PM
  ( Model,
  )
import Grisette.Internal.SymPrim.Prim.SomeTerm (SomeTerm (SomeTerm))
import Grisette.Internal.SymPrim.Prim.Term
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
    PEvalFPTerm
      ( sbvFPBinaryTerm,
        sbvFPFMATerm,
        sbvFPRoundingBinaryTerm,
        sbvFPRoundingUnaryTerm,
        sbvFPTraitTerm,
        sbvFPUnaryTerm
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
    SymbolKind (AnyKind),
    Term,
    TypedConstantSymbol,
    TypedSymbol (TypedSymbol),
    someTypedSymbol,
    symTerm,
    pattern AbsNumTerm,
    pattern AddNumTerm,
    pattern AndBitsTerm,
    pattern AndTerm,
    pattern ApplyTerm,
    pattern BVConcatTerm,
    pattern BVExtendTerm,
    pattern BVSelectTerm,
    pattern BitCastOrTerm,
    pattern BitCastTerm,
    pattern ComplementBitsTerm,
    pattern ConTerm,
    pattern DistinctTerm,
    pattern DivIntegralTerm,
    pattern EqTerm,
    pattern ExistsTerm,
    pattern FPBinaryTerm,
    pattern FPFMATerm,
    pattern FPRoundingBinaryTerm,
    pattern FPRoundingUnaryTerm,
    pattern FPTraitTerm,
    pattern FPUnaryTerm,
    pattern FdivTerm,
    pattern FloatingUnaryTerm,
    pattern ForallTerm,
    pattern FromFPOrTerm,
    pattern FromIntegralTerm,
    pattern ITETerm,
    pattern LeOrdTerm,
    pattern LtOrdTerm,
    pattern ModIntegralTerm,
    pattern MulNumTerm,
    pattern NegNumTerm,
    pattern NotTerm,
    pattern OrBitsTerm,
    pattern OrTerm,
    pattern PowerTerm,
    pattern QuotIntegralTerm,
    pattern RecipTerm,
    pattern RemIntegralTerm,
    pattern RotateLeftTerm,
    pattern RotateRightTerm,
    pattern ShiftLeftTerm,
    pattern ShiftRightTerm,
    pattern SignumNumTerm,
    pattern SupportedTerm,
    pattern SymTerm,
    pattern ToFPTerm,
    pattern XorBitsTerm,
    pattern SelectTerm,
    pattern StoreTerm,
    pattern ConstArrayTerm,
  )
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
    (newSymBiMap, lowered, dummyConstraint) <-
      lowerSinglePrimCached formula symBiMap
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
      let handler (e :: SomeException) =
            liftIO $
              atomically $ do
                setTerminated sbvSolverHandleStatus
                writeTChan
                  sbvSolverHandleOutChan
                  (Left (SolvingError $ T.pack $ displayException e))
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

sbvForall,
  sbvExists ::
    forall t.
    (SupportedNonFuncPrim t) =>
    TypedConstantSymbol t ->
    (QuantifiedStack -> SBV.SBool) ->
    QuantifiedStack ->
    SBV.SBool
#if MIN_VERSION_sbv(10,1,0)
sbvForall sb r qst = withNonFuncPrim @t $
  SBV.quantifiedBool $
    \(SBV.Forall (a :: SBVType t)) ->
      r $ addQuantified sb (toDyn a) qst
sbvExists sb r qst = withNonFuncPrim @t $
  SBV.quantifiedBool $
    \(SBV.Exists (a :: SBVType t)) ->
      r $ addQuantified sb (toDyn a) qst
#else
sbvForall =
  error "Quantifiers are only available when you build with SBV 10.1.0 or later"
sbvExists =
  error "Quantifiers are only available when you build with SBV 10.1.0 or later"
#endif

-- | Lower a single primitive term to SBV. With an explicitly provided
-- 'SymBiMap' cache.
lowerSinglePrimCached ::
  forall t m.
  (HasCallStack, SBVFreshMonad m) =>
  Term t ->
  SymBiMap ->
  m (SymBiMap, QuantifiedStack -> SBVType t, SBV.SBool)
lowerSinglePrimCached t' m' = do
  mapState <- liftIO $ newIORef m'
  accumulatedDummyConstraints <- liftIO $ newIORef SBV.sTrue
  -- quantifiedSymbols <- liftIO $ newIORef emptyQuantifiedSymbols
  let goCached ::
        forall x.
        QuantifiedSymbols ->
        Term x ->
        m (QuantifiedStack -> SBVType x)
      goCached qs t@SupportedTerm = do
        mp <- liftIO $ readIORef mapState
        case lookupTerm (SomeTerm t) mp of
          Just x -> return (\qst -> withPrim @x $ fromDyn (x qst) undefined)
          Nothing -> goCachedImpl qs t
      goCachedImpl ::
        forall a.
        (SupportedPrim a) =>
        QuantifiedSymbols ->
        Term a ->
        m (QuantifiedStack -> SBVType a)
      goCachedImpl _ (ConTerm v) =
        return $ const $ conSBVTerm v
      goCachedImpl qs t@(SymTerm ts) = do
        if isQuantifiedSymbol ts qs
          then withPrim @a $ do
            let retDyn qst =
                  case lookupQuantified (someTypedSymbol ts) qst of
                    Just v -> v
                    Nothing ->
                      error "BUG: Symbol not found in the quantified stack"
            liftIO $
              modifyIORef' mapState $
                \m -> addBiMapIntermediate (SomeTerm t) retDyn m
            return $
              \x ->
                fromDyn
                  (retDyn x)
                  (error "BUG: Symbol not found in the quantified stack")
          else withPrim @a $ do
            m <- liftIO $ readIORef mapState
            let name = symSBVName ts (sizeBiMap m)
            g <- symSBVTerm @a name
            liftIO $
              modifyIORef' accumulatedDummyConstraints $
                \c -> c SBV..&& funcDummyConstraint @a g
            liftIO $
              modifyIORef' mapState $
                addBiMap (SomeTerm t) (toDyn g) name (someTypedSymbol ts)
            return $ const g
      goCachedImpl qs t@(ForallTerm (ts :: TypedConstantSymbol t1) v) =
        withNonFuncPrim @t1 $ do
          do
            m <- liftIO $ readIORef mapState
            let (newm, sb) =
                  attachNextQuantifiedSymbolInfo m ts
            liftIO $ writeIORef mapState newm
            let substedTerm = substTerm ts (symTerm sb) HS.empty v
            r <- goCached (addQuantifiedSymbol sb qs) substedTerm
            let ret = sbvForall sb r
            liftIO $
              modifyIORef' mapState $
                addBiMapIntermediate (SomeTerm t) (toDyn . ret)
            return ret
      goCachedImpl qs t@(ExistsTerm (ts :: TypedConstantSymbol t1) v) =
        withNonFuncPrim @t1 $ do
          do
            m <- liftIO $ readIORef mapState
            let (newm, sb) =
                  attachNextQuantifiedSymbolInfo m ts
            liftIO $ writeIORef mapState newm
            let substedTerm = substTerm ts (symTerm sb) HS.empty v
            r <- goCached (addQuantifiedSymbol sb qs) substedTerm
            let ret = sbvExists sb r
            liftIO $
              modifyIORef' mapState $
                addBiMapIntermediate (SomeTerm t) (toDyn . ret)
            return ret
      goCachedImpl qs t =
        withPrim @a $ do
          r <- goCachedIntermediate qs t
          let memoed = htmemo r
              {-# NOINLINE memoed #-}
          liftIO $
            modifyIORef' mapState $
              addBiMapIntermediate (SomeTerm t) (toDyn . memoed)
          return memoed
      goCachedIntermediate ::
        forall a.
        (SupportedPrim a) =>
        QuantifiedSymbols ->
        Term a ->
        m (QuantifiedStack -> SBVType a)
      goCachedIntermediate qs (NotTerm t) = do
        r <- goCached qs t
        return $ \qst -> SBV.sNot (r qst)
      goCachedIntermediate qs (OrTerm a b) = do
        a' <- goCached qs a
        b' <- goCached qs b
        return $ \qst -> a' qst SBV..|| b' qst
      goCachedIntermediate qs (AndTerm a b) = do
        a' <- goCached qs a
        b' <- goCached qs b
        return $ \qst -> a' qst SBV..&& b' qst
      goCachedIntermediate qs (EqTerm (a :: Term v) b) = do
        a' <- goCached qs a
        b' <- goCached qs b
        return $
          \qst -> sbvEq @v (a' qst) (b' qst)
      goCachedIntermediate
        qs
        (DistinctTerm (args :: NonEmpty (Term t0))) = do
          args' <- traverse (goCached qs) args
          return $ \qst -> sbvDistinct @t0 (fmap ($ qst) args')
      goCachedIntermediate qs (ITETerm c a b) = do
        c' <- goCached qs c
        a' <- goCached qs a
        b' <- goCached qs b
        return $ \qst -> sbvIte @a (c' qst) (a' qst) (b' qst)
      goCachedIntermediate qs (AddNumTerm a b) = do
        a' <- goCached qs a
        b' <- goCached qs b
        return $ \qst -> sbvAddNumTerm @a (a' qst) (b' qst)
      goCachedIntermediate qs (NegNumTerm a) = do
        a' <- goCached qs a
        return $ sbvNegNumTerm @a . a'
      goCachedIntermediate qs (MulNumTerm a b) = do
        a' <- goCached qs a
        b' <- goCached qs b
        return $ \qst -> sbvMulNumTerm @a (a' qst) (b' qst)
      goCachedIntermediate qs (AbsNumTerm a) = do
        a' <- goCached qs a
        return $ sbvAbsNumTerm @a . a'
      goCachedIntermediate qs (SignumNumTerm a) = do
        a' <- goCached qs a
        return $ sbvSignumNumTerm @a . a'
      goCachedIntermediate qs (LtOrdTerm (a :: Term v) b) = do
        a' <- goCached qs a
        b' <- goCached qs b
        return $ \qst -> sbvLtOrdTerm @v (a' qst) (b' qst)
      goCachedIntermediate qs (LeOrdTerm (a :: Term v) b) = do
        a' <- goCached qs a
        b' <- goCached qs b
        return $ \qst -> sbvLeOrdTerm @v (a' qst) (b' qst)
      goCachedIntermediate qs (AndBitsTerm a b) = do
        a' <- goCached qs a
        b' <- goCached qs b
        return $ \qst -> sbvAndBitsTerm @a (a' qst) (b' qst)
      goCachedIntermediate qs (OrBitsTerm a b) = do
        a' <- goCached qs a
        b' <- goCached qs b
        return $ \qst -> sbvOrBitsTerm @a (a' qst) (b' qst)
      goCachedIntermediate qs (XorBitsTerm a b) = do
        a' <- goCached qs a
        b' <- goCached qs b
        return $ \qst -> sbvXorBitsTerm @a (a' qst) (b' qst)
      goCachedIntermediate qs (ComplementBitsTerm a) = do
        a' <- goCached qs a
        return $ sbvComplementBitsTerm @a . a'
      goCachedIntermediate qs (ShiftLeftTerm a b) = do
        a' <- goCached qs a
        b' <- goCached qs b
        return $ \qst -> sbvShiftLeftTerm @a (a' qst) (b' qst)
      goCachedIntermediate qs (ShiftRightTerm a b) = do
        a' <- goCached qs a
        b' <- goCached qs b
        return $ \qst -> sbvShiftRightTerm @a (a' qst) (b' qst)
      goCachedIntermediate qs (RotateLeftTerm a b) = do
        a' <- goCached qs a
        b' <- goCached qs b
        return $ \qst -> sbvRotateLeftTerm @a (a' qst) (b' qst)
      goCachedIntermediate qs (RotateRightTerm a b) = do
        a' <- goCached qs a
        b' <- goCached qs b
        return $ \qst -> sbvRotateRightTerm @a (a' qst) (b' qst)
      goCachedIntermediate qs (ApplyTerm (f :: Term f) a) = do
        l1 <- goCached qs f
        l2 <- goCached qs a
        return $ \qst -> sbvApplyTerm @f (l1 qst) (l2 qst)
      goCachedIntermediate qs (BitCastTerm (a :: Term x)) = do
        a' <- goCached qs a
        return $ sbvBitCast @x @a . a'
      goCachedIntermediate
        qs
        (BitCastOrTerm (d :: Term a) (a :: Term x)) = do
          d' <- goCached qs d
          a' <- goCached qs a
          return $ \qst -> sbvBitCastOr @x @a (d' qst) (a' qst)
      goCachedIntermediate
        qs
        (BVConcatTerm (a :: Term (bv l)) (b :: Term (bv r))) =
          do
            a' <- goCached qs a
            b' <- goCached qs b
            return $
              \qst ->
                sbvBVConcatTerm @bv (Proxy @l) (Proxy @r) (a' qst) (b' qst)
      goCachedIntermediate
        qs
        (BVExtendTerm signed (pr :: p r) (a :: Term (bv l))) =
          do
            a' <- goCached qs a
            return $ sbvBVExtendTerm @bv (Proxy @l) pr signed . a'
      goCachedIntermediate
        qs
        (BVSelectTerm (pix :: p ix) (pw :: q w) (a :: Term (bv n))) =
          do
            a' <- goCached qs a
            return $ sbvBVSelectTerm @bv pix pw (Proxy @n) . a'
      goCachedIntermediate qs (DivIntegralTerm a b) = do
        a' <- goCached qs a
        b' <- goCached qs b
        return $ \qst -> sbvDivIntegralTerm @a (a' qst) (b' qst)
      goCachedIntermediate qs (ModIntegralTerm a b) = do
        a' <- goCached qs a
        b' <- goCached qs b
        return $ \qst -> sbvModIntegralTerm @a (a' qst) (b' qst)
      goCachedIntermediate qs (QuotIntegralTerm a b) = do
        a' <- goCached qs a
        b' <- goCached qs b
        return $ \qst -> sbvQuotIntegralTerm @a (a' qst) (b' qst)
      goCachedIntermediate qs (RemIntegralTerm a b) = do
        a' <- goCached qs a
        b' <- goCached qs b
        return $ \qst -> sbvRemIntegralTerm @a (a' qst) (b' qst)
      goCachedIntermediate qs (FPTraitTerm trait (a :: Term (fp eb sb))) = do
        a' <- goCached qs a
        return $ sbvFPTraitTerm @fp @eb @sb trait . a'
      goCachedIntermediate qs (FdivTerm a b) = do
        a <- goCached qs a
        b <- goCached qs b
        return $ \qst -> sbvFdivTerm @a (a qst) (b qst)
      goCachedIntermediate qs (RecipTerm a) = do
        a <- goCached qs a
        return $ sbvRecipTerm @a . a
      goCachedIntermediate qs (FloatingUnaryTerm op a) = do
        a <- goCached qs a
        return $ sbvFloatingUnaryTerm @a op . a
      goCachedIntermediate qs (PowerTerm a b) = do
        a <- goCached qs a
        b <- goCached qs b
        return $ \qst -> sbvPowerTerm @a (a qst) (b qst)
      goCachedIntermediate qs (FPUnaryTerm op (a :: Term (fp eb sb))) = do
        a <- goCached qs a
        return $ sbvFPUnaryTerm @fp @eb @sb op . a
      goCachedIntermediate qs (FPBinaryTerm op (a :: Term (fp eb sb)) b) = do
        a <- goCached qs a
        b <- goCached qs b
        return $ \qst -> sbvFPBinaryTerm @fp @eb @sb op (a qst) (b qst)
      goCachedIntermediate qs (FPRoundingUnaryTerm op round (a :: Term (fp eb sb))) = do
        round <- goCached qs round
        a <- goCached qs a
        return $ \qst -> sbvFPRoundingUnaryTerm @fp @eb @sb op (round qst) (a qst)
      goCachedIntermediate qs (FPRoundingBinaryTerm op round (a :: Term (fp eb sb)) b) = do
        round <- goCached qs round
        a <- goCached qs a
        b <- goCached qs b
        return $ \qst -> sbvFPRoundingBinaryTerm @fp @eb @sb op (round qst) (a qst) (b qst)
      goCachedIntermediate qs (FPFMATerm round (a :: Term (fp eb sb)) b c) = do
        round <- goCached qs round
        a <- goCached qs a
        b <- goCached qs b
        c <- goCached qs c
        return $ \qst -> sbvFPFMATerm @fp @eb @sb (round qst) (a qst) (b qst) (c qst)
      goCachedIntermediate qs (FromIntegralTerm (b :: Term b)) = do
        b <- goCached qs b
        return $ sbvFromIntegralTerm @b @a . b
      goCachedIntermediate qs (FromFPOrTerm d mode arg) = do
        d <- goCached qs d
        mode <- goCached qs mode
        arg <- goCached qs arg
        return $ \qst -> sbvFromFPOrTerm @a (d qst) (mode qst) (arg qst)
      goCachedIntermediate qs (ToFPTerm mode (arg :: Term b) _ _) = do
        mode <- goCached qs mode
        arg <- goCached qs arg
        return $ \qst -> sbvToFPTerm @b (mode qst) (arg qst)
      goCachedIntermediate qs (SelectTerm (arr :: Term arr) key) = withPrim @arr $ do
        arr' <- goCached qs arr
        key' <- goCached qs key
        pure $ \qst -> SBV.readArray (arr' qst) (key' qst)
      goCachedIntermediate qs (StoreTerm arr key val) = withPrim @a $ do
        arr' <- goCached qs arr
        key' <- goCached qs key
        val' <- goCached qs val
        pure $ \qst -> SBV.writeArray (arr' qst) (key' qst) (val' qst)
      goCachedIntermediate qs (ConstArrayTerm _ val) = withPrim @a $ do
        val' <- goCached qs val
        pure $ \qst -> SBV.constArray $ val' qst
      goCachedIntermediate _ ConTerm {} = error "Should not happen"
      goCachedIntermediate _ SymTerm {} = error "Should not happen"
      goCachedIntermediate _ ForallTerm {} = error "Should not happen"
      goCachedIntermediate _ ExistsTerm {} = error "Should not happen"
  r <- goCached emptyQuantifiedSymbols t'
  m <- liftIO $ readIORef mapState
  constraint <- liftIO $ readIORef accumulatedDummyConstraints
  return (m, r, constraint)

-- | Lower a single primitive term to SBV.
lowerSinglePrim ::
  forall a m.
  (HasCallStack, SBVFreshMonad m) =>
  Term a ->
  m (SymBiMap, QuantifiedStack -> SBVType a, SBV.SBool)
lowerSinglePrim t =
  lowerSinglePrimCached t emptySymBiMap

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
      Just (SomeTypedSymbol (s@TypedSymbol {} :: TypedSymbol 'AnyKind r)) ->
        insertValue
          s
          (parseSMTModelResult 0 cv :: r)
          m
      Nothing ->
        error $
          "BUG: Please send a bug report. The model is not consistent with the "
            <> "list of symbols that have been defined. The map is "
            <> show mp
            <> ". The model is "
            <> show model
