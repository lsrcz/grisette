{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Grisette.Backend.SBV.Data.SMT.Lowering
  ( lowerSinglePrimCached,
    lowerSinglePrim,
    SymBiMap,
    parseModel,
  )
where

import Data.Dynamic (fromDyn, toDyn)
import Data.Proxy (Proxy (Proxy))
import qualified Data.SBV as SBV
import qualified Data.SBV.Dynamic as SBVD
import qualified Data.SBV.Internals as SBVI
import GHC.Stack (HasCallStack)
import {-# SOURCE #-} Grisette.Backend.SBV.Data.SMT.Solving
  ( ApproximationConfig (Approx, NoApprox),
    ExtraConfig (ExtraConfig),
    GrisetteSMTConfig (GrisetteSMTConfig),
  )
import Grisette.Backend.SBV.Data.SMT.SymBiMap
  ( SymBiMap,
    addBiMap,
    addBiMapIntermediate,
    emptySymBiMap,
    findStringToSymbol,
    lookupTerm,
    sizeBiMap,
  )
import Grisette.Core.Data.Class.ModelOps (ModelOps (emptyModel, insertValue))
import Grisette.IR.SymPrim.Data.Prim.Internal.IsZero (KnownIsZero)
import Grisette.IR.SymPrim.Data.Prim.Internal.Term
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
import qualified Grisette.IR.SymPrim.Data.Prim.Model as PM
import Grisette.IR.SymPrim.Data.Prim.SomeTerm (SomeTerm (SomeTerm))

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
    (\case
      (a, (_, Right c)) -> Just (a, c)
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
