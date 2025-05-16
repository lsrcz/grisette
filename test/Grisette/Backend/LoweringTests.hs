{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Grisette.Backend.LoweringTests (loweringTests) where

import Control.Monad.Trans (MonadIO (liftIO), MonadTrans (lift))
import Data.Bits
  ( Bits (complement, xor, (.&.), (.|.)),
  )
import Data.Dynamic (Typeable, fromDynamic)
import Data.Either (isRight)
import qualified Data.HashMap.Strict as M
import Data.Proxy (Proxy (Proxy))
import qualified Data.SBV as SBV
import qualified Data.SBV.Control as SBV
import qualified Data.Text as T
import GHC.Stack (HasCallStack)
import Grisette
  ( AsKey (AsKey),
    EvalSym (evalSym),
    FP,
    FPRoundingMode,
    Function ((#)),
    IntN,
    LogicalOp ((.&&)),
    Solvable (con),
    SymEq ((.==)),
    SymInteger,
    SymRep (SymType),
    WordN,
    solve,
    type (-~>),
    type (=~>),
  )
import Grisette.Internal.Backend.QuantifiedStack
  ( emptyQuantifiedStack,
  )
import Grisette.Internal.Backend.Solving
  ( GrisetteSMTConfig (sbvConfig),
    lowerSinglePrim,
    lowerSinglePrimCached,
    z3,
  )
import Grisette.Internal.Backend.SymBiMap
  ( SymBiMap (biMapToSBV),
  )
import Grisette.Internal.Core.Data.Class.AsKey (KeyEq)
import Grisette.Internal.SymPrim.AlgReal (AlgReal)
import Grisette.Internal.SymPrim.FP (FP32)
import Grisette.Internal.SymPrim.Prim.SomeTerm
  ( SomeTerm (SomeTerm),
  )
import Grisette.Internal.SymPrim.Prim.Term
  ( FPTrait
      ( FPIsInfinite,
        FPIsNaN,
        FPIsNegative,
        FPIsNegativeInfinite,
        FPIsNegativeZero,
        FPIsNormal,
        FPIsPoint,
        FPIsPositive,
        FPIsPositiveInfinite,
        FPIsPositiveZero,
        FPIsSubnormal,
        FPIsZero
      ),
    FloatingUnaryOp
      ( FloatingAcos,
        FloatingAsin,
        FloatingAtan,
        FloatingCos,
        FloatingCosh,
        FloatingSin,
        FloatingSinh,
        FloatingTan,
        FloatingTanh
      ),
    SBVRep (SBVType),
    SupportedPrim,
    Term,
    TypedConstantSymbol,
    absNumTerm,
    addNumTerm,
    andBitsTerm,
    andTerm,
    bitCastOrTerm,
    bitCastTerm,
    bvConcatTerm,
    bvSelectTerm,
    bvsignExtendTerm,
    bvzeroExtendTerm,
    complementBitsTerm,
    conTerm,
    divIntegralTerm,
    eqTerm,
    existsTerm,
    fdivTerm,
    floatingUnaryTerm,
    forallTerm,
    fpTraitTerm,
    iteTerm,
    leOrdTerm,
    ltOrdTerm,
    modIntegralTerm,
    mulNumTerm,
    negNumTerm,
    notTerm,
    orBitsTerm,
    orTerm,
    pevalAndTerm,
    pevalFPTraitTerm,
    pevalNotTerm,
    quotIntegralTerm,
    recipTerm,
    remIntegralTerm,
    rotateLeftTerm,
    rotateRightTerm,
    shiftLeftTerm,
    shiftRightTerm,
    signumNumTerm,
    ssymTerm,
    xorBitsTerm,
  )
import Test.Framework (Test, TestOptions' (topt_timeout), plusTestOptions, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.HUnit (Assertion, assertBool, assertFailure, (@?=))
import Test.QuickCheck (Arbitrary, ioProperty)
import Type.Reflection (typeRep)

testUnaryOpLowering ::
  forall a b as.
  ( HasCallStack,
    SupportedPrim a,
    SBV.EqSymbolic (SBVType b),
    Typeable (SBVType a),
    SBV.SymVal as,
    SBVType a ~ SBV.SBV as,
    Show as
  ) =>
  GrisetteSMTConfig ->
  (Term a -> Term b) ->
  String ->
  (SBVType a -> SBVType b) ->
  Assertion
testUnaryOpLowering = testUnaryOpLowering' Nothing

testUnaryOpLowering' ::
  forall a b as.
  ( HasCallStack,
    SupportedPrim a,
    SBV.EqSymbolic (SBVType b),
    Typeable (SBVType a),
    SBV.SymVal as,
    SBVType a ~ SBV.SBV as,
    Show as
  ) =>
  (Maybe (SBVType a -> SBVType Bool)) ->
  GrisetteSMTConfig ->
  (Term a -> Term b) ->
  String ->
  (SBVType a -> SBVType b) ->
  Assertion
testUnaryOpLowering' precond config f name sbvfun = do
  let a :: Term a = ssymTerm "a"
  let fa :: Term b = f a
  SBV.runSMTWith (sbvConfig config) $ do
    (m, lt, _) <- lowerSinglePrim fa
    let sbva :: Maybe (SBVType a) =
          M.lookup (SomeTerm a) (biMapToSBV m)
            >>= \f -> fromDynamic (f emptyQuantifiedStack)
    case sbva of
      Nothing -> lift $ assertFailure "Failed to extract the term"
      Just sbvav -> SBV.query $ do
        SBV.constrain $ lt emptyQuantifiedStack SBV..== sbvfun sbvav
        satres <- SBV.checkSat
        case satres of
          SBV.Sat -> return ()
          _ -> lift $ assertFailure $ "Lowering for " ++ name ++ " generated unsolvable formula"
  SBV.runSMTWith (sbvConfig config) $ do
    (m, lt, _) <- lowerSinglePrim fa
    let sbvv :: Maybe (SBVType a) =
          M.lookup (SomeTerm a) (biMapToSBV m)
            >>= \f -> fromDynamic (f emptyQuantifiedStack)
    case sbvv of
      Nothing -> lift $ assertFailure "Failed to extract the term"
      Just sbvvv -> SBV.query $ do
        case precond of
          Just p -> SBV.constrain $ p sbvvv
          Nothing -> return ()
        SBV.constrain $ lt emptyQuantifiedStack SBV../= sbvfun sbvvv
        r <- SBV.checkSat
        case r of
          SBV.Sat -> do
            counterExample <- SBV.getValue sbvvv
            lift $ assertFailure $ "Translation counter example found: " ++ show counterExample
          SBV.Unsat -> return ()
          _ -> lift $ assertFailure $ "Lowering for " ++ name ++ " generated unknown formula"

testBinaryOpLowering ::
  forall a b c as bs.
  ( HasCallStack,
    SupportedPrim a,
    SupportedPrim b,
    SBV.EqSymbolic (SBVType c),
    Typeable (SBVType a),
    Typeable (SBVType b),
    SBV.SymVal as,
    SBV.SymVal bs,
    Show as,
    Show bs,
    SBVType a ~ SBV.SBV as,
    SBVType b ~ SBV.SBV bs
  ) =>
  GrisetteSMTConfig ->
  (Term a -> Term b -> Term c) ->
  String ->
  (SBVType a -> SBVType b -> SBVType c) ->
  Assertion
testBinaryOpLowering config f name sbvfun = do
  let a :: Term a = ssymTerm "a"
  let b :: Term b = ssymTerm "b"
  let fab :: Term c = f a b
  SBV.runSMTWith (sbvConfig config) $ do
    (m, lt, _) <- lowerSinglePrim fab
    let sbva :: Maybe (SBVType a) =
          M.lookup (SomeTerm a) (biMapToSBV m)
            >>= \f -> fromDynamic (f emptyQuantifiedStack)
    let sbvb :: Maybe (SBVType b) =
          M.lookup (SomeTerm b) (biMapToSBV m)
            >>= \f -> fromDynamic (f emptyQuantifiedStack)
    case (sbva, sbvb) of
      (Just sbvav, Just sbvbv) -> SBV.query $ do
        SBV.constrain $ lt emptyQuantifiedStack SBV..== sbvfun sbvav sbvbv
        satres <- SBV.checkSat
        case satres of
          SBV.Sat -> return ()
          _ -> lift $ assertFailure $ "Lowering for " ++ name ++ " generated unsolvable formula"
      _ -> lift $ assertFailure "Failed to extract the term"
  SBV.runSMTWith (sbvConfig config) $ do
    (m, lt, _) <- lowerSinglePrim fab
    let sbva :: Maybe (SBVType a) =
          M.lookup (SomeTerm a) (biMapToSBV m)
            >>= \f -> fromDynamic (f emptyQuantifiedStack)
    let sbvb :: Maybe (SBVType b) =
          M.lookup (SomeTerm b) (biMapToSBV m)
            >>= \f -> fromDynamic (f emptyQuantifiedStack)
    case (sbva, sbvb) of
      (Just sbvav, Just sbvbv) -> SBV.query $ do
        SBV.constrain $ lt emptyQuantifiedStack SBV../= sbvfun sbvav sbvbv
        r <- SBV.checkSat
        case r of
          SBV.Sat -> do
            counterExampleA <- SBV.getValue sbvav
            counterExampleB <- SBV.getValue sbvbv
            lift $ assertFailure $ "Translation counter example found: " ++ show (counterExampleA, counterExampleB)
          SBV.Unsat -> return ()
          _ -> lift $ assertFailure $ "Lowering for " ++ name ++ " generated unknown formula"
      _ -> lift $ assertFailure "Failed to extract the term"

testTernaryOpLowering ::
  forall a b c d as bs cs.
  ( HasCallStack,
    SupportedPrim a,
    SupportedPrim b,
    SupportedPrim c,
    SBV.EqSymbolic (SBVType d),
    Typeable (SBVType a),
    Typeable (SBVType b),
    Typeable (SBVType c),
    SBV.SymVal as,
    SBV.SymVal bs,
    SBV.SymVal cs,
    Show as,
    Show bs,
    Show cs,
    SBVType a ~ SBV.SBV as,
    SBVType b ~ SBV.SBV bs,
    SBVType c ~ SBV.SBV cs
  ) =>
  GrisetteSMTConfig ->
  (Term a -> Term b -> Term c -> Term Bool) ->
  (Term a -> Term b -> Term c -> Term d) ->
  T.Text ->
  (SBVType a -> SBVType b -> SBVType c -> SBVType d) ->
  Assertion
testTernaryOpLowering config precond f name sbvfun = do
  let a :: Term a = ssymTerm "a"
  let b :: Term b = ssymTerm "b"
  let c :: Term c = ssymTerm "c"
  let fabc :: Term d = f a b c
  SBV.runSMTWith (sbvConfig config) $ do
    (m, lt, _) <- lowerSinglePrim fabc
    let sbva :: Maybe (SBVType a) =
          M.lookup (SomeTerm a) (biMapToSBV m)
            >>= \f -> fromDynamic (f emptyQuantifiedStack)
    let sbvb :: Maybe (SBVType b) =
          M.lookup (SomeTerm b) (biMapToSBV m)
            >>= \f -> fromDynamic (f emptyQuantifiedStack)
    let sbvc :: Maybe (SBVType c) =
          M.lookup (SomeTerm c) (biMapToSBV m)
            >>= \f -> fromDynamic (f emptyQuantifiedStack)
    case (sbva, sbvb, sbvc) of
      (Just sbvav, Just sbvbv, Just sbvcv) -> SBV.query $ do
        SBV.constrain $ lt emptyQuantifiedStack SBV..== sbvfun sbvav sbvbv sbvcv
        satres <- SBV.checkSat
        case satres of
          SBV.Sat -> return ()
          _ -> lift $ assertFailure $ T.unpack $ "Lowering for " <> name <> " generated unsolvable formula"
      _ -> lift $ assertFailure "Failed to extract the term"
  SBV.runSMTWith (sbvConfig config) $ do
    (m, lt, _) <- lowerSinglePrim fabc
    (m2, p, _) <- lowerSinglePrimCached (precond a b c) m
    let sbva :: Maybe (SBVType a) =
          M.lookup (SomeTerm a) (biMapToSBV m2)
            >>= \f -> fromDynamic (f emptyQuantifiedStack)
    let sbvb :: Maybe (SBVType b) =
          M.lookup (SomeTerm b) (biMapToSBV m2)
            >>= \f -> fromDynamic (f emptyQuantifiedStack)
    let sbvc :: Maybe (SBVType c) =
          M.lookup (SomeTerm c) (biMapToSBV m2)
            >>= \f -> fromDynamic (f emptyQuantifiedStack)
    case (sbva, sbvb, sbvc) of
      (Just sbvav, Just sbvbv, Just sbvcv) -> SBV.query $ do
        SBV.constrain $
          (lt emptyQuantifiedStack SBV../= sbvfun sbvav sbvbv sbvcv)
            SBV..&& p emptyQuantifiedStack
        r <- SBV.checkSat
        case r of
          SBV.Sat -> do
            counterExampleA <- SBV.getValue sbvav
            counterExampleB <- SBV.getValue sbvbv
            counterExampleC <- SBV.getValue sbvcv
            lift $
              assertFailure $
                "Translation counter example found: "
                  ++ show (counterExampleA, counterExampleB, counterExampleC)
          SBV.Unsat -> return ()
          _ -> lift $ assertFailure $ T.unpack $ "Lowering for " <> name <> " generated unknown formula"
      _ -> lift $ assertFailure "Failed to extract the term"

modelParseTestBody ::
  forall t.
  ( Solvable t (SymType t),
    SymEq (SymType t),
    EvalSym (SymType t),
    KeyEq (SymType t),
    Show (SymType t)
  ) =>
  t ->
  Assertion
modelParseTestBody v = do
  let a = "a" :: SymType t
  r <- solve z3 $ a .== con v
  case r of
    Left err -> assertFailure $ "Failed to solve: " ++ show err
    Right m -> AsKey (evalSym False m a) @?= AsKey (con v)

testModelParse ::
  forall t.
  ( Show t,
    Arbitrary t,
    Solvable t (SymType t),
    SymEq (SymType t),
    EvalSym (SymType t),
    Show (SymType t),
    Typeable t,
    KeyEq (SymType t)
  ) =>
  Test
testModelParse = testProperty ("Model parse(" ++ show (typeRep @t) ++ ")") $
  \(v :: t) -> ioProperty $ modelParseTestBody v

loweringTests :: Test
loweringTests =
  let unboundedConfig = z3 {sbvConfig = SBV.z3 {SBV.solverSetOptions = [SBV.SetLogic SBV.Logic_ALL]}}
   in testGroup
        "Lowering"
        [ plusTestOptions (mempty {topt_timeout = Just (Just 1000000)}) $
            testCase "proper memo" $ do
              let pair = ("a" :: SymInteger, "b" :: SymInteger)
              let iter (x, y) = (y, x + y)
              let r = iterate iter pair !! 100
              m <- solve z3 $ snd r .== 0
              assertBool "should success" $ isRight m,
          testGroup
            "Bool Lowering"
            [ testModelParse @Bool,
              testCase "Not" $ do
                testUnaryOpLowering @Bool @Bool unboundedConfig notTerm "not" SBV.sNot,
              testCase "And" $ do
                testBinaryOpLowering @Bool @Bool @Bool unboundedConfig andTerm "and" (SBV..&&)
                testBinaryOpLowering @Bool @Bool @Bool
                  unboundedConfig
                  andTerm
                  "and"
                  (\x y -> SBV.sNot (x SBV..<+> y) SBV..&& (x SBV..|| y)),
              testCase "Or" $ do
                testBinaryOpLowering @Bool @Bool @Bool unboundedConfig orTerm "or" (SBV..||)
                testBinaryOpLowering @Bool @Bool @Bool
                  unboundedConfig
                  orTerm
                  "or"
                  (\x y -> (x SBV..<+> y) SBV..|| (x SBV..&& y)),
              testCase "Eqv" $ do
                testBinaryOpLowering @Bool @Bool @Bool unboundedConfig eqTerm "eqv" (SBV..==)
                testBinaryOpLowering @Bool @Bool @Bool
                  unboundedConfig
                  eqTerm
                  "eqv"
                  (\x y -> SBV.sNot (x SBV..<+> y)),
              testCase "ITE" $ do
                let truePrecond _ _ _ = conTerm True
                testTernaryOpLowering @Bool @Bool @Bool @Bool
                  unboundedConfig
                  truePrecond
                  iteTerm
                  "ite"
                  SBV.ite
                testTernaryOpLowering @Bool @Bool @Bool @Bool
                  unboundedConfig
                  truePrecond
                  iteTerm
                  "ite"
                  (\c x y -> (c SBV..=> x) SBV..&& (SBV.sNot c SBV..=> y))
            ],
          testGroup
            "Integer Lowering"
            [ testModelParse @Integer,
              testCase "Add" $ do
                testBinaryOpLowering @Integer @Integer @Integer unboundedConfig addNumTerm "(+)" (+)
                testBinaryOpLowering @Integer @Integer @Integer
                  unboundedConfig
                  addNumTerm
                  "(+)"
                  (\x y -> (x + 1) * (y + 1) - x * y - 1),
              testCase "Uminus" $ do
                testUnaryOpLowering @Integer @Integer unboundedConfig negNumTerm "negate" negate
                testUnaryOpLowering @Integer @Integer
                  unboundedConfig
                  negNumTerm
                  "negate"
                  (\x -> (x + 1) * (x + 1) - 3 * x - x * x - 1),
              testCase "Abs" $ do
                testUnaryOpLowering @Integer @Integer unboundedConfig absNumTerm "abs" abs,
              testCase "Signum" $ do
                testUnaryOpLowering @Integer @Integer unboundedConfig signumNumTerm "signum" signum,
              testCase "Times" $ do
                testBinaryOpLowering @Integer @Integer @Integer unboundedConfig mulNumTerm "(*)" (*)
                testBinaryOpLowering @Integer @Integer @Integer
                  unboundedConfig
                  mulNumTerm
                  "(*)"
                  (\x y -> (x + 1) * (y + 1) - x - y - 1),
              testCase "Lt" $ do
                testBinaryOpLowering @Integer @Integer @Bool unboundedConfig ltOrdTerm "(<)" (SBV..<)
                testBinaryOpLowering @Integer @Integer @Bool
                  unboundedConfig
                  ltOrdTerm
                  "(<)"
                  (\x y -> x * 2 - x SBV..< y * 2 - y),
              testCase "Le" $ do
                testBinaryOpLowering @Integer @Integer @Bool unboundedConfig leOrdTerm "(<=)" (SBV..<=)
                testBinaryOpLowering @Integer @Integer @Bool
                  unboundedConfig
                  leOrdTerm
                  "(<=)"
                  (\x y -> x * 2 - x SBV..<= y * 2 - y),
              testCase "Div" $ do
                testBinaryOpLowering @Integer @Integer @Integer unboundedConfig divIntegralTerm "div" SBV.sDiv,
              testCase "Mod" $ do
                testBinaryOpLowering @Integer @Integer @Integer unboundedConfig modIntegralTerm "mod" SBV.sMod,
              testCase "Quot" $ do
                testBinaryOpLowering @Integer @Integer @Integer unboundedConfig quotIntegralTerm "quot" SBV.sQuot,
              testCase "Rem" $ do
                testBinaryOpLowering @Integer @Integer @Integer unboundedConfig remIntegralTerm "rem" SBV.sRem
            ],
          testGroup
            "IntN Lowering"
            [ testModelParse @(IntN 4),
              testCase "Add" $ do
                testBinaryOpLowering @(IntN 5) @(IntN 5) unboundedConfig addNumTerm "(+)" (+)
                testBinaryOpLowering @(IntN 5) @(IntN 5)
                  unboundedConfig
                  addNumTerm
                  "(+)"
                  (\x y -> (x + 1) * (y + 1) - x * y - 1),
              testCase "Uminus" $ do
                testUnaryOpLowering @(IntN 5) unboundedConfig negNumTerm "negate" negate
                testUnaryOpLowering @(IntN 5)
                  unboundedConfig
                  negNumTerm
                  "negate"
                  (\x -> (x + 1) * (x + 1) - 3 * x - x * x - 1),
              testCase "Abs" $ do
                testUnaryOpLowering @(IntN 5) unboundedConfig absNumTerm "abs" abs,
              testCase "Signum" $ do
                testUnaryOpLowering @(IntN 5) unboundedConfig signumNumTerm "signum" signum,
              testCase "Times" $ do
                testBinaryOpLowering @(IntN 5) @(IntN 5) unboundedConfig mulNumTerm "(*)" (*)
                testBinaryOpLowering @(IntN 5) @(IntN 5)
                  unboundedConfig
                  mulNumTerm
                  "(*)"
                  (\x y -> (x + 1) * (y + 1) - x - y - 1),
              testCase "Lt" $ do
                testBinaryOpLowering @(IntN 5) @(IntN 5) unboundedConfig ltOrdTerm "(<)" (SBV..<)
                testBinaryOpLowering @(IntN 5) @(IntN 5)
                  unboundedConfig
                  ltOrdTerm
                  "(<)"
                  (\x y -> x * 2 - x SBV..< y * 2 - y),
              testCase "Le" $ do
                testBinaryOpLowering @(IntN 5) @(IntN 5) unboundedConfig leOrdTerm "(<=)" (SBV..<=)
                testBinaryOpLowering @(IntN 5) @(IntN 5)
                  unboundedConfig
                  leOrdTerm
                  "(<=)"
                  (\x y -> x * 2 - x SBV..<= y * 2 - y),
              testCase "Extract" $ do
                testUnaryOpLowering @(IntN 5) @(IntN 1)
                  unboundedConfig
                  (bvSelectTerm (Proxy @0) (Proxy @1))
                  "select"
                  (SBV.bvExtract @0 @0 @5 Proxy Proxy)
                testUnaryOpLowering @(IntN 5) @(IntN 1)
                  unboundedConfig
                  (bvSelectTerm (Proxy @1) (Proxy @1))
                  "select"
                  (SBV.bvExtract @1 @1 @5 Proxy Proxy)
                testUnaryOpLowering @(IntN 5) @(IntN 1)
                  unboundedConfig
                  (bvSelectTerm (Proxy @2) (Proxy @1))
                  "select"
                  (SBV.bvExtract @2 @2 @5 Proxy Proxy)
                testUnaryOpLowering @(IntN 5) @(IntN 1)
                  unboundedConfig
                  (bvSelectTerm (Proxy @3) (Proxy @1))
                  "select"
                  (SBV.bvExtract @3 @3 @5 Proxy Proxy)
                testUnaryOpLowering @(IntN 5) @(IntN 1)
                  unboundedConfig
                  (bvSelectTerm (Proxy @4) (Proxy @1))
                  "select"
                  (SBV.bvExtract @4 @4 @5 Proxy Proxy)
                testUnaryOpLowering @(IntN 5) @(IntN 2)
                  unboundedConfig
                  (bvSelectTerm (Proxy @0) (Proxy @2))
                  "select"
                  (SBV.bvExtract @1 @0 @5 Proxy Proxy)
                testUnaryOpLowering @(IntN 5) @(IntN 2)
                  unboundedConfig
                  (bvSelectTerm (Proxy @1) (Proxy @2))
                  "select"
                  (SBV.bvExtract @2 @1 @5 Proxy Proxy)
                testUnaryOpLowering @(IntN 5) @(IntN 2)
                  unboundedConfig
                  (bvSelectTerm (Proxy @2) (Proxy @2))
                  "select"
                  (SBV.bvExtract @3 @2 @5 Proxy Proxy)
                testUnaryOpLowering @(IntN 5) @(IntN 2)
                  unboundedConfig
                  (bvSelectTerm (Proxy @3) (Proxy @2))
                  "select"
                  (SBV.bvExtract @4 @3 @5 Proxy Proxy)
                testUnaryOpLowering @(IntN 5) @(IntN 3)
                  unboundedConfig
                  (bvSelectTerm (Proxy @0) (Proxy @3))
                  "select"
                  (SBV.bvExtract @2 @0 @5 Proxy Proxy)
                testUnaryOpLowering @(IntN 5) @(IntN 3)
                  unboundedConfig
                  (bvSelectTerm (Proxy @1) (Proxy @3))
                  "select"
                  (SBV.bvExtract @3 @1 @5 Proxy Proxy)
                testUnaryOpLowering @(IntN 5) @(IntN 3)
                  unboundedConfig
                  (bvSelectTerm (Proxy @2) (Proxy @3))
                  "select"
                  (SBV.bvExtract @4 @2 @5 Proxy Proxy)
                testUnaryOpLowering @(IntN 5) @(IntN 4)
                  unboundedConfig
                  (bvSelectTerm (Proxy @0) (Proxy @4))
                  "select"
                  (SBV.bvExtract @3 @0 @5 Proxy Proxy)
                testUnaryOpLowering @(IntN 5) @(IntN 4)
                  unboundedConfig
                  (bvSelectTerm (Proxy @1) (Proxy @4))
                  "select"
                  (SBV.bvExtract @4 @1 @5 Proxy Proxy)
                testUnaryOpLowering @(IntN 5) @(IntN 5)
                  unboundedConfig
                  (bvSelectTerm (Proxy @0) (Proxy @5))
                  "select"
                  id,
              testCase "Extension" $ do
                testUnaryOpLowering @(IntN 5) @(IntN 6)
                  unboundedConfig
                  (bvzeroExtendTerm (Proxy @6))
                  "bvzeroExtend"
                  SBV.zeroExtend
                testUnaryOpLowering @(IntN 5) @(IntN 10)
                  unboundedConfig
                  (bvzeroExtendTerm (Proxy @10))
                  "bvzeroExtend"
                  SBV.zeroExtend
                testUnaryOpLowering @(IntN 5) @(IntN 6)
                  unboundedConfig
                  (bvsignExtendTerm (Proxy @6))
                  "bvsignExtend"
                  SBV.signExtend
                testUnaryOpLowering @(IntN 5) @(IntN 10)
                  unboundedConfig
                  (bvsignExtendTerm (Proxy @10))
                  "bvsignExtend"
                  SBV.signExtend,
              testCase "Concat" $ do
                testBinaryOpLowering @(IntN 4) @(IntN 5) @(IntN 9)
                  unboundedConfig
                  bvConcatTerm
                  "bvconcat"
                  (SBV.#),
              testCase "AndBits" $ do
                testBinaryOpLowering @(IntN 5) @(IntN 5) unboundedConfig andBitsTerm "(.&.)" (.&.),
              testCase "OrBits" $ do
                testBinaryOpLowering @(IntN 5) @(IntN 5) unboundedConfig orBitsTerm "(.|.)" (.|.),
              testCase "XorBits" $ do
                testBinaryOpLowering @(IntN 5) @(IntN 5) unboundedConfig xorBitsTerm "xor" xor,
              testCase "ComplementBits" $ do
                testUnaryOpLowering @(IntN 5) unboundedConfig complementBitsTerm "complement" complement,
              testCase "ShiftLeft" $ do
                testBinaryOpLowering @(IntN 5) unboundedConfig shiftLeftTerm "shiftLeft" SBV.sShiftLeft,
              testCase "ShiftRight" $ do
                testBinaryOpLowering @(IntN 5) unboundedConfig shiftRightTerm "shiftRight" SBV.sShiftRight,
              testCase "RotateLeft" $ do
                testBinaryOpLowering @(IntN 5)
                  unboundedConfig
                  rotateLeftTerm
                  "rotateLeft"
                  ( \a b ->
                      SBV.sFromIntegral $
                        SBV.sRotateLeft
                          (SBV.sFromIntegral a :: SBV.SWord 5)
                          (SBV.sFromIntegral b :: SBV.SWord 5)
                  ),
              testCase "RotateRight" $ do
                testBinaryOpLowering @(IntN 5)
                  unboundedConfig
                  rotateRightTerm
                  "rotateRight"
                  ( \a b ->
                      SBV.sFromIntegral $
                        SBV.sRotateRight
                          (SBV.sFromIntegral a :: SBV.SWord 5)
                          (SBV.sFromIntegral b :: SBV.SWord 5)
                  ),
              testCase "Div - bounded" $ do
                testBinaryOpLowering @(IntN 5) @(IntN 5) @(IntN 5) unboundedConfig divIntegralTerm "div" SBV.sDiv,
              testCase "Mod - bounded" $ do
                testBinaryOpLowering @(IntN 5) @(IntN 5) @(IntN 5) unboundedConfig modIntegralTerm "mod" SBV.sMod,
              testCase "Quot - bounded" $ do
                testBinaryOpLowering @(IntN 5) @(IntN 5) @(IntN 5) unboundedConfig quotIntegralTerm "quot" SBV.sQuot,
              testCase "Rem - bounded" $ do
                testBinaryOpLowering @(IntN 5) @(IntN 5) @(IntN 5) unboundedConfig remIntegralTerm "rem" SBV.sRem,
              testCase "BitCast" $ do
                testUnaryOpLowering @(IntN 5) @(WordN 5) unboundedConfig bitCastTerm "bitCast" SBV.sFromIntegral
                testUnaryOpLowering @(IntN 1) @Bool unboundedConfig bitCastTerm "bitCast" (`SBV.sTestBit` 0)
                testUnaryOpLowering @Bool @(IntN 1) unboundedConfig bitCastTerm "bitCast" (\x -> SBV.ite x 1 0)
            ],
          testGroup
            "WordN"
            [ testModelParse @(WordN 4),
              testCase "Add" $ do
                testBinaryOpLowering @(WordN 5) @(WordN 5) unboundedConfig addNumTerm "(+)" (+)
                testBinaryOpLowering @(WordN 5) @(WordN 5)
                  unboundedConfig
                  addNumTerm
                  "(+)"
                  (\x y -> (x + 1) * (y + 1) - x * y - 1),
              testCase "Uminus" $ do
                testUnaryOpLowering @(WordN 5) unboundedConfig negNumTerm "negate" negate
                testUnaryOpLowering @(WordN 5)
                  unboundedConfig
                  negNumTerm
                  "negate"
                  (\x -> (x + 1) * (x + 1) - 3 * x - x * x - 1),
              testCase "Abs" $ do
                testUnaryOpLowering @(WordN 5) unboundedConfig absNumTerm "abs" abs,
              testCase "Signum" $ do
                testUnaryOpLowering @(WordN 5) unboundedConfig signumNumTerm "signum" signum,
              testCase "Times" $ do
                testBinaryOpLowering @(WordN 5) @(WordN 5) unboundedConfig mulNumTerm "(*)" (*)
                testBinaryOpLowering @(WordN 5) @(WordN 5)
                  unboundedConfig
                  mulNumTerm
                  "(*)"
                  (\x y -> (x + 1) * (y + 1) - x - y - 1),
              testCase "Lt" $ do
                testBinaryOpLowering @(WordN 5) @(WordN 5) unboundedConfig ltOrdTerm "(<)" (SBV..<)
                testBinaryOpLowering @(WordN 5) @(WordN 5)
                  unboundedConfig
                  ltOrdTerm
                  "(<)"
                  (\x y -> x * 2 - x SBV..< y * 2 - y),
              testCase "Le" $ do
                testBinaryOpLowering @(WordN 5) @(WordN 5) unboundedConfig leOrdTerm "(<=)" (SBV..<=)
                testBinaryOpLowering @(WordN 5) @(WordN 5)
                  unboundedConfig
                  leOrdTerm
                  "(<=)"
                  (\x y -> x * 2 - x SBV..<= y * 2 - y),
              testCase "Extract" $ do
                testUnaryOpLowering @(WordN 5) @(WordN 1)
                  unboundedConfig
                  (bvSelectTerm (Proxy @0) (Proxy @1))
                  "select"
                  (SBV.bvExtract @0 @0 @5 Proxy Proxy)
                testUnaryOpLowering @(WordN 5) @(WordN 1)
                  unboundedConfig
                  (bvSelectTerm (Proxy @1) (Proxy @1))
                  "select"
                  (SBV.bvExtract @1 @1 @5 Proxy Proxy)
                testUnaryOpLowering @(WordN 5) @(WordN 1)
                  unboundedConfig
                  (bvSelectTerm (Proxy @2) (Proxy @1))
                  "select"
                  (SBV.bvExtract @2 @2 @5 Proxy Proxy)
                testUnaryOpLowering @(WordN 5) @(WordN 1)
                  unboundedConfig
                  (bvSelectTerm (Proxy @3) (Proxy @1))
                  "select"
                  (SBV.bvExtract @3 @3 @5 Proxy Proxy)
                testUnaryOpLowering @(WordN 5) @(WordN 1)
                  unboundedConfig
                  (bvSelectTerm (Proxy @4) (Proxy @1))
                  "select"
                  (SBV.bvExtract @4 @4 @5 Proxy Proxy)
                testUnaryOpLowering @(WordN 5) @(WordN 2)
                  unboundedConfig
                  (bvSelectTerm (Proxy @0) (Proxy @2))
                  "select"
                  (SBV.bvExtract @1 @0 @5 Proxy Proxy)
                testUnaryOpLowering @(WordN 5) @(WordN 2)
                  unboundedConfig
                  (bvSelectTerm (Proxy @1) (Proxy @2))
                  "select"
                  (SBV.bvExtract @2 @1 @5 Proxy Proxy)
                testUnaryOpLowering @(WordN 5) @(WordN 2)
                  unboundedConfig
                  (bvSelectTerm (Proxy @2) (Proxy @2))
                  "select"
                  (SBV.bvExtract @3 @2 @5 Proxy Proxy)
                testUnaryOpLowering @(WordN 5) @(WordN 2)
                  unboundedConfig
                  (bvSelectTerm (Proxy @3) (Proxy @2))
                  "select"
                  (SBV.bvExtract @4 @3 @5 Proxy Proxy)
                testUnaryOpLowering @(WordN 5) @(WordN 3)
                  unboundedConfig
                  (bvSelectTerm (Proxy @0) (Proxy @3))
                  "select"
                  (SBV.bvExtract @2 @0 @5 Proxy Proxy)
                testUnaryOpLowering @(WordN 5) @(WordN 3)
                  unboundedConfig
                  (bvSelectTerm (Proxy @1) (Proxy @3))
                  "select"
                  (SBV.bvExtract @3 @1 @5 Proxy Proxy)
                testUnaryOpLowering @(WordN 5) @(WordN 3)
                  unboundedConfig
                  (bvSelectTerm (Proxy @2) (Proxy @3))
                  "select"
                  (SBV.bvExtract @4 @2 @5 Proxy Proxy)
                testUnaryOpLowering @(WordN 5) @(WordN 4)
                  unboundedConfig
                  (bvSelectTerm (Proxy @0) (Proxy @4))
                  "select"
                  (SBV.bvExtract @3 @0 @5 Proxy Proxy)
                testUnaryOpLowering @(WordN 5) @(WordN 4)
                  unboundedConfig
                  (bvSelectTerm (Proxy @1) (Proxy @4))
                  "select"
                  (SBV.bvExtract @4 @1 @5 Proxy Proxy)
                testUnaryOpLowering @(WordN 5) @(WordN 5)
                  unboundedConfig
                  (bvSelectTerm (Proxy @0) (Proxy @5))
                  "select"
                  id,
              testCase "Extension" $ do
                testUnaryOpLowering @(WordN 5) @(WordN 6)
                  unboundedConfig
                  (bvzeroExtendTerm (Proxy @6))
                  "bvzeroExtend"
                  SBV.zeroExtend
                testUnaryOpLowering @(WordN 5) @(WordN 10)
                  unboundedConfig
                  (bvzeroExtendTerm (Proxy @10))
                  "bvzeroExtend"
                  SBV.zeroExtend
                testUnaryOpLowering @(WordN 5) @(WordN 6)
                  unboundedConfig
                  (bvsignExtendTerm (Proxy @6))
                  "bvsignExtend"
                  SBV.signExtend
                testUnaryOpLowering @(WordN 5) @(WordN 10)
                  unboundedConfig
                  (bvsignExtendTerm (Proxy @10))
                  "bvsignExtend"
                  SBV.signExtend,
              testCase "Concat" $ do
                testBinaryOpLowering @(WordN 4) @(WordN 5) @(WordN 9)
                  unboundedConfig
                  bvConcatTerm
                  "bvconcat"
                  (SBV.#),
              testCase "AndBits" $ do
                testBinaryOpLowering @(WordN 5) @(WordN 5) unboundedConfig andBitsTerm "(.&.)" (.&.),
              testCase "OrBits" $ do
                testBinaryOpLowering @(WordN 5) @(WordN 5) unboundedConfig orBitsTerm "(.|.)" (.|.),
              testCase "XorBits" $ do
                testBinaryOpLowering @(WordN 5) @(WordN 5) unboundedConfig xorBitsTerm "xor" xor,
              testCase "ComplementBits" $ do
                testUnaryOpLowering @(WordN 5) unboundedConfig complementBitsTerm "complement" complement,
              testCase "ShiftLeft" $ do
                testBinaryOpLowering @(WordN 5) unboundedConfig shiftLeftTerm "shiftLeft" SBV.sShiftLeft,
              testCase "ShiftRight" $ do
                testBinaryOpLowering @(WordN 5) unboundedConfig shiftRightTerm "shiftRight" SBV.sShiftRight,
              testCase "RotateLeft" $ do
                testBinaryOpLowering @(WordN 5) unboundedConfig rotateLeftTerm "rotateLeft" SBV.sRotateLeft,
              testCase "RotateRight" $ do
                testBinaryOpLowering @(WordN 5) unboundedConfig rotateRightTerm "rotateRight" SBV.sRotateRight,
              testCase "Div" $ do
                testBinaryOpLowering @(WordN 5) @(WordN 5) @(WordN 5) unboundedConfig divIntegralTerm "div" SBV.sDiv,
              testCase "Mod" $ do
                testBinaryOpLowering @(WordN 5) @(WordN 5) @(WordN 5) unboundedConfig modIntegralTerm "mod" SBV.sMod,
              testCase "Quot" $ do
                testBinaryOpLowering @(WordN 5) @(WordN 5) @(WordN 5) unboundedConfig quotIntegralTerm "quot" SBV.sQuot,
              testCase "Rem" $ do
                testBinaryOpLowering @(WordN 5) @(WordN 5) @(WordN 5) unboundedConfig remIntegralTerm "rem" SBV.sRem,
              testCase "BitCast" $ do
                testUnaryOpLowering @(WordN 5) @(IntN 5) unboundedConfig bitCastTerm "bitCast" SBV.sFromIntegral
                testUnaryOpLowering @(WordN 1) @Bool unboundedConfig bitCastTerm "bitCast" (`SBV.sTestBit` 0)
                testUnaryOpLowering @Bool @(WordN 1) unboundedConfig bitCastTerm "bitCast" (\x -> SBV.ite x 1 0)
            ],
          testGroup
            "FP"
            [ testCase "Model parse (float)" $ modelParseTestBody (10.012 :: FP32),
              testModelParse @FPRoundingMode,
              testCase "Eqv" $
                testBinaryOpLowering @FP32 @FP32 @Bool unboundedConfig eqTerm "eqv" (SBV..==),
              testCase "ITE" $ do
                let precond _ l r =
                      pevalAndTerm
                        ( pevalNotTerm $
                            pevalFPTraitTerm FPIsNaN (l :: Term FP32)
                        )
                        ( pevalNotTerm $
                            pevalFPTraitTerm FPIsNaN (r :: Term FP32)
                        )
                testTernaryOpLowering @Bool @FP32 @FP32 @FP32
                  unboundedConfig
                  precond
                  iteTerm
                  "ite"
                  SBV.ite,
              testGroup "FPTrait" $ do
                (name, trait, op) <-
                  [ ("isNaN", FPIsNaN, SBV.fpIsNaN),
                    ("isPositive", FPIsPositive, SBV.fpIsPositive),
                    ("isNegative", FPIsNegative, SBV.fpIsNegative),
                    ( "isPositiveInfinite",
                      FPIsPositiveInfinite,
                      \x -> SBV.fpIsPositive x SBV..&& SBV.fpIsInfinite x
                    ),
                    ( "isNegativeInfinite",
                      FPIsNegativeInfinite,
                      \x -> SBV.fpIsNegative x SBV..&& SBV.fpIsInfinite x
                    ),
                    ("isInfinite", FPIsInfinite, SBV.fpIsInfinite),
                    ("isPositiveZero", FPIsPositiveZero, SBV.fpIsPositiveZero),
                    ("isNegativeZero", FPIsNegativeZero, SBV.fpIsNegativeZero),
                    ("isZero", FPIsZero, SBV.fpIsZero),
                    ("isNormal", FPIsNormal, SBV.fpIsNormal),
                    ("isSubnormal", FPIsSubnormal, SBV.fpIsSubnormal),
                    ("isPoint", FPIsPoint, SBV.fpIsPoint)
                  ]
                return $ testCase name $ do
                  testUnaryOpLowering @FP32 @Bool
                    unboundedConfig
                    (fpTraitTerm trait)
                    "isNaN"
                    op,
              testCase "BitCastOr" $ do
                testBinaryOpLowering @(WordN 8) @(FP 3 5)
                  unboundedConfig
                  bitCastOrTerm
                  "bitCastOr"
                  ( \d v ->
                      SBV.ite
                        (SBV.fpIsNaN v)
                        d
                        (SBV.sFloatingPointAsSWord v)
                  )
                testBinaryOpLowering @(IntN 8) @(FP 3 5)
                  unboundedConfig
                  bitCastOrTerm
                  "bitCastOr"
                  ( \d v ->
                      SBV.ite
                        (SBV.fpIsNaN v)
                        d
                        (SBV.sFromIntegral $ SBV.sFloatingPointAsSWord v)
                  ),
              testCase "BitCast" $ do
                testUnaryOpLowering' @(WordN 8) @(FP 3 5)
                  ( Just $ \x ->
                      SBV.sNot $
                        SBV.fpIsNaN
                          ( SBV.sWordAsSFloatingPoint x ::
                              SBV.SFloatingPoint 3 5
                          )
                  )
                  unboundedConfig
                  bitCastTerm
                  "bitCast"
                  SBV.sWordAsSFloatingPoint
                testUnaryOpLowering' @(IntN 8) @(FP 3 5)
                  ( Just $ \x ->
                      SBV.sNot $
                        SBV.fpIsNaN
                          ( SBV.sWordAsSFloatingPoint . SBV.sFromIntegral $ x ::
                              SBV.SFloatingPoint 3 5
                          )
                  )
                  unboundedConfig
                  bitCastTerm
                  "bitCast"
                  (SBV.sWordAsSFloatingPoint . SBV.sFromIntegral)
            ],
          testGroup
            "AlgReal"
            [ testModelParse @AlgReal,
              testCase "Eqv" $
                testBinaryOpLowering @AlgReal @AlgReal @Bool
                  unboundedConfig
                  eqTerm
                  "eqv"
                  (SBV..==),
              testCase "ITE" $ do
                let truePrecond _ _ _ = conTerm True
                testTernaryOpLowering @Bool @AlgReal @AlgReal @AlgReal
                  unboundedConfig
                  truePrecond
                  iteTerm
                  "ite"
                  SBV.ite,
              testCase "Add" $ do
                testBinaryOpLowering @AlgReal @AlgReal @AlgReal unboundedConfig addNumTerm "(+)" (+)
                testBinaryOpLowering @AlgReal @AlgReal @AlgReal
                  unboundedConfig
                  addNumTerm
                  "(+)"
                  (\x y -> (x + 1) * (y + 1) - x * y - 1),
              testCase "Uminus" $ do
                testUnaryOpLowering @AlgReal @AlgReal unboundedConfig negNumTerm "negate" negate
                testUnaryOpLowering @AlgReal @AlgReal
                  unboundedConfig
                  negNumTerm
                  "negate"
                  (\x -> (x + 1) * (x + 1) - 3 * x - x * x - 1),
              testCase "Abs" $
                testUnaryOpLowering @AlgReal @AlgReal unboundedConfig absNumTerm "abs" abs,
              testCase "Signum" $
                testUnaryOpLowering @AlgReal @AlgReal unboundedConfig signumNumTerm "signum" signum,
              testCase "Times" $ do
                testBinaryOpLowering @AlgReal @AlgReal @AlgReal unboundedConfig mulNumTerm "(*)" (*)
                testBinaryOpLowering @AlgReal @AlgReal @AlgReal
                  unboundedConfig
                  mulNumTerm
                  "(*)"
                  (\x y -> (x + 1) * (y + 1) - x - y - 1),
              testCase "Lt" $ do
                testBinaryOpLowering @Integer @Integer @Bool unboundedConfig ltOrdTerm "(<)" (SBV..<)
                testBinaryOpLowering @Integer @Integer @Bool
                  unboundedConfig
                  ltOrdTerm
                  "(<)"
                  (\x y -> x * 2 - x SBV..< y * 2 - y),
              testCase "Le" $ do
                testBinaryOpLowering @Integer @Integer @Bool unboundedConfig leOrdTerm "(<=)" (SBV..<=)
                testBinaryOpLowering @Integer @Integer @Bool
                  unboundedConfig
                  leOrdTerm
                  "(<=)"
                  (\x y -> x * 2 - x SBV..<= y * 2 - y),
              testCase "fdiv" $ do
                testBinaryOpLowering @AlgReal @AlgReal @AlgReal
                  unboundedConfig
                  fdivTerm
                  "fdiv"
                  (/),
              testCase "recip" $ do
                testUnaryOpLowering @AlgReal @AlgReal
                  unboundedConfig
                  recipTerm
                  "recip"
                  recip,
              testGroup "Floating unary" $ do
                (name, f, op) <-
                  -- Those unsupported by z3 are commented out
                  [ -- ("exp", exp, FloatingExp),
                    -- ("log", log, FloatingLog),
                    -- ("sqrt", sqrt, FloatingSqrt),
                    ("sin", sin, FloatingSin),
                    ("cos", cos, FloatingCos),
                    ("tan", tan, FloatingTan),
                    ("asin", asin, FloatingAsin),
                    ("acos", acos, FloatingAcos),
                    ("atan", atan, FloatingAtan),
                    ("sinh", sinh, FloatingSinh),
                    ("cosh", cosh, FloatingCosh),
                    ("tanh", tanh, FloatingTanh)
                  ]
                return $
                  testCase name $
                    testUnaryOpLowering @AlgReal @AlgReal
                      unboundedConfig
                      (floatingUnaryTerm op)
                      name
                      f {-,
                        testCase "**" $ do
                          testBinaryOpLowering @AlgReal @AlgReal @AlgReal
                            unboundedConfig
                            powerTerm
                            "(**)"
                            (**)-}
            ],
          testCase "TabularFun" $ do
            let f = "f" :: SymInteger =~> SymInteger =~> SymInteger
            let a = "a" :: SymInteger
            let b = "b" :: SymInteger
            let c = "c" :: SymInteger
            let d = "d" :: SymInteger
            Right m <-
              solve unboundedConfig $
                (f # a # b .== a + b)
                  .&& (f # a # c .== a + c)
                  .&& (f # a # d .== a + d)
                  .&& (f # b # d .== b + d)
                  .&& (a .== 10 .&& b .== 20 .&& c .== 30 .&& d .== 40)
            AsKey (evalSym False m (f # a # b .== a + b)) @?= con True
            AsKey (evalSym False m (f # a # c .== a + c)) @?= con True
            AsKey (evalSym False m (f # a # d .== a + d)) @?= con True
            AsKey (evalSym False m (f # b # d .== b + d)) @?= con True,
          testCase "GeneralFun" $ do
            let f = "f" :: SymInteger -~> SymInteger -~> SymInteger
            let a = "a" :: SymInteger
            let b = "b" :: SymInteger
            let c = "c" :: SymInteger
            let d = "d" :: SymInteger
            r <-
              solve unboundedConfig $
                (f # a # b .== a + b)
                  .&& (f # a # c .== a + c)
                  .&& (f # a # d .== a + d)
                  .&& (f # b # d .== b + d)
                  .&& (a .== 10 .&& b .== 20 .&& c .== 30 .&& d .== 40)
            case r of
              Left err -> fail $ show err
              Right m -> do
                AsKey (evalSym False m (f # a # b .== a + b)) @?= con True
                AsKey (evalSym False m (f # a # c .== a + c)) @?= con True
                AsKey (evalSym False m (f # a # d .== a + d)) @?= con True
                AsKey (evalSym False m (f # b # d .== b + d)) @?= con True,
          sbvVersionCheck $
            testGroup
              "Quantifiers"
              [ testCase "Forall" $ do
                  let asym :: TypedConstantSymbol Integer = "a"
                  let a :: Term Integer = ssymTerm "a"
                  let xsym :: TypedConstantSymbol Integer = "x"
                  let x :: Term Integer = ssymTerm "x"
                  let xterm =
                        forallTerm
                          xsym
                          (eqTerm (addNumTerm a x) (addNumTerm x $ conTerm 10))
                  let yterm =
                        forallTerm
                          asym
                          (eqTerm (addNumTerm a x) (addNumTerm a $ conTerm 20))
                  SBV.runSMTWith SBV.z3 $ do
                    (m, v, _) <- lowerSinglePrim (andTerm xterm yterm)
                    let sbva =
                          M.lookup (SomeTerm a) (biMapToSBV m)
                            >>= \f -> fromDynamic (f emptyQuantifiedStack)
                    let sbvx =
                          M.lookup (SomeTerm x) (biMapToSBV m)
                            >>= \f -> fromDynamic (f emptyQuantifiedStack)
                    case (sbva, sbvx) of
                      (Just (sbvav :: SBV.SInteger), Just (sbvxv :: SBV.SInteger)) ->
                        SBV.query $ do
                          SBV.constrain $ v emptyQuantifiedStack
                          satres <- SBV.checkSat
                          case satres of
                            SBV.Sat -> do
                              av <- SBV.getValue sbvav
                              liftIO $ av @?= 10
                              xv <- SBV.getValue sbvxv
                              liftIO $ xv @?= 20
                            _ -> liftIO $ assertFailure "Unsat"
                      _ -> liftIO $ assertFailure "Failed to find a",
                testCase "Forall failed" $ do
                  let xsym :: TypedConstantSymbol Integer = "x"
                  let x :: Term Integer = ssymTerm "x"
                  let xterm = forallTerm xsym (eqTerm x (conTerm 10))
                  SBV.runSMTWith SBV.z3 $ do
                    (_, v, _) <- lowerSinglePrim xterm
                    SBV.query $ do
                      SBV.constrain $ v emptyQuantifiedStack
                      satres <- SBV.checkSat
                      case satres of
                        SBV.Unsat -> return ()
                        _ -> liftIO $ assertFailure "Should be unsat",
                testCase "Forall-Exists" $ do
                  let asym :: TypedConstantSymbol Integer = "a"
                  let a :: Term Integer = ssymTerm "a"
                  let xsym :: TypedConstantSymbol Integer = "x"
                  let x :: Term Integer = ssymTerm "x"
                  let xterm =
                        forallTerm xsym $ existsTerm asym (ltOrdTerm x a)
                  SBV.runSMTWith SBV.z3 $ do
                    (_, v, _) <- lowerSinglePrim xterm
                    SBV.query $ do
                      SBV.constrain $ v emptyQuantifiedStack
                      satres <- SBV.checkSat
                      case satres of
                        SBV.Sat -> return ()
                        _ -> liftIO $ assertFailure "Unsat",
                testCase "Exists-Forall" $ do
                  let asym :: TypedConstantSymbol Integer = "a"
                  let a :: Term Integer = ssymTerm "a"
                  let xsym :: TypedConstantSymbol Integer = "x"
                  let x :: Term Integer = ssymTerm "x"
                  let xterm =
                        existsTerm asym $ forallTerm xsym (ltOrdTerm x a)
                  SBV.runSMTWith SBV.z3 $ do
                    (_, v, _) <- lowerSinglePrim xterm
                    SBV.query $ do
                      SBV.constrain $ v emptyQuantifiedStack
                      satres <- SBV.checkSat
                      case satres of
                        SBV.Unsat -> return ()
                        _ -> liftIO $ assertFailure "should be unsat"
              ]
        ]

#if MIN_VERSION_sbv(10,1,0)
sbvVersionCheck :: Test -> Test
sbvVersionCheck = id
#else
sbvVersionCheck :: Test -> Test
sbvVersionCheck _ = testGroup "Quantifiers" []
#endif
