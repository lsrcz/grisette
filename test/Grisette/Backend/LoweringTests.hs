{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Grisette.Backend.LoweringTests (loweringTests) where

import Control.Monad.Trans (MonadTrans (lift))
import Data.Bits
  ( Bits (complement, xor, (.&.), (.|.)),
  )
import Data.Dynamic (Typeable, fromDynamic)
import qualified Data.HashMap.Strict as M
import Data.Proxy (Proxy (Proxy))
import qualified Data.SBV as SBV
import qualified Data.SBV.Control as SBV
import qualified Data.Text as T
import GHC.Stack (HasCallStack)
import Grisette
  ( EvaluateSym (evaluateSym),
    Function ((#)),
    IntN,
    LogicalOp ((.&&)),
    SEq ((.==)),
    Solvable (con),
    SymInteger,
    WordN,
    solve,
    type (-~>),
    type (=~>),
  )
import Grisette.Internal.Backend.Solving
  ( GrisetteSMTConfig (sbvConfig),
    approx,
    lowerSinglePrim,
    lowerSinglePrimCached,
    precise,
  )
import Grisette.Internal.Backend.SymBiMap
  ( SymBiMap (biMapToSBV),
  )
import Grisette.Internal.SymPrim.FP (FP32)
import Grisette.Internal.SymPrim.Prim.SomeTerm
  ( SomeTerm (SomeTerm),
  )
import Grisette.Internal.SymPrim.Prim.Term
  ( FPTrait (FPIsInfinite, FPIsNaN, FPIsNegative, FPIsNegativeInfinite, FPIsNegativeZero, FPIsNormal, FPIsPoint, FPIsPositive, FPIsPositiveInfinite, FPIsPositiveZero, FPIsSubnormal, FPIsZero),
    SBVRep (SBVType),
    SupportedPrim,
    Term,
    absNumTerm,
    addNumTerm,
    andBitsTerm,
    andTerm,
    bvconcatTerm,
    bvselectTerm,
    bvsignExtendTerm,
    bvzeroExtendTerm,
    complementBitsTerm,
    conTerm,
    divIntegralTerm,
    eqTerm,
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
    remIntegralTerm,
    rotateLeftTerm,
    rotateRightTerm,
    shiftLeftTerm,
    shiftRightTerm,
    signumNumTerm,
    ssymTerm,
    toSignedTerm,
    toUnsignedTerm,
    xorBitsTerm,
  )
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (Assertion, assertFailure, (@?=))

testUnaryOpLowering ::
  forall a b as n.
  ( HasCallStack,
    SupportedPrim a,
    SBV.EqSymbolic (SBVType n b),
    Typeable (SBVType n a),
    SBV.SymVal as,
    SBVType n a ~ SBV.SBV as,
    Show as
  ) =>
  GrisetteSMTConfig n ->
  (Term a -> Term b) ->
  String ->
  (SBVType n a -> SBVType n b) ->
  Assertion
testUnaryOpLowering config f name sbvfun = do
  let a :: Term a = ssymTerm "a"
  let fa :: Term b = f a
  SBV.runSMTWith (sbvConfig config) $ do
    (m, lt) <- lowerSinglePrim config fa
    let sbva :: Maybe (SBVType n a) = M.lookup (SomeTerm a) (biMapToSBV m) >>= fromDynamic
    case sbva of
      Nothing -> lift $ assertFailure "Failed to extract the term"
      Just sbvav -> SBV.query $ do
        SBV.constrain $ lt SBV..== sbvfun sbvav
        satres <- SBV.checkSat
        case satres of
          SBV.Sat -> return ()
          _ -> lift $ assertFailure $ "Lowering for " ++ name ++ " generated unsolvable formula"
  SBV.runSMTWith (sbvConfig config) $ do
    (m, lt) <- lowerSinglePrim config fa
    let sbvv :: Maybe (SBVType n a) = M.lookup (SomeTerm a) (biMapToSBV m) >>= fromDynamic
    case sbvv of
      Nothing -> lift $ assertFailure "Failed to extract the term"
      Just sbvvv -> SBV.query $ do
        SBV.constrain $ lt SBV../= sbvfun sbvvv
        r <- SBV.checkSat
        case r of
          SBV.Sat -> do
            counterExample <- SBV.getValue sbvvv
            lift $ assertFailure $ "Translation counter example found: " ++ show counterExample
          SBV.Unsat -> return ()
          _ -> lift $ assertFailure $ "Lowering for " ++ name ++ " generated unknown formula"

testBinaryOpLowering ::
  forall a b c as bs n.
  ( HasCallStack,
    SupportedPrim a,
    SupportedPrim b,
    SBV.EqSymbolic (SBVType n c),
    Typeable (SBVType n a),
    Typeable (SBVType n b),
    SBV.SymVal as,
    SBV.SymVal bs,
    Show as,
    Show bs,
    SBVType n a ~ SBV.SBV as,
    SBVType n b ~ SBV.SBV bs
  ) =>
  GrisetteSMTConfig n ->
  (Term a -> Term b -> Term c) ->
  String ->
  (SBVType n a -> SBVType n b -> SBVType n c) ->
  Assertion
testBinaryOpLowering config f name sbvfun = do
  let a :: Term a = ssymTerm "a"
  let b :: Term b = ssymTerm "b"
  let fab :: Term c = f a b
  SBV.runSMTWith (sbvConfig config) $ do
    (m, lt) <- lowerSinglePrim config fab
    let sbva :: Maybe (SBVType n a) = M.lookup (SomeTerm a) (biMapToSBV m) >>= fromDynamic
    let sbvb :: Maybe (SBVType n b) = M.lookup (SomeTerm b) (biMapToSBV m) >>= fromDynamic
    case (sbva, sbvb) of
      (Just sbvav, Just sbvbv) -> SBV.query $ do
        SBV.constrain $ lt SBV..== sbvfun sbvav sbvbv
        satres <- SBV.checkSat
        case satres of
          SBV.Sat -> return ()
          _ -> lift $ assertFailure $ "Lowering for " ++ name ++ " generated unsolvable formula"
      _ -> lift $ assertFailure "Failed to extract the term"
  SBV.runSMTWith (sbvConfig config) $ do
    (m, lt) <- lowerSinglePrim config fab
    let sbva :: Maybe (SBVType n a) = M.lookup (SomeTerm a) (biMapToSBV m) >>= fromDynamic
    let sbvb :: Maybe (SBVType n b) = M.lookup (SomeTerm b) (biMapToSBV m) >>= fromDynamic
    case (sbva, sbvb) of
      (Just sbvav, Just sbvbv) -> SBV.query $ do
        SBV.constrain $ lt SBV../= sbvfun sbvav sbvbv
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
  forall a b c d as bs cs n.
  ( HasCallStack,
    SupportedPrim a,
    SupportedPrim b,
    SupportedPrim c,
    SBV.EqSymbolic (SBVType n d),
    Typeable (SBVType n a),
    Typeable (SBVType n b),
    Typeable (SBVType n c),
    SBV.SymVal as,
    SBV.SymVal bs,
    SBV.SymVal cs,
    Show as,
    Show bs,
    Show cs,
    SBVType n a ~ SBV.SBV as,
    SBVType n b ~ SBV.SBV bs,
    SBVType n c ~ SBV.SBV cs
  ) =>
  GrisetteSMTConfig n ->
  (Term a -> Term b -> Term c -> Term Bool) ->
  (Term a -> Term b -> Term c -> Term d) ->
  T.Text ->
  (SBVType n a -> SBVType n b -> SBVType n c -> SBVType n d) ->
  Assertion
testTernaryOpLowering config precond f name sbvfun = do
  let a :: Term a = ssymTerm "a"
  let b :: Term b = ssymTerm "b"
  let c :: Term c = ssymTerm "c"
  let fabc :: Term d = f a b c
  SBV.runSMTWith (sbvConfig config) $ do
    (m, lt) <- lowerSinglePrim config fabc
    let sbva :: Maybe (SBVType n a) = M.lookup (SomeTerm a) (biMapToSBV m) >>= fromDynamic
    let sbvb :: Maybe (SBVType n b) = M.lookup (SomeTerm b) (biMapToSBV m) >>= fromDynamic
    let sbvc :: Maybe (SBVType n c) = M.lookup (SomeTerm c) (biMapToSBV m) >>= fromDynamic
    case (sbva, sbvb, sbvc) of
      (Just sbvav, Just sbvbv, Just sbvcv) -> SBV.query $ do
        SBV.constrain $ lt SBV..== sbvfun sbvav sbvbv sbvcv
        satres <- SBV.checkSat
        case satres of
          SBV.Sat -> return ()
          _ -> lift $ assertFailure $ T.unpack $ "Lowering for " <> name <> " generated unsolvable formula"
      _ -> lift $ assertFailure "Failed to extract the term"
  SBV.runSMTWith (sbvConfig config) $ do
    (m, lt) <- lowerSinglePrim config fabc
    (m2, p) <- lowerSinglePrimCached config (precond a b c) m
    let sbva :: Maybe (SBVType n a) = M.lookup (SomeTerm a) (biMapToSBV m2) >>= fromDynamic
    let sbvb :: Maybe (SBVType n b) = M.lookup (SomeTerm b) (biMapToSBV m2) >>= fromDynamic
    let sbvc :: Maybe (SBVType n c) = M.lookup (SomeTerm c) (biMapToSBV m2) >>= fromDynamic
    case (sbva, sbvb, sbvc) of
      (Just sbvav, Just sbvbv, Just sbvcv) -> SBV.query $ do
        SBV.constrain $ (lt SBV../= sbvfun sbvav sbvbv sbvcv) SBV..&& p
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

loweringTests :: Test
loweringTests =
  let unboundedConfig = precise SBV.z3
      boundedConfig = approx (Proxy @5) SBV.z3
   in testGroup
        "Lowering"
        [ testGroup
            "Bool Lowering"
            [ testCase "Not" $ do
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
            [ testCase "Add" $ do
                testBinaryOpLowering @Integer @Integer @Integer unboundedConfig addNumTerm "(+)" (+)
                testBinaryOpLowering @Integer @Integer @Integer
                  unboundedConfig
                  addNumTerm
                  "(+)"
                  (\x y -> (x + 1) * (y + 1) - x * y - 1)
                testBinaryOpLowering @Integer @Integer @Integer boundedConfig addNumTerm "(+)" (+)
                testBinaryOpLowering @Integer @Integer @Integer
                  boundedConfig
                  addNumTerm
                  "(+)"
                  (\x y -> (x + 1) * (y + 1) - x * y - 1),
              testCase "Uminus" $ do
                testUnaryOpLowering @Integer @Integer unboundedConfig negNumTerm "negate" negate
                testUnaryOpLowering @Integer @Integer
                  unboundedConfig
                  negNumTerm
                  "negate"
                  (\x -> (x + 1) * (x + 1) - 3 * x - x * x - 1)
                testUnaryOpLowering @Integer @Integer boundedConfig negNumTerm "negate" negate
                testUnaryOpLowering @Integer @Integer
                  boundedConfig
                  negNumTerm
                  "negate"
                  (\x -> (x + 1) * (x + 1) - 3 * x - x * x - 1),
              testCase "Abs" $ do
                testUnaryOpLowering @Integer @Integer unboundedConfig absNumTerm "abs" abs
                testUnaryOpLowering @Integer @Integer boundedConfig absNumTerm "abs" abs,
              testCase "Signum" $ do
                testUnaryOpLowering @Integer @Integer unboundedConfig signumNumTerm "signum" signum
                testUnaryOpLowering @Integer @Integer boundedConfig signumNumTerm "signum" signum,
              testCase "Times" $ do
                testBinaryOpLowering @Integer @Integer @Integer unboundedConfig mulNumTerm "(*)" (*)
                testBinaryOpLowering @Integer @Integer @Integer
                  unboundedConfig
                  mulNumTerm
                  "(*)"
                  (\x y -> (x + 1) * (y + 1) - x - y - 1)
                testBinaryOpLowering @Integer @Integer @Integer boundedConfig mulNumTerm "(*)" (*)
                testBinaryOpLowering @Integer @Integer @Integer
                  boundedConfig
                  mulNumTerm
                  "(*)"
                  (\x y -> (x + 1) * (y + 1) - x - y - 1),
              testCase "Lt" $ do
                testBinaryOpLowering @Integer @Integer @Bool unboundedConfig ltOrdTerm "(<)" (SBV..<)
                testBinaryOpLowering @Integer @Integer @Bool
                  unboundedConfig
                  ltOrdTerm
                  "(<)"
                  (\x y -> x * 2 - x SBV..< y * 2 - y)
                testBinaryOpLowering @Integer @Integer @Bool boundedConfig ltOrdTerm "(<)" (SBV..<)
                testBinaryOpLowering @Integer @Integer @Bool
                  boundedConfig
                  ltOrdTerm
                  "(<=)"
                  (\x y -> x * 2 - x SBV..< y * 2 - y),
              testCase "Le" $ do
                testBinaryOpLowering @Integer @Integer @Bool unboundedConfig leOrdTerm "(<=)" (SBV..<=)
                testBinaryOpLowering @Integer @Integer @Bool
                  unboundedConfig
                  leOrdTerm
                  "(<=)"
                  (\x y -> x * 2 - x SBV..<= y * 2 - y)
                testBinaryOpLowering @Integer @Integer @Bool boundedConfig leOrdTerm "(<=)" (SBV..<=)
                testBinaryOpLowering @Integer @Integer @Bool
                  boundedConfig
                  leOrdTerm
                  "(<=)"
                  (\x y -> x * 2 - x SBV..<= y * 2 - y),
              testCase "Div" $ do
                testBinaryOpLowering @Integer @Integer @Integer unboundedConfig divIntegralTerm "div" SBV.sDiv
                testBinaryOpLowering @Integer @Integer @Integer boundedConfig divIntegralTerm "div" SBV.sDiv,
              testCase "Mod" $ do
                testBinaryOpLowering @Integer @Integer @Integer unboundedConfig modIntegralTerm "mod" SBV.sMod
                testBinaryOpLowering @Integer @Integer @Integer boundedConfig modIntegralTerm "mod" SBV.sMod,
              testCase "Quot" $ do
                testBinaryOpLowering @Integer @Integer @Integer unboundedConfig quotIntegralTerm "quot" SBV.sQuot
                testBinaryOpLowering @Integer @Integer @Integer boundedConfig quotIntegralTerm "quot" SBV.sQuot,
              testCase "Rem" $ do
                testBinaryOpLowering @Integer @Integer @Integer unboundedConfig remIntegralTerm "rem" SBV.sRem
                testBinaryOpLowering @Integer @Integer @Integer boundedConfig remIntegralTerm "rem" SBV.sRem
            ],
          testGroup
            "IntN Lowering"
            [ testCase "Add" $ do
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
                  (bvselectTerm (Proxy @0) (Proxy @1))
                  "select"
                  (SBV.bvExtract @0 @0 @5 Proxy Proxy)
                testUnaryOpLowering @(IntN 5) @(IntN 1)
                  unboundedConfig
                  (bvselectTerm (Proxy @1) (Proxy @1))
                  "select"
                  (SBV.bvExtract @1 @1 @5 Proxy Proxy)
                testUnaryOpLowering @(IntN 5) @(IntN 1)
                  unboundedConfig
                  (bvselectTerm (Proxy @2) (Proxy @1))
                  "select"
                  (SBV.bvExtract @2 @2 @5 Proxy Proxy)
                testUnaryOpLowering @(IntN 5) @(IntN 1)
                  unboundedConfig
                  (bvselectTerm (Proxy @3) (Proxy @1))
                  "select"
                  (SBV.bvExtract @3 @3 @5 Proxy Proxy)
                testUnaryOpLowering @(IntN 5) @(IntN 1)
                  unboundedConfig
                  (bvselectTerm (Proxy @4) (Proxy @1))
                  "select"
                  (SBV.bvExtract @4 @4 @5 Proxy Proxy)
                testUnaryOpLowering @(IntN 5) @(IntN 2)
                  unboundedConfig
                  (bvselectTerm (Proxy @0) (Proxy @2))
                  "select"
                  (SBV.bvExtract @1 @0 @5 Proxy Proxy)
                testUnaryOpLowering @(IntN 5) @(IntN 2)
                  unboundedConfig
                  (bvselectTerm (Proxy @1) (Proxy @2))
                  "select"
                  (SBV.bvExtract @2 @1 @5 Proxy Proxy)
                testUnaryOpLowering @(IntN 5) @(IntN 2)
                  unboundedConfig
                  (bvselectTerm (Proxy @2) (Proxy @2))
                  "select"
                  (SBV.bvExtract @3 @2 @5 Proxy Proxy)
                testUnaryOpLowering @(IntN 5) @(IntN 2)
                  unboundedConfig
                  (bvselectTerm (Proxy @3) (Proxy @2))
                  "select"
                  (SBV.bvExtract @4 @3 @5 Proxy Proxy)
                testUnaryOpLowering @(IntN 5) @(IntN 3)
                  unboundedConfig
                  (bvselectTerm (Proxy @0) (Proxy @3))
                  "select"
                  (SBV.bvExtract @2 @0 @5 Proxy Proxy)
                testUnaryOpLowering @(IntN 5) @(IntN 3)
                  unboundedConfig
                  (bvselectTerm (Proxy @1) (Proxy @3))
                  "select"
                  (SBV.bvExtract @3 @1 @5 Proxy Proxy)
                testUnaryOpLowering @(IntN 5) @(IntN 3)
                  unboundedConfig
                  (bvselectTerm (Proxy @2) (Proxy @3))
                  "select"
                  (SBV.bvExtract @4 @2 @5 Proxy Proxy)
                testUnaryOpLowering @(IntN 5) @(IntN 4)
                  unboundedConfig
                  (bvselectTerm (Proxy @0) (Proxy @4))
                  "select"
                  (SBV.bvExtract @3 @0 @5 Proxy Proxy)
                testUnaryOpLowering @(IntN 5) @(IntN 4)
                  unboundedConfig
                  (bvselectTerm (Proxy @1) (Proxy @4))
                  "select"
                  (SBV.bvExtract @4 @1 @5 Proxy Proxy)
                testUnaryOpLowering @(IntN 5) @(IntN 5)
                  unboundedConfig
                  (bvselectTerm (Proxy @0) (Proxy @5))
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
                  bvconcatTerm
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
                testBinaryOpLowering @(IntN 5) @(IntN 5) @(IntN 5) unboundedConfig divIntegralTerm "div" SBV.sDiv
                testBinaryOpLowering @(IntN 5) @(IntN 5) @(IntN 5) boundedConfig divIntegralTerm "div" SBV.sDiv,
              testCase "Mod - bounded" $ do
                testBinaryOpLowering @(IntN 5) @(IntN 5) @(IntN 5) unboundedConfig modIntegralTerm "mod" SBV.sMod
                testBinaryOpLowering @(IntN 5) @(IntN 5) @(IntN 5) boundedConfig modIntegralTerm "mod" SBV.sMod,
              testCase "Quot - bounded" $ do
                testBinaryOpLowering @(IntN 5) @(IntN 5) @(IntN 5) unboundedConfig quotIntegralTerm "quot" SBV.sQuot
                testBinaryOpLowering @(IntN 5) @(IntN 5) @(IntN 5) boundedConfig quotIntegralTerm "quot" SBV.sQuot,
              testCase "Rem - bounded" $ do
                testBinaryOpLowering @(IntN 5) @(IntN 5) @(IntN 5) unboundedConfig remIntegralTerm "rem" SBV.sRem
                testBinaryOpLowering @(IntN 5) @(IntN 5) @(IntN 5) boundedConfig remIntegralTerm "rem" SBV.sRem,
              testCase "ToUnsigned" $ do
                testUnaryOpLowering @(IntN 5) @(WordN 5) unboundedConfig toUnsignedTerm "toUnsigned" SBV.sFromIntegral
                testUnaryOpLowering @(IntN 5) @(WordN 5) boundedConfig toUnsignedTerm "toUnsigned" SBV.sFromIntegral
            ],
          testGroup
            "WordN"
            [ testCase "Add" $ do
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
                  (bvselectTerm (Proxy @0) (Proxy @1))
                  "select"
                  (SBV.bvExtract @0 @0 @5 Proxy Proxy)
                testUnaryOpLowering @(WordN 5) @(WordN 1)
                  unboundedConfig
                  (bvselectTerm (Proxy @1) (Proxy @1))
                  "select"
                  (SBV.bvExtract @1 @1 @5 Proxy Proxy)
                testUnaryOpLowering @(WordN 5) @(WordN 1)
                  unboundedConfig
                  (bvselectTerm (Proxy @2) (Proxy @1))
                  "select"
                  (SBV.bvExtract @2 @2 @5 Proxy Proxy)
                testUnaryOpLowering @(WordN 5) @(WordN 1)
                  unboundedConfig
                  (bvselectTerm (Proxy @3) (Proxy @1))
                  "select"
                  (SBV.bvExtract @3 @3 @5 Proxy Proxy)
                testUnaryOpLowering @(WordN 5) @(WordN 1)
                  unboundedConfig
                  (bvselectTerm (Proxy @4) (Proxy @1))
                  "select"
                  (SBV.bvExtract @4 @4 @5 Proxy Proxy)
                testUnaryOpLowering @(WordN 5) @(WordN 2)
                  unboundedConfig
                  (bvselectTerm (Proxy @0) (Proxy @2))
                  "select"
                  (SBV.bvExtract @1 @0 @5 Proxy Proxy)
                testUnaryOpLowering @(WordN 5) @(WordN 2)
                  unboundedConfig
                  (bvselectTerm (Proxy @1) (Proxy @2))
                  "select"
                  (SBV.bvExtract @2 @1 @5 Proxy Proxy)
                testUnaryOpLowering @(WordN 5) @(WordN 2)
                  unboundedConfig
                  (bvselectTerm (Proxy @2) (Proxy @2))
                  "select"
                  (SBV.bvExtract @3 @2 @5 Proxy Proxy)
                testUnaryOpLowering @(WordN 5) @(WordN 2)
                  unboundedConfig
                  (bvselectTerm (Proxy @3) (Proxy @2))
                  "select"
                  (SBV.bvExtract @4 @3 @5 Proxy Proxy)
                testUnaryOpLowering @(WordN 5) @(WordN 3)
                  unboundedConfig
                  (bvselectTerm (Proxy @0) (Proxy @3))
                  "select"
                  (SBV.bvExtract @2 @0 @5 Proxy Proxy)
                testUnaryOpLowering @(WordN 5) @(WordN 3)
                  unboundedConfig
                  (bvselectTerm (Proxy @1) (Proxy @3))
                  "select"
                  (SBV.bvExtract @3 @1 @5 Proxy Proxy)
                testUnaryOpLowering @(WordN 5) @(WordN 3)
                  unboundedConfig
                  (bvselectTerm (Proxy @2) (Proxy @3))
                  "select"
                  (SBV.bvExtract @4 @2 @5 Proxy Proxy)
                testUnaryOpLowering @(WordN 5) @(WordN 4)
                  unboundedConfig
                  (bvselectTerm (Proxy @0) (Proxy @4))
                  "select"
                  (SBV.bvExtract @3 @0 @5 Proxy Proxy)
                testUnaryOpLowering @(WordN 5) @(WordN 4)
                  unboundedConfig
                  (bvselectTerm (Proxy @1) (Proxy @4))
                  "select"
                  (SBV.bvExtract @4 @1 @5 Proxy Proxy)
                testUnaryOpLowering @(WordN 5) @(WordN 5)
                  unboundedConfig
                  (bvselectTerm (Proxy @0) (Proxy @5))
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
                  bvconcatTerm
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
                testBinaryOpLowering @(WordN 5) @(WordN 5) @(WordN 5) unboundedConfig divIntegralTerm "div" SBV.sDiv
                testBinaryOpLowering @(WordN 5) @(WordN 5) @(WordN 5) boundedConfig divIntegralTerm "div" SBV.sDiv,
              testCase "Mod" $ do
                testBinaryOpLowering @(WordN 5) @(WordN 5) @(WordN 5) unboundedConfig modIntegralTerm "mod" SBV.sMod
                testBinaryOpLowering @(WordN 5) @(WordN 5) @(WordN 5) boundedConfig modIntegralTerm "mod" SBV.sMod,
              testCase "Quot" $ do
                testBinaryOpLowering @(WordN 5) @(WordN 5) @(WordN 5) unboundedConfig quotIntegralTerm "quot" SBV.sQuot
                testBinaryOpLowering @(WordN 5) @(WordN 5) @(WordN 5) boundedConfig quotIntegralTerm "quot" SBV.sQuot,
              testCase "Rem" $ do
                testBinaryOpLowering @(WordN 5) @(WordN 5) @(WordN 5) unboundedConfig remIntegralTerm "rem" SBV.sRem
                testBinaryOpLowering @(WordN 5) @(WordN 5) @(WordN 5) boundedConfig remIntegralTerm "rem" SBV.sRem,
              testCase "ToSigned" $ do
                testUnaryOpLowering @(WordN 5) @(IntN 5) unboundedConfig toSignedTerm "toSigned" SBV.sFromIntegral
                testUnaryOpLowering @(WordN 5) @(IntN 5) boundedConfig toSignedTerm "toSigned" SBV.sFromIntegral
            ],
          testGroup
            "FP"
            [ testCase "Eqv" $
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
                    op
            ],
          testCase "TabularFun" $ do
            let f = "f" :: SymInteger =~> SymInteger =~> SymInteger
            let a = "a" :: SymInteger
            let b = "b" :: SymInteger
            let c = "c" :: SymInteger
            let d = "d" :: SymInteger
            Right m <-
              solve unboundedConfig $
                (f # a # b .== a + b .&& a .== 10 .&& b .== 20)
                  .&& (f # a # c .== a + c .&& a .== 10 .&& c .== 30)
                  .&& (f # a # d .== a + d .&& a .== 10 .&& d .== 40)
            evaluateSym False m (f # a # b .== a + b) @?= con True
            evaluateSym False m (f # a # c .== a + c) @?= con True
            evaluateSym False m (f # a # d .== a + d) @?= con True,
          testCase "GeneralFun" $ do
            let f = "f" :: SymInteger -~> SymInteger -~> SymInteger
            let a = "a" :: SymInteger
            let b = "b" :: SymInteger
            let c = "c" :: SymInteger
            let d = "d" :: SymInteger
            Right m <-
              solve unboundedConfig $
                (f # a # b .== a + b .&& a .== 10 .&& b .== 20)
                  .&& (f # a # c .== a + c .&& a .== 10 .&& c .== 30)
                  .&& (f # a # d .== a + d .&& a .== 10 .&& d .== 40)
            evaluateSym False m (f # a # b .== a + b) @?= con True
            evaluateSym False m (f # a # c .== a + c) @?= con True
            evaluateSym False m (f # a # d .== a + d) @?= con True
        ]
