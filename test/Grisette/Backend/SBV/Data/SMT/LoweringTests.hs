{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Grisette.Backend.SBV.Data.SMT.LoweringTests (loweringTests) where

import Control.Monad.Trans (MonadTrans (lift))
import Data.Bits
  ( Bits (complement, rotate, shift, xor, (.&.), (.|.)),
  )
import Data.Dynamic (Typeable, fromDynamic)
import qualified Data.HashMap.Strict as M
import Data.Proxy (Proxy (Proxy))
import qualified Data.SBV as SBV
import qualified Data.SBV.Control as SBV
import GHC.Stack (HasCallStack)
import Grisette.Backend.SBV.Data.SMT.Lowering (lowerSinglePrim)
import Grisette.Backend.SBV.Data.SMT.Solving
  ( GrisetteSMTConfig (sbvConfig),
    TermTy,
    approx,
    precise,
  )
import Grisette.Backend.SBV.Data.SMT.SymBiMap
  ( SymBiMap (biMapToSBV),
  )
import Grisette.Core.Data.BV (IntN, WordN)
import Grisette.IR.SymPrim.Data.Prim.InternedTerm.InternedCtors
  ( absNumTerm,
    addNumTerm,
    andBitsTerm,
    andTerm,
    bvToSignedTerm,
    bvToUnsignedTerm,
    bvconcatTerm,
    bvselectTerm,
    bvsignExtendTerm,
    bvzeroExtendTerm,
    complementBitsTerm,
    divBoundedIntegralTerm,
    divIntegralTerm,
    eqvTerm,
    iteTerm,
    leNumTerm,
    ltNumTerm,
    modBoundedIntegralTerm,
    modIntegralTerm,
    notTerm,
    orBitsTerm,
    orTerm,
    quotBoundedIntegralTerm,
    quotIntegralTerm,
    remBoundedIntegralTerm,
    remIntegralTerm,
    rotateBitsTerm,
    shiftBitsTerm,
    signumNumTerm,
    ssymTerm,
    timesNumTerm,
    uminusNumTerm,
    xorBitsTerm,
  )
import Grisette.IR.SymPrim.Data.Prim.InternedTerm.SomeTerm
  ( SomeTerm (SomeTerm),
  )
import Grisette.IR.SymPrim.Data.Prim.InternedTerm.Term
  ( SupportedPrim,
    Term,
  )
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (Assertion, assertFailure)

testUnaryOpLowering ::
  forall a b as n.
  ( HasCallStack,
    SupportedPrim a,
    SBV.EqSymbolic (TermTy n b),
    Typeable (TermTy n a),
    SBV.SymVal as,
    TermTy n a ~ SBV.SBV as,
    Show as
  ) =>
  GrisetteSMTConfig n ->
  (Term a -> Term b) ->
  String ->
  (TermTy n a -> TermTy n b) ->
  Assertion
testUnaryOpLowering config f name sbvfun = do
  let a :: Term a = ssymTerm "a"
  let fa :: Term b = f a
  SBV.runSMTWith (sbvConfig config) $ do
    (m, lt) <- lowerSinglePrim config fa
    let sbva :: Maybe (TermTy n a) = M.lookup (SomeTerm a) (biMapToSBV m) >>= fromDynamic
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
    let sbvv :: Maybe (TermTy n a) = M.lookup (SomeTerm a) (biMapToSBV m) >>= fromDynamic
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

-- testUnaryOpLowering' ::
--   forall a b as n tag.
--   ( HasCallStack,
--     UnaryOp tag a b,
--     SBV.EqSymbolic (TermTy n b),
--     Typeable (TermTy n a),
--     SBV.SymVal as,
--     TermTy n a ~ SBV.SBV as,
--     Show as
--   ) =>
--   GrisetteSMTConfig n ->
--   tag ->
--   (TermTy n a -> TermTy n b) ->
--   Assertion
-- testUnaryOpLowering' config t = testUnaryOpLowering @a @b @as config (constructUnary t) (show t)

testBinaryOpLowering ::
  forall a b c as bs n.
  ( HasCallStack,
    SupportedPrim a,
    SupportedPrim b,
    SBV.EqSymbolic (TermTy n c),
    Typeable (TermTy n a),
    Typeable (TermTy n b),
    SBV.SymVal as,
    SBV.SymVal bs,
    Show as,
    Show bs,
    TermTy n a ~ SBV.SBV as,
    TermTy n b ~ SBV.SBV bs
  ) =>
  GrisetteSMTConfig n ->
  (Term a -> Term b -> Term c) ->
  String ->
  (TermTy n a -> TermTy n b -> TermTy n c) ->
  Assertion
testBinaryOpLowering config f name sbvfun = do
  let a :: Term a = ssymTerm "a"
  let b :: Term b = ssymTerm "b"
  let fab :: Term c = f a b
  SBV.runSMTWith (sbvConfig config) $ do
    (m, lt) <- lowerSinglePrim config fab
    let sbva :: Maybe (TermTy n a) = M.lookup (SomeTerm a) (biMapToSBV m) >>= fromDynamic
    let sbvb :: Maybe (TermTy n b) = M.lookup (SomeTerm b) (biMapToSBV m) >>= fromDynamic
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
    let sbva :: Maybe (TermTy n a) = M.lookup (SomeTerm a) (biMapToSBV m) >>= fromDynamic
    let sbvb :: Maybe (TermTy n b) = M.lookup (SomeTerm b) (biMapToSBV m) >>= fromDynamic
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

-- testBinaryOpLowering' ::
--   forall a b c as bs n tag.
--   ( HasCallStack,
--     BinaryOp tag a b c,
--     SBV.EqSymbolic (TermTy n c),
--     Typeable (TermTy n a),
--     Typeable (TermTy n b),
--     SBV.SymVal as,
--     SBV.SymVal bs,
--     Show as,
--     Show bs,
--     TermTy n a ~ SBV.SBV as,
--     TermTy n b ~ SBV.SBV bs
--   ) =>
--   GrisetteSMTConfig n ->
--   tag ->
--   (TermTy n a -> TermTy n b -> TermTy n c) ->
--   Assertion
-- testBinaryOpLowering' config t = testBinaryOpLowering @a @b @c @as @bs config (constructBinary t) (show t)

testTernaryOpLowering ::
  forall a b c d as bs cs n.
  ( HasCallStack,
    SupportedPrim a,
    SupportedPrim b,
    SupportedPrim c,
    SBV.EqSymbolic (TermTy n d),
    Typeable (TermTy n a),
    Typeable (TermTy n b),
    Typeable (TermTy n c),
    SBV.SymVal as,
    SBV.SymVal bs,
    SBV.SymVal cs,
    Show as,
    Show bs,
    Show cs,
    TermTy n a ~ SBV.SBV as,
    TermTy n b ~ SBV.SBV bs,
    TermTy n c ~ SBV.SBV cs
  ) =>
  GrisetteSMTConfig n ->
  (Term a -> Term b -> Term c -> Term d) ->
  String ->
  (TermTy n a -> TermTy n b -> TermTy n c -> TermTy n d) ->
  Assertion
testTernaryOpLowering config f name sbvfun = do
  let a :: Term a = ssymTerm "a"
  let b :: Term b = ssymTerm "b"
  let c :: Term c = ssymTerm "c"
  let fabc :: Term d = f a b c
  SBV.runSMTWith (sbvConfig config) $ do
    (m, lt) <- lowerSinglePrim config fabc
    let sbva :: Maybe (TermTy n a) = M.lookup (SomeTerm a) (biMapToSBV m) >>= fromDynamic
    let sbvb :: Maybe (TermTy n b) = M.lookup (SomeTerm b) (biMapToSBV m) >>= fromDynamic
    let sbvc :: Maybe (TermTy n c) = M.lookup (SomeTerm c) (biMapToSBV m) >>= fromDynamic
    case (sbva, sbvb, sbvc) of
      (Just sbvav, Just sbvbv, Just sbvcv) -> SBV.query $ do
        SBV.constrain $ lt SBV..== sbvfun sbvav sbvbv sbvcv
        satres <- SBV.checkSat
        case satres of
          SBV.Sat -> return ()
          _ -> lift $ assertFailure $ "Lowering for " ++ name ++ " generated unsolvable formula"
      _ -> lift $ assertFailure "Failed to extract the term"
  SBV.runSMTWith (sbvConfig config) $ do
    (m, lt) <- lowerSinglePrim config fabc
    let sbva :: Maybe (TermTy n a) = M.lookup (SomeTerm a) (biMapToSBV m) >>= fromDynamic
    let sbvb :: Maybe (TermTy n b) = M.lookup (SomeTerm b) (biMapToSBV m) >>= fromDynamic
    let sbvc :: Maybe (TermTy n c) = M.lookup (SomeTerm c) (biMapToSBV m) >>= fromDynamic
    case (sbva, sbvb, sbvc) of
      (Just sbvav, Just sbvbv, Just sbvcv) -> SBV.query $ do
        SBV.constrain $ lt SBV../= sbvfun sbvav sbvbv sbvcv
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
          _ -> lift $ assertFailure $ "Lowering for " ++ name ++ " generated unknown formula"
      _ -> lift $ assertFailure "Failed to extract the term"

-- testTernaryOpLowering' ::
--   forall a b c d as bs cs n tag.
--   ( HasCallStack,
--     TernaryOp tag a b c d,
--     SBV.EqSymbolic (TermTy n d),
--     Typeable (TermTy n a),
--     Typeable (TermTy n b),
--     Typeable (TermTy n c),
--     SBV.SymVal as,
--     SBV.SymVal bs,
--     SBV.SymVal cs,
--     Show as,
--     Show bs,
--     Show cs,
--     TermTy n a ~ SBV.SBV as,
--     TermTy n b ~ SBV.SBV bs,
--     TermTy n c ~ SBV.SBV cs
--   ) =>
--   GrisetteSMTConfig n ->
--   tag ->
--   (TermTy n a -> TermTy n b -> TermTy n c -> TermTy n d) ->
--   Assertion
-- testTernaryOpLowering' config t = testTernaryOpLowering @a @b @c @d @as @bs @cs config (constructTernary t) (show t)

loweringTests :: Test
loweringTests =
  let unboundedConfig = precise SBV.z3
      boundedConfig = approx (Proxy @5) SBV.z3
   in testGroup
        "LoweringTests"
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
                testBinaryOpLowering @Bool @Bool @Bool unboundedConfig eqvTerm "eqv" (SBV..==)
                testBinaryOpLowering @Bool @Bool @Bool
                  unboundedConfig
                  eqvTerm
                  "eqv"
                  (\x y -> SBV.sNot (x SBV..<+> y)),
              testCase "ITE" $ do
                testTernaryOpLowering @Bool @Bool @Bool @Bool unboundedConfig iteTerm "ite" SBV.ite
                testTernaryOpLowering @Bool @Bool @Bool @Bool
                  unboundedConfig
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
                testUnaryOpLowering @Integer @Integer unboundedConfig uminusNumTerm "negate" negate
                testUnaryOpLowering @Integer @Integer
                  unboundedConfig
                  uminusNumTerm
                  "negate"
                  (\x -> (x + 1) * (x + 1) - 3 * x - x * x - 1)
                testUnaryOpLowering @Integer @Integer boundedConfig uminusNumTerm "negate" negate
                testUnaryOpLowering @Integer @Integer
                  boundedConfig
                  uminusNumTerm
                  "negate"
                  (\x -> (x + 1) * (x + 1) - 3 * x - x * x - 1),
              testCase "Abs" $ do
                testUnaryOpLowering @Integer @Integer unboundedConfig absNumTerm "abs" abs
                testUnaryOpLowering @Integer @Integer boundedConfig absNumTerm "abs" abs,
              testCase "Signum" $ do
                testUnaryOpLowering @Integer @Integer unboundedConfig signumNumTerm "signum" signum
                testUnaryOpLowering @Integer @Integer boundedConfig signumNumTerm "signum" signum,
              testCase "Times" $ do
                testBinaryOpLowering @Integer @Integer @Integer unboundedConfig timesNumTerm "(*)" (*)
                testBinaryOpLowering @Integer @Integer @Integer
                  unboundedConfig
                  timesNumTerm
                  "(*)"
                  (\x y -> (x + 1) * (y + 1) - x - y - 1)
                testBinaryOpLowering @Integer @Integer @Integer boundedConfig timesNumTerm "(*)" (*)
                testBinaryOpLowering @Integer @Integer @Integer
                  boundedConfig
                  timesNumTerm
                  "(*)"
                  (\x y -> (x + 1) * (y + 1) - x - y - 1),
              testCase "Lt" $ do
                testBinaryOpLowering @Integer @Integer @Bool unboundedConfig ltNumTerm "(<)" (SBV..<)
                testBinaryOpLowering @Integer @Integer @Bool
                  unboundedConfig
                  ltNumTerm
                  "(<)"
                  (\x y -> x * 2 - x SBV..< y * 2 - y)
                testBinaryOpLowering @Integer @Integer @Bool boundedConfig ltNumTerm "(<)" (SBV..<)
                testBinaryOpLowering @Integer @Integer @Bool
                  boundedConfig
                  ltNumTerm
                  "(<=)"
                  (\x y -> x * 2 - x SBV..< y * 2 - y),
              testCase "Le" $ do
                testBinaryOpLowering @Integer @Integer @Bool unboundedConfig leNumTerm "(<=)" (SBV..<=)
                testBinaryOpLowering @Integer @Integer @Bool
                  unboundedConfig
                  leNumTerm
                  "(<=)"
                  (\x y -> x * 2 - x SBV..<= y * 2 - y)
                testBinaryOpLowering @Integer @Integer @Bool boundedConfig leNumTerm "(<=)" (SBV..<=)
                testBinaryOpLowering @Integer @Integer @Bool
                  boundedConfig
                  leNumTerm
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
                testUnaryOpLowering @(IntN 5) unboundedConfig uminusNumTerm "negate" negate
                testUnaryOpLowering @(IntN 5)
                  unboundedConfig
                  uminusNumTerm
                  "negate"
                  (\x -> (x + 1) * (x + 1) - 3 * x - x * x - 1),
              testCase "Abs" $ do
                testUnaryOpLowering @(IntN 5) unboundedConfig absNumTerm "abs" abs,
              testCase "Signum" $ do
                testUnaryOpLowering @(IntN 5) unboundedConfig signumNumTerm "signum" signum,
              testCase "Times" $ do
                testBinaryOpLowering @(IntN 5) @(IntN 5) unboundedConfig timesNumTerm "(*)" (*)
                testBinaryOpLowering @(IntN 5) @(IntN 5)
                  unboundedConfig
                  timesNumTerm
                  "(*)"
                  (\x y -> (x + 1) * (y + 1) - x - y - 1),
              testCase "Lt" $ do
                testBinaryOpLowering @(IntN 5) @(IntN 5) unboundedConfig ltNumTerm "(<)" (SBV..<)
                testBinaryOpLowering @(IntN 5) @(IntN 5)
                  unboundedConfig
                  ltNumTerm
                  "(<)"
                  (\x y -> x * 2 - x SBV..< y * 2 - y),
              testCase "Le" $ do
                testBinaryOpLowering @(IntN 5) @(IntN 5) unboundedConfig leNumTerm "(<=)" (SBV..<=)
                testBinaryOpLowering @(IntN 5) @(IntN 5)
                  unboundedConfig
                  leNumTerm
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
              testCase "ShiftBits" $ do
                testUnaryOpLowering @(IntN 5) unboundedConfig (`shiftBitsTerm` 0) "shift" id
                testUnaryOpLowering @(IntN 5) unboundedConfig (`shiftBitsTerm` 1) "shift" (`shift` 1)
                testUnaryOpLowering @(IntN 5) unboundedConfig (`shiftBitsTerm` 2) "shift" (`shift` 2)
                testUnaryOpLowering @(IntN 5) unboundedConfig (`shiftBitsTerm` 3) "shift" (`shift` 3)
                testUnaryOpLowering @(IntN 5) unboundedConfig (`shiftBitsTerm` 4) "shift" (`shift` 4)
                testUnaryOpLowering @(IntN 5) unboundedConfig (`shiftBitsTerm` 5) "shift" (`shift` 5)
                testUnaryOpLowering @(IntN 5) unboundedConfig (`shiftBitsTerm` 5) "shift" (const 0)
                testUnaryOpLowering @(IntN 5) unboundedConfig (`shiftBitsTerm` (-1)) "shift" (`shift` (-1))
                testUnaryOpLowering @(IntN 5) unboundedConfig (`shiftBitsTerm` (-2)) "shift" (`shift` (-2))
                testUnaryOpLowering @(IntN 5) unboundedConfig (`shiftBitsTerm` (-3)) "shift" (`shift` (-3))
                testUnaryOpLowering @(IntN 5) unboundedConfig (`shiftBitsTerm` (-4)) "shift" (`shift` (-4))
                testUnaryOpLowering @(IntN 5) unboundedConfig (`shiftBitsTerm` (-5)) "shift" (`shift` (-5))
                testUnaryOpLowering @(IntN 5)
                  unboundedConfig
                  (`shiftBitsTerm` (-5))
                  "shift"
                  (\x -> SBV.ite (x SBV..>= 0) 0 (-1)),
              testCase "RotateBits" $ do
                testUnaryOpLowering @(IntN 5) unboundedConfig (`rotateBitsTerm` 0) "rotate" id
                testUnaryOpLowering @(IntN 5) unboundedConfig (`rotateBitsTerm` 1) "rotate" (`rotate` 1)
                testUnaryOpLowering @(IntN 5) unboundedConfig (`rotateBitsTerm` 2) "rotate" (`rotate` 2)
                testUnaryOpLowering @(IntN 5) unboundedConfig (`rotateBitsTerm` 3) "rotate" (`rotate` 3)
                testUnaryOpLowering @(IntN 5) unboundedConfig (`rotateBitsTerm` 4) "rotate" (`rotate` 4)
                testUnaryOpLowering @(IntN 5) unboundedConfig (`rotateBitsTerm` 5) "rotate" (`rotate` 5)
                testUnaryOpLowering @(IntN 5) unboundedConfig (`rotateBitsTerm` 5) "rotate" id
                testUnaryOpLowering @(IntN 5) unboundedConfig (`rotateBitsTerm` (-1)) "rotate" (`rotate` (-1))
                testUnaryOpLowering @(IntN 5) unboundedConfig (`rotateBitsTerm` (-1)) "rotate" (`rotate` 4)
                testUnaryOpLowering @(IntN 5) unboundedConfig (`rotateBitsTerm` (-2)) "rotate" (`rotate` (-2))
                testUnaryOpLowering @(IntN 5) unboundedConfig (`rotateBitsTerm` (-2)) "rotate" (`rotate` 3)
                testUnaryOpLowering @(IntN 5) unboundedConfig (`rotateBitsTerm` (-3)) "rotate" (`rotate` (-3))
                testUnaryOpLowering @(IntN 5) unboundedConfig (`rotateBitsTerm` (-3)) "rotate" (`rotate` 2)
                testUnaryOpLowering @(IntN 5) unboundedConfig (`rotateBitsTerm` (-4)) "rotate" (`rotate` (-4))
                testUnaryOpLowering @(IntN 5) unboundedConfig (`rotateBitsTerm` (-4)) "rotate" (`rotate` 1)
                testUnaryOpLowering @(IntN 5) unboundedConfig (`rotateBitsTerm` (-5)) "rotate" (`rotate` (-5))
                testUnaryOpLowering @(IntN 5) unboundedConfig (`rotateBitsTerm` (-5)) "rotate" id,
              testCase "Div - bounded" $ do
                testBinaryOpLowering @(IntN 5) @(IntN 5) @(IntN 5) unboundedConfig divBoundedIntegralTerm "div" SBV.sDiv
                testBinaryOpLowering @(IntN 5) @(IntN 5) @(IntN 5) boundedConfig divBoundedIntegralTerm "div" SBV.sDiv,
              testCase "Mod - bounded" $ do
                testBinaryOpLowering @(IntN 5) @(IntN 5) @(IntN 5) unboundedConfig modBoundedIntegralTerm "mod" SBV.sMod
                testBinaryOpLowering @(IntN 5) @(IntN 5) @(IntN 5) boundedConfig modBoundedIntegralTerm "mod" SBV.sMod,
              testCase "Quot - bounded" $ do
                testBinaryOpLowering @(IntN 5) @(IntN 5) @(IntN 5) unboundedConfig quotBoundedIntegralTerm "quot" SBV.sQuot
                testBinaryOpLowering @(IntN 5) @(IntN 5) @(IntN 5) boundedConfig quotBoundedIntegralTerm "quot" SBV.sQuot,
              testCase "Rem - bounded" $ do
                testBinaryOpLowering @(IntN 5) @(IntN 5) @(IntN 5) unboundedConfig remBoundedIntegralTerm "rem" SBV.sRem
                testBinaryOpLowering @(IntN 5) @(IntN 5) @(IntN 5) boundedConfig remBoundedIntegralTerm "rem" SBV.sRem,
              testCase "ToUnsigned" $ do
                testUnaryOpLowering @(IntN 5) @(WordN 5) unboundedConfig bvToUnsignedTerm "toUnsigned" SBV.sFromIntegral
                testUnaryOpLowering @(IntN 5) @(WordN 5) boundedConfig bvToUnsignedTerm "toUnsigned" SBV.sFromIntegral
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
                testUnaryOpLowering @(WordN 5) unboundedConfig uminusNumTerm "negate" negate
                testUnaryOpLowering @(WordN 5)
                  unboundedConfig
                  uminusNumTerm
                  "negate"
                  (\x -> (x + 1) * (x + 1) - 3 * x - x * x - 1),
              testCase "Abs" $ do
                testUnaryOpLowering @(WordN 5) unboundedConfig absNumTerm "abs" abs,
              testCase "Signum" $ do
                testUnaryOpLowering @(WordN 5) unboundedConfig signumNumTerm "signum" signum,
              testCase "Times" $ do
                testBinaryOpLowering @(WordN 5) @(WordN 5) unboundedConfig timesNumTerm "(*)" (*)
                testBinaryOpLowering @(WordN 5) @(WordN 5)
                  unboundedConfig
                  timesNumTerm
                  "(*)"
                  (\x y -> (x + 1) * (y + 1) - x - y - 1),
              testCase "Lt" $ do
                testBinaryOpLowering @(WordN 5) @(WordN 5) unboundedConfig ltNumTerm "(<)" (SBV..<)
                testBinaryOpLowering @(WordN 5) @(WordN 5)
                  unboundedConfig
                  ltNumTerm
                  "(<)"
                  (\x y -> x * 2 - x SBV..< y * 2 - y),
              testCase "Le" $ do
                testBinaryOpLowering @(WordN 5) @(WordN 5) unboundedConfig leNumTerm "(<=)" (SBV..<=)
                testBinaryOpLowering @(WordN 5) @(WordN 5)
                  unboundedConfig
                  leNumTerm
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
              testCase "ShiftBits" $ do
                testUnaryOpLowering @(WordN 5) unboundedConfig (`shiftBitsTerm` 0) "shift" id
                testUnaryOpLowering @(WordN 5) unboundedConfig (`shiftBitsTerm` 1) "shift" (`shift` 1)
                testUnaryOpLowering @(WordN 5) unboundedConfig (`shiftBitsTerm` 2) "shift" (`shift` 2)
                testUnaryOpLowering @(WordN 5) unboundedConfig (`shiftBitsTerm` 3) "shift" (`shift` 3)
                testUnaryOpLowering @(WordN 5) unboundedConfig (`shiftBitsTerm` 4) "shift" (`shift` 4)
                testUnaryOpLowering @(WordN 5) unboundedConfig (`shiftBitsTerm` 5) "shift" (`shift` 5)
                testUnaryOpLowering @(WordN 5) unboundedConfig (`shiftBitsTerm` 5) "shift" (const 0)
                testUnaryOpLowering @(WordN 5) unboundedConfig (`shiftBitsTerm` (-1)) "shift" (`shift` (-1))
                testUnaryOpLowering @(WordN 5) unboundedConfig (`shiftBitsTerm` (-2)) "shift" (`shift` (-2))
                testUnaryOpLowering @(WordN 5) unboundedConfig (`shiftBitsTerm` (-3)) "shift" (`shift` (-3))
                testUnaryOpLowering @(WordN 5) unboundedConfig (`shiftBitsTerm` (-4)) "shift" (`shift` (-4))
                testUnaryOpLowering @(WordN 5) unboundedConfig (`shiftBitsTerm` (-5)) "shift" (`shift` (-5))
                testUnaryOpLowering @(WordN 5)
                  unboundedConfig
                  (`shiftBitsTerm` (-5))
                  "shift"
                  (\x -> SBV.ite (x SBV..>= 0) 0 (-1)),
              testCase "RotateBits" $ do
                testUnaryOpLowering @(WordN 5) unboundedConfig (`rotateBitsTerm` 0) "rotate" id
                testUnaryOpLowering @(WordN 5) unboundedConfig (`rotateBitsTerm` 1) "rotate" (`rotate` 1)
                testUnaryOpLowering @(WordN 5) unboundedConfig (`rotateBitsTerm` 2) "rotate" (`rotate` 2)
                testUnaryOpLowering @(WordN 5) unboundedConfig (`rotateBitsTerm` 3) "rotate" (`rotate` 3)
                testUnaryOpLowering @(WordN 5) unboundedConfig (`rotateBitsTerm` 4) "rotate" (`rotate` 4)
                testUnaryOpLowering @(WordN 5) unboundedConfig (`rotateBitsTerm` 5) "rotate" (`rotate` 5)
                testUnaryOpLowering @(WordN 5) unboundedConfig (`rotateBitsTerm` 5) "rotate" id
                testUnaryOpLowering @(WordN 5) unboundedConfig (`rotateBitsTerm` (-1)) "rotate" (`rotate` (-1))
                testUnaryOpLowering @(WordN 5) unboundedConfig (`rotateBitsTerm` (-1)) "rotate" (`rotate` 4)
                testUnaryOpLowering @(WordN 5) unboundedConfig (`rotateBitsTerm` (-2)) "rotate" (`rotate` (-2))
                testUnaryOpLowering @(WordN 5) unboundedConfig (`rotateBitsTerm` (-2)) "rotate" (`rotate` 3)
                testUnaryOpLowering @(WordN 5) unboundedConfig (`rotateBitsTerm` (-3)) "rotate" (`rotate` (-3))
                testUnaryOpLowering @(WordN 5) unboundedConfig (`rotateBitsTerm` (-3)) "rotate" (`rotate` 2)
                testUnaryOpLowering @(WordN 5) unboundedConfig (`rotateBitsTerm` (-4)) "rotate" (`rotate` (-4))
                testUnaryOpLowering @(WordN 5) unboundedConfig (`rotateBitsTerm` (-4)) "rotate" (`rotate` 1)
                testUnaryOpLowering @(WordN 5) unboundedConfig (`rotateBitsTerm` (-5)) "rotate" (`rotate` (-5))
                testUnaryOpLowering @(WordN 5) unboundedConfig (`rotateBitsTerm` (-5)) "rotate" id,
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
                testUnaryOpLowering @(WordN 5) @(IntN 5) unboundedConfig bvToSignedTerm "toSigned" SBV.sFromIntegral
                testUnaryOpLowering @(WordN 5) @(IntN 5) boundedConfig bvToSignedTerm "toSigned" SBV.sFromIntegral
            ]
        ]
