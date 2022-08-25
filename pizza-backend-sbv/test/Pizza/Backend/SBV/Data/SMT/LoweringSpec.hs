{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Pizza.Backend.SBV.Data.SMT.LoweringSpec where

import Control.Monad.Trans
import Data.Bits
import Data.Dynamic
import qualified Data.HashMap.Strict as M
import Data.Proxy
import qualified Data.SBV as SBV
import qualified Data.SBV.Control as SBV
import Pizza.Backend.SBV.Data.SMT.Config
import Pizza.Backend.SBV.Data.SMT.Lowering
import Pizza.Backend.SBV.Data.SMT.SymBiMap
import Pizza.IR.SymPrim.Data.BV
import Pizza.IR.SymPrim.Data.Prim.InternedTerm.InternedCtors
import Pizza.IR.SymPrim.Data.Prim.InternedTerm.SomeTerm
import Pizza.IR.SymPrim.Data.Prim.InternedTerm.Term
import Test.Hspec

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
  PizzaSMTConfig n ->
  (Term a -> Term b) ->
  String ->
  (TermTy n a -> TermTy n b) ->
  Expectation
testUnaryOpLowering config f name sbvfun = do
  let a :: Term a = ssymbTerm "a"
  let fa :: Term b = f a
  SBV.runSMTWith (sbvConfig config) $ do
    (m, lt) <- lowerSinglePrim config fa
    let sbva :: Maybe (TermTy n a) = M.lookup (SomeTerm a) (biMapToSBV m) >>= fromDynamic
    case sbva of
      Nothing -> lift $ expectationFailure "Failed to extract the term"
      Just sbvav -> SBV.query $ do
        SBV.constrain $ lt SBV..== sbvfun sbvav
        satres <- SBV.checkSat
        case satres of
          SBV.Sat -> return ()
          _ -> lift $ expectationFailure $ "Lowering for " ++ name ++ " generated unsolvable formula"
  SBV.runSMTWith (sbvConfig config) $ do
    (m, lt) <- lowerSinglePrim config fa
    let sbvv :: Maybe (TermTy n a) = M.lookup (SomeTerm a) (biMapToSBV m) >>= fromDynamic
    case sbvv of
      Nothing -> lift $ expectationFailure "Failed to extract the term"
      Just sbvvv -> SBV.query $ do
        SBV.constrain $ lt SBV../= sbvfun sbvvv
        r <- SBV.checkSat
        case r of
          SBV.Sat -> do
            counterExample <- SBV.getValue sbvvv
            lift $ expectationFailure $ "Translation counter example found: " ++ show counterExample
          SBV.Unsat -> return ()
          _ -> lift $ expectationFailure $ "Lowering for " ++ name ++ " generated unknown formula"

testUnaryOpLowering' ::
  forall a b as n tag.
  ( HasCallStack,
    UnaryOp tag a b,
    SBV.EqSymbolic (TermTy n b),
    Typeable (TermTy n a),
    SBV.SymVal as,
    TermTy n a ~ SBV.SBV as,
    Show as
  ) =>
  PizzaSMTConfig n ->
  tag ->
  (TermTy n a -> TermTy n b) ->
  Expectation
testUnaryOpLowering' config t = testUnaryOpLowering @a @b @as config (constructUnary t) (show t)

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
  PizzaSMTConfig n ->
  (Term a -> Term b -> Term c) ->
  String ->
  (TermTy n a -> TermTy n b -> TermTy n c) ->
  Expectation
testBinaryOpLowering config f name sbvfun = do
  let a :: Term a = ssymbTerm "a"
  let b :: Term b = ssymbTerm "b"
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
          _ -> lift $ expectationFailure $ "Lowering for " ++ name ++ " generated unsolvable formula"
      _ -> lift $ expectationFailure "Failed to extract the term"
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
            lift $ expectationFailure $ "Translation counter example found: " ++ show (counterExampleA, counterExampleB)
          SBV.Unsat -> return ()
          _ -> lift $ expectationFailure $ "Lowering for " ++ name ++ " generated unknown formula"
      _ -> lift $ expectationFailure "Failed to extract the term"

testBinaryOpLowering' ::
  forall a b c as bs n tag.
  ( HasCallStack,
    BinaryOp tag a b c,
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
  PizzaSMTConfig n ->
  tag ->
  (TermTy n a -> TermTy n b -> TermTy n c) ->
  Expectation
testBinaryOpLowering' config t = testBinaryOpLowering @a @b @c @as @bs config (constructBinary t) (show t)

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
  PizzaSMTConfig n ->
  (Term a -> Term b -> Term c -> Term d) ->
  String ->
  (TermTy n a -> TermTy n b -> TermTy n c -> TermTy n d) ->
  Expectation
testTernaryOpLowering config f name sbvfun = do
  let a :: Term a = ssymbTerm "a"
  let b :: Term b = ssymbTerm "b"
  let c :: Term c = ssymbTerm "c"
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
          _ -> lift $ expectationFailure $ "Lowering for " ++ name ++ " generated unsolvable formula"
      _ -> lift $ expectationFailure "Failed to extract the term"
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
              expectationFailure $
                "Translation counter example found: "
                  ++ show (counterExampleA, counterExampleB, counterExampleC)
          SBV.Unsat -> return ()
          _ -> lift $ expectationFailure $ "Lowering for " ++ name ++ " generated unknown formula"
      _ -> lift $ expectationFailure "Failed to extract the term"

testTernaryOpLowering' ::
  forall a b c d as bs cs n tag.
  ( HasCallStack,
    TernaryOp tag a b c d,
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
  PizzaSMTConfig n ->
  tag ->
  (TermTy n a -> TermTy n b -> TermTy n c -> TermTy n d) ->
  Expectation
testTernaryOpLowering' config t = testTernaryOpLowering @a @b @c @d @as @bs @cs config (constructTernary t) (show t)

spec :: Spec
spec = do
  let unboundedConfig = UnboundedReasoning SBV.z3
  let boundedConfig = BoundedReasoning @5 SBV.z3
  describe "Test Bool Lowering" $ do
    it "Not lowering should work" $ do
      testUnaryOpLowering @Bool @Bool unboundedConfig notTerm "not" SBV.sNot
    it "And lowering should work" $ do
      testBinaryOpLowering @Bool @Bool @Bool unboundedConfig andTerm "and" (SBV..&&)
      testBinaryOpLowering @Bool @Bool @Bool
        unboundedConfig
        andTerm
        "and"
        (\x y -> SBV.sNot (x SBV..<+> y) SBV..&& (x SBV..|| y))
    it "Or lowering should work" $ do
      testBinaryOpLowering @Bool @Bool @Bool unboundedConfig orTerm "or" (SBV..||)
      testBinaryOpLowering @Bool @Bool @Bool
        unboundedConfig
        orTerm
        "or"
        (\x y -> (x SBV..<+> y) SBV..|| (x SBV..&& y))
    it "Eqv lowering should work" $ do
      testBinaryOpLowering @Bool @Bool @Bool unboundedConfig eqvTerm "eqv" (SBV..==)
      testBinaryOpLowering @Bool @Bool @Bool
        unboundedConfig
        eqvTerm
        "eqv"
        (\x y -> SBV.sNot (x SBV..<+> y))
    it "ITE lowering should work" $ do
      testTernaryOpLowering @Bool @Bool @Bool @Bool unboundedConfig iteTerm "ite" SBV.ite
      testTernaryOpLowering @Bool @Bool @Bool @Bool
        unboundedConfig
        iteTerm
        "ite"
        (\c x y -> (c SBV..=> x) SBV..&& (SBV.sNot c SBV..=> y))
  describe "Test Integer Lowering" $ do
    it "Add lowering should work" $ do
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
        (\x y -> (x + 1) * (y + 1) - x * y - 1)
    it "Uminus lowering should work" $ do
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
        (\x -> (x + 1) * (x + 1) - 3 * x - x * x - 1)
    it "Abs lowering should work" $ do
      testUnaryOpLowering @Integer @Integer unboundedConfig absNumTerm "abs" abs
      testUnaryOpLowering @Integer @Integer boundedConfig absNumTerm "abs" abs
    it "Signum lowering should work" $ do
      testUnaryOpLowering @Integer @Integer unboundedConfig signumNumTerm "signum" signum
      testUnaryOpLowering @Integer @Integer boundedConfig signumNumTerm "signum" signum
    it "Times lowering should work" $ do
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
        (\x y -> (x + 1) * (y + 1) - x - y - 1)
    it "Lt lowering should work" $ do
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
        (\x y -> x * 2 - x SBV..< y * 2 - y)
    it "Le lowering should work" $ do
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
        (\x y -> x * 2 - x SBV..<= y * 2 - y)
    it "DivI lowering should work" $ do
      testBinaryOpLowering @Integer @Integer @Integer unboundedConfig divIntegerTerm "div" SBV.sDiv
      testBinaryOpLowering @Integer @Integer @Integer boundedConfig divIntegerTerm "div" SBV.sDiv
    it "ModI lowering should work" $ do
      testBinaryOpLowering @Integer @Integer @Integer unboundedConfig modIntegerTerm "mod" SBV.sMod
      testBinaryOpLowering @Integer @Integer @Integer boundedConfig modIntegerTerm "mod" SBV.sMod
  describe "Test IntN Lowering" $ do
    it "Add lowering should work" $ do
      testBinaryOpLowering @(IntN 5) @(IntN 5) unboundedConfig addNumTerm "(+)" (+)
      testBinaryOpLowering @(IntN 5) @(IntN 5)
        unboundedConfig
        addNumTerm
        "(+)"
        (\x y -> (x + 1) * (y + 1) - x * y - 1)
    it "Uminus lowering should work" $ do
      testUnaryOpLowering @(IntN 5) unboundedConfig uminusNumTerm "negate" negate
      testUnaryOpLowering @(IntN 5)
        unboundedConfig
        uminusNumTerm
        "negate"
        (\x -> (x + 1) * (x + 1) - 3 * x - x * x - 1)
    it "Abs lowering should work" $ do
      testUnaryOpLowering @(IntN 5) unboundedConfig absNumTerm "abs" abs
    it "Signum lowering should work" $ do
      testUnaryOpLowering @(IntN 5) unboundedConfig signumNumTerm "signum" signum
    it "Times lowering should work" $ do
      testBinaryOpLowering @(IntN 5) @(IntN 5) unboundedConfig timesNumTerm "(*)" (*)
      testBinaryOpLowering @(IntN 5) @(IntN 5)
        unboundedConfig
        timesNumTerm
        "(*)"
        (\x y -> (x + 1) * (y + 1) - x - y - 1)
    it "Lt lowering should work" $ do
      testBinaryOpLowering @(IntN 5) @(IntN 5) unboundedConfig ltNumTerm "(<)" (SBV..<)
      testBinaryOpLowering @(IntN 5) @(IntN 5)
        unboundedConfig
        ltNumTerm
        "(<)"
        (\x y -> x * 2 - x SBV..< y * 2 - y)
    it "Le lowering should work" $ do
      testBinaryOpLowering @(IntN 5) @(IntN 5) unboundedConfig leNumTerm "(<=)" (SBV..<=)
      testBinaryOpLowering @(IntN 5) @(IntN 5)
        unboundedConfig
        leNumTerm
        "(<=)"
        (\x y -> x * 2 - x SBV..<= y * 2 - y)
    it "Extract lowering should work" $ do
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
        id
    it "Extension lowering should work" $ do
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
        SBV.signExtend
    it "Concat should work" $ do
      testBinaryOpLowering @(IntN 4) @(IntN 5) @(IntN 9)
        unboundedConfig
        bvconcatTerm
        "bvconcat"
        (SBV.#)

    it "AndBits lowering should work" $ do
      testBinaryOpLowering @(IntN 5) @(IntN 5) unboundedConfig andBitsTerm "(.&.)" (.&.)
    it "OrBits lowering should work" $ do
      testBinaryOpLowering @(IntN 5) @(IntN 5) unboundedConfig orBitsTerm "(.|.)" (.|.)
    it "XorBits lowering should work" $ do
      testBinaryOpLowering @(IntN 5) @(IntN 5) unboundedConfig xorBitsTerm "xor" xor
    it "ComplementBits lowering should work" $ do
      testUnaryOpLowering @(IntN 5) unboundedConfig complementBitsTerm "complement" complement
    it "ShiftBits lowering should work" $ do
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
        (\x -> SBV.ite (x SBV..>= 0) 0 (-1))
    it "RotateBits lowering should work" $ do
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
      testUnaryOpLowering @(IntN 5) unboundedConfig (`rotateBitsTerm` (-5)) "rotate" id

  describe "Test WordN Lowering" $ do
    it "Add lowering should work" $ do
      testBinaryOpLowering @(WordN 5) @(WordN 5) unboundedConfig addNumTerm "(+)" (+)
      testBinaryOpLowering @(WordN 5) @(WordN 5)
        unboundedConfig
        addNumTerm
        "(+)"
        (\x y -> (x + 1) * (y + 1) - x * y - 1)
    it "Uminus lowering should work" $ do
      testUnaryOpLowering @(WordN 5) unboundedConfig uminusNumTerm "negate" negate
      testUnaryOpLowering @(WordN 5)
        unboundedConfig
        uminusNumTerm
        "negate"
        (\x -> (x + 1) * (x + 1) - 3 * x - x * x - 1)
    it "Abs lowering should work" $ do
      testUnaryOpLowering @(WordN 5) unboundedConfig absNumTerm "abs" abs
    it "Signum lowering should work" $ do
      testUnaryOpLowering @(WordN 5) unboundedConfig signumNumTerm "signum" signum
    it "Times lowering should work" $ do
      testBinaryOpLowering @(WordN 5) @(WordN 5) unboundedConfig timesNumTerm "(*)" (*)
      testBinaryOpLowering @(WordN 5) @(WordN 5)
        unboundedConfig
        timesNumTerm
        "(*)"
        (\x y -> (x + 1) * (y + 1) - x - y - 1)
    it "Lt lowering should work" $ do
      testBinaryOpLowering @(WordN 5) @(WordN 5) unboundedConfig ltNumTerm "(<)" (SBV..<)
      testBinaryOpLowering @(WordN 5) @(WordN 5)
        unboundedConfig
        ltNumTerm
        "(<)"
        (\x y -> x * 2 - x SBV..< y * 2 - y)
    it "Le lowering should work" $ do
      testBinaryOpLowering @(WordN 5) @(WordN 5) unboundedConfig leNumTerm "(<=)" (SBV..<=)
      testBinaryOpLowering @(WordN 5) @(WordN 5)
        unboundedConfig
        leNumTerm
        "(<=)"
        (\x y -> x * 2 - x SBV..<= y * 2 - y)
    it "Extract lowering should work" $ do
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
        id
    it "Extension lowering should work" $ do
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
        SBV.signExtend
    it "Concat should work" $ do
      testBinaryOpLowering @(WordN 4) @(WordN 5) @(WordN 9)
        unboundedConfig
        bvconcatTerm
        "bvconcat"
        (SBV.#)
    it "AndBits lowering should work" $ do
      testBinaryOpLowering @(WordN 5) @(WordN 5) unboundedConfig andBitsTerm "(.&.)" (.&.)
    it "OrBits lowering should work" $ do
      testBinaryOpLowering @(WordN 5) @(WordN 5) unboundedConfig orBitsTerm "(.|.)" (.|.)
    it "XorBits lowering should work" $ do
      testBinaryOpLowering @(WordN 5) @(WordN 5) unboundedConfig xorBitsTerm "xor" xor
    it "ComplementBits lowering should work" $ do
      testUnaryOpLowering @(WordN 5) unboundedConfig complementBitsTerm "complement" complement
    it "ShiftBits lowering should work" $ do
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
        (\x -> SBV.ite (x SBV..>= 0) 0 (-1))
    it "RotateBits lowering should work" $ do
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
      testUnaryOpLowering @(WordN 5) unboundedConfig (`rotateBitsTerm` (-5)) "rotate" id

{-
it "DivI lowering should work" $ do
  testBinaryOpLowering' @Integer @Integer unboundedConfig DivI SBV.sDiv
it "ModI lowering should work" $ do
  testBinaryOpLowering' @(IntN 5) @(IntN 5) @(IntN 5) unboundedConfig ModI SBV.sMod
  -}
