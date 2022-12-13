{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

module Grisette.Backend.SBV.Data.SMT.CEGISTests where

import Control.Monad.Except
import qualified Data.HashSet as S
import Data.Proxy
import qualified Data.SBV as SBV
import Grisette.Backend.SBV.Data.SMT.Solving
import Grisette.Core.Control.Exception
import Grisette.Core.Data.Class.BitVector
import Grisette.Core.Data.Class.Bool
import Grisette.Core.Data.Class.CEGISSolver
import Grisette.Core.Data.Class.Error
import Grisette.Core.Data.Class.Evaluate
import Grisette.Core.Data.Class.ExtractSymbolics
import Grisette.Core.Data.Class.SOrd
import Grisette.Core.Data.Class.SimpleMergeable
import Grisette.Core.Data.Class.Solvable
import Grisette.Core.Data.Class.Solver
import Grisette.IR.SymPrim.Control.Monad.UnionM
import Grisette.IR.SymPrim.Data.Class.Evaluate
import Grisette.IR.SymPrim.Data.Class.ExtractSymbolics
import Grisette.IR.SymPrim.Data.Prim.InternedTerm.Term
import Grisette.IR.SymPrim.Data.Prim.Model
import Grisette.IR.SymPrim.Data.SymPrim
import Test.Tasty
import Test.Tasty.HUnit

testCegis :: (HasCallStack, ExtractSymbolics a, EvaluateSym a, Show a) => GrisetteSMTConfig i -> Bool -> a -> [SymBool] -> Assertion
testCegis config shouldSuccess a bs = do
  x <- cegisExceptVC config (a, ssymb "internal" :: SymInteger) return (runExceptT $ buildFormula bs)
  case x of
    Left _ -> shouldSuccess @=? False
    Right (_, m) -> do
      shouldSuccess @=? True
      verify bs
      where
        verify [] = return ()
        verify (v : vs) = do
          y <- solveFormula config (evaluateSym False m $ nots v)
          case y of
            Left _ -> do
              verify vs
            Right _ -> assertFailure $ "Failed to verify " ++ show v ++ " with the model " ++ show m
  where
    buildFormula :: [SymBool] -> ExceptT VerificationConditions UnionM ()
    buildFormula l = do
      symFailIfNot AssumptionViolation ((ssymb "internal" :: SymInteger) `gsymge` 0)
      go l 0
      where
        go :: [SymBool] -> SymInteger -> ExceptT VerificationConditions UnionM ()
        go [] _ = return ()
        go (x : xs) i =
          mrgIf
            (ssymb "internal" `gsymge` i &&~ ssymb "internal" `gsymlt` (i + 1))
            (symFailIfNot AssertionViolation x)
            (go xs (i + 1))

cegisTests :: TestTree
cegisTests =
  let unboundedConfig = UnboundedReasoning SBV.z3 -- {SBV.verbose=True}
   in testGroup
        "CEGISTests"
        [ testGroup
            "Boolean"
            [ testCase "Basic" $ do
                testCegis
                  unboundedConfig
                  True
                  ()
                  [ssymb "a", ssymb "b", ssymb "c"]
                testCegis
                  unboundedConfig
                  False
                  ()
                  [ssymb "a", nots $ ssymb "a"],
              testCase "And" $ do
                testCegis
                  unboundedConfig
                  True
                  ()
                  [ssymb "a" &&~ ssymb "b", ssymb "b" &&~ nots (ssymb "c"), ssymb "a", ssymb "b", nots (ssymb "c")]
                testCegis
                  unboundedConfig
                  False
                  ()
                  [ssymb "a" &&~ ssymb "b", ssymb "b" &&~ nots (ssymb "c"), ssymb "a", ssymb "b", ssymb "c"]
                testCegis
                  unboundedConfig
                  True
                  (ssymb "a" :: SymBool)
                  [nots $ ssymb "a" &&~ ssymb "b", nots $ ssymb "b"]
                testCegis
                  unboundedConfig
                  False
                  (ssymb "a" :: SymBool)
                  [nots $ ssymb "a" &&~ ssymb "b", ssymb "b"],
              testCase "Or" $ do
                testCegis
                  unboundedConfig
                  True
                  ()
                  [ssymb "a" ||~ ssymb "b", ssymb "b" ||~ nots (ssymb "c"), ssymb "a", ssymb "b", nots (ssymb "c")]
                testCegis
                  unboundedConfig
                  True
                  ()
                  [ssymb "a" ||~ ssymb "b", ssymb "b" ||~ nots (ssymb "c"), ssymb "a", ssymb "b", ssymb "c"]
                testCegis
                  unboundedConfig
                  True
                  (ssymb "a" :: SymBool)
                  [ssymb "a" ||~ ssymb "b", ssymb "b"]
                testCegis
                  unboundedConfig
                  False
                  (ssymb "a" :: SymBool)
                  [ssymb "a" ||~ ssymb "b", nots $ ssymb "b"],
              testCase "And / Or should be consistent" $ do
                testCegis
                  unboundedConfig
                  True
                  ()
                  [ssymb "a" &&~ ssymb "b", ssymb "a" ||~ ssymb "b"]
                testCegis
                  unboundedConfig
                  True
                  ()
                  [nots $ ssymb "a" &&~ ssymb "b", ssymb "a" ||~ ssymb "b"]
                testCegis
                  unboundedConfig
                  False
                  ()
                  [ssymb "a" &&~ ssymb "b", nots $ ssymb "a" ||~ ssymb "b"]
                testCegis
                  unboundedConfig
                  True
                  ()
                  [nots $ ssymb "a" &&~ ssymb "b", nots $ ssymb "a" ||~ ssymb "b"],
              testCase "Eqv" $ do
                testCegis
                  unboundedConfig
                  True
                  ()
                  [(ssymb "a" :: SymBool) `gsymeq` ssymb "b", ssymb "a", ssymb "b"]
                testCegis
                  unboundedConfig
                  True
                  ()
                  [(ssymb "a" :: SymBool) `gsymeq` ssymb "b", nots $ ssymb "a", nots $ ssymb "b"]
                testCegis
                  unboundedConfig
                  False
                  ()
                  [(ssymb "a" :: SymBool) `gsymeq` ssymb "b", nots $ ssymb "a", ssymb "b"]
                testCegis
                  unboundedConfig
                  False
                  ()
                  [(ssymb "a" :: SymBool) `gsymeq` ssymb "b", nots $ ssymb "a", ssymb "b"]
                testCegis
                  unboundedConfig
                  True
                  ()
                  [(ssymb "a" :: SymBool) `gsymeq` ssymb "b", nots (ssymb "a") `xors` ssymb "b"]
                testCegis
                  unboundedConfig
                  False
                  ()
                  [(ssymb "a" :: SymBool) `gsymeq` ssymb "b", ssymb "a" `xors` ssymb "b"],
              testCase "ites" $ do
                testCegis
                  unboundedConfig
                  True
                  (ssymb "c" :: SymBool)
                  [ites (ssymb "a" :: SymBool) (ssymb "b") (ssymb "c"), ssymb "a", ssymb "b"]
                testCegis
                  unboundedConfig
                  False
                  (ssymb "c" :: SymBool)
                  [ites (ssymb "a" :: SymBool) (ssymb "b") (ssymb "c"), nots $ ssymb "a"]
                testCegis
                  unboundedConfig
                  True
                  (ssymb "b" :: SymBool)
                  [ites (ssymb "a" :: SymBool) (ssymb "b") (ssymb "c"), nots $ ssymb "a", ssymb "c"]
                testCegis
                  unboundedConfig
                  False
                  (ssymb "b" :: SymBool)
                  [ites (ssymb "a" :: SymBool) (ssymb "b") (ssymb "c"), ssymb "a"]
                testCegis
                  unboundedConfig
                  True
                  ()
                  [ites (ssymb "a" :: SymBool) (ssymb "b") (ssymb "c"), ssymb "a", ssymb "b", ssymb "c"]
                testCegis
                  unboundedConfig
                  True
                  ()
                  [ites (ssymb "a" :: SymBool) (ssymb "b") (ssymb "c"), ssymb "a", ssymb "b", nots $ ssymb "c"]
                testCegis
                  unboundedConfig
                  True
                  ()
                  [ites (ssymb "a" :: SymBool) (ssymb "b") (ssymb "c"), nots $ ssymb "a", ssymb "b", ssymb "c"]
                testCegis
                  unboundedConfig
                  True
                  ()
                  [ites (ssymb "a" :: SymBool) (ssymb "b") (ssymb "c"), nots $ ssymb "a", nots $ ssymb "b", ssymb "c"]
                testCegis
                  unboundedConfig
                  False
                  ()
                  [ites (ssymb "a" :: SymBool) (ssymb "b") (ssymb "c"), ssymb "a", nots $ ssymb "b", ssymb "c"]
                testCegis
                  unboundedConfig
                  False
                  ()
                  [ites (ssymb "a" :: SymBool) (ssymb "b") (ssymb "c"), ssymb "a", nots $ ssymb "b", nots $ ssymb "c"]
                testCegis
                  unboundedConfig
                  False
                  ()
                  [ites (ssymb "a" :: SymBool) (ssymb "b") (ssymb "c"), nots $ ssymb "a", ssymb "b", nots $ ssymb "c"]
                testCegis
                  unboundedConfig
                  False
                  ()
                  [ites (ssymb "a" :: SymBool) (ssymb "b") (ssymb "c"), nots $ ssymb "a", nots $ ssymb "b", nots $ ssymb "c"]
            ],
          let a = ssymb "a" :: SymIntN 5
              b = ssymb "b" :: SymIntN 5
              c = ssymb "c" :: SymIntN 5
              d = ssymb "c" :: SymIntN 10
           in testGroup
                "Different sized BV"
                [ testGroup
                    "Extract"
                    [ testCase "Extract" $ do
                        testCegis
                          unboundedConfig
                          True
                          ()
                          [bvselect (Proxy @2) (Proxy @2) a `gsymeq` (conc 1 :: SymIntN 2), a `gsymeq` conc 0b10101]
                        testCegis
                          unboundedConfig
                          False
                          ()
                          [bvselect (Proxy @2) (Proxy @2) a `gsymeq` (conc 1 :: SymIntN 2), a `gsymeq` conc 0b10001],
                      testCase "Extract when lowered twice" $ do
                        testCegis
                          unboundedConfig
                          True
                          a
                          [bvselect (Proxy @2) (Proxy @2) (bvconcat a b) `gsymeq` (conc 1 :: SymIntN 2)]
                        testCegis
                          unboundedConfig
                          True
                          b
                          [bvselect (Proxy @7) (Proxy @2) (bvconcat a b) `gsymeq` (conc 1 :: SymIntN 2)]
                    ],
                  testGroup
                    "Concat"
                    [ testCase "Concat" $ do
                        testCegis
                          unboundedConfig
                          True
                          ()
                          [bvconcat a b `gsymeq` d, a `gsymeq` conc 1, b `gsymeq` conc 1, d `gsymeq` conc 0b100001]
                        testCegis
                          unboundedConfig
                          False
                          ()
                          [bvconcat a b `gsymeq` d, a `gsymeq` conc 1, b `gsymeq` conc 1, d `gsymeq` conc 0b100010],
                      testCase "Concat when lowered twice" $ do
                        testCegis
                          unboundedConfig
                          True
                          (a, c)
                          [bvconcat c (bvselect (Proxy @2) (Proxy @2) (bvconcat a b) :: SymIntN 2) `gsymeq` bvconcat c (conc 1 :: SymIntN 2)]
                        testCegis
                          unboundedConfig
                          True
                          (b, c)
                          [bvconcat c (bvselect (Proxy @7) (Proxy @2) (bvconcat a b) :: SymIntN 2) `gsymeq` bvconcat c (conc 1 :: SymIntN 2)]
                    ],
                  testGroup
                    "Zext"
                    [ testCase "bvzeroExtend" $ do
                        testCegis
                          unboundedConfig
                          True
                          ()
                          [bvzeroExtend (Proxy @10) a `gsymeq` d, a `gsymeq` conc 1, d `gsymeq` (conc 1 :: SymIntN 10)]
                        testCegis
                          unboundedConfig
                          True
                          ()
                          [bvzeroExtend (Proxy @10) a `gsymeq` d, a `gsymeq` conc 0b11111, d `gsymeq` (conc 0b11111 :: SymIntN 10)]
                        testCegis
                          unboundedConfig
                          False
                          ()
                          [bvzeroExtend (Proxy @10) a `gsymeq` d, d `gsymeq` (conc 0b111111 :: SymIntN 10)]
                        testCegis
                          unboundedConfig
                          False
                          ()
                          [bvzeroExtend (Proxy @10) a `gsymeq` d, d `gsymeq` (conc 0b1111111111 :: SymIntN 10)],
                      testCase "bvzeroExtend when lowered twice" $ do
                        testCegis
                          unboundedConfig
                          True
                          a
                          [bvzeroExtend (Proxy @10) (bvselect (Proxy @2) (Proxy @2) (bvconcat a b) :: SymIntN 2) `gsymeq` (conc 1 :: SymIntN 10)]
                        testCegis
                          unboundedConfig
                          True
                          b
                          [bvzeroExtend (Proxy @10) (bvselect (Proxy @7) (Proxy @2) (bvconcat a b) :: SymIntN 2) `gsymeq` (conc 1 :: SymIntN 10)]
                    ],
                  testGroup
                    "Sext"
                    [ testCase "bvsignExtend" $ do
                        testCegis
                          unboundedConfig
                          True
                          ()
                          [bvsignExtend (Proxy @10) a `gsymeq` d, a `gsymeq` conc 1, d `gsymeq` (conc 1 :: SymIntN 10)]
                        testCegis
                          unboundedConfig
                          True
                          ()
                          [bvsignExtend (Proxy @10) a `gsymeq` d, a `gsymeq` conc 0b11111, d `gsymeq` (conc 0b1111111111 :: SymIntN 10)]
                        testCegis
                          unboundedConfig
                          False
                          ()
                          [bvsignExtend (Proxy @10) a `gsymeq` d, d `gsymeq` (conc 0b111111 :: SymIntN 10)]
                        testCegis
                          unboundedConfig
                          False
                          ()
                          [bvsignExtend (Proxy @10) a `gsymeq` d, d `gsymeq` (conc 0b11111 :: SymIntN 10)],
                      testCase "bvsignExtend when lowered twice" $ do
                        testCegis
                          unboundedConfig
                          True
                          a
                          [bvsignExtend (Proxy @10) (bvselect (Proxy @2) (Proxy @2) (bvconcat a b) :: SymIntN 2) `gsymeq` (conc 1 :: SymIntN 10)]
                        testCegis
                          unboundedConfig
                          True
                          b
                          [bvsignExtend (Proxy @10) (bvselect (Proxy @7) (Proxy @2) (bvconcat a b) :: SymIntN 2) `gsymeq` (conc 1 :: SymIntN 10)]
                    ]
                ]
        ]
