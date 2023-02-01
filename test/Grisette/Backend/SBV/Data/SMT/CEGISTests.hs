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
import Grisette.Core.Control.Monad.UnionM
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
import Grisette.IR.SymPrim.Data.Prim.InternedTerm.Term
import Grisette.IR.SymPrim.Data.Prim.Model
import Grisette.IR.SymPrim.Data.SymPrim
import Test.Tasty
import Test.Tasty.HUnit

testCegis :: (HasCallStack, ExtractSymbolics a, EvaluateSym a, Show a) => GrisetteSMTConfig i -> Bool -> a -> [SymBool] -> Assertion
testCegis config shouldSuccess a bs = do
  x <- cegisExceptVC config (a, ssym "internal" :: SymInteger) return (runExceptT $ buildFormula bs)
  case x of
    Left _ -> shouldSuccess @=? False
    Right (_, m) -> do
      shouldSuccess @=? True
      verify bs
      where
        verify [] = return ()
        verify (v : vs) = do
          y <- solve config (evaluateSym False m $ nots v)
          case y of
            Left _ -> do
              verify vs
            Right _ -> assertFailure $ "Failed to verify " ++ show v ++ " with the model " ++ show m
  where
    buildFormula :: [SymBool] -> ExceptT VerificationConditions UnionM ()
    buildFormula l = do
      symAssume ((ssym "internal" :: SymInteger) >=~ 0)
      go l 0
      where
        go :: [SymBool] -> SymInteger -> ExceptT VerificationConditions UnionM ()
        go [] _ = return ()
        go (x : xs) i =
          mrgIf
            (ssym "internal" >=~ i &&~ ssym "internal" <~ (i + 1))
            (symAssert x)
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
                  [ssym "a", ssym "b", ssym "c"]
                testCegis
                  unboundedConfig
                  False
                  ()
                  [ssym "a", nots $ ssym "a"],
              testCase "And" $ do
                testCegis
                  unboundedConfig
                  True
                  ()
                  [ssym "a" &&~ ssym "b", ssym "b" &&~ nots (ssym "c"), ssym "a", ssym "b", nots (ssym "c")]
                testCegis
                  unboundedConfig
                  False
                  ()
                  [ssym "a" &&~ ssym "b", ssym "b" &&~ nots (ssym "c"), ssym "a", ssym "b", ssym "c"]
                testCegis
                  unboundedConfig
                  True
                  (ssym "a" :: SymBool)
                  [nots $ ssym "a" &&~ ssym "b", nots $ ssym "b"]
                testCegis
                  unboundedConfig
                  False
                  (ssym "a" :: SymBool)
                  [nots $ ssym "a" &&~ ssym "b", ssym "b"],
              testCase "Or" $ do
                testCegis
                  unboundedConfig
                  True
                  ()
                  [ssym "a" ||~ ssym "b", ssym "b" ||~ nots (ssym "c"), ssym "a", ssym "b", nots (ssym "c")]
                testCegis
                  unboundedConfig
                  True
                  ()
                  [ssym "a" ||~ ssym "b", ssym "b" ||~ nots (ssym "c"), ssym "a", ssym "b", ssym "c"]
                testCegis
                  unboundedConfig
                  True
                  (ssym "a" :: SymBool)
                  [ssym "a" ||~ ssym "b", ssym "b"]
                testCegis
                  unboundedConfig
                  False
                  (ssym "a" :: SymBool)
                  [ssym "a" ||~ ssym "b", nots $ ssym "b"],
              testCase "And / Or should be consistent" $ do
                testCegis
                  unboundedConfig
                  True
                  ()
                  [ssym "a" &&~ ssym "b", ssym "a" ||~ ssym "b"]
                testCegis
                  unboundedConfig
                  True
                  ()
                  [nots $ ssym "a" &&~ ssym "b", ssym "a" ||~ ssym "b"]
                testCegis
                  unboundedConfig
                  False
                  ()
                  [ssym "a" &&~ ssym "b", nots $ ssym "a" ||~ ssym "b"]
                testCegis
                  unboundedConfig
                  True
                  ()
                  [nots $ ssym "a" &&~ ssym "b", nots $ ssym "a" ||~ ssym "b"],
              testCase "Eqv" $ do
                testCegis
                  unboundedConfig
                  True
                  ()
                  [(ssym "a" :: SymBool) ==~ ssym "b", ssym "a", ssym "b"]
                testCegis
                  unboundedConfig
                  True
                  ()
                  [(ssym "a" :: SymBool) ==~ ssym "b", nots $ ssym "a", nots $ ssym "b"]
                testCegis
                  unboundedConfig
                  False
                  ()
                  [(ssym "a" :: SymBool) ==~ ssym "b", nots $ ssym "a", ssym "b"]
                testCegis
                  unboundedConfig
                  False
                  ()
                  [(ssym "a" :: SymBool) ==~ ssym "b", nots $ ssym "a", ssym "b"]
                testCegis
                  unboundedConfig
                  True
                  ()
                  [(ssym "a" :: SymBool) ==~ ssym "b", nots (ssym "a") `xors` ssym "b"]
                testCegis
                  unboundedConfig
                  False
                  ()
                  [(ssym "a" :: SymBool) ==~ ssym "b", ssym "a" `xors` ssym "b"],
              testCase "ites" $ do
                testCegis
                  unboundedConfig
                  True
                  (ssym "c" :: SymBool)
                  [ites (ssym "a" :: SymBool) (ssym "b") (ssym "c"), ssym "a", ssym "b"]
                testCegis
                  unboundedConfig
                  False
                  (ssym "c" :: SymBool)
                  [ites (ssym "a" :: SymBool) (ssym "b") (ssym "c"), nots $ ssym "a"]
                testCegis
                  unboundedConfig
                  True
                  (ssym "b" :: SymBool)
                  [ites (ssym "a" :: SymBool) (ssym "b") (ssym "c"), nots $ ssym "a", ssym "c"]
                testCegis
                  unboundedConfig
                  False
                  (ssym "b" :: SymBool)
                  [ites (ssym "a" :: SymBool) (ssym "b") (ssym "c"), ssym "a"]
                testCegis
                  unboundedConfig
                  True
                  ()
                  [ites (ssym "a" :: SymBool) (ssym "b") (ssym "c"), ssym "a", ssym "b", ssym "c"]
                testCegis
                  unboundedConfig
                  True
                  ()
                  [ites (ssym "a" :: SymBool) (ssym "b") (ssym "c"), ssym "a", ssym "b", nots $ ssym "c"]
                testCegis
                  unboundedConfig
                  True
                  ()
                  [ites (ssym "a" :: SymBool) (ssym "b") (ssym "c"), nots $ ssym "a", ssym "b", ssym "c"]
                testCegis
                  unboundedConfig
                  True
                  ()
                  [ites (ssym "a" :: SymBool) (ssym "b") (ssym "c"), nots $ ssym "a", nots $ ssym "b", ssym "c"]
                testCegis
                  unboundedConfig
                  False
                  ()
                  [ites (ssym "a" :: SymBool) (ssym "b") (ssym "c"), ssym "a", nots $ ssym "b", ssym "c"]
                testCegis
                  unboundedConfig
                  False
                  ()
                  [ites (ssym "a" :: SymBool) (ssym "b") (ssym "c"), ssym "a", nots $ ssym "b", nots $ ssym "c"]
                testCegis
                  unboundedConfig
                  False
                  ()
                  [ites (ssym "a" :: SymBool) (ssym "b") (ssym "c"), nots $ ssym "a", ssym "b", nots $ ssym "c"]
                testCegis
                  unboundedConfig
                  False
                  ()
                  [ites (ssym "a" :: SymBool) (ssym "b") (ssym "c"), nots $ ssym "a", nots $ ssym "b", nots $ ssym "c"]
            ],
          let a = ssym "a" :: SymIntN 5
              b = ssym "b" :: SymIntN 5
              c = ssym "c" :: SymIntN 5
              d = ssym "c" :: SymIntN 10
           in testGroup
                "Different sized BV"
                [ testGroup
                    "Select"
                    [ testCase "sizedBVSelect" $ do
                        testCegis
                          unboundedConfig
                          True
                          ()
                          [sizedBVSelect (Proxy @2) (Proxy @2) a ==~ (con 1 :: SymIntN 2), a ==~ con 0b10101]
                        testCegis
                          unboundedConfig
                          False
                          ()
                          [sizedBVSelect (Proxy @2) (Proxy @2) a ==~ (con 1 :: SymIntN 2), a ==~ con 0b10001],
                      testCase "sizedBVSelect when lowered twice" $ do
                        testCegis
                          unboundedConfig
                          True
                          a
                          [sizedBVSelect (Proxy @2) (Proxy @2) (sizedBVConcat a b) ==~ (con 1 :: SymIntN 2)]
                        testCegis
                          unboundedConfig
                          True
                          b
                          [sizedBVSelect (Proxy @7) (Proxy @2) (sizedBVConcat a b) ==~ (con 1 :: SymIntN 2)]
                    ],
                  testGroup
                    "Concat"
                    [ testCase "sizedBVConcat" $ do
                        testCegis
                          unboundedConfig
                          True
                          ()
                          [sizedBVConcat a b ==~ d, a ==~ con 1, b ==~ con 1, d ==~ con 0b100001]
                        testCegis
                          unboundedConfig
                          False
                          ()
                          [sizedBVConcat a b ==~ d, a ==~ con 1, b ==~ con 1, d ==~ con 0b100010],
                      testCase "sizedBVConcat when lowered twice" $ do
                        testCegis
                          unboundedConfig
                          True
                          (a, c)
                          [sizedBVConcat c (sizedBVSelect (Proxy @2) (Proxy @2) (sizedBVConcat a b) :: SymIntN 2) ==~ sizedBVConcat c (con 1 :: SymIntN 2)]
                        testCegis
                          unboundedConfig
                          True
                          (b, c)
                          [sizedBVConcat c (sizedBVSelect (Proxy @7) (Proxy @2) (sizedBVConcat a b) :: SymIntN 2) ==~ sizedBVConcat c (con 1 :: SymIntN 2)]
                    ],
                  testGroup
                    "Zext"
                    [ testCase "sizedBVZext" $ do
                        testCegis
                          unboundedConfig
                          True
                          ()
                          [sizedBVZext (Proxy @10) a ==~ d, a ==~ con 1, d ==~ (con 1 :: SymIntN 10)]
                        testCegis
                          unboundedConfig
                          True
                          ()
                          [sizedBVZext (Proxy @10) a ==~ d, a ==~ con 0b11111, d ==~ (con 0b11111 :: SymIntN 10)]
                        testCegis
                          unboundedConfig
                          False
                          ()
                          [sizedBVZext (Proxy @10) a ==~ d, d ==~ (con 0b111111 :: SymIntN 10)]
                        testCegis
                          unboundedConfig
                          False
                          ()
                          [sizedBVZext (Proxy @10) a ==~ d, d ==~ (con 0b1111111111 :: SymIntN 10)],
                      testCase "sizedBVZext when lowered twice" $ do
                        testCegis
                          unboundedConfig
                          True
                          a
                          [sizedBVZext (Proxy @10) (sizedBVSelect (Proxy @2) (Proxy @2) (sizedBVConcat a b) :: SymIntN 2) ==~ (con 1 :: SymIntN 10)]
                        testCegis
                          unboundedConfig
                          True
                          b
                          [sizedBVZext (Proxy @10) (sizedBVSelect (Proxy @7) (Proxy @2) (sizedBVConcat a b) :: SymIntN 2) ==~ (con 1 :: SymIntN 10)]
                    ],
                  testGroup
                    "Sext"
                    [ testCase "sizedBVSext" $ do
                        testCegis
                          unboundedConfig
                          True
                          ()
                          [sizedBVSext (Proxy @10) a ==~ d, a ==~ con 1, d ==~ (con 1 :: SymIntN 10)]
                        testCegis
                          unboundedConfig
                          True
                          ()
                          [sizedBVSext (Proxy @10) a ==~ d, a ==~ con 0b11111, d ==~ (con 0b1111111111 :: SymIntN 10)]
                        testCegis
                          unboundedConfig
                          False
                          ()
                          [sizedBVSext (Proxy @10) a ==~ d, d ==~ (con 0b111111 :: SymIntN 10)]
                        testCegis
                          unboundedConfig
                          False
                          ()
                          [sizedBVSext (Proxy @10) a ==~ d, d ==~ (con 0b11111 :: SymIntN 10)],
                      testCase "sizedBVSext when lowered twice" $ do
                        testCegis
                          unboundedConfig
                          True
                          a
                          [sizedBVSext (Proxy @10) (sizedBVSelect (Proxy @2) (Proxy @2) (sizedBVConcat a b) :: SymIntN 2) ==~ (con 1 :: SymIntN 10)]
                        testCegis
                          unboundedConfig
                          True
                          b
                          [sizedBVSext (Proxy @10) (sizedBVSelect (Proxy @7) (Proxy @2) (sizedBVConcat a b) :: SymIntN 2) ==~ (con 1 :: SymIntN 10)]
                    ]
                ]
        ]
