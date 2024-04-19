{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Grisette.Backend.SBV.Data.SMT.CEGISTests (cegisTests) where

import Control.Monad.Except (ExceptT)
import Data.Proxy (Proxy (Proxy))
import qualified Data.SBV as SBV
import Data.String (IsString (fromString))
import GHC.Stack (HasCallStack)
import Grisette.Backend.SBV (GrisetteSMTConfig, precise, z3)
import Grisette.Core.Control.Exception
  ( VerificationConditions,
  )
import Grisette.Core.Control.Monad.UnionM (UnionM)
import Grisette.Core.Data.Class.BitVector
  ( SizedBV (sizedBVConcat, sizedBVSelect, sizedBVSext, sizedBVZext),
  )
import Grisette.Core.Data.Class.CEGISSolver
  ( CEGISResult (CEGISSuccess),
    cegis,
    cegisExceptVC,
    cegisForAllExceptVC,
    cegisMultiInputs,
    cegisPostCond,
  )
import Grisette.Core.Data.Class.Error
  ( symAssert,
    symAssume,
  )
import Grisette.Core.Data.Class.EvaluateSym (EvaluateSym (evaluateSym))
import Grisette.Core.Data.Class.ExtractSymbolics
  ( ExtractSymbolics,
  )
import Grisette.Core.Data.Class.Function (Apply (apply), Function ((#)))
import Grisette.Core.Data.Class.ITEOp (ITEOp (symIte))
import Grisette.Core.Data.Class.LogicalOp
  ( LogicalOp (symNot, symXor, (.&&), (.||)),
  )
import Grisette.Core.Data.Class.SEq (SEq ((.==)))
import Grisette.Core.Data.Class.SOrd (SOrd ((.<), (.>=)))
import Grisette.Core.Data.Class.SimpleMergeable (mrgIf)
import Grisette.Core.Data.Class.Solvable (Solvable (con))
import Grisette.Core.Data.Class.Solver (solve)
import Grisette.SymPrim
  ( SymBool,
    SymIntN,
    SymInteger,
    type (-~>),
    type (=~>),
  )
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (Assertion, assertFailure, (@=?))

testCegis ::
  (HasCallStack, ExtractSymbolics a, EvaluateSym a, Show a, SEq a) =>
  GrisetteSMTConfig i ->
  Bool ->
  a ->
  (a -> [SymBool]) ->
  Assertion
testCegis config shouldSuccess inputs bs = do
  cegisExceptVCResult <-
    cegisExceptVC config (inputs, "internal" :: SymInteger) return $
      \(cexInputs, internal) -> buildFormula internal (bs cexInputs)
  case cegisExceptVCResult of
    (_, CEGISSuccess m) -> do
      shouldSuccess @=? True
      verify "cegisExceptVC" m (bs inputs)
    _ -> shouldSuccess @=? False
  cegisForAllExceptVCResult <-
    cegisForAllExceptVC config (inputs, "internal" :: SymInteger) return $
      buildFormula "internal" (bs inputs)
  case cegisForAllExceptVCResult of
    (_, CEGISSuccess m) -> do
      shouldSuccess @=? True
      verify "cegisForAllExceptVC" m (bs inputs)
    _ -> shouldSuccess @=? False
  where
    verify _ _ [] = return ()
    verify funName m (v : vs) = do
      y <- solve config (evaluateSym False m $ symNot v)
      case y of
        Left _ -> verify funName m vs
        Right _ ->
          assertFailure $
            funName
              ++ ": Failed to verify "
              ++ show v
              ++ " with the model "
              ++ show m
    buildFormula internal l = do
      symAssume (internal .>= 0)
      go l 0
      where
        go ::
          [SymBool] -> SymInteger -> ExceptT VerificationConditions UnionM ()
        go [] _ = return ()
        go (x : xs) i =
          mrgIf
            (internal .>= i .&& internal .< (i + 1))
            (symAssert x)
            (go xs (i + 1))

cegisTests :: Test
cegisTests =
  let unboundedConfig = precise SBV.z3
   in testGroup
        "CEGIS"
        [ testGroup
            "Regression"
            [ testCase "Empty symbolic inputs makes cegis work like solve" $ do
                (_, CEGISSuccess m1) <-
                  cegisMultiInputs
                    (precise z3)
                    [1 :: Integer, 2]
                    (\idx -> cegisPostCond $ fromString $ "a" ++ show idx)
                Right m2 <- solve (precise z3) ("a1" .&& "a2")
                m1 @=? m2,
              testCase "Lowering of TabularFun" $ do
                let s1 = "s1" :: SymInteger =~> SymInteger
                let s2 = "s2" :: SymInteger =~> SymInteger
                (_, CEGISSuccess m1) <-
                  cegis unboundedConfig ("cond" :: SymBool) $
                    \cond ->
                      cegisPostCond $
                        apply (symIte cond s1 s2) (symIte cond 1 2)
                          .== 10
                          .&& apply (symIte cond s1 s2) (symIte cond 3 4)
                            .== 100
                let s1e = evaluateSym False m1 s1
                let s2e = evaluateSym False m1 s2
                s1e # 1 @=? 10
                s1e # 3 @=? 100
                s2e # 2 @=? 10
                s2e # 4 @=? 100,
              testCase "Lowering of GeneralFun" $ do
                let s1 = "s1" :: SymInteger -~> SymInteger
                let s2 = "s2" :: SymInteger -~> SymInteger
                (_, CEGISSuccess m1) <-
                  cegis unboundedConfig ("cond" :: SymBool) $
                    \cond ->
                      cegisPostCond $
                        apply (symIte cond s1 s2) (symIte cond 1 2)
                          .== 10
                          .&& apply (symIte cond s1 s2) (symIte cond 3 4)
                            .== 100
                let s1e = evaluateSym False m1 s1
                let s2e = evaluateSym False m1 s2
                s1e # 1 @=? 10
                s1e # 3 @=? 100
                s2e # 2 @=? 10
                s2e # 4 @=? 100
            ],
          testGroup
            "Boolean"
            [ testCase "Basic" $ do
                testCegis unboundedConfig True () $ const ["a", "b", "c"]
                testCegis unboundedConfig False () $ const ["a", symNot "a"],
              testCase "And" $ do
                testCegis unboundedConfig True () $
                  const ["a" .&& "b", "b" .&& symNot "c", "a", "b", symNot "c"]
                testCegis unboundedConfig False () $
                  const ["a" .&& "b", "b" .&& symNot "c", "a", "b", "c"]
                testCegis unboundedConfig True ("a" :: SymBool) $
                  \a -> [symNot $ a .&& "b", symNot "b"]
                testCegis unboundedConfig False ("a" :: SymBool) $
                  \a -> [symNot $ a .&& "b", "b"],
              testCase "Or" $ do
                testCegis unboundedConfig True () $
                  const ["a" .|| "b", "b" .|| symNot "c", "a", "b", symNot "c"]
                testCegis unboundedConfig True () $
                  const ["a" .|| "b", "b" .|| symNot "c", "a", "b", "c"]
                testCegis unboundedConfig True ("a" :: SymBool) $
                  \a -> [a .|| "b", "b"]
                testCegis unboundedConfig False ("a" :: SymBool) $
                  \a -> [a .|| "b", symNot "b"],
              testCase "And / Or should be consistent" $ do
                testCegis unboundedConfig True () $
                  const ["a" .&& "b", "a" .|| "b"]
                testCegis unboundedConfig True () $
                  const [symNot "a" .&& "b", "a" .|| "b"]
                testCegis unboundedConfig False () $
                  const ["a" .&& "b", symNot $ "a" .|| "b"]
                testCegis unboundedConfig True () $
                  const [symNot $ "a" .&& "b", symNot $ "a" .|| "b"],
              testCase "Eqv" $ do
                testCegis unboundedConfig True () $
                  const [("a" :: SymBool) .== "b", "a", "b"]
                testCegis unboundedConfig True () $
                  const [("a" :: SymBool) .== "b", symNot "a", symNot "b"]
                testCegis unboundedConfig False () $
                  const [("a" :: SymBool) .== "b", symNot "a", "b"]
                testCegis unboundedConfig False () $
                  const [("a" :: SymBool) .== "b", symNot "a", "b"]
                testCegis unboundedConfig True () $
                  const [("a" :: SymBool) .== "b", symNot "a" `symXor` "b"]
                testCegis unboundedConfig False () $
                  const [("a" :: SymBool) .== "b", "a" `symXor` "b"],
              testCase "symIte" $ do
                testCegis unboundedConfig True ("c" :: SymBool) $
                  \c -> [symIte "a" "b" c, "a", "b"]
                testCegis unboundedConfig False ("c" :: SymBool) $
                  \c -> [symIte "a" "b" c, symNot "a"]
                testCegis unboundedConfig True ("b" :: SymBool) $
                  \b -> [symIte "a" b "c", symNot "a", "c"]
                testCegis unboundedConfig False ("b" :: SymBool) $
                  \b -> [symIte "a" b "c", "a"]
                testCegis unboundedConfig True () $
                  const [symIte "a" "b" "c", "a", "b", "c"]
                testCegis unboundedConfig True () $
                  const [symIte "a" "b" "c", "a", "b", symNot "c"]
                testCegis unboundedConfig True () $
                  const [symIte "a" "b" "c", symNot "a", "b", "c"]
                testCegis unboundedConfig True () $
                  const [symIte "a" "b" "c", symNot "a", symNot "b", "c"]
                testCegis unboundedConfig False () $
                  const [symIte "a" "b" "c", "a", symNot "b", "c"]
                testCegis unboundedConfig False () $
                  const [symIte "a" "b" "c", "a", symNot "b", symNot "c"]
                testCegis unboundedConfig False () $
                  const [symIte "a" "b" "c", symNot "a", "b", symNot "c"]
                testCegis unboundedConfig False () $
                  const [symIte "a" "b" "c", symNot "a", symNot "b", symNot "c"]
            ],
          let a = "a" :: SymIntN 5
              b = "b" :: SymIntN 5
              c = "c" :: SymIntN 5
              d = "c" :: SymIntN 10
           in testGroup
                "Different sized BV"
                [ testGroup
                    "Select"
                    [ testCase "sizedBVSelect" $ do
                        testCegis unboundedConfig True () $
                          const
                            [ sizedBVSelect (Proxy @2) (Proxy @2) a
                                .== (con 1 :: SymIntN 2),
                              a .== con 0b10101
                            ]
                        testCegis unboundedConfig False () $
                          const
                            [ sizedBVSelect (Proxy @2) (Proxy @2) a
                                .== (con 1 :: SymIntN 2),
                              a .== con 0b10001
                            ],
                      testCase "sizedBVSelect when lowered twice" $ do
                        testCegis unboundedConfig True a $
                          \ca ->
                            [ sizedBVSelect
                                (Proxy @2)
                                (Proxy @2)
                                (sizedBVConcat ca b)
                                .== (con 1 :: SymIntN 2)
                            ]
                        testCegis unboundedConfig True b $
                          \cb ->
                            [ sizedBVSelect
                                (Proxy @7)
                                (Proxy @2)
                                (sizedBVConcat a cb)
                                .== (con 1 :: SymIntN 2)
                            ]
                    ],
                  testGroup
                    "Concat"
                    [ testCase "sizedBVConcat" $ do
                        testCegis unboundedConfig True () $
                          const
                            [ sizedBVConcat a b .== d,
                              a .== con 1,
                              b .== con 1,
                              d .== con 0b100001
                            ]
                        testCegis unboundedConfig False () $
                          const
                            [ sizedBVConcat a b .== d,
                              a .== con 1,
                              b .== con 1,
                              d .== con 0b100010
                            ],
                      testCase "sizedBVConcat when lowered twice" $ do
                        testCegis unboundedConfig True (a, c) $
                          \(ca, cc) ->
                            [ sizedBVConcat
                                cc
                                ( sizedBVSelect
                                    (Proxy @2)
                                    (Proxy @2)
                                    (sizedBVConcat ca b) ::
                                    SymIntN 2
                                )
                                .== sizedBVConcat cc (con 1 :: SymIntN 2)
                            ]
                        testCegis unboundedConfig True (b, c) $
                          \(cb, cc) ->
                            [ sizedBVConcat
                                cc
                                ( sizedBVSelect
                                    (Proxy @7)
                                    (Proxy @2)
                                    (sizedBVConcat a cb) ::
                                    SymIntN 2
                                )
                                .== sizedBVConcat cc (con 1 :: SymIntN 2)
                            ]
                    ],
                  testGroup
                    "Zext"
                    [ testCase "sizedBVZext" $ do
                        testCegis unboundedConfig True () $
                          const
                            [ sizedBVZext (Proxy @10) a .== d,
                              a .== con 1,
                              d .== (con 1 :: SymIntN 10)
                            ]
                        testCegis unboundedConfig True () $
                          const
                            [ sizedBVZext (Proxy @10) a .== d,
                              a .== con 0b11111,
                              d .== (con 0b11111 :: SymIntN 10)
                            ]
                        testCegis unboundedConfig False () $
                          const
                            [ sizedBVZext (Proxy @10) a .== d,
                              d .== (con 0b111111 :: SymIntN 10)
                            ]
                        testCegis unboundedConfig False () $
                          const
                            [ sizedBVZext (Proxy @10) a .== d,
                              d .== (con 0b1111111111 :: SymIntN 10)
                            ],
                      testCase "sizedBVZext when lowered twice" $ do
                        testCegis unboundedConfig True a $
                          \ca ->
                            [ sizedBVZext
                                (Proxy @10)
                                ( sizedBVSelect
                                    (Proxy @2)
                                    (Proxy @2)
                                    (sizedBVConcat ca b) ::
                                    SymIntN 2
                                )
                                .== (con 1 :: SymIntN 10)
                            ]
                        testCegis unboundedConfig True b $
                          \cb ->
                            [ sizedBVZext
                                (Proxy @10)
                                ( sizedBVSelect
                                    (Proxy @7)
                                    (Proxy @2)
                                    (sizedBVConcat a cb) ::
                                    SymIntN 2
                                )
                                .== (con 1 :: SymIntN 10)
                            ]
                    ],
                  testGroup
                    "Sext"
                    [ testCase "sizedBVSext" $ do
                        testCegis unboundedConfig True () $
                          const
                            [ sizedBVSext (Proxy @10) a .== d,
                              a .== con 1,
                              d .== (con 1 :: SymIntN 10)
                            ]
                        testCegis unboundedConfig True () $
                          const
                            [ sizedBVSext (Proxy @10) a .== d,
                              a .== con 0b11111,
                              d .== (con 0b1111111111 :: SymIntN 10)
                            ]
                        testCegis unboundedConfig False () $
                          const
                            [ sizedBVSext (Proxy @10) a .== d,
                              d .== (con 0b111111 :: SymIntN 10)
                            ]
                        testCegis unboundedConfig False () $
                          const
                            [ sizedBVSext (Proxy @10) a .== d,
                              d .== (con 0b11111 :: SymIntN 10)
                            ],
                      testCase "sizedBVSext when lowered twice" $ do
                        testCegis unboundedConfig True a $ \ca ->
                          [ sizedBVSext
                              (Proxy @10)
                              ( sizedBVSelect
                                  (Proxy @2)
                                  (Proxy @2)
                                  (sizedBVConcat ca b) ::
                                  SymIntN 2
                              )
                              .== (con 1 :: SymIntN 10)
                          ]
                        testCegis unboundedConfig True b $
                          \cb ->
                            [ sizedBVSext
                                (Proxy @10)
                                ( sizedBVSelect
                                    (Proxy @7)
                                    (Proxy @2)
                                    (sizedBVConcat a cb) ::
                                    SymIntN 2
                                )
                                .== (con 1 :: SymIntN 10)
                            ]
                    ]
                ]
        ]
