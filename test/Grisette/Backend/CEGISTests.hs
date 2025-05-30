{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Grisette.Backend.CEGISTests (cegisTests) where

import Control.Monad.Except (ExceptT)
import Data.IORef (atomicModifyIORef', modifyIORef', newIORef, readIORef)
import Data.Proxy (Proxy (Proxy))
import Data.String (IsString (fromString))
import GHC.Stack (HasCallStack)
import Grisette
  ( Apply (apply),
    AsKey (AsKey),
    CEGISResult (CEGISSolverFailure, CEGISSuccess, CEGISVerifierFailure),
    EvalSym (evalSym),
    ExtractSym,
    Function ((#)),
    GrisetteSMTConfig,
    ITEOp (symIte),
    LogicalOp (symNot, symXor, true, (.&&), (.||)),
    ModelRep (buildModel),
    ModelValuePair ((::=)),
    SizedBV (sizedBVConcat, sizedBVSelect, sizedBVSext, sizedBVZext),
    Solvable (con),
    SymEq ((.==)),
    SymOrd ((.<), (.>=)),
    Union,
    VerificationConditions,
    VerifierResult (CEGISVerifierFoundCex, CEGISVerifierNoCex),
    cegis,
    cegisExceptVC,
    cegisForAll,
    cegisForAllExceptVC,
    cegisMultiInputs,
    cegisPostCond,
    mrgIf,
    solve,
    solverGenericCEGIS,
    symAssert,
    symAssume,
    withSolver,
    z3,
  )
import Grisette.SymPrim
  ( SymBool,
    SymIntN,
    SymInteger,
    type (-~>),
    type (=~>),
  )
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (Assertion, assertFailure, (@?=))

testCegis ::
  (HasCallStack, ExtractSym a, EvalSym a, SymEq a) =>
  GrisetteSMTConfig ->
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
      shouldSuccess @?= True
      verify "cegisExceptVC" m (bs inputs)
    _ -> shouldSuccess @?= False
  cegisForAllExceptVCResult <-
    cegisForAllExceptVC config (inputs, "internal" :: SymInteger) return $
      buildFormula "internal" (bs inputs)
  case cegisForAllExceptVCResult of
    (_, CEGISSuccess m) -> do
      shouldSuccess @?= True
      verify "cegisForAllExceptVC" m (bs inputs)
    _ -> shouldSuccess @?= False
  where
    verify _ _ [] = return ()
    verify funName m (v : vs) = do
      y <- solve config (evalSym False m $ symNot v)
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
          [SymBool] -> SymInteger -> ExceptT VerificationConditions Union ()
        go [] _ = return ()
        go (x : xs) i =
          mrgIf
            (internal .>= i .&& internal .< (i + 1))
            (symAssert x)
            (go xs (i + 1))

cegisTests :: Test
cegisTests =
  let unboundedConfig = z3
   in testGroup
        "CEGIS"
        [ testGroup
            "Regression"
            [ testCase "Empty symbolic inputs makes cegis work like solve" $ do
                (_, CEGISSuccess m1) <-
                  cegisMultiInputs
                    z3
                    [1 :: Integer, 2]
                    (\idx -> cegisPostCond $ fromString $ "a" ++ show idx)
                Right m2 <- solve z3 ("a1" .&& "a2")
                m1 @?= m2,
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
                let s1e = evalSym False m1 s1
                let s2e = evalSym False m1 s2
                AsKey (s1e # 1) @?= AsKey 10
                AsKey (s1e # 3) @?= AsKey 100
                AsKey (s2e # 2) @?= AsKey 10
                AsKey (s2e # 4) @?= AsKey 100,
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
                let s1e = evalSym False m1 s1
                let s2e = evalSym False m1 s2
                AsKey (s1e # 1) @?= AsKey 10
                AsKey (s1e # 3) @?= AsKey 100
                AsKey (s2e # 2) @?= AsKey 10
                AsKey (s2e # 4) @?= AsKey 100
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
                ],
          testCase "cegisMultiInputs" $ do
            r <-
              cegisMultiInputs
                unboundedConfig
                [["a" :: SymInteger], ["b", "c"]]
                ( \case
                    [a] ->
                      cegisPostCond $
                        symIte
                          (a .== 1)
                          "x"
                          ("x" .&& symNot "y")
                    [b, _] ->
                      cegisPostCond $
                        symIte
                          (b .== 1)
                          (symIte "x" "z" "w")
                          (symIte "x" (symNot "w") (symNot "z"))
                    _ -> cegisPostCond $ con True
                )
            case snd r of
              CEGISSuccess m -> do
                let expectedModel =
                      buildModel
                        ( "x" ::= True,
                          "y" ::= False,
                          "z" ::= True,
                          "w" ::= False
                        )
                m @?= expectedModel
              CEGISVerifierFailure _ -> fail "Verifier failed"
              CEGISSolverFailure failure -> fail $ show failure,
          testCase "cegisForAll" $ do
            let a = "a" :: SymInteger
            let b = "b"
            r <-
              cegisForAll
                unboundedConfig
                [a, b]
                ( cegisPostCond $
                    symIte
                      (a .== 1)
                      ( symIte
                          (b .== 1)
                          "x"
                          ("x" .&& symNot "y")
                      )
                      ( symIte
                          (b .== 1)
                          (symIte "x" "z" "w")
                          (symIte "x" (symNot "w") (symNot "z"))
                      )
                )
            case snd r of
              CEGISSuccess m -> do
                let expectedModel =
                      buildModel
                        ( "x" ::= True,
                          "y" ::= False,
                          "z" ::= True,
                          "w" ::= False
                        )
                m @?= expectedModel
              CEGISVerifierFailure _ -> fail "Verifier failed"
              CEGISSolverFailure failure -> fail $ show failure,
          testGroup "rerun" $ do
            let verifier n trace retsIORef _ = do
                  modifyIORef' trace (n :)
                  ret <- atomicModifyIORef' retsIORef (\(x : xs) -> (xs, x))
                  if ret
                    then return $ CEGISVerifierFoundCex "Found"
                    else return $ CEGISVerifierNoCex True
            let createTestCase :: String -> Bool -> [[Bool]] -> [Int] -> Test
                createTestCase name rerun rets expected = testCase name $ do
                  trace <- newIORef []
                  retsIORefs <- traverse newIORef rets
                  withSolver unboundedConfig $ \handle ->
                    solverGenericCEGIS
                      handle
                      rerun
                      true
                      (const $ return true)
                      (zipWith (`verifier` trace) [0 ..] retsIORefs)
                  tracev <- readIORef trace
                  tracev @?= expected
            [ createTestCase
                "no rerun"
                False
                [[False], [True, False], [False]]
                [2, 1, 1, 0],
              createTestCase
                "do rerun"
                True
                [[False, False], [True, False], [False]]
                [0, 2, 1, 1, 0],
              createTestCase
                "do rerun complex"
                True
                [ [False, False, False],
                  [True, False, True, False],
                  [True, False, False],
                  [False, False]
                ]
                [0, 3, 2, 1, 1, 0, 3, 2, 2, 1, 1, 0]
              ]
        ]
