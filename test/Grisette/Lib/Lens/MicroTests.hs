{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Grisette.Lib.Lens.MicroTests
  ( microTests,
    optimise,
    Expr (..),
    mrgVar,
    mrgLit,
    mrgAdd,
  )
where

import Control.Applicative (Alternative ((<|>)))
import GHC.Generics (Generic)
import Grisette
  ( Default (Default),
    EvaluateSym,
    Mergeable,
    SymInteger,
    UnionM,
    mkMergeConstructor,
    mrgIf,
  )
import Grisette.Core.Data.Class.ITEOp (ITEOp (symIte))
import Grisette.Core.Data.Class.LogicalOp (LogicalOp (symNot))
import Grisette.Core.Data.Class.SEq (SEq)
import Grisette.Core.Data.Class.TryMerge (mrgSingle)
import Grisette.Lib.Control.Monad (mrgReturn)
import Grisette.Lib.Data.Function ((.&))
import Grisette.Lib.Data.Functor ((.<&>))
import Grisette.Lib.Data.Maybe (mrgJust, mrgNothing)
import Grisette.Lib.Lens.Micro
  ( mrgForOf_,
    mrgOver,
    mrgRewriteOf,
    mrgSet,
    mrgTransformOf,
    mrgTraverseOf_,
    (.%~),
    (.+~),
    (.-~),
    (..?~),
    (..~),
    (.<%~),
    (.<<%~),
    (.<<.~),
    (.<>~),
    (.?~),
    (.^.),
    (.^..),
    (.^?),
  )
import Grisette.TestUtil.NoMerge (noMergeNotMerged)
import Grisette.TestUtil.SymbolicAssertion ((.@?=))
import Lens.Micro (Traversal', traversed, (&))
import Lens.Micro.TH (makeLenses)
import Test.Framework
  ( Test,
    TestOptions' (topt_timeout),
    plusTestOptions,
    testGroup,
  )
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit ((@?=))

data A
  = A1 {_a1 :: UnionM [SymInteger], _a2 :: SymInteger}
  | A2 {_a1 :: UnionM [SymInteger]}
  | A3 {_a2 :: SymInteger}
  | A4 {_a4 :: UnionM Int}
  | A5 {_a5 :: UnionM (Maybe SymInteger), _a6 :: Maybe (UnionM SymInteger)}
  deriving (Eq, Show, Generic)
  deriving (Mergeable) via (Default A)

makeLenses ''A

value1 :: A
value1 = A1 (mrgIf "c1" (return ["v1"]) (return ["v2", "v3"])) "v4"

value2 :: A
value2 = A2 (mrgIf "c2" (return ["u1"]) (return ["u2", "u3"]))

value3 :: A
value3 = A3 "w"

value4 :: A
value4 = A4 (mrgIf "c4" (return 1) (return 2))

value5 :: A
value5 =
  A5
    (mrgIf "c5" (mrgJust "x1") mrgNothing)
    (Just (mrgReturn "x2"))

value12 :: UnionM A
value12 = mrgIf "c12" (return value1) (return value2)

value13 :: UnionM A
value13 = mrgIf "c13" (return value1) (return value3)

data Expr = Var String | Lit Int | Add (UnionM Expr) (UnionM Expr)
  deriving (Show, Eq, Generic)
  deriving (Mergeable, SEq, EvaluateSym) via (Default Expr)

mkMergeConstructor "mrg" ''Expr

subExprs :: Traversal' Expr (UnionM Expr)
subExprs f = \case
  Var name -> pure (Var name)
  Lit n -> pure (Lit n)
  Add a b -> Add <$> f a <*> f b

constantFold :: Expr -> UnionM (Maybe Expr)
constantFold (Add l r) = do
  l1 <- l
  r1 <- r
  case (l1, r1) of
    (Lit lv, Lit rv) -> mrgJust (Lit (lv + rv))
    _ -> mrgNothing
constantFold _ = mrgNothing

zeroAdditionIdentity :: Expr -> UnionM (Maybe Expr)
zeroAdditionIdentity (Add l r) = do
  l1 <- l
  r1 <- r
  case (l1, r1) of
    (Lit 0, x) -> mrgJust x
    (x, Lit 0) -> mrgJust x
    _ -> mrgNothing
zeroAdditionIdentity _ = mrgNothing

optimise :: Expr -> UnionM Expr
optimise =
  mrgRewriteOf
    subExprs
    ( \expr -> do
        constantFoldRes <- constantFold expr
        zeroAdditionIdentityRes <- zeroAdditionIdentity expr
        mrgReturn $ constantFoldRes <|> zeroAdditionIdentityRes
    )

constantFoldSingle :: Expr -> UnionM Expr
constantFoldSingle (Add l r) = do
  l1 <- l
  r1 <- r
  case (l1, r1) of
    (Lit lv, Lit rv) -> mrgLit (lv + rv)
    _ -> mrgAdd (mrgReturn l1) (mrgReturn r1)
constantFoldSingle v = mrgReturn v

microTests :: Test
microTests =
  testGroup "Micro" $
    concat
      [ do
          (name, op) <- [("(.%~)", (.%~)), ("mrgOver", mrgOver)]
          return . testGroup name $
            [ testCase "value13" $ do
                let actual = value13 .<&> a1 `op` (++ ["a"])
                let expected =
                      mrgIf
                        "c13"
                        ( return $
                            A1
                              ( mrgIf
                                  "c1"
                                  (return ["v1", "a"])
                                  (return ["v2", "v3", "a"])
                              )
                              "v4"
                        )
                        (return value3)
                actual @?= expected,
              testCase "value12" $ do
                let actual = value12 .<&> a1 `op` (++ ["a"])
                let expected =
                      mrgIf
                        "c12"
                        ( return $
                            A1
                              ( mrgIf
                                  "c1"
                                  (return ["v1", "a"])
                                  (return ["v2", "v3", "a"])
                              )
                              "v4"
                        )
                        ( return $
                            A2
                              ( mrgIf
                                  "c2"
                                  (return ["u1", "a"])
                                  (return ["u2", "u3", "a"])
                              )
                        )
                actual @?= expected
            ],
        do
          (value, name, op, v, vop) <-
            [ (value4, ".+~", (.+~), 1, (+ 1)),
              (value4, ".-~", (.-~), 1, (\x -> x - 1))
              ]
          return . testCase name $ do
            let actual = value & a4 `op` v
            let expected =
                  A4 (mrgIf "c4" (return $ vop 1) (return $ vop 2))
            actual @?= expected,
        [ testCase ".<>~" $ do
            let actual = value1 & a1 .<>~ ["b"]
            let expected =
                  A1
                    ( mrgIf "c1" (return ["v1", "b"]) (return ["v2", "v3", "b"])
                    )
                    "v4"
            actual @?= expected
        ],
        do
          (name, op) <- [("(..~)", (..~)), ("mrgSet", mrgSet)]
          return $ testCase name $ do
            let actual = value1 & a1 `op` ["b"]
            let expected = A1 (mrgReturn ["b"]) "v4"
            actual @?= expected,
        [ testCase ".?~" $ do
            let actual = value5 & a5 .?~ "y"
            let expected =
                  A5
                    (mrgJust "y")
                    (Just (mrgReturn "x2"))
            actual @?= expected,
          testCase "..?~" $ do
            let actual = value5 & a6 ..?~ "y"
            let expected =
                  A5
                    (mrgIf "c5" (mrgJust "x1") mrgNothing)
                    (Just (mrgReturn "y"))
            actual @?= expected,
          testCase ".<%~" $ do
            let actual = value1 & a1 .<%~ (++ ["b"])
            let u = mrgIf "c1" (return ["v1", "b"]) (return ["v2", "v3", "b"])
            let expected = (u, A1 u "v4")
            actual @?= expected,
          testCase ".<<%~" $ do
            let actual = value1 & a1 .<<%~ (++ ["b"])
            let uorig = mrgIf "c1" (return ["v1"]) (return ["v2", "v3"])
            let u = mrgIf "c1" (return ["v1", "b"]) (return ["v2", "v3", "b"])
            let expected = (uorig, A1 u "v4")
            actual @?= expected,
          testCase ".<<.~" $ do
            let actual = value1 & a1 .<<.~ ["b"]
            let uorig = mrgIf "c1" (return ["v1"]) (return ["v2", "v3"])
            let expected = (uorig, A1 (mrgReturn ["b"]) "v4")
            actual @?= expected,
          testGroup
            "mrgRewriteOf"
            [ testCase "simple" $ do
                let expr = Add (mrgLit 2) (mrgAdd (mrgLit (-1)) (mrgLit (-1)))
                let actual = optimise expr
                let expected = mrgLit 0
                actual @?= expected,
              testCase "complex" $ do
                let expr =
                      Add
                        (mrgVar "a")
                        ( mrgAdd
                            (mrgIf "z" (mrgLit (-1)) (mrgVar "b"))
                            (mrgIf "x" (mrgLit 1) (mrgLit 10))
                        )
                let actual = optimise expr
                let expected =
                      mrgIf
                        "z"
                        ( mrgIf
                            "x"
                            (mrgVar "a")
                            (mrgAdd (mrgVar "a") (mrgLit 9))
                        )
                        ( mrgAdd
                            (mrgVar "a")
                            ( mrgAdd
                                (mrgVar "b")
                                (mrgIf "x" (mrgLit 1) (mrgLit 10))
                            )
                        )
                actual .@?= expected
            ],
          testGroup
            "mrgTransformOf"
            [ testCase "simple" $ do
                let expr = Add (mrgLit 2) (mrgAdd (mrgLit (-1)) (mrgLit (-1)))
                let actual = mrgTransformOf subExprs constantFoldSingle expr
                let expected = mrgLit 0
                actual @?= expected,
              testCase "complex" $ do
                let expr =
                      Add
                        (mrgVar "a")
                        ( mrgAdd
                            (mrgIf "z" (mrgLit (-1)) (mrgVar "b"))
                            (mrgIf "x" (mrgLit 1) (mrgLit 10))
                        )
                let actual = mrgTransformOf subExprs constantFoldSingle expr
                let expected =
                      mrgAdd
                        (mrgVar "a")
                        ( mrgIf
                            "z"
                            (mrgIf "x" (mrgLit 0) (mrgLit 9))
                            ( mrgAdd
                                (mrgVar "b")
                                (mrgIf "x" (mrgLit 1) (mrgLit 10))
                            )
                        )
                actual .@?= expected
            ],
          testCase ".^." $ do
            let actual = value12 .^. a1
            let expected =
                  mrgIf
                    (symIte "c12" "c1" "c2")
                    (return [symIte "c12" "v1" "u1"])
                    ( return
                        [symIte "c12" "v2" "u2", symIte "c12" "v3" "u3"]
                    )
            actual @?= expected,
          testCase ".^.." $ do
            let expr =
                  Add (mrgLit 2) (mrgAdd (mrgLit (-1)) (mrgLit (-1)))
            let actual = expr .^.. subExprs
            let expected =
                  mrgReturn [Lit 2, Add (mrgLit (-1)) (mrgLit (-1))]
            actual @?= expected,
          testCase ".^?" $ do
            let actual = value13 .& (.^? a1)
            let expected =
                  mrgIf
                    (symNot "c13")
                    mrgNothing
                    (mrgIf "c1" (mrgJust ["v1"]) (mrgJust ["v2", "v3"]))
            actual @?= expected,
          plusTestOptions (mempty {topt_timeout = Just (Just 1000000)}) $
            testCase "mrgTraverseOf_" $ do
              let expr = [1 .. 1000]
              let actual = mrgTraverseOf_ traversed (const noMergeNotMerged) expr
              let expected = mrgReturn ()
              actual @?= expected,
          plusTestOptions (mempty {topt_timeout = Just (Just 1000000)}) $
            testCase "mrgForOf_" $ do
              let expr = [1 .. 1000]
              let actual = mrgForOf_ traversed expr (const noMergeNotMerged)
              let expected = mrgReturn ()
              actual @?= expected
        ]
      ]
