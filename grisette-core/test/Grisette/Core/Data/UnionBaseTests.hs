{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Grisette.Core.Data.UnionBaseTests where

import GHC.Generics
import Grisette.Core.Data.Class.Bool
import Grisette.Core.Data.Class.Mergeable
import Grisette.Core.Data.UnionBase
import Grisette.TestUtils.SBool
-- import Test.Hspec

import Test.Tasty
import Test.Tasty.HUnit

data TripleSum a b c = TS1 a | TS2 b | TS3 c deriving (Show, Eq, Generic)

instance (SymBoolOp bool, GMergeable bool a, GMergeable bool b, GMergeable bool c) => GMergeable bool (TripleSum a b c) where
  grootStrategy =
    SortedStrategy
      (\case TS1 _ -> (0 :: Int); TS2 _ -> (1 :: Int); TS3 _ -> (2 :: Int))
      ( \case
          0 -> gwrapStrategy grootStrategy TS1 (\(TS1 x) -> x)
          1 -> gwrapStrategy grootStrategy TS2 (\(TS2 x) -> x)
          2 -> gwrapStrategy grootStrategy TS3 (\(TS3 x) -> x)
          _ -> error "Bad"
      )

unionBaseTests :: TestTree
unionBaseTests =
  testGroup
    "UnionBaseTests"
    [ testGroup
        "ifWithLeftMost"
        [ testCase "ifWithLeftMost should maintain left most info on Singles" $ do
            ifWithLeftMost False (SSBool "a") (Single (1 :: Integer)) (Single 2)
              @=? If 1 False (SSBool "a") (Single 1) (Single 2),
          testCase "ifWithLeftMost should maintain left most info on Ifs" $ do
            ifWithLeftMost
              True
              (SSBool "a")
              (If 1 True (SSBool "b") (Single (1 :: Integer)) (Single 2))
              (If 3 True (SSBool "c") (Single 3) (Single 4))
              @=? If
                1
                True
                (SSBool "a")
                (If 1 True (SSBool "b") (Single (1 :: Integer)) (Single 2))
                (If 3 True (SSBool "c") (Single 3) (Single 4))
        ],
      testGroup
        "ifWithStrategy"
        [ testGroup
            "ifWithStrategy with concrete condition"
            [ testCase "true" $ do
                ifWithStrategy grootStrategy (CBool True) (Single (1 :: Integer)) (Single 2) @=? Single 1,
              testCase "false" $ do
                ifWithStrategy grootStrategy (CBool False) (Single (1 :: Integer)) (Single 2) @=? Single 2
            ],
          let a = ifWithStrategy grootStrategy (SSBool "a") (Single (1 :: Integer)) (Single 2)
           in testGroup
                "ifWithStrategy with condition equal to sub conditions"
                [ testCase "ifTrue" $ do
                    ifWithStrategy grootStrategy (SSBool "a") a (Single 3)
                      @=? If 1 True (SSBool "a") (Single 1) (Single 3),
                  testCase "ifFalse" $ do
                    ifWithStrategy grootStrategy (SSBool "a") (Single 0) a
                      @=? If 0 True (SSBool "a") (Single 0) (Single 2)
                ],
          testCase "ifWithStrategy with simple mergeables" $ do
            ifWithStrategy grootStrategy (SSBool "a") (Single (SSBool "b")) (Single (SSBool "c"))
              @=? Single (ITE (SSBool "a") (SSBool "b") (SSBool "c")),
          testGroup
            "ifWithStrategy with ordered mergeables"
            [ testGroup
                "ifWithStrategy on Single/Single"
                [ testGroup
                    "idxt < idxf"
                    [ testCase "Integer" $
                        ifWithStrategy grootStrategy (SSBool "a") (Single (1 :: Integer)) (Single 2)
                          @=? If 1 True (SSBool "a") (Single 1) (Single 2),
                      testCase "Maybe Integer" $
                        ifWithStrategy grootStrategy (SSBool "a") (Single Nothing) (Single (Just (2 :: Integer)))
                          @=? If Nothing True (SSBool "a") (Single Nothing) (Single (Just 2))
                    ],
                  testGroup
                    "idxt == idxf"
                    [ testGroup
                        "idxt == idxf as terminal"
                        [ testCase "Integer" $
                            ifWithStrategy grootStrategy (SSBool "a") (Single (1 :: Integer)) (Single 1)
                              @=? Single 1,
                          testCase "Maybe Integer" $
                            ifWithStrategy grootStrategy (SSBool "a") (Single (Just (SSBool "b"))) (Single (Just (SSBool "c")))
                              @=? Single (Just (ITE (SSBool "a") (SSBool "b") (SSBool "c")))
                        ],
                      testGroup
                        "idxt == idxf but not terminal"
                        [ testCase "Maybe Integer" $
                            ifWithStrategy grootStrategy (SSBool "a") (Single (Just (1 :: Integer))) (Single (Just (2 :: Integer)))
                              @=? If (Just 1) True (SSBool "a") (Single $ Just 1) (Single (Just 2)),
                          testCase "Maybe (Maybe Integer)" $
                            ifWithStrategy
                              grootStrategy
                              (SSBool "a")
                              (Single $ Just $ Just $ SSBool "b")
                              (Single $ Just $ Just $ SSBool "c")
                              @=? Single (Just (Just (ITE (SSBool "a") (SSBool "b") (SSBool "c"))))
                        ]
                    ],
                  testGroup
                    "idxt > idxf"
                    [ testCase "Integer" $
                        ifWithStrategy grootStrategy (SSBool "a") (Single (2 :: Integer)) (Single 1)
                          @=? If 1 True (Not $ SSBool "a") (Single 1) (Single 2),
                      testCase "Maybe Integer" $
                        ifWithStrategy grootStrategy (SSBool "a") (Single (Just (2 :: Integer))) (Single Nothing)
                          @=? If Nothing True (Not $ SSBool "a") (Single Nothing) (Single (Just 2))
                    ]
                ],
              testGroup
                "ifWithStrategy on Single/If"
                [ testGroup
                    "Degenerate to Single/Single when idxft == idxff"
                    [ testCase "Degenerated case with idxt < idxf" $ do
                        let x =
                              ifWithStrategy
                                grootStrategy
                                (SSBool "a")
                                (Single (Just (1 :: Integer)))
                                (Single (Just (2 :: Integer)))
                        ifWithStrategy grootStrategy (SSBool "b") (Single Nothing) x
                          @=? If
                            Nothing
                            True
                            (SSBool "b")
                            (Single Nothing)
                            (If (Just 1) True (SSBool "a") (Single $ Just 1) (Single (Just 2))),
                      let x =
                            ifWithStrategy
                              grootStrategy
                              (SSBool "a")
                              (Single (Just (1 :: Integer)))
                              (Single (Just (3 :: Integer)))
                       in testGroup
                            "Degenerated case with idxt == idxf"
                            [ testCase "sub-idxt < sub-idxft" $
                                ifWithStrategy grootStrategy (SSBool "b") (Single $ Just 0) x
                                  @=? If
                                    (Just 0)
                                    True
                                    (SSBool "b")
                                    (Single $ Just 0)
                                    (If (Just 1) True (SSBool "a") (Single $ Just 1) (Single (Just 3))),
                              testCase "sub-idxt == sub-idxft" $
                                ifWithStrategy grootStrategy (SSBool "b") (Single $ Just 1) x
                                  @=? If (Just 1) True (Or (SSBool "b") (SSBool "a")) (Single $ Just 1) (Single (Just 3)),
                              testCase "subidxft < sub-idxt < sub-idxff" $
                                ifWithStrategy grootStrategy (SSBool "b") (Single $ Just 2) x
                                  @=? If
                                    (Just 1)
                                    True
                                    (And (Not (SSBool "b")) (SSBool "a"))
                                    (Single $ Just 1)
                                    (If (Just 2) True (SSBool "b") (Single $ Just 2) (Single $ Just 3)),
                              testCase "sub-idxt == sub-idxff" $
                                ifWithStrategy grootStrategy (SSBool "b") (Single $ Just 3) x
                                  @=? If (Just 1) True (And (Not (SSBool "b")) (SSBool "a")) (Single $ Just 1) (Single (Just 3)),
                              testCase "sub-idxff < sub-idxt" $
                                ifWithStrategy grootStrategy (SSBool "b") (Single $ Just 4) x
                                  @=? If
                                    (Just 1)
                                    True
                                    (And (Not (SSBool "b")) (SSBool "a"))
                                    (Single $ Just 1)
                                    (If (Just 3) True (Not $ SSBool "b") (Single $ Just 3) (Single $ Just 4))
                            ],
                      testCase "Degenerated case with idxt > idxf" $ do
                        let x =
                              ifWithStrategy
                                grootStrategy
                                (SSBool "a")
                                (Single (Left (1 :: Integer)))
                                (Single (Left (2 :: Integer)))
                        ifWithStrategy grootStrategy (SSBool "b") (Single $ Right (1 :: Integer)) x
                          @=? If
                            (Left 1)
                            True
                            (Not $ SSBool "b")
                            (If (Left 1) True (SSBool "a") (Single $ Left 1) (Single (Left 2)))
                            (Single $ Right 1)
                    ],
                  testCase "idxt < idxft" $ do
                    let x =
                          ifWithStrategy
                            grootStrategy
                            (SSBool "a")
                            (Single (1 :: Integer))
                            (Single (3 :: Integer))
                    ifWithStrategy grootStrategy (SSBool "b") (Single 0) x
                      @=? If 0 True (SSBool "b") (Single 0) (If 1 True (SSBool "a") (Single 1) (Single 3)),
                  testCase "idxt == idxft" $ do
                    let x =
                          ifWithStrategy
                            grootStrategy
                            (SSBool "a")
                            (Single $ Left (1 :: Integer))
                            (Single $ Right (3 :: Integer))
                    ifWithStrategy grootStrategy (SSBool "b") (Single $ Left 0) x
                      @=? If
                        (Left 0)
                        True
                        (Or (SSBool "b") (SSBool "a"))
                        (If (Left 0) True (SSBool "b") (Single $ Left 0) (Single $ Left 1))
                        (Single $ Right 3),
                  testCase "idxt > idxft" $ do
                    let x =
                          ifWithStrategy
                            grootStrategy
                            (SSBool "a")
                            (Single $ Left (1 :: Integer))
                            (Single $ Right (3 :: Integer))
                    ifWithStrategy grootStrategy (SSBool "b") (Single $ Right 0) x
                      @=? If
                        (Left 1)
                        True
                        (And (Not (SSBool "b")) (SSBool "a"))
                        (Single $ Left 1)
                        (If (Right 0) True (SSBool "b") (Single $ Right 0) (Single $ Right 3))
                ],
              testGroup
                "ifWithStrategy on If/Single"
                [ testGroup
                    "Degenerate to Single/Single when idxtt == idxtf"
                    [ testCase "Degenerated case with idxt < idxf" $ do
                        let x =
                              ifWithStrategy
                                grootStrategy
                                (SSBool "a")
                                (Single (Left (1 :: Integer)))
                                (Single (Left (2 :: Integer)))
                        ifWithStrategy grootStrategy (SSBool "b") x (Single $ Right (2 :: Integer))
                          @=? If
                            (Left 1)
                            True
                            (SSBool "b")
                            (If (Left 1) True (SSBool "a") (Single $ Left 1) (Single (Left 2)))
                            (Single $ Right 2),
                      let x =
                            ifWithStrategy
                              grootStrategy
                              (SSBool "a")
                              (Single (Just (1 :: Integer)))
                              (Single (Just (3 :: Integer)))
                       in testGroup
                            "Degenerated case with idxt == idxf"
                            [ testCase "sub-idxf < sub-idxtt" $
                                ifWithStrategy grootStrategy (SSBool "b") x (Single $ Just 0)
                                  @=? If
                                    (Just 0)
                                    True
                                    (Not (SSBool "b"))
                                    (Single $ Just 0)
                                    (If (Just 1) True (SSBool "a") (Single $ Just 1) (Single (Just 3))),
                              testCase "sub-idxf == sub-idxtt" $
                                ifWithStrategy grootStrategy (SSBool "b") x (Single $ Just 1)
                                  @=? If (Just 1) True (Or (Not $ SSBool "b") (SSBool "a")) (Single $ Just 1) (Single (Just 3)),
                              testCase "sub-idxtt < sub-idxf < sub-idxtf" $
                                ifWithStrategy grootStrategy (SSBool "b") x (Single $ Just 2)
                                  @=? If
                                    (Just 1)
                                    True
                                    (And (SSBool "b") (SSBool "a"))
                                    (Single $ Just 1)
                                    (If (Just 2) True (Not $ SSBool "b") (Single $ Just 2) (Single $ Just 3)),
                              testCase "sub-idxf == sub-idxtf" $
                                ifWithStrategy grootStrategy (SSBool "b") x (Single $ Just 3)
                                  @=? If (Just 1) True (And (SSBool "b") (SSBool "a")) (Single $ Just 1) (Single (Just 3)),
                              testCase "sub-idxtf < sub-idxf" $
                                ifWithStrategy grootStrategy (SSBool "b") x (Single $ Just 4)
                                  @=? If
                                    (Just 1)
                                    True
                                    (And (SSBool "b") (SSBool "a"))
                                    (Single $ Just 1)
                                    (If (Just 3) True (SSBool "b") (Single $ Just 3) (Single $ Just 4))
                            ],
                      testCase "Degenerated case with idxt > idxf" $ do
                        let x =
                              ifWithStrategy
                                grootStrategy
                                (SSBool "a")
                                (Single (Right (1 :: Integer)))
                                (Single (Right (2 :: Integer)))
                        ifWithStrategy grootStrategy (SSBool "b") x (Single $ Left (1 :: Integer))
                          @=? If
                            (Left 1)
                            True
                            (Not $ SSBool "b")
                            (Single $ Left 1)
                            (If (Right 1) True (SSBool "a") (Single $ Right 1) (Single (Right 2)))
                    ],
                  testCase "idxtt < idxf" $ do
                    let x =
                          ifWithStrategy
                            grootStrategy
                            (SSBool "a")
                            (Single $ Left (1 :: Integer))
                            (Single $ Right (3 :: Integer))
                    ifWithStrategy grootStrategy (SSBool "b") x (Single $ Right 0)
                      @=? If
                        (Left 1)
                        True
                        (And (SSBool "b") (SSBool "a"))
                        (Single $ Left 1)
                        (If (Right 0) True (Not $ SSBool "b") (Single $ Right 0) (Single $ Right 3)),
                  testCase "idxtt == idxf" $ do
                    let x =
                          ifWithStrategy
                            grootStrategy
                            (SSBool "a")
                            (Single $ Left (1 :: Integer))
                            (Single $ Right (3 :: Integer))
                    ifWithStrategy grootStrategy (SSBool "b") x (Single $ Left 0)
                      @=? If
                        (Left 0)
                        True
                        (Or (Not $ SSBool "b") (SSBool "a"))
                        (If (Left 0) True (Not $ SSBool "b") (Single $ Left 0) (Single $ Left 1))
                        (Single $ Right 3),
                  testCase "idxtt > idxf" $ do
                    let x =
                          ifWithStrategy
                            grootStrategy
                            (SSBool "a")
                            (Single (1 :: Integer))
                            (Single (3 :: Integer))
                    ifWithStrategy grootStrategy (SSBool "b") x (Single 0)
                      @=? If 0 True (Not $ SSBool "b") (Single 0) (If 1 True (SSBool "a") (Single 1) (Single 3))
                ],
              testGroup
                "ifWithStrategy on If/If"
                [ testCase "Degenerate to Single/If when idxtt == idxtf" $ do
                    let x = ifWithStrategy grootStrategy (SSBool "a") (Single $ Left (1 :: Integer)) (Single $ Left (2 :: Integer))
                    let y = ifWithStrategy grootStrategy (SSBool "b") (Single $ Left (1 :: Integer)) (Single $ Right (2 :: Integer))
                    ifWithStrategy grootStrategy (SSBool "c") x y
                      @=? If
                        (Left 1)
                        True
                        (Or (SSBool "c") (SSBool "b"))
                        (If (Left 1) True (Or (Not (SSBool "c")) (SSBool "a")) (Single $ Left 1) (Single $ Left 2))
                        (Single $ Right 2),
                  testCase "Degenerate to Single/If when idxff == idxft" $ do
                    let x = ifWithStrategy grootStrategy (SSBool "a") (Single $ Left (1 :: Integer)) (Single $ Left (2 :: Integer))
                    let y = ifWithStrategy grootStrategy (SSBool "b") (Single $ Left (1 :: Integer)) (Single $ Right (2 :: Integer))
                    ifWithStrategy grootStrategy (SSBool "c") y x
                      @=? If
                        (Left 1)
                        True
                        (Or (Not $ SSBool "c") (SSBool "b"))
                        (If (Left 1) True (Or (SSBool "c") (SSBool "a")) (Single $ Left 1) (Single $ Left 2))
                        (Single $ Right 2),
                  testCase "Non-degenerated case when idxtt < idxft" $ do
                    let x = ifWithStrategy grootStrategy (SSBool "a") (Single $ TS1 (1 :: Integer)) (Single $ TS2 (2 :: Integer))
                    let y = ifWithStrategy grootStrategy (SSBool "b") (Single $ TS2 (1 :: Integer)) (Single $ TS3 (2 :: Integer))
                    ifWithStrategy grootStrategy (SSBool "c") x y
                      @=? If
                        (TS1 1)
                        True
                        (And (SSBool "c") (SSBool "a"))
                        (Single $ TS1 1)
                        ( If
                            (TS2 1)
                            True
                            (Or (SSBool "c") (SSBool "b"))
                            (If (TS2 1) True (Not $ SSBool "c") (Single $ TS2 1) (Single $ TS2 2))
                            (Single $ TS3 2)
                        ),
                  testCase "Non-degenerated case when idxtt == idxft" $ do
                    let x = ifWithStrategy grootStrategy (SSBool "a") (Single $ TS1 (1 :: Integer)) (Single $ TS2 (2 :: Integer))
                    let y = ifWithStrategy grootStrategy (SSBool "b") (Single $ TS1 (2 :: Integer)) (Single $ TS3 (2 :: Integer))
                    ifWithStrategy grootStrategy (SSBool "c") x y
                      @=? If
                        (TS1 1)
                        True
                        (ITE (SSBool "c") (SSBool "a") (SSBool "b"))
                        (If (TS1 1) True (SSBool "c") (Single $ TS1 1) (Single $ TS1 2))
                        (If (TS2 2) True (SSBool "c") (Single $ TS2 2) (Single $ TS3 2)),
                  testCase "Non-degenerated case when idxtt > idxft" $ do
                    let x = ifWithStrategy grootStrategy (SSBool "a") (Single $ TS2 (1 :: Integer)) (Single $ TS3 (2 :: Integer))
                    let y = ifWithStrategy grootStrategy (SSBool "b") (Single $ TS1 (1 :: Integer)) (Single $ TS2 (2 :: Integer))
                    ifWithStrategy grootStrategy (SSBool "c") x y
                      @=? If
                        (TS1 1)
                        True
                        (And (Not $ SSBool "c") (SSBool "b"))
                        (Single $ TS1 1)
                        ( If
                            (TS2 1)
                            True
                            (Or (Not $ SSBool "c") (SSBool "a"))
                            (If (TS2 1) True (SSBool "c") (Single $ TS2 1) (Single $ TS2 2))
                            (Single $ TS3 2)
                        )
                ],
              testCase "ifWithStrategy should tolerate non-merged Ifs" $ do
                let x = If (Right 2) False (SSBool "a") (Single $ Right (2 :: Integer)) (Single $ Left (2 :: Integer))
                let y = If (Right 3) False (SSBool "b") (Single $ Right 3) (Single $ Left 1)
                ifWithStrategy grootStrategy (SSBool "c") x y
                  @=? If
                    (Left 1)
                    True
                    (ITE (SSBool "c") (Not $ SSBool "a") (Not $ SSBool "b"))
                    (If (Left 1) True (Not $ SSBool "c") (Single $ Left 1) (Single $ Left 2))
                    (If (Right 2) True (SSBool "c") (Single $ Right 2) (Single $ Right 3))
            ]
        ],
      testGroup
        "fullReconstruct"
        [ testCase "fullReconstruct should work" $ do
            let x = If (Right 2) False (SSBool "a") (Single $ Right (2 :: Integer)) (Single $ Left (2 :: Integer))
            let y = If (Right 3) False (SSBool "b") (Single $ Right 3) (Single $ Left 1)
            let z = If (Right 2) False (SSBool "c") x y
            fullReconstruct grootStrategy z
              @=? If
                (Left 1)
                True
                (ITE (SSBool "c") (Not $ SSBool "a") (Not $ SSBool "b"))
                (If (Left 1) True (Not $ SSBool "c") (Single $ Left 1) (Single $ Left 2))
                (If (Right 2) True (SSBool "c") (Single $ Right 2) (Single $ Right 3))
        ]
    ]
