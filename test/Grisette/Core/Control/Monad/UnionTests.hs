{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Grisette.Core.Control.Monad.UnionTests (unionTests) where

import GHC.Generics (Generic)
import Grisette.Core.Data.Class.Bool
  ( ITEOp (ites),
    LogicalOp (nots, (&&~), (||~)),
  )
import Grisette.Core.Data.Class.Mergeable
  ( Mergeable (rootStrategy),
    MergingStrategy (SortedStrategy),
    wrapStrategy,
  )
import Grisette.Core.Data.Class.Solvable (Solvable (con))
import Grisette.Core.Data.Union
  ( Union (If, Single),
    fullReconstruct,
    ifWithLeftMost,
    ifWithStrategy,
  )
import Grisette.IR.SymPrim.Data.SymPrim (SymInteger)
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit ((@?=))

data TripleSum a b c = TS1 a | TS2 b | TS3 c deriving (Show, Eq, Generic)

instance
  (Mergeable a, Mergeable b, Mergeable c) =>
  Mergeable (TripleSum a b c)
  where
  rootStrategy =
    SortedStrategy
      (\case TS1 _ -> (0 :: Int); TS2 _ -> (1 :: Int); TS3 _ -> (2 :: Int))
      ( \case
          0 -> wrapStrategy rootStrategy TS1 (\(TS1 x) -> x)
          1 -> wrapStrategy rootStrategy TS2 (\(TS2 x) -> x)
          2 -> wrapStrategy rootStrategy TS3 (\(TS3 x) -> x)
          _ -> error "Bad"
      )

unionTests :: Test
unionTests =
  testGroup
    "Union"
    [ testGroup
        "ifWithLeftMost"
        [ testCase
            "ifWithLeftMost should maintain left most info on Singles"
            $ do
              ifWithLeftMost False "a" (Single (1 :: Integer)) (Single 2)
                @?= If 1 False "a" (Single 1) (Single 2),
          testCase "ifWithLeftMost should maintain left most info on Ifs" $ do
            ifWithLeftMost
              True
              "a"
              (If 1 True "b" (Single (1 :: Integer)) (Single 2))
              (If 3 True "c" (Single 3) (Single 4))
              @?= If
                1
                True
                "a"
                (If 1 True "b" (Single (1 :: Integer)) (Single 2))
                (If 3 True "c" (Single 3) (Single 4))
        ],
      testGroup
        "ifWithStrategy"
        [ testGroup
            "ifWithStrategy with concrete condition"
            [ testCase "true" $ do
                ifWithStrategy
                  rootStrategy
                  (con True)
                  (Single (1 :: Integer))
                  (Single 2)
                  @?= Single 1,
              testCase "false" $ do
                ifWithStrategy
                  rootStrategy
                  (con False)
                  (Single (1 :: Integer))
                  (Single 2)
                  @?= Single 2
            ],
          let a =
                ifWithStrategy
                  rootStrategy
                  "a"
                  (Single (1 :: Integer))
                  (Single 2)
           in testGroup
                "ifWithStrategy with condition equal to sub conditions"
                [ testCase "ifTrue" $ do
                    ifWithStrategy rootStrategy "a" a (Single 3)
                      @?= If 1 True "a" (Single 1) (Single 3),
                  testCase "ifFalse" $ do
                    ifWithStrategy rootStrategy "a" (Single 0) a
                      @?= If 0 True "a" (Single 0) (Single 2)
                ],
          testCase "ifWithStrategy with simple mergeables" $ do
            ifWithStrategy
              rootStrategy
              "a"
              (Single ("b" :: SymInteger))
              (Single "c")
              @?= Single (ites "a" "b" "c"),
          testGroup
            "ifWithStrategy with ordered mergeables"
            [ testGroup
                "ifWithStrategy on Single/Single"
                [ testGroup
                    "idxt < idxf"
                    [ testCase "Integer" $
                        ifWithStrategy
                          rootStrategy
                          "a"
                          (Single (1 :: Integer))
                          (Single 2)
                          @?= If 1 True "a" (Single 1) (Single 2),
                      testCase "Maybe Integer" $
                        ifWithStrategy
                          rootStrategy
                          "a"
                          (Single Nothing)
                          (Single (Just (2 :: Integer)))
                          @?= If
                            Nothing
                            True
                            "a"
                            (Single Nothing)
                            (Single (Just 2))
                    ],
                  testGroup
                    "idxt == idxf"
                    [ testGroup
                        "idxt == idxf as terminal"
                        [ testCase "Integer" $
                            ifWithStrategy
                              rootStrategy
                              "a"
                              (Single (1 :: Integer))
                              (Single 1)
                              @?= Single 1,
                          testCase "Maybe Integer" $
                            ifWithStrategy
                              rootStrategy
                              "a"
                              (Single (Just ("b" :: SymInteger)))
                              (Single (Just "c"))
                              @?= Single (Just (ites "a" "b" "c"))
                        ],
                      testGroup
                        "idxt == idxf but not terminal"
                        [ testCase "Maybe Integer" $
                            ifWithStrategy
                              rootStrategy
                              "a"
                              (Single (Just (1 :: Integer)))
                              (Single (Just (2 :: Integer)))
                              @?= If
                                (Just 1)
                                True
                                "a"
                                (Single $ Just 1)
                                (Single (Just 2)),
                          testCase "Maybe (Maybe Integer)" $
                            ifWithStrategy
                              rootStrategy
                              "a"
                              (Single $ Just $ Just ("b" :: SymInteger))
                              (Single $ Just $ Just "c")
                              @?= Single (Just (Just (ites "a" "b" "c")))
                        ]
                    ],
                  testGroup
                    "idxt > idxf"
                    [ testCase "Integer" $
                        ifWithStrategy
                          rootStrategy
                          "a"
                          (Single (2 :: Integer))
                          (Single 1)
                          @?= If 1 True (nots "a") (Single 1) (Single 2),
                      testCase "Maybe Integer" $
                        ifWithStrategy
                          rootStrategy
                          "a"
                          (Single (Just (2 :: Integer)))
                          (Single Nothing)
                          @?= If
                            Nothing
                            True
                            (nots "a")
                            (Single Nothing)
                            (Single (Just 2))
                    ]
                ],
              testGroup
                "ifWithStrategy on Single/If"
                [ testGroup
                    "Degenerate to Single/Single when idxft == idxff"
                    [ testCase "Degenerated case with idxt < idxf" $ do
                        let x =
                              ifWithStrategy
                                rootStrategy
                                "a"
                                (Single (Just (1 :: Integer)))
                                (Single (Just (2 :: Integer)))
                        ifWithStrategy rootStrategy "b" (Single Nothing) x
                          @?= If
                            Nothing
                            True
                            "b"
                            (Single Nothing)
                            ( If
                                (Just 1)
                                True
                                "a"
                                (Single $ Just 1)
                                (Single (Just 2))
                            ),
                      let x =
                            ifWithStrategy
                              rootStrategy
                              "a"
                              (Single (Just (1 :: Integer)))
                              (Single (Just (3 :: Integer)))
                       in testGroup
                            "Degenerated case with idxt == idxf"
                            [ testCase "sub-idxt < sub-idxft" $
                                ifWithStrategy
                                  rootStrategy
                                  "b"
                                  (Single $ Just 0)
                                  x
                                  @?= If
                                    (Just 0)
                                    True
                                    "b"
                                    (Single $ Just 0)
                                    ( If
                                        (Just 1)
                                        True
                                        "a"
                                        (Single $ Just 1)
                                        (Single (Just 3))
                                    ),
                              testCase "sub-idxt == sub-idxft" $
                                ifWithStrategy
                                  rootStrategy
                                  "b"
                                  (Single $ Just 1)
                                  x
                                  @?= If
                                    (Just 1)
                                    True
                                    ("b" ||~ "a")
                                    (Single $ Just 1)
                                    (Single (Just 3)),
                              testCase "subidxft < sub-idxt < sub-idxff" $
                                ifWithStrategy
                                  rootStrategy
                                  "b"
                                  (Single $ Just 2)
                                  x
                                  @?= If
                                    (Just 1)
                                    True
                                    ((nots "b") &&~ "a")
                                    (Single $ Just 1)
                                    ( If
                                        (Just 2)
                                        True
                                        "b"
                                        (Single $ Just 2)
                                        (Single $ Just 3)
                                    ),
                              testCase "sub-idxt == sub-idxff" $
                                ifWithStrategy
                                  rootStrategy
                                  "b"
                                  (Single $ Just 3)
                                  x
                                  @?= If
                                    (Just 1)
                                    True
                                    ((nots "b") &&~ "a")
                                    (Single $ Just 1)
                                    (Single (Just 3)),
                              testCase "sub-idxff < sub-idxt" $
                                ifWithStrategy
                                  rootStrategy
                                  "b"
                                  (Single $ Just 4)
                                  x
                                  @?= If
                                    (Just 1)
                                    True
                                    ((nots "b") &&~ "a")
                                    (Single $ Just 1)
                                    ( If
                                        (Just 3)
                                        True
                                        (nots "b")
                                        (Single $ Just 3)
                                        (Single $ Just 4)
                                    )
                            ],
                      testCase "Degenerated case with idxt > idxf" $ do
                        let x =
                              ifWithStrategy
                                rootStrategy
                                "a"
                                (Single (Left (1 :: Integer)))
                                (Single (Left (2 :: Integer)))
                        ifWithStrategy
                          rootStrategy
                          "b"
                          (Single $ Right (1 :: Integer))
                          x
                          @?= If
                            (Left 1)
                            True
                            (nots "b")
                            ( If
                                (Left 1)
                                True
                                "a"
                                (Single $ Left 1)
                                (Single (Left 2))
                            )
                            (Single $ Right 1)
                    ],
                  testCase "idxt < idxft" $ do
                    let x =
                          ifWithStrategy
                            rootStrategy
                            "a"
                            (Single (1 :: Integer))
                            (Single (3 :: Integer))
                    ifWithStrategy rootStrategy "b" (Single 0) x
                      @?= If
                        0
                        True
                        "b"
                        (Single 0)
                        ( If
                            1
                            True
                            "a"
                            (Single 1)
                            (Single 3)
                        ),
                  testCase "idxt == idxft" $ do
                    let x =
                          ifWithStrategy
                            rootStrategy
                            "a"
                            (Single $ Left (1 :: Integer))
                            (Single $ Right (3 :: Integer))
                    ifWithStrategy rootStrategy "b" (Single $ Left 0) x
                      @?= If
                        (Left 0)
                        True
                        ("b" ||~ "a")
                        ( If
                            (Left 0)
                            True
                            "b"
                            (Single $ Left 0)
                            (Single $ Left 1)
                        )
                        (Single $ Right 3),
                  testCase "idxt > idxft" $ do
                    let x =
                          ifWithStrategy
                            rootStrategy
                            "a"
                            (Single $ Left (1 :: Integer))
                            (Single $ Right (3 :: Integer))
                    ifWithStrategy rootStrategy "b" (Single $ Right 0) x
                      @?= If
                        (Left 1)
                        True
                        ((nots "b") &&~ "a")
                        (Single $ Left 1)
                        ( If
                            (Right 0)
                            True
                            "b"
                            (Single $ Right 0)
                            (Single $ Right 3)
                        )
                ],
              testGroup
                "ifWithStrategy on If/Single"
                [ testGroup
                    "Degenerate to Single/Single when idxtt == idxtf"
                    [ testCase "Degenerated case with idxt < idxf" $ do
                        let x =
                              ifWithStrategy
                                rootStrategy
                                "a"
                                (Single (Left (1 :: Integer)))
                                (Single (Left (2 :: Integer)))
                        ifWithStrategy
                          rootStrategy
                          "b"
                          x
                          (Single $ Right (2 :: Integer))
                          @?= If
                            (Left 1)
                            True
                            "b"
                            ( If
                                (Left 1)
                                True
                                "a"
                                (Single $ Left 1)
                                (Single (Left 2))
                            )
                            (Single $ Right 2),
                      let x =
                            ifWithStrategy
                              rootStrategy
                              "a"
                              (Single (Just (1 :: Integer)))
                              (Single (Just (3 :: Integer)))
                       in testGroup
                            "Degenerated case with idxt == idxf"
                            [ testCase "sub-idxf < sub-idxtt" $
                                ifWithStrategy
                                  rootStrategy
                                  "b"
                                  x
                                  (Single $ Just 0)
                                  @?= If
                                    (Just 0)
                                    True
                                    (nots "b")
                                    (Single $ Just 0)
                                    ( If
                                        (Just 1)
                                        True
                                        "a"
                                        (Single $ Just 1)
                                        (Single (Just 3))
                                    ),
                              testCase "sub-idxf == sub-idxtt" $
                                ifWithStrategy
                                  rootStrategy
                                  "b"
                                  x
                                  (Single $ Just 1)
                                  @?= If
                                    (Just 1)
                                    True
                                    ((nots "b") ||~ "a")
                                    (Single $ Just 1)
                                    (Single (Just 3)),
                              testCase "sub-idxtt < sub-idxf < sub-idxtf" $
                                ifWithStrategy
                                  rootStrategy
                                  "b"
                                  x
                                  (Single $ Just 2)
                                  @?= If
                                    (Just 1)
                                    True
                                    ("b" &&~ "a")
                                    (Single $ Just 1)
                                    ( If
                                        (Just 2)
                                        True
                                        (nots "b")
                                        (Single $ Just 2)
                                        (Single $ Just 3)
                                    ),
                              testCase "sub-idxf == sub-idxtf" $
                                ifWithStrategy
                                  rootStrategy
                                  "b"
                                  x
                                  (Single $ Just 3)
                                  @?= If
                                    (Just 1)
                                    True
                                    ("b" &&~ "a")
                                    (Single $ Just 1)
                                    (Single (Just 3)),
                              testCase "sub-idxtf < sub-idxf" $
                                ifWithStrategy
                                  rootStrategy
                                  "b"
                                  x
                                  (Single $ Just 4)
                                  @?= If
                                    (Just 1)
                                    True
                                    ("b" &&~ "a")
                                    (Single $ Just 1)
                                    ( If
                                        (Just 3)
                                        True
                                        "b"
                                        (Single $ Just 3)
                                        (Single $ Just 4)
                                    )
                            ],
                      testCase "Degenerated case with idxt > idxf" $ do
                        let x =
                              ifWithStrategy
                                rootStrategy
                                "a"
                                (Single (Right (1 :: Integer)))
                                (Single (Right (2 :: Integer)))
                        ifWithStrategy
                          rootStrategy
                          "b"
                          x
                          (Single $ Left (1 :: Integer))
                          @?= If
                            (Left 1)
                            True
                            (nots "b")
                            (Single $ Left 1)
                            ( If
                                (Right 1)
                                True
                                "a"
                                (Single $ Right 1)
                                (Single (Right 2))
                            )
                    ],
                  testCase "idxtt < idxf" $ do
                    let x =
                          ifWithStrategy
                            rootStrategy
                            "a"
                            (Single $ Left (1 :: Integer))
                            (Single $ Right (3 :: Integer))
                    ifWithStrategy rootStrategy "b" x (Single $ Right 0)
                      @?= If
                        (Left 1)
                        True
                        ("b" &&~ "a")
                        (Single $ Left 1)
                        ( If
                            (Right 0)
                            True
                            (nots "b")
                            (Single $ Right 0)
                            (Single $ Right 3)
                        ),
                  testCase "idxtt == idxf" $ do
                    let x =
                          ifWithStrategy
                            rootStrategy
                            "a"
                            (Single $ Left (1 :: Integer))
                            (Single $ Right (3 :: Integer))
                    ifWithStrategy rootStrategy "b" x (Single $ Left 0)
                      @?= If
                        (Left 0)
                        True
                        ((nots "b") ||~ "a")
                        ( If
                            (Left 0)
                            True
                            (nots "b")
                            (Single $ Left 0)
                            (Single $ Left 1)
                        )
                        (Single $ Right 3),
                  testCase "idxtt > idxf" $ do
                    let x =
                          ifWithStrategy
                            rootStrategy
                            "a"
                            (Single (1 :: Integer))
                            (Single (3 :: Integer))
                    ifWithStrategy rootStrategy "b" x (Single 0)
                      @?= If
                        0
                        True
                        (nots "b")
                        (Single 0)
                        (If 1 True "a" (Single 1) (Single 3))
                ],
              testGroup
                "ifWithStrategy on If/If"
                [ testCase "Degenerate to Single/If when idxtt == idxtf" $ do
                    let x =
                          ifWithStrategy
                            rootStrategy
                            "a"
                            (Single $ Left (1 :: Integer))
                            (Single $ Left (2 :: Integer))
                    let y =
                          ifWithStrategy
                            rootStrategy
                            "b"
                            (Single $ Left (1 :: Integer))
                            (Single $ Right (2 :: Integer))
                    ifWithStrategy rootStrategy "c" x y
                      @?= If
                        (Left 1)
                        True
                        ("c" ||~ "b")
                        ( If
                            (Left 1)
                            True
                            ((nots "c") ||~ "a")
                            (Single $ Left 1)
                            (Single $ Left 2)
                        )
                        (Single $ Right 2),
                  testCase "Degenerate to Single/If when idxff == idxft" $ do
                    let x =
                          ifWithStrategy
                            rootStrategy
                            "a"
                            (Single $ Left (1 :: Integer))
                            (Single $ Left (2 :: Integer))
                    let y =
                          ifWithStrategy
                            rootStrategy
                            "b"
                            (Single $ Left (1 :: Integer))
                            (Single $ Right (2 :: Integer))
                    ifWithStrategy rootStrategy "c" y x
                      @?= If
                        (Left 1)
                        True
                        ((nots "c") ||~ "b")
                        ( If
                            (Left 1)
                            True
                            ("c" ||~ "a")
                            (Single $ Left 1)
                            (Single $ Left 2)
                        )
                        (Single $ Right 2),
                  testCase "Non-degenerated case when idxtt < idxft" $ do
                    let x =
                          ifWithStrategy
                            rootStrategy
                            "a"
                            (Single $ TS1 (1 :: Integer))
                            (Single $ TS2 (2 :: Integer))
                    let y =
                          ifWithStrategy
                            rootStrategy
                            "b"
                            (Single $ TS2 (1 :: Integer))
                            (Single $ TS3 (2 :: Integer))
                    ifWithStrategy rootStrategy "c" x y
                      @?= If
                        (TS1 1)
                        True
                        ("c" &&~ "a")
                        (Single $ TS1 1)
                        ( If
                            (TS2 1)
                            True
                            ("c" ||~ "b")
                            ( If
                                (TS2 1)
                                True
                                (nots "c")
                                (Single $ TS2 1)
                                (Single $ TS2 2)
                            )
                            (Single $ TS3 2)
                        ),
                  testCase "Non-degenerated case when idxtt == idxft" $ do
                    let x =
                          ifWithStrategy
                            rootStrategy
                            "a"
                            (Single $ TS1 (1 :: Integer))
                            (Single $ TS2 (2 :: Integer))
                    let y =
                          ifWithStrategy
                            rootStrategy
                            "b"
                            (Single $ TS1 (2 :: Integer))
                            (Single $ TS3 (2 :: Integer))
                    ifWithStrategy rootStrategy "c" x y
                      @?= If
                        (TS1 1)
                        True
                        (ites "c" "a" "b")
                        (If (TS1 1) True "c" (Single $ TS1 1) (Single $ TS1 2))
                        (If (TS2 2) True "c" (Single $ TS2 2) (Single $ TS3 2)),
                  testCase "Non-degenerated case when idxtt > idxft" $ do
                    let x =
                          ifWithStrategy
                            rootStrategy
                            "a"
                            (Single $ TS2 (1 :: Integer))
                            (Single $ TS3 (2 :: Integer))
                    let y =
                          ifWithStrategy
                            rootStrategy
                            "b"
                            (Single $ TS1 (1 :: Integer))
                            (Single $ TS2 (2 :: Integer))
                    ifWithStrategy rootStrategy "c" x y
                      @?= If
                        (TS1 1)
                        True
                        ((nots "c") &&~ "b")
                        (Single $ TS1 1)
                        ( If
                            (TS2 1)
                            True
                            ((nots "c") ||~ "a")
                            (If (TS2 1) True "c" (Single $ TS2 1) (Single $ TS2 2))
                            (Single $ TS3 2)
                        )
                ],
              testCase "ifWithStrategy should tolerate non-merged Ifs" $ do
                let x =
                      If
                        (Right 2)
                        False
                        "a"
                        (Single $ Right (2 :: Integer))
                        (Single $ Left (2 :: Integer))
                let y =
                      If
                        (Right 3)
                        False
                        "b"
                        (Single $ Right 3)
                        (Single $ Left 1)
                ifWithStrategy rootStrategy "c" x y
                  @?= If
                    (Left 1)
                    True
                    (ites "c" (nots "a") (nots "b"))
                    ( If
                        (Left 1)
                        True
                        (nots "c")
                        (Single $ Left 1)
                        (Single $ Left 2)
                    )
                    ( If
                        (Right 2)
                        True
                        "c"
                        (Single $ Right 2)
                        (Single $ Right 3)
                    )
            ]
        ],
      testGroup
        "fullReconstruct"
        [ testCase "fullReconstruct should work" $ do
            let x =
                  If
                    (Right 2)
                    False
                    "a"
                    (Single $ Right (2 :: Integer))
                    (Single $ Left (2 :: Integer))
            let y = If (Right 3) False "b" (Single $ Right 3) (Single $ Left 1)
            let z = If (Right 2) False "c" x y
            fullReconstruct rootStrategy z
              @?= If
                (Left 1)
                True
                (ites "c" (nots "a") (nots "b"))
                ( If
                    (Left 1)
                    True
                    (nots "c")
                    (Single $ Left 1)
                    (Single $ Left 2)
                )
                (If (Right 2) True "c" (Single $ Right 2) (Single $ Right 3))
        ]
    ]
