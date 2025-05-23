{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Grisette.Core.Data.UnionBaseTests (unionBaseTests) where

import GHC.Generics (Generic)
import Grisette
  ( AsKey (AsKey),
    ITEOp (symIte),
    LogicalOp (symNot, (.&&), (.||)),
    Mergeable (rootStrategy),
    MergingStrategy (SortedStrategy),
    Solvable (con),
    SymInteger,
    wrapStrategy,
  )
import Grisette.Internal.Core.Data.UnionBase
  ( UnionBase (UnionIf, UnionSingle),
    fullReconstruct,
    ifWithLeftMost,
    ifWithStrategy,
  )
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

unionBaseTests :: Test
unionBaseTests =
  testGroup
    "UnionBase"
    [ testGroup
        "ifWithLeftMost"
        [ testCase
            "ifWithLeftMost should maintain left most info on Singles"
            $ do
              AsKey
                ( ifWithLeftMost
                    False
                    "a"
                    (UnionSingle (1 :: Integer))
                    (UnionSingle 2)
                )
                @?= AsKey (UnionIf 1 False "a" (UnionSingle 1) (UnionSingle 2)),
          testCase "ifWithLeftMost should maintain left most info on Ifs" $ do
            AsKey
              ( ifWithLeftMost
                  True
                  "a"
                  ( UnionIf
                      1
                      True
                      "b"
                      (UnionSingle (1 :: Integer))
                      (UnionSingle 2)
                  )
                  (UnionIf 3 True "c" (UnionSingle 3) (UnionSingle 4))
              )
              @?= AsKey
                ( UnionIf
                    1
                    True
                    "a"
                    ( UnionIf
                        1
                        True
                        "b"
                        (UnionSingle (1 :: Integer))
                        (UnionSingle 2)
                    )
                    (UnionIf 3 True "c" (UnionSingle 3) (UnionSingle 4))
                )
        ],
      testGroup
        "ifWithStrategy"
        [ testGroup
            "ifWithStrategy with concrete condition"
            [ testCase "true" $ do
                AsKey
                  ( ifWithStrategy
                      rootStrategy
                      (con True)
                      (UnionSingle (1 :: Integer))
                      (UnionSingle 2)
                  )
                  @?= AsKey (UnionSingle 1),
              testCase "false" $ do
                AsKey
                  ( ifWithStrategy
                      rootStrategy
                      (con False)
                      (UnionSingle (1 :: Integer))
                      (UnionSingle 2)
                  )
                  @?= AsKey (UnionSingle 2)
            ],
          let a =
                ifWithStrategy
                  rootStrategy
                  "a"
                  (UnionSingle (1 :: Integer))
                  (UnionSingle 2)
           in testGroup
                "ifWithStrategy with condition equal to sub conditions"
                [ testCase "ifTrue" $ do
                    AsKey
                      (ifWithStrategy rootStrategy "a" a (UnionSingle 3))
                      @?= AsKey (UnionIf 1 True "a" (UnionSingle 1) (UnionSingle 3)),
                  testCase "ifFalse" $ do
                    AsKey
                      (ifWithStrategy rootStrategy "a" (UnionSingle 0) a)
                      @?= AsKey (UnionIf 0 True "a" (UnionSingle 0) (UnionSingle 2))
                ],
          testCase "ifWithStrategy with simple mergeables" $ do
            AsKey
              ( ifWithStrategy
                  rootStrategy
                  "a"
                  (UnionSingle ("b" :: AsKey SymInteger))
                  (UnionSingle "c")
              )
              @?= AsKey (UnionSingle (symIte "a" "b" "c")),
          testGroup
            "ifWithStrategy with ordered mergeables"
            [ testGroup
                "ifWithStrategy on Single/Single"
                [ testGroup
                    "idxt < idxf"
                    [ testCase "Integer" $
                        AsKey
                          ( ifWithStrategy
                              rootStrategy
                              "a"
                              (UnionSingle (1 :: Integer))
                              (UnionSingle 2)
                          )
                          @?= AsKey
                            ( UnionIf
                                1
                                True
                                "a"
                                (UnionSingle 1)
                                (UnionSingle 2)
                            ),
                      testCase "Maybe Integer" $
                        AsKey
                          ( ifWithStrategy
                              rootStrategy
                              "a"
                              (UnionSingle Nothing)
                              (UnionSingle (Just (2 :: Integer)))
                          )
                          @?= AsKey
                            ( UnionIf
                                Nothing
                                True
                                "a"
                                (UnionSingle Nothing)
                                (UnionSingle (Just 2))
                            )
                    ],
                  testGroup
                    "idxt == idxf"
                    [ testGroup
                        "idxt == idxf as terminal"
                        [ testCase "Integer" $
                            AsKey
                              ( ifWithStrategy
                                  rootStrategy
                                  "a"
                                  (UnionSingle (1 :: Integer))
                                  (UnionSingle 1)
                              )
                              @?= AsKey (UnionSingle 1),
                          testCase "Maybe Integer" $
                            AsKey
                              ( ifWithStrategy
                                  rootStrategy
                                  "a"
                                  (UnionSingle (Just ("b" :: AsKey SymInteger)))
                                  (UnionSingle (Just "c"))
                              )
                              @?= AsKey (UnionSingle (Just (symIte "a" "b" "c")))
                        ],
                      testGroup
                        "idxt == idxf but not terminal"
                        [ testCase "Maybe Integer" $
                            AsKey
                              ( ifWithStrategy
                                  rootStrategy
                                  "a"
                                  (UnionSingle (Just (1 :: Integer)))
                                  (UnionSingle (Just (2 :: Integer)))
                              )
                              @?= AsKey
                                ( UnionIf
                                    (Just 1)
                                    True
                                    "a"
                                    (UnionSingle $ Just 1)
                                    (UnionSingle (Just 2))
                                ),
                          testCase "Maybe (Maybe Integer)" $
                            AsKey
                              ( ifWithStrategy
                                  rootStrategy
                                  "a"
                                  (UnionSingle $ Just $ Just ("b" :: AsKey SymInteger))
                                  (UnionSingle $ Just $ Just "c")
                              )
                              @?= AsKey (UnionSingle (Just (Just (symIte "a" "b" "c"))))
                        ]
                    ],
                  testGroup
                    "idxt > idxf"
                    [ testCase "Integer" $
                        AsKey
                          ( ifWithStrategy
                              rootStrategy
                              "a"
                              (UnionSingle (2 :: Integer))
                              (UnionSingle 1)
                          )
                          @?= AsKey
                            ( UnionIf
                                1
                                True
                                (symNot "a")
                                (UnionSingle 1)
                                (UnionSingle 2)
                            ),
                      testCase "Maybe Integer" $
                        AsKey
                          ( ifWithStrategy
                              rootStrategy
                              "a"
                              (UnionSingle (Just (2 :: Integer)))
                              (UnionSingle Nothing)
                          )
                          @?= AsKey
                            ( UnionIf
                                Nothing
                                True
                                (symNot "a")
                                (UnionSingle Nothing)
                                (UnionSingle (Just 2))
                            )
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
                                (UnionSingle (Just (1 :: Integer)))
                                (UnionSingle (Just (2 :: Integer)))
                        AsKey
                          ( ifWithStrategy
                              rootStrategy
                              "b"
                              (UnionSingle Nothing)
                              x
                          )
                          @?= AsKey
                            ( UnionIf
                                Nothing
                                True
                                "b"
                                (UnionSingle Nothing)
                                ( UnionIf
                                    (Just 1)
                                    True
                                    "a"
                                    (UnionSingle $ Just 1)
                                    (UnionSingle (Just 2))
                                )
                            ),
                      let x =
                            ifWithStrategy
                              rootStrategy
                              "a"
                              (UnionSingle (Just (1 :: Integer)))
                              (UnionSingle (Just (3 :: Integer)))
                       in testGroup
                            "Degenerated case with idxt == idxf"
                            [ testCase "sub-idxt < sub-idxft" $
                                AsKey
                                  ( ifWithStrategy
                                      rootStrategy
                                      "b"
                                      (UnionSingle $ Just 0)
                                      x
                                  )
                                  @?= AsKey
                                    ( UnionIf
                                        (Just 0)
                                        True
                                        "b"
                                        (UnionSingle $ Just 0)
                                        ( UnionIf
                                            (Just 1)
                                            True
                                            "a"
                                            (UnionSingle $ Just 1)
                                            (UnionSingle (Just 3))
                                        )
                                    ),
                              testCase "sub-idxt == sub-idxft" $
                                AsKey
                                  ( ifWithStrategy
                                      rootStrategy
                                      "b"
                                      (UnionSingle $ Just 1)
                                      x
                                  )
                                  @?= AsKey
                                    ( UnionIf
                                        (Just 1)
                                        True
                                        ("b" .|| "a")
                                        (UnionSingle $ Just 1)
                                        (UnionSingle (Just 3))
                                    ),
                              testCase "subidxft < sub-idxt < sub-idxff" $
                                AsKey
                                  ( ifWithStrategy
                                      rootStrategy
                                      "b"
                                      (UnionSingle $ Just 2)
                                      x
                                  )
                                  @?= AsKey
                                    ( UnionIf
                                        (Just 1)
                                        True
                                        ((symNot "b") .&& "a")
                                        (UnionSingle $ Just 1)
                                        ( UnionIf
                                            (Just 2)
                                            True
                                            "b"
                                            (UnionSingle $ Just 2)
                                            (UnionSingle $ Just 3)
                                        )
                                    ),
                              testCase "sub-idxt == sub-idxff" $
                                AsKey
                                  ( ifWithStrategy
                                      rootStrategy
                                      "b"
                                      (UnionSingle $ Just 3)
                                      x
                                  )
                                  @?= AsKey
                                    ( UnionIf
                                        (Just 1)
                                        True
                                        ((symNot "b") .&& "a")
                                        (UnionSingle $ Just 1)
                                        (UnionSingle (Just 3))
                                    ),
                              testCase "sub-idxff < sub-idxt" $
                                AsKey
                                  ( ifWithStrategy
                                      rootStrategy
                                      "b"
                                      (UnionSingle $ Just 4)
                                      x
                                  )
                                  @?= AsKey
                                    ( UnionIf
                                        (Just 1)
                                        True
                                        ((symNot "b") .&& "a")
                                        (UnionSingle $ Just 1)
                                        ( UnionIf
                                            (Just 3)
                                            True
                                            (symNot "b")
                                            (UnionSingle $ Just 3)
                                            (UnionSingle $ Just 4)
                                        )
                                    )
                            ],
                      testCase "Degenerated case with idxt > idxf" $ do
                        let x =
                              ifWithStrategy
                                rootStrategy
                                "a"
                                (UnionSingle (Left (1 :: Integer)))
                                (UnionSingle (Left (2 :: Integer)))
                        AsKey
                          ( ifWithStrategy
                              rootStrategy
                              "b"
                              (UnionSingle $ Right (1 :: Integer))
                              x
                          )
                          @?= AsKey
                            ( UnionIf
                                (Left 1)
                                True
                                (symNot "b")
                                ( UnionIf
                                    (Left 1)
                                    True
                                    "a"
                                    (UnionSingle $ Left 1)
                                    (UnionSingle (Left 2))
                                )
                                (UnionSingle $ Right 1)
                            )
                    ],
                  testCase "idxt < idxft" $ do
                    let x =
                          ifWithStrategy
                            rootStrategy
                            "a"
                            (UnionSingle (1 :: Integer))
                            (UnionSingle (3 :: Integer))
                    AsKey
                      ( ifWithStrategy
                          rootStrategy
                          "b"
                          (UnionSingle 0)
                          x
                      )
                      @?= AsKey
                        ( UnionIf
                            0
                            True
                            "b"
                            (UnionSingle 0)
                            ( UnionIf
                                1
                                True
                                "a"
                                (UnionSingle 1)
                                (UnionSingle 3)
                            )
                        ),
                  testCase "idxt == idxft" $ do
                    let x =
                          ifWithStrategy
                            rootStrategy
                            "a"
                            (UnionSingle $ Left (1 :: Integer))
                            (UnionSingle $ Right (3 :: Integer))
                    AsKey
                      ( ifWithStrategy
                          rootStrategy
                          "b"
                          (UnionSingle $ Left 0)
                          x
                      )
                      @?= AsKey
                        ( UnionIf
                            (Left 0)
                            True
                            ("b" .|| "a")
                            ( UnionIf
                                (Left 0)
                                True
                                "b"
                                (UnionSingle $ Left 0)
                                (UnionSingle $ Left 1)
                            )
                            (UnionSingle $ Right 3)
                        ),
                  testCase "idxt > idxft" $ do
                    let x =
                          ifWithStrategy
                            rootStrategy
                            "a"
                            (UnionSingle $ Left (1 :: Integer))
                            (UnionSingle $ Right (3 :: Integer))
                    AsKey
                      ( ifWithStrategy
                          rootStrategy
                          "b"
                          (UnionSingle $ Right 0)
                          x
                      )
                      @?= AsKey
                        ( UnionIf
                            (Left 1)
                            True
                            ((symNot "b") .&& "a")
                            (UnionSingle $ Left 1)
                            ( UnionIf
                                (Right 0)
                                True
                                "b"
                                (UnionSingle $ Right 0)
                                (UnionSingle $ Right 3)
                            )
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
                                (UnionSingle (Left (1 :: Integer)))
                                (UnionSingle (Left (2 :: Integer)))
                        AsKey
                          ( ifWithStrategy
                              rootStrategy
                              "b"
                              x
                              (UnionSingle $ Right (2 :: Integer))
                          )
                          @?= AsKey
                            ( UnionIf
                                (Left 1)
                                True
                                "b"
                                ( UnionIf
                                    (Left 1)
                                    True
                                    "a"
                                    (UnionSingle $ Left 1)
                                    (UnionSingle (Left 2))
                                )
                                (UnionSingle $ Right 2)
                            ),
                      let x =
                            ifWithStrategy
                              rootStrategy
                              "a"
                              (UnionSingle (Just (1 :: Integer)))
                              (UnionSingle (Just (3 :: Integer)))
                       in testGroup
                            "Degenerated case with idxt == idxf"
                            [ testCase "sub-idxf < sub-idxtt" $
                                AsKey
                                  ( ifWithStrategy
                                      rootStrategy
                                      "b"
                                      x
                                      (UnionSingle $ Just 0)
                                  )
                                  @?= AsKey
                                    ( UnionIf
                                        (Just 0)
                                        True
                                        (symNot "b")
                                        (UnionSingle $ Just 0)
                                        ( UnionIf
                                            (Just 1)
                                            True
                                            "a"
                                            (UnionSingle $ Just 1)
                                            (UnionSingle (Just 3))
                                        )
                                    ),
                              testCase "sub-idxf == sub-idxtt" $
                                AsKey
                                  ( ifWithStrategy
                                      rootStrategy
                                      "b"
                                      x
                                      (UnionSingle $ Just 1)
                                  )
                                  @?= AsKey
                                    ( UnionIf
                                        (Just 1)
                                        True
                                        ((symNot "b") .|| "a")
                                        (UnionSingle $ Just 1)
                                        (UnionSingle (Just 3))
                                    ),
                              testCase "sub-idxtt < sub-idxf < sub-idxtf" $
                                AsKey
                                  ( ifWithStrategy
                                      rootStrategy
                                      "b"
                                      x
                                      (UnionSingle $ Just 2)
                                  )
                                  @?= AsKey
                                    ( UnionIf
                                        (Just 1)
                                        True
                                        ("b" .&& "a")
                                        (UnionSingle $ Just 1)
                                        ( UnionIf
                                            (Just 2)
                                            True
                                            (symNot "b")
                                            (UnionSingle $ Just 2)
                                            (UnionSingle $ Just 3)
                                        )
                                    ),
                              testCase "sub-idxf == sub-idxtf" $
                                AsKey
                                  ( ifWithStrategy
                                      rootStrategy
                                      "b"
                                      x
                                      (UnionSingle $ Just 3)
                                  )
                                  @?= AsKey
                                    ( UnionIf
                                        (Just 1)
                                        True
                                        ("b" .&& "a")
                                        (UnionSingle $ Just 1)
                                        (UnionSingle (Just 3))
                                    ),
                              testCase "sub-idxtf < sub-idxf" $
                                AsKey
                                  ( ifWithStrategy
                                      rootStrategy
                                      "b"
                                      x
                                      (UnionSingle $ Just 4)
                                  )
                                  @?= AsKey
                                    ( UnionIf
                                        (Just 1)
                                        True
                                        ("b" .&& "a")
                                        (UnionSingle $ Just 1)
                                        ( UnionIf
                                            (Just 3)
                                            True
                                            "b"
                                            (UnionSingle $ Just 3)
                                            (UnionSingle $ Just 4)
                                        )
                                    )
                            ],
                      testCase "Degenerated case with idxt > idxf" $ do
                        let x =
                              ifWithStrategy
                                rootStrategy
                                "a"
                                (UnionSingle (Right (1 :: Integer)))
                                (UnionSingle (Right (2 :: Integer)))
                        AsKey
                          ( ifWithStrategy
                              rootStrategy
                              "b"
                              x
                              (UnionSingle $ Left (1 :: Integer))
                          )
                          @?= AsKey
                            ( UnionIf
                                (Left 1)
                                True
                                (symNot "b")
                                (UnionSingle $ Left 1)
                                ( UnionIf
                                    (Right 1)
                                    True
                                    "a"
                                    (UnionSingle $ Right 1)
                                    (UnionSingle (Right 2))
                                )
                            )
                    ],
                  testCase "idxtt < idxf" $ do
                    let x =
                          ifWithStrategy
                            rootStrategy
                            "a"
                            (UnionSingle $ Left (1 :: Integer))
                            (UnionSingle $ Right (3 :: Integer))
                    AsKey
                      ( ifWithStrategy
                          rootStrategy
                          "b"
                          x
                          (UnionSingle $ Right 0)
                      )
                      @?= AsKey
                        ( UnionIf
                            (Left 1)
                            True
                            ("b" .&& "a")
                            (UnionSingle $ Left 1)
                            ( UnionIf
                                (Right 0)
                                True
                                (symNot "b")
                                (UnionSingle $ Right 0)
                                (UnionSingle $ Right 3)
                            )
                        ),
                  testCase "idxtt == idxf" $ do
                    let x =
                          ifWithStrategy
                            rootStrategy
                            "a"
                            (UnionSingle $ Left (1 :: Integer))
                            (UnionSingle $ Right (3 :: Integer))
                    AsKey
                      ( ifWithStrategy
                          rootStrategy
                          "b"
                          x
                          (UnionSingle $ Left 0)
                      )
                      @?= AsKey
                        ( UnionIf
                            (Left 0)
                            True
                            ((symNot "b") .|| "a")
                            ( UnionIf
                                (Left 0)
                                True
                                (symNot "b")
                                (UnionSingle $ Left 0)
                                (UnionSingle $ Left 1)
                            )
                            (UnionSingle $ Right 3)
                        ),
                  testCase "idxtt > idxf" $ do
                    let x =
                          ifWithStrategy
                            rootStrategy
                            "a"
                            (UnionSingle (1 :: Integer))
                            (UnionSingle (3 :: Integer))
                    AsKey
                      ( ifWithStrategy
                          rootStrategy
                          "b"
                          x
                          (UnionSingle 0)
                      )
                      @?= AsKey
                        ( UnionIf
                            0
                            True
                            (symNot "b")
                            (UnionSingle 0)
                            (UnionIf 1 True "a" (UnionSingle 1) (UnionSingle 3))
                        )
                ],
              testGroup
                "ifWithStrategy on If/If"
                [ testCase "Degenerate to Single/If when idxtt == idxtf" $ do
                    let x =
                          ifWithStrategy
                            rootStrategy
                            "a"
                            (UnionSingle $ Left (1 :: Integer))
                            (UnionSingle $ Left (2 :: Integer))
                    let y =
                          ifWithStrategy
                            rootStrategy
                            "b"
                            (UnionSingle $ Left (1 :: Integer))
                            (UnionSingle $ Right (2 :: Integer))
                    AsKey
                      ( ifWithStrategy
                          rootStrategy
                          "c"
                          x
                          y
                      )
                      @?= AsKey
                        ( UnionIf
                            (Left 1)
                            True
                            ("c" .|| "b")
                            ( UnionIf
                                (Left 1)
                                True
                                ((symNot "c") .|| "a")
                                (UnionSingle $ Left 1)
                                (UnionSingle $ Left 2)
                            )
                            (UnionSingle $ Right 2)
                        ),
                  testCase "Degenerate to Single/If when idxff == idxft" $ do
                    let x =
                          ifWithStrategy
                            rootStrategy
                            "a"
                            (UnionSingle $ Left (1 :: Integer))
                            (UnionSingle $ Left (2 :: Integer))
                    let y =
                          ifWithStrategy
                            rootStrategy
                            "b"
                            (UnionSingle $ Left (1 :: Integer))
                            (UnionSingle $ Right (2 :: Integer))
                    AsKey
                      ( ifWithStrategy
                          rootStrategy
                          "c"
                          y
                          x
                      )
                      @?= AsKey
                        ( UnionIf
                            (Left 1)
                            True
                            ((symNot "c") .|| "b")
                            ( UnionIf
                                (Left 1)
                                True
                                ("c" .|| "a")
                                (UnionSingle $ Left 1)
                                (UnionSingle $ Left 2)
                            )
                            (UnionSingle $ Right 2)
                        ),
                  testCase "Non-degenerated case when idxtt < idxft" $ do
                    let x =
                          ifWithStrategy
                            rootStrategy
                            "a"
                            (UnionSingle $ TS1 (1 :: Integer))
                            (UnionSingle $ TS2 (2 :: Integer))
                    let y =
                          ifWithStrategy
                            rootStrategy
                            "b"
                            (UnionSingle $ TS2 (1 :: Integer))
                            (UnionSingle $ TS3 (2 :: Integer))
                    AsKey
                      ( ifWithStrategy
                          rootStrategy
                          "c"
                          x
                          y
                      )
                      @?= AsKey
                        ( UnionIf
                            (TS1 1)
                            True
                            ("c" .&& "a")
                            (UnionSingle $ TS1 1)
                            ( UnionIf
                                (TS2 1)
                                True
                                ("c" .|| "b")
                                ( UnionIf
                                    (TS2 1)
                                    True
                                    (symNot "c")
                                    (UnionSingle $ TS2 1)
                                    (UnionSingle $ TS2 2)
                                )
                                (UnionSingle $ TS3 2)
                            )
                        ),
                  testCase "Non-degenerated case when idxtt == idxft" $ do
                    let x =
                          ifWithStrategy
                            rootStrategy
                            "a"
                            (UnionSingle $ TS1 (1 :: Integer))
                            (UnionSingle $ TS2 (2 :: Integer))
                    let y =
                          ifWithStrategy
                            rootStrategy
                            "b"
                            (UnionSingle $ TS1 (2 :: Integer))
                            (UnionSingle $ TS3 (2 :: Integer))
                    AsKey
                      ( ifWithStrategy
                          rootStrategy
                          "c"
                          x
                          y
                      )
                      @?= AsKey
                        ( UnionIf
                            (TS1 1)
                            True
                            (symIte "c" "a" "b")
                            ( UnionIf
                                (TS1 1)
                                True
                                "c"
                                (UnionSingle $ TS1 1)
                                (UnionSingle $ TS1 2)
                            )
                            ( UnionIf
                                (TS2 2)
                                True
                                "c"
                                (UnionSingle $ TS2 2)
                                (UnionSingle $ TS3 2)
                            )
                        ),
                  testCase "Non-degenerated case when idxtt > idxft" $ do
                    let x =
                          ifWithStrategy
                            rootStrategy
                            "a"
                            (UnionSingle $ TS2 (1 :: Integer))
                            (UnionSingle $ TS3 (2 :: Integer))
                    let y =
                          ifWithStrategy
                            rootStrategy
                            "b"
                            (UnionSingle $ TS1 (1 :: Integer))
                            (UnionSingle $ TS2 (2 :: Integer))
                    AsKey
                      ( ifWithStrategy
                          rootStrategy
                          "c"
                          x
                          y
                      )
                      @?= AsKey
                        ( UnionIf
                            (TS1 1)
                            True
                            ((symNot "c") .&& "b")
                            (UnionSingle $ TS1 1)
                            ( UnionIf
                                (TS2 1)
                                True
                                ((symNot "c") .|| "a")
                                ( UnionIf
                                    (TS2 1)
                                    True
                                    "c"
                                    (UnionSingle $ TS2 1)
                                    (UnionSingle $ TS2 2)
                                )
                                (UnionSingle $ TS3 2)
                            )
                        )
                ],
              testCase "ifWithStrategy should tolerate non-merged Ifs" $ do
                let x =
                      UnionIf
                        (Right 2)
                        False
                        "a"
                        (UnionSingle $ Right (2 :: Integer))
                        (UnionSingle $ Left (2 :: Integer))
                let y =
                      UnionIf
                        (Right 3)
                        False
                        "b"
                        (UnionSingle $ Right 3)
                        (UnionSingle $ Left 1)
                AsKey
                  ( ifWithStrategy
                      rootStrategy
                      "c"
                      x
                      y
                  )
                  @?= AsKey
                    ( UnionIf
                        (Left 1)
                        True
                        (symIte "c" (symNot "a") (symNot "b"))
                        ( UnionIf
                            (Left 1)
                            True
                            (symNot "c")
                            (UnionSingle $ Left 1)
                            (UnionSingle $ Left 2)
                        )
                        ( UnionIf
                            (Right 2)
                            True
                            "c"
                            (UnionSingle $ Right 2)
                            (UnionSingle $ Right 3)
                        )
                    )
            ]
        ],
      testGroup
        "fullReconstruct"
        [ testCase "fullReconstruct should work" $ do
            let x =
                  UnionIf
                    (Right 2)
                    False
                    "a"
                    (UnionSingle $ Right (2 :: Integer))
                    (UnionSingle $ Left (2 :: Integer))
            let y =
                  UnionIf
                    (Right 3)
                    False
                    "b"
                    (UnionSingle $ Right 3)
                    (UnionSingle $ Left 1)
            let z = UnionIf (Right 2) False "c" x y
            AsKey (fullReconstruct rootStrategy z)
              @?= AsKey
                ( UnionIf
                    (Left 1)
                    True
                    (symIte "c" (symNot "a") (symNot "b"))
                    ( UnionIf
                        (Left 1)
                        True
                        (symNot "c")
                        (UnionSingle $ Left 1)
                        (UnionSingle $ Left 2)
                    )
                    ( UnionIf
                        (Right 2)
                        True
                        "c"
                        (UnionSingle $ Right 2)
                        (UnionSingle $ Right 3)
                    )
                )
        ]
    ]
