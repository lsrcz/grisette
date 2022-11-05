{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Grisette.Core.Data.Class.GenSymTests where

import Control.Monad.Except
import Control.Monad.Trans.Maybe
import Data.Proxy
import Grisette.Core.Control.Monad.UnionMBase
import Grisette.Core.Data.Class.Bool
import Grisette.Core.Data.Class.GenSym
import Grisette.Core.Data.Class.SimpleMergeable
import Grisette.TestUtils.SBool
import Test.Tasty
import Test.Tasty.HUnit

genSymTests :: TestTree
genSymTests =
  testGroup
    "GenSymTests"
    [ testGroup
        "GenSym for common types"
        [ testGroup
            "SBool"
            [ testGroup
                "() spec"
                [ testCase "genSym" $
                    (ggenSym @SBool () "a" :: UnionMBase SBool SBool)
                      @=? mrgSingle (ISBool "a" 0),
                  testCase "genSymSimple" $
                    (genSymSimple () "a" :: SBool)
                      @=? ISBool "a" 0
                ],
              testGroup
                "SBool spec"
                [ testCase "genSym" $
                    (ggenSym @SBool (CBool True) "a" :: UnionMBase SBool SBool)
                      @=? mrgSingle (ISBool "a" 0),
                  testCase "genSymSimple" $
                    (genSymSimple (CBool True) "a" :: SBool)
                      @=? ISBool "a" 0
                ]
            ],
          testGroup
            "Bool"
            [ testCase "() spec" $
                (ggenSym @SBool () "a" :: UnionMBase SBool Bool)
                  @=? mrgIf (ISBool "a" 0) (mrgSingle False) (mrgSingle True),
              testGroup
                "Bool spec"
                [ testGroup
                    "genSym"
                    [ testCase "True" $
                        (ggenSym @SBool True "a" :: UnionMBase SBool Bool)
                          @=? mrgSingle True,
                      testCase "False" $
                        (ggenSym @SBool False "a" :: UnionMBase SBool Bool)
                          @=? mrgSingle False
                    ],
                  testGroup
                    "genSymSimple"
                    [ testCase "True" $
                        (genSymSimple True "a" :: Bool)
                          @=? True,
                      testCase "False" $
                        (genSymSimple False "a" :: Bool)
                          @=? False
                    ]
                ]
            ],
          testGroup
            "Integer"
            [ testGroup
                "Integer spec"
                [ testCase "genSym" $
                    (ggenSym @SBool (1 :: Integer) "a" :: UnionMBase SBool Integer)
                      @=? mrgSingle 1,
                  testCase "genSymSimple" $
                    (genSymSimple (1 :: Integer) "a" :: Integer)
                      @=? 1
                ],
              testCase "Upper bound spec" $
                (ggenSym @SBool (EnumGenUpperBound (3 :: Integer)) "a" :: UnionMBase SBool Integer)
                  @=? mrgIf (ISBool "a" 0) (mrgSingle 0) (mrgIf (ISBool "a" 1) (mrgSingle 1) (mrgSingle 2)),
              testCase "Bound spec" $
                (ggenSym @SBool (EnumGenBound (-1 :: Integer) 2) "a" :: UnionMBase SBool Integer)
                  @=? mrgIf (ISBool "a" 0) (mrgSingle (-1)) (mrgIf (ISBool "a" 1) (mrgSingle 0) (mrgSingle 1))
            ],
          testGroup
            "Char"
            [ testGroup
                "Char spec"
                [ testCase "genSym" $
                    (ggenSym @SBool 'x' "a" :: UnionMBase SBool Char)
                      @=? mrgSingle 'x',
                  testCase "genSymSimple" $
                    (genSymSimple 'x' "a" :: Char) @=? 'x'
                ],
              testCase "Upper bound spec" $
                (ggenSym @SBool (EnumGenUpperBound @Char (toEnum 3)) "a" :: UnionMBase SBool Char)
                  @=? mrgIf (ISBool "a" 0) (mrgSingle $ toEnum 0) (mrgIf (ISBool "a" 1) (mrgSingle $ toEnum 1) (mrgSingle $ toEnum 2)),
              testCase "Bound spec" $
                (ggenSym @SBool (EnumGenBound 'a' 'd') "a" :: UnionMBase SBool Char)
                  @=? mrgIf (ISBool "a" 0) (mrgSingle 'a') (mrgIf (ISBool "a" 1) (mrgSingle 'b') (mrgSingle 'c'))
            ],
          testGroup
            "Maybe SBool"
            [ testGroup
                "Maybe SBool spec"
                [ testGroup
                    "Nothing"
                    [ testCase "genSym" $
                        (ggenSym (Nothing :: Maybe SBool) "a" :: UnionMBase SBool (Maybe SBool)) @=? mrgSingle Nothing,
                      testCase "genSymSimple" $
                        (genSymSimple (Nothing :: Maybe SBool) "a" :: Maybe SBool) @=? Nothing
                    ],
                  testGroup
                    "Just v"
                    [ testCase "genSym" $
                        (ggenSym (Just (SSBool "a")) "a" :: UnionMBase SBool (Maybe SBool)) @=? mrgSingle (Just (ISBool "a" 0)),
                      testCase "genSymSimple" $
                        (genSymSimple (Just (SSBool "a")) "a" :: Maybe SBool) @=? Just (ISBool "a" 0)
                    ]
                ],
              testCase "() spec" $
                (ggenSym () "a" :: UnionMBase SBool (Maybe SBool))
                  @=? mrgIf (ISBool "a" 0) (mrgSingle Nothing) (mrgSingle (Just (ISBool "a" 1)))
            ],
          testGroup
            "Either SBool SBool"
            [ testGroup
                "Either SBool SBool spec"
                [ testGroup
                    "Left v"
                    [ testCase "genSym" $
                        (ggenSym (Left (SSBool "a") :: Either SBool SBool) "a" :: UnionMBase SBool (Either SBool SBool))
                          @=? mrgSingle (Left (ISBool "a" 0)),
                      testCase "genSymSimple" $
                        (genSymSimple (Left (SSBool "a") :: Either SBool SBool) "a" :: Either SBool SBool)
                          @=? Left (ISBool "a" 0)
                    ],
                  testGroup
                    "Right v"
                    [ testCase "genSym" $
                        (ggenSym (Right (SSBool "a") :: Either SBool SBool) "a" :: UnionMBase SBool (Either SBool SBool))
                          @=? mrgSingle (Right (ISBool "a" 0)),
                      testCase "genSymSimple" $
                        (genSymSimple (Right (SSBool "a") :: Either SBool SBool) "a" :: Either SBool SBool)
                          @=? Right (ISBool "a" 0)
                    ]
                ],
              testCase "() spec" $ do
                (ggenSym () "a" :: UnionMBase SBool (Either SBool SBool))
                  @=? mrgIf (ISBool "a" 0) (mrgSingle $ Left $ ISBool "a" 1) (mrgSingle $ Right $ ISBool "a" 2)
            ],
          testGroup
            "lists"
            [ testGroup
                "Max length spec"
                [ testCase "max length = 0" $
                    (ggenSym (0 :: Integer) "a" :: UnionMBase SBool [SBool]) @=? mrgSingle [],
                  testCase "max length = 3" $
                    (ggenSym (3 :: Integer) "a" :: UnionMBase SBool [SBool])
                      @=? mrgIf
                        (ISBool "a" 3)
                        (mrgSingle [])
                        ( mrgIf
                            (ISBool "a" 4)
                            (mrgSingle [ISBool "a" 2])
                            ( mrgIf
                                (ISBool "a" 5)
                                (mrgSingle [ISBool "a" 1, ISBool "a" 2])
                                (mrgSingle [ISBool "a" 0, ISBool "a" 1, ISBool "a" 2])
                            )
                        )
                ],
              testGroup
                "Min & max length spec"
                [ testCase "min length = 1, max length = 3" $
                    (ggenSym (ListSpec 1 3 ()) "a" :: UnionMBase SBool [SBool])
                      @=? mrgIf
                        (ISBool "a" 3)
                        (mrgSingle [ISBool "a" 2])
                        ( mrgIf
                            (ISBool "a" 4)
                            (mrgSingle [ISBool "a" 1, ISBool "a" 2])
                            (mrgSingle [ISBool "a" 0, ISBool "a" 1, ISBool "a" 2])
                        ),
                  testCase "min length = 1, max length = 2, nested" $
                    (ggenSym (ListSpec 1 2 (ListSpec 1 2 ())) "a" :: UnionMBase SBool [UnionMBase SBool [SBool]])
                      @=? mrgIf
                        (ISBool "a" 6)
                        ( mrgSingle
                            [ mrgIf
                                (ISBool "a" 5)
                                (mrgSingle [ISBool "a" 4])
                                (mrgSingle [ISBool "a" 3, ISBool "a" 4])
                            ]
                        )
                        ( mrgSingle
                            [ mrgIf
                                (ISBool "a" 2)
                                (mrgSingle [ISBool "a" 1])
                                (mrgSingle [ISBool "a" 0, ISBool "a" 1]),
                              mrgIf
                                (ISBool "a" 5)
                                (mrgSingle [ISBool "a" 4])
                                (mrgSingle [ISBool "a" 3, ISBool "a" 4])
                            ]
                        )
                ],
              testGroup
                "Exact length spec"
                [ testGroup
                    "length = 2"
                    [ testCase "genSym" $
                        (ggenSym (SimpleListSpec 2 ()) "a" :: UnionMBase SBool [SBool])
                          @=? mrgSingle [ISBool "a" 0, ISBool "a" 1],
                      testCase "genSymSimple" $
                        (genSymSimple (SimpleListSpec 2 ()) "a" :: [SBool])
                          @=? [ISBool "a" 0, ISBool "a" 1]
                    ],
                  testGroup
                    "length = 2, nested"
                    [ testCase "genSym" $
                        (ggenSym (SimpleListSpec 2 (SimpleListSpec 2 ())) "a" :: UnionMBase SBool [[SBool]])
                          @=? mrgSingle [[ISBool "a" 0, ISBool "a" 1], [ISBool "a" 2, ISBool "a" 3]],
                      testCase "genSymSimple" $
                        (genSymSimple (SimpleListSpec 2 (SimpleListSpec 2 ())) "a" :: [[SBool]])
                          @=? [[ISBool "a" 0, ISBool "a" 1], [ISBool "a" 2, ISBool "a" 3]]
                    ]
                ],
              testGroup
                "List with same shape spec"
                [ testCase "genSym" $
                    (ggenSym [[CBool True], [SSBool "a", SSBool "b"]] "a" :: UnionMBase SBool [[SBool]])
                      @=? mrgSingle [[ISBool "a" 0], [ISBool "a" 1, ISBool "a" 2]],
                  testCase "genSymSimple" $
                    (genSymSimple [[CBool True], [SSBool "a", SSBool "b"]] "a" :: [[SBool]])
                      @=? [[ISBool "a" 0], [ISBool "a" 1, ISBool "a" 2]]
                ]
            ],
          testGroup
            "()"
            [ testCase "() spec" $ do
                (ggenSym () "a" :: UnionMBase SBool ()) @=? mrgSingle ()
                (genSymSimple () "a" :: ()) @=? ()
            ],
          testGroup
            "(,)"
            [ testGroup
                "Some spec"
                [ testCase "genSym" $
                    (ggenSym (EnumGenUpperBound @Integer 2, EnumGenUpperBound @Integer 2) "a" :: UnionMBase SBool (Integer, Integer))
                      @=? do
                        x1 <- mrgIf (ISBool "a" 0) (mrgSingle 0) (mrgSingle 1)
                        x2 <- mrgIf (ISBool "a" 1) (mrgSingle 0) (mrgSingle 1)
                        mrgSingle (x1, x2),
                  testCase "genSymSimple" $
                    (genSymSimple ((), [[SSBool "b"], [SSBool "b", SSBool "c"]]) "a" :: (SBool, [[SBool]]))
                      @=? (ISBool "a" 0, [[ISBool "a" 1], [ISBool "a" 2, ISBool "a" 3]])
                ],
              testGroup
                "No spec"
                [ testCase "genSym" $
                    (ggenSym () "a" :: UnionMBase SBool (SBool, SBool)) @=? mrgSingle (ISBool "a" 0, ISBool "a" 1),
                  testCase "genSymSimple" $
                    (genSymSimple () "a" :: (SBool, SBool)) @=? (ISBool "a" 0, ISBool "a" 1)
                ]
            ],
          testGroup
            "(,,)"
            [ testGroup
                "Some spec"
                [ testCase "genSym" $
                    ( ggenSym
                        ( EnumGenUpperBound @Integer 2,
                          EnumGenUpperBound @Integer 2,
                          EnumGenUpperBound @Integer 2
                        )
                        "a" ::
                        UnionMBase SBool (Integer, Integer, Integer)
                    )
                      @=? do
                        x1 <- mrgIf (ISBool "a" 0) (mrgSingle 0) (mrgSingle 1)
                        x2 <- mrgIf (ISBool "a" 1) (mrgSingle 0) (mrgSingle 1)
                        x3 <- mrgIf (ISBool "a" 2) (mrgSingle 0) (mrgSingle 1)
                        mrgSingle (x1, x2, x3),
                  testCase "genSymSimple" $
                    (genSymSimple ((), [[SSBool "b"], [SSBool "b", SSBool "c"]], ()) "a" :: (SBool, [[SBool]], SBool))
                      @=? (ISBool "a" 0, [[ISBool "a" 1], [ISBool "a" 2, ISBool "a" 3]], ISBool "a" 4)
                ],
              testGroup
                "No spec"
                [ testCase "genSym" $
                    (ggenSym () "a" :: UnionMBase SBool (SBool, SBool, SBool))
                      @=? mrgSingle (ISBool "a" 0, ISBool "a" 1, ISBool "a" 2),
                  testCase "genSymSimple" $
                    (genSymSimple () "a" :: (SBool, SBool, SBool))
                      @=? (ISBool "a" 0, ISBool "a" 1, ISBool "a" 2)
                ]
            ],
          testGroup
            "(,,,)"
            [ testGroup
                "Some spec"
                [ testCase "genSym" $
                    ( ggenSym
                        ( EnumGenUpperBound @Integer 2,
                          EnumGenUpperBound @Integer 2,
                          EnumGenUpperBound @Integer 2,
                          EnumGenUpperBound @Integer 2
                        )
                        "a" ::
                        UnionMBase SBool (Integer, Integer, Integer, Integer)
                    )
                      @=? do
                        x1 <- mrgIf (ISBool "a" 0) (mrgSingle 0) (mrgSingle 1)
                        x2 <- mrgIf (ISBool "a" 1) (mrgSingle 0) (mrgSingle 1)
                        x3 <- mrgIf (ISBool "a" 2) (mrgSingle 0) (mrgSingle 1)
                        x4 <- mrgIf (ISBool "a" 3) (mrgSingle 0) (mrgSingle 1)
                        mrgSingle (x1, x2, x3, x4),
                  testCase "genSymSimple" $
                    ( genSymSimple
                        ((), [[SSBool "b"], [SSBool "b", SSBool "c"]], (), ())
                        "a" ::
                        (SBool, [[SBool]], SBool, SBool)
                    )
                      @=? ( ISBool "a" 0,
                            [[ISBool "a" 1], [ISBool "a" 2, ISBool "a" 3]],
                            ISBool "a" 4,
                            ISBool "a" 5
                          )
                ],
              testGroup
                "No spec"
                [ testCase "genSym" $
                    (ggenSym () "a" :: UnionMBase SBool (SBool, SBool, SBool, SBool))
                      @=? mrgSingle (ISBool "a" 0, ISBool "a" 1, ISBool "a" 2, ISBool "a" 3),
                  testCase "genSymSimple" $
                    (genSymSimple () "a" :: (SBool, SBool, SBool, SBool))
                      @=? (ISBool "a" 0, ISBool "a" 1, ISBool "a" 2, ISBool "a" 3)
                ]
            ],
          testGroup
            "(,,,,)"
            [ testGroup
                "Some spec"
                [ testCase "genSym" $
                    ( ggenSym
                        ( EnumGenUpperBound @Integer 2,
                          EnumGenUpperBound @Integer 2,
                          EnumGenUpperBound @Integer 2,
                          EnumGenUpperBound @Integer 2,
                          EnumGenUpperBound @Integer 2
                        )
                        "a" ::
                        UnionMBase SBool (Integer, Integer, Integer, Integer, Integer)
                    )
                      @=? do
                        x1 <- mrgIf (ISBool "a" 0) (mrgSingle 0) (mrgSingle 1)
                        x2 <- mrgIf (ISBool "a" 1) (mrgSingle 0) (mrgSingle 1)
                        x3 <- mrgIf (ISBool "a" 2) (mrgSingle 0) (mrgSingle 1)
                        x4 <- mrgIf (ISBool "a" 3) (mrgSingle 0) (mrgSingle 1)
                        x5 <- mrgIf (ISBool "a" 4) (mrgSingle 0) (mrgSingle 1)
                        mrgSingle (x1, x2, x3, x4, x5),
                  testCase "genSymSimple" $
                    ( genSymSimple
                        ((), [[SSBool "b"], [SSBool "b", SSBool "c"]], (), (), ())
                        "a" ::
                        (SBool, [[SBool]], SBool, SBool, SBool)
                    )
                      @=? ( ISBool "a" 0,
                            [[ISBool "a" 1], [ISBool "a" 2, ISBool "a" 3]],
                            ISBool "a" 4,
                            ISBool "a" 5,
                            ISBool "a" 6
                          )
                ],
              testGroup
                "No spec"
                [ testCase "genSym" $
                    (ggenSym () "a" :: UnionMBase SBool (SBool, SBool, SBool, SBool, SBool))
                      @=? mrgSingle
                        (ISBool "a" 0, ISBool "a" 1, ISBool "a" 2, ISBool "a" 3, ISBool "a" 4),
                  testCase "genSymSimple" $
                    (genSymSimple () "a" :: (SBool, SBool, SBool, SBool, SBool))
                      @=? ( ISBool "a" 0,
                            ISBool "a" 1,
                            ISBool "a" 2,
                            ISBool "a" 3,
                            ISBool "a" 4
                          )
                ]
            ],
          testGroup
            "(,,,,,)"
            [ testGroup
                "Some spec"
                [ testCase "genSym" $
                    ( ggenSym
                        ( EnumGenUpperBound @Integer 2,
                          EnumGenUpperBound @Integer 2,
                          EnumGenUpperBound @Integer 2,
                          EnumGenUpperBound @Integer 2,
                          EnumGenUpperBound @Integer 2,
                          EnumGenUpperBound @Integer 2
                        )
                        "a" ::
                        UnionMBase SBool (Integer, Integer, Integer, Integer, Integer, Integer)
                    )
                      @=? do
                        x1 <- mrgIf (ISBool "a" 0) (mrgSingle 0) (mrgSingle 1)
                        x2 <- mrgIf (ISBool "a" 1) (mrgSingle 0) (mrgSingle 1)
                        x3 <- mrgIf (ISBool "a" 2) (mrgSingle 0) (mrgSingle 1)
                        x4 <- mrgIf (ISBool "a" 3) (mrgSingle 0) (mrgSingle 1)
                        x5 <- mrgIf (ISBool "a" 4) (mrgSingle 0) (mrgSingle 1)
                        x6 <- mrgIf (ISBool "a" 5) (mrgSingle 0) (mrgSingle 1)
                        mrgSingle (x1, x2, x3, x4, x5, x6),
                  testCase "genSymSimple" $
                    ( genSymSimple
                        ((), [[SSBool "b"], [SSBool "b", SSBool "c"]], (), (), (), ())
                        "a" ::
                        (SBool, [[SBool]], SBool, SBool, SBool, SBool)
                    )
                      @=? ( ISBool "a" 0,
                            [[ISBool "a" 1], [ISBool "a" 2, ISBool "a" 3]],
                            ISBool "a" 4,
                            ISBool "a" 5,
                            ISBool "a" 6,
                            ISBool "a" 7
                          )
                ],
              testGroup
                "No spec"
                [ testCase "genSym" $
                    (ggenSym () "a" :: UnionMBase SBool (SBool, SBool, SBool, SBool, SBool, SBool))
                      @=? mrgSingle
                        ( ISBool "a" 0,
                          ISBool "a" 1,
                          ISBool "a" 2,
                          ISBool "a" 3,
                          ISBool "a" 4,
                          ISBool "a" 5
                        ),
                  testCase "genSymSimple" $
                    (genSymSimple () "a" :: (SBool, SBool, SBool, SBool, SBool, SBool))
                      @=? ( ISBool "a" 0,
                            ISBool "a" 1,
                            ISBool "a" 2,
                            ISBool "a" 3,
                            ISBool "a" 4,
                            ISBool "a" 5
                          )
                ]
            ],
          testGroup
            "(,,,,,,)"
            [ testGroup
                "Some spec"
                [ testCase "genSym" $
                    ( ggenSym
                        ( EnumGenUpperBound @Integer 2,
                          EnumGenUpperBound @Integer 2,
                          EnumGenUpperBound @Integer 2,
                          EnumGenUpperBound @Integer 2,
                          EnumGenUpperBound @Integer 2,
                          EnumGenUpperBound @Integer 2,
                          EnumGenUpperBound @Integer 2
                        )
                        "a" ::
                        UnionMBase SBool (Integer, Integer, Integer, Integer, Integer, Integer, Integer)
                    )
                      @=? do
                        x1 <- mrgIf (ISBool "a" 0) (mrgSingle 0) (mrgSingle 1)
                        x2 <- mrgIf (ISBool "a" 1) (mrgSingle 0) (mrgSingle 1)
                        x3 <- mrgIf (ISBool "a" 2) (mrgSingle 0) (mrgSingle 1)
                        x4 <- mrgIf (ISBool "a" 3) (mrgSingle 0) (mrgSingle 1)
                        x5 <- mrgIf (ISBool "a" 4) (mrgSingle 0) (mrgSingle 1)
                        x6 <- mrgIf (ISBool "a" 5) (mrgSingle 0) (mrgSingle 1)
                        x7 <- mrgIf (ISBool "a" 6) (mrgSingle 0) (mrgSingle 1)
                        mrgSingle (x1, x2, x3, x4, x5, x6, x7),
                  testCase "genSymSimple" $
                    ( genSymSimple
                        ((), [[SSBool "b"], [SSBool "b", SSBool "c"]], (), (), (), (), ())
                        "a" ::
                        (SBool, [[SBool]], SBool, SBool, SBool, SBool, SBool)
                    )
                      @=? ( ISBool "a" 0,
                            [[ISBool "a" 1], [ISBool "a" 2, ISBool "a" 3]],
                            ISBool "a" 4,
                            ISBool "a" 5,
                            ISBool "a" 6,
                            ISBool "a" 7,
                            ISBool "a" 8
                          )
                ],
              testGroup
                "No spec"
                [ testCase "genSym" $
                    (ggenSym () "a" :: UnionMBase SBool (SBool, SBool, SBool, SBool, SBool, SBool, SBool))
                      @=? mrgSingle
                        ( ISBool "a" 0,
                          ISBool "a" 1,
                          ISBool "a" 2,
                          ISBool "a" 3,
                          ISBool "a" 4,
                          ISBool "a" 5,
                          ISBool "a" 6
                        ),
                  testCase "genSymSimple" $
                    (genSymSimple () "a" :: (SBool, SBool, SBool, SBool, SBool, SBool, SBool))
                      @=? ( ISBool "a" 0,
                            ISBool "a" 1,
                            ISBool "a" 2,
                            ISBool "a" 3,
                            ISBool "a" 4,
                            ISBool "a" 5,
                            ISBool "a" 6
                          )
                ]
            ],
          testGroup
            "(,,,,,,,)"
            [ testGroup
                "Some spec"
                [ testCase "genSym" $
                    ( ggenSym
                        ( EnumGenUpperBound @Integer 2,
                          EnumGenUpperBound @Integer 2,
                          EnumGenUpperBound @Integer 2,
                          EnumGenUpperBound @Integer 2,
                          EnumGenUpperBound @Integer 2,
                          EnumGenUpperBound @Integer 2,
                          EnumGenUpperBound @Integer 2,
                          EnumGenUpperBound @Integer 2
                        )
                        "a" ::
                        UnionMBase SBool (Integer, Integer, Integer, Integer, Integer, Integer, Integer, Integer)
                    )
                      @=? do
                        x1 <- mrgIf (ISBool "a" 0) (mrgSingle 0) (mrgSingle 1)
                        x2 <- mrgIf (ISBool "a" 1) (mrgSingle 0) (mrgSingle 1)
                        x3 <- mrgIf (ISBool "a" 2) (mrgSingle 0) (mrgSingle 1)
                        x4 <- mrgIf (ISBool "a" 3) (mrgSingle 0) (mrgSingle 1)
                        x5 <- mrgIf (ISBool "a" 4) (mrgSingle 0) (mrgSingle 1)
                        x6 <- mrgIf (ISBool "a" 5) (mrgSingle 0) (mrgSingle 1)
                        x7 <- mrgIf (ISBool "a" 6) (mrgSingle 0) (mrgSingle 1)
                        x8 <- mrgIf (ISBool "a" 7) (mrgSingle 0) (mrgSingle 1)
                        mrgSingle (x1, x2, x3, x4, x5, x6, x7, x8),
                  testCase "genSymSimple" $
                    ( genSymSimple
                        ((), [[SSBool "b"], [SSBool "b", SSBool "c"]], (), (), (), (), (), ())
                        "a" ::
                        (SBool, [[SBool]], SBool, SBool, SBool, SBool, SBool, SBool)
                    )
                      @=? ( ISBool "a" 0,
                            [[ISBool "a" 1], [ISBool "a" 2, ISBool "a" 3]],
                            ISBool "a" 4,
                            ISBool "a" 5,
                            ISBool "a" 6,
                            ISBool "a" 7,
                            ISBool "a" 8,
                            ISBool "a" 9
                          )
                ],
              testGroup
                "No spec"
                [ testCase "genSym" $
                    (ggenSym () "a" :: UnionMBase SBool (SBool, SBool, SBool, SBool, SBool, SBool, SBool, SBool))
                      @=? mrgSingle
                        ( ISBool "a" 0,
                          ISBool "a" 1,
                          ISBool "a" 2,
                          ISBool "a" 3,
                          ISBool "a" 4,
                          ISBool "a" 5,
                          ISBool "a" 6,
                          ISBool "a" 7
                        ),
                  testCase "genSymSimple" $
                    (genSymSimple () "a" :: (SBool, SBool, SBool, SBool, SBool, SBool, SBool, SBool))
                      @=? ( ISBool "a" 0,
                            ISBool "a" 1,
                            ISBool "a" 2,
                            ISBool "a" 3,
                            ISBool "a" 4,
                            ISBool "a" 5,
                            ISBool "a" 6,
                            ISBool "a" 7
                          )
                ]
            ],
          testGroup
            "MaybeT Maybe SBool"
            [ testGroup
                "Same shape spec"
                [ testGroup
                    "MaybeT Nothing"
                    [ testCase "genSym" $
                        (ggenSym (MaybeT Nothing :: MaybeT Maybe SBool) "a" :: UnionMBase SBool (MaybeT Maybe SBool))
                          @=? mrgSingle (MaybeT Nothing),
                      testCase "genSymSimple" $
                        (genSymSimple (MaybeT Nothing :: MaybeT Maybe SBool) "a" :: MaybeT Maybe SBool)
                          @=? MaybeT Nothing
                    ],
                  testGroup
                    "MaybeT (Just Nothing)"
                    [ testCase "genSym" $
                        (ggenSym (MaybeT (Just Nothing) :: MaybeT Maybe SBool) "a" :: UnionMBase SBool (MaybeT Maybe SBool))
                          @=? mrgSingle (MaybeT (Just Nothing)),
                      testCase "genSymSimple" $
                        (genSymSimple (MaybeT (Just (Just $ SSBool "a")) :: MaybeT Maybe SBool) "a" :: MaybeT Maybe SBool)
                          @=? MaybeT (Just (Just $ ISBool "a" 0))
                    ],
                  testGroup
                    "MaybeT (Just (Just v))"
                    [ testCase "genSym" $
                        (ggenSym (MaybeT (Just (Just $ SSBool "a")) :: MaybeT Maybe SBool) "a" :: UnionMBase SBool (MaybeT Maybe SBool))
                          @=? mrgSingle (MaybeT (Just (Just $ ISBool "a" 0))),
                      testCase "genSymSimple" $
                        (genSymSimple (MaybeT (Just (Just $ SSBool "a")) :: MaybeT Maybe SBool) "a" :: MaybeT Maybe SBool)
                          @=? MaybeT (Just (Just $ ISBool "a" 0))
                    ]
                ],
              testCase "No spec" $
                (ggenSym () "a" :: UnionMBase SBool (MaybeT Maybe SBool))
                  @=? mrgIf
                    (ISBool "a" 0)
                    (mrgSingle $ MaybeT Nothing)
                    ( mrgIf
                        (ISBool "a" 1)
                        (mrgSingle $ MaybeT $ Just Nothing)
                        (mrgSingle $ MaybeT $ Just $ Just $ ISBool "a" 2)
                    ),
              testGroup
                "Maybe (Maybe SBool) spec"
                [ testGroup
                    "Nothing"
                    [ testCase "genSym" $
                        (ggenSym (Nothing :: Maybe (Maybe SBool)) "a" :: UnionMBase SBool (MaybeT Maybe SBool))
                          @=? mrgSingle (MaybeT Nothing),
                      testCase "genSymSimple" $
                        (genSymSimple (Nothing :: Maybe (Maybe SBool)) "a" :: MaybeT Maybe SBool)
                          @=? MaybeT Nothing
                    ],
                  testGroup
                    "Just Nothing"
                    [ testCase "genSym" $
                        (ggenSym (Just Nothing :: Maybe (Maybe SBool)) "a" :: UnionMBase SBool (MaybeT Maybe SBool))
                          @=? mrgSingle (MaybeT (Just Nothing)),
                      testCase "genSymSimple" $
                        (genSymSimple (Just Nothing :: Maybe (Maybe SBool)) "a" :: MaybeT Maybe SBool)
                          @=? MaybeT (Just Nothing)
                    ],
                  testGroup
                    "Just (Just v)"
                    [ testCase "genSym" $
                        (ggenSym (Just $ Just $ SSBool "a" :: Maybe (Maybe SBool)) "a" :: UnionMBase SBool (MaybeT Maybe SBool))
                          @=? mrgSingle (MaybeT (Just (Just $ ISBool "a" 0))),
                      testCase "genSymSimple" $
                        (genSymSimple (Just $ Just $ SSBool "a" :: Maybe (Maybe SBool)) "a" :: MaybeT Maybe SBool)
                          @=? MaybeT (Just (Just $ ISBool "a" 0))
                    ]
                ]
            ],
          testGroup
            "ExceptT SBool Maybe SBool"
            [ testGroup
                "Same shape spec"
                [ testGroup
                    "ExceptT Nothing"
                    [ testCase "genSym" $
                        (ggenSym (ExceptT Nothing :: ExceptT SBool Maybe SBool) "a" :: UnionMBase SBool (ExceptT SBool Maybe SBool))
                          @=? mrgSingle (ExceptT Nothing),
                      testCase "genSymSimple" $
                        (genSymSimple (ExceptT Nothing :: ExceptT SBool Maybe SBool) "a" :: ExceptT SBool Maybe SBool)
                          @=? ExceptT Nothing
                    ],
                  testGroup
                    "ExceptT (Just (Left v))"
                    [ testCase "genSym" $
                        ( ggenSym (ExceptT $ Just $ Left $ SSBool "a" :: ExceptT SBool Maybe SBool) "a" ::
                            UnionMBase SBool (ExceptT SBool Maybe SBool)
                        )
                          @=? mrgSingle (ExceptT $ Just $ Left $ ISBool "a" 0),
                      testCase "genSymSimple" $
                        ( genSymSimple
                            (ExceptT $ Just $ Left $ SSBool "a" :: ExceptT SBool Maybe SBool)
                            "a" ::
                            ExceptT SBool Maybe SBool
                        )
                          @=? ExceptT (Just $ Left $ ISBool "a" 0)
                    ],
                  testGroup
                    "ExceptT (Just (Right v))"
                    [ testCase "genSym" $
                        ( ggenSym (ExceptT $ Just $ Right $ SSBool "a" :: ExceptT SBool Maybe SBool) "a" ::
                            UnionMBase SBool (ExceptT SBool Maybe SBool)
                        )
                          @=? mrgSingle (ExceptT $ Just $ Right $ ISBool "a" 0),
                      testCase "genSymSimple" $
                        ( genSymSimple
                            (ExceptT $ Just $ Right $ SSBool "a" :: ExceptT SBool Maybe SBool)
                            "a" ::
                            ExceptT SBool Maybe SBool
                        )
                          @=? ExceptT (Just $ Right $ ISBool "a" 0)
                    ]
                ],
              testCase "() spec" $ do
                (ggenSym () "a" :: UnionMBase SBool (ExceptT SBool Maybe SBool))
                  @=? mrgIf
                    (ISBool "a" 0)
                    (mrgSingle $ ExceptT Nothing)
                    ( mrgIf
                        (ISBool "a" 1)
                        (mrgSingle $ ExceptT $ Just $ Left $ ISBool "a" 2)
                        (mrgSingle $ ExceptT $ Just $ Right $ ISBool "a" 3)
                    ),
              testGroup
                "Maybe (Either SBool SBool) spec"
                [ testGroup
                    "Nothing"
                    [ testCase "genSym" $
                        (ggenSym (Nothing :: Maybe (Either SBool SBool)) "a" :: UnionMBase SBool (ExceptT SBool Maybe SBool))
                          @=? mrgSingle (ExceptT Nothing),
                      testCase "genSymSimple" $
                        (genSymSimple (Nothing :: Maybe (Either SBool SBool)) "a" :: ExceptT SBool Maybe SBool)
                          @=? ExceptT Nothing
                    ],
                  testGroup
                    "Just (left v)"
                    [ testCase "genSym" $
                        (ggenSym (Just $ Left $ SSBool "a" :: Maybe (Either SBool SBool)) "a" :: UnionMBase SBool (ExceptT SBool Maybe SBool))
                          @=? mrgSingle (ExceptT (Just (Left $ ISBool "a" 0))),
                      testCase "genSymSimple" $
                        (genSymSimple (Just $ Left $ SSBool "a" :: Maybe (Either SBool SBool)) "a" :: ExceptT SBool Maybe SBool)
                          @=? ExceptT (Just (Left $ ISBool "a" 0))
                    ],
                  testGroup
                    "Just (left v)"
                    [ testCase "genSym" $
                        (ggenSym (Just $ Right $ SSBool "a" :: Maybe (Either SBool SBool)) "a" :: UnionMBase SBool (ExceptT SBool Maybe SBool))
                          @=? mrgSingle (ExceptT (Just (Right $ ISBool "a" 0))),
                      testCase "genSymSimple" $
                        (genSymSimple (Just $ Right $ SSBool "a" :: Maybe (Either SBool SBool)) "a" :: ExceptT SBool Maybe SBool)
                          @=? ExceptT (Just (Right $ ISBool "a" 0))
                    ]
                ]
            ]
        ],
      testGroup
        "gchoose*"
        [ testCase "gchooseFresh" $ do
            (runGenSymFresh (gchooseFresh [1, 2, 3]) "a" :: UnionMBase SBool Int)
              @=? mrgIf (ISBool "a" 0) (mrgSingle 1) (mrgIf (ISBool "a" 1) (mrgSingle 2) (mrgSingle 3)),
          testCase "gchoose" $ do
            (gchoose [1, 2, 3] "a" :: UnionMBase SBool Int)
              @=? mrgIf (ISBool "a" 0) (mrgSingle 1) (mrgIf (ISBool "a" 1) (mrgSingle 2) (mrgSingle 3)),
          testCase "gchooseSimpleFresh" $ do
            (runGenSymFresh (gchooseSimpleFresh (Proxy @SBool) ["x", "y", "z"]) "a" :: SBool)
              @=? ites (ISBool "a" 0) (SSBool "x") (ites (ISBool "a" 1) (SSBool "y") (SSBool "z")),
          testCase "gchooseSimple" $ do
            (gchooseSimple (Proxy @SBool) ["x", "y", "z"] "a" :: SBool)
              @=? ites (ISBool "a" 0) (SSBool "x") (ites (ISBool "a" 1) (SSBool "y") (SSBool "z")),
          testCase "gchooseUnionFresh" $ do
            ( runGenSymFresh
                (gchooseUnionFresh [mrgIf (SSBool "x") 1 2, mrgIf (SSBool "x") 2 3, mrgIf (SSBool "x") 3 4])
                "a" ::
                UnionMBase SBool Int
              )
              @=? mrgIf
                (ISBool "a" 0)
                (mrgIf (SSBool "x") 1 2)
                ( mrgIf
                    (ISBool "a" 1)
                    (mrgIf (SSBool "x") 2 3)
                    (mrgIf (SSBool "x") 3 4)
                ),
          testCase "gchooseUnion" $ do
            (gchooseUnion [mrgIf (SSBool "x") 1 2, mrgIf (SSBool "x") 2 3, mrgIf (SSBool "x") 3 4] "a" :: UnionMBase SBool Int)
              @=? mrgIf
                (ISBool "a" 0)
                (mrgIf (SSBool "x") 1 2)
                ( mrgIf
                    (ISBool "a" 1)
                    (mrgIf (SSBool "x") 2 3)
                    (mrgIf (SSBool "x") 3 4)
                )
        ]
    ]
