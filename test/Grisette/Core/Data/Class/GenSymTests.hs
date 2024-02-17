{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Grisette.Core.Data.Class.GenSymTests (genSymTests) where

import Control.Monad.Except (ExceptT (ExceptT))
import Control.Monad.Trans.Maybe (MaybeT (MaybeT))
import Grisette.Core.Control.Monad.UnionM (UnionM)
import Grisette.Core.Data.Class.GenSym
  ( EnumGenBound (EnumGenBound),
    EnumGenUpperBound (EnumGenUpperBound),
    Fresh,
    FreshT,
    GenSymSimple (simpleFresh),
    ListSpec (ListSpec),
    SimpleListSpec (SimpleListSpec),
    choose,
    chooseFresh,
    chooseSimple,
    chooseSimpleFresh,
    chooseUnion,
    chooseUnionFresh,
    genSym,
    genSymSimple,
    liftFresh,
    runFresh,
    runFreshT,
  )
import Grisette.Core.Data.Class.ITEOp (ITEOp (symIte))
import Grisette.Core.Data.Class.SimpleMergeable (mrgIf)
import Grisette.Core.Data.Class.TestValues (conBool, isymBool, ssymBool)
import Grisette.Core.Data.Class.TryMerge (mrgPure)
import Grisette.IR.SymPrim.Data.SymPrim (SymBool)
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit ((@?=))

genSymTests :: Test
genSymTests =
  testGroup
    "GenSym"
    [ testGroup
        "GenSym for common types"
        [ testGroup
            "SymBool"
            [ testGroup
                "() spec"
                [ testCase "genSym" $
                    (genSym () "a" :: UnionM SymBool)
                      @?= mrgPure (isymBool "a" 0),
                  testCase "genSymSimple" $
                    (genSymSimple () "a" :: SymBool)
                      @?= isymBool "a" 0
                ],
              testGroup
                "SymBool spec"
                [ testCase "genSym" $
                    (genSym (conBool True) "a" :: UnionM SymBool)
                      @?= mrgPure (isymBool "a" 0),
                  testCase "genSymSimple" $
                    (genSymSimple (conBool True) "a" :: SymBool)
                      @?= isymBool "a" 0
                ]
            ],
          testGroup
            "Bool"
            [ testCase "() spec" $
                (genSym () "a" :: UnionM Bool)
                  @?= mrgIf (isymBool "a" 0) (mrgPure False) (mrgPure True),
              testGroup
                "Bool spec"
                [ testGroup
                    "genSym"
                    [ testCase "True" $
                        (genSym True "a" :: UnionM Bool)
                          @?= mrgPure True,
                      testCase "False" $
                        (genSym False "a" :: UnionM Bool)
                          @?= mrgPure False
                    ],
                  testGroup
                    "genSymSimple"
                    [ testCase "True" $
                        (genSymSimple True "a" :: Bool)
                          @?= True,
                      testCase "False" $
                        (genSymSimple False "a" :: Bool)
                          @?= False
                    ]
                ]
            ],
          testGroup
            "Integer"
            [ testGroup
                "Integer spec"
                [ testCase "genSym" $
                    (genSym (1 :: Integer) "a" :: UnionM Integer)
                      @?= mrgPure 1,
                  testCase "genSymSimple" $
                    (genSymSimple (1 :: Integer) "a" :: Integer)
                      @?= 1
                ],
              testCase "Upper bound spec" $
                ( genSym (EnumGenUpperBound (3 :: Integer)) "a" ::
                    UnionM Integer
                )
                  @?= mrgIf
                    (isymBool "a" 0)
                    (mrgPure 0)
                    (mrgIf (isymBool "a" 1) (mrgPure 1) (mrgPure 2)),
              testCase "Bound spec" $
                (genSym (EnumGenBound (-1 :: Integer) 2) "a" :: UnionM Integer)
                  @?= mrgIf
                    (isymBool "a" 0)
                    (mrgPure (-1))
                    (mrgIf (isymBool "a" 1) (mrgPure 0) (mrgPure 1))
            ],
          testGroup
            "Char"
            [ testGroup
                "Char spec"
                [ testCase "genSym" $
                    (genSym 'x' "a" :: UnionM Char)
                      @?= mrgPure 'x',
                  testCase "genSymSimple" $
                    (genSymSimple 'x' "a" :: Char) @?= 'x'
                ],
              testCase "Upper bound spec" $
                (genSym (EnumGenUpperBound @Char (toEnum 3)) "a" :: UnionM Char)
                  @?= mrgIf
                    (isymBool "a" 0)
                    (mrgPure $ toEnum 0)
                    ( mrgIf
                        (isymBool "a" 1)
                        (mrgPure $ toEnum 1)
                        (mrgPure $ toEnum 2)
                    ),
              testCase "Bound spec" $
                (genSym (EnumGenBound 'a' 'd') "a" :: UnionM Char)
                  @?= mrgIf
                    (isymBool "a" 0)
                    (mrgPure 'a')
                    (mrgIf (isymBool "a" 1) (mrgPure 'b') (mrgPure 'c'))
            ],
          testGroup
            "Maybe SymBool"
            [ testGroup
                "Maybe SymBool spec"
                [ testGroup
                    "Nothing"
                    [ testCase "genSym" $
                        ( genSym (Nothing :: Maybe SymBool) "a" ::
                            UnionM (Maybe SymBool)
                        )
                          @?= mrgPure Nothing,
                      testCase "genSymSimple" $
                        ( genSymSimple (Nothing :: Maybe SymBool) "a" ::
                            Maybe SymBool
                        )
                          @?= Nothing
                    ],
                  testGroup
                    "Just v"
                    [ testCase "genSym" $
                        ( genSym (Just (ssymBool "a")) "a" ::
                            UnionM (Maybe SymBool)
                        )
                          @?= mrgPure (Just (isymBool "a" 0)),
                      testCase "genSymSimple" $
                        ( genSymSimple (Just (ssymBool "a")) "a" ::
                            Maybe SymBool
                        )
                          @?= Just (isymBool "a" 0)
                    ]
                ],
              testCase "() spec" $
                (genSym () "a" :: UnionM (Maybe SymBool))
                  @?= mrgIf
                    (isymBool "a" 0)
                    (mrgPure Nothing)
                    (mrgPure (Just (isymBool "a" 1)))
            ],
          testGroup
            "Either SymBool SymBool"
            [ testGroup
                "Either SymBool SymBool spec"
                [ testGroup
                    "Left v"
                    [ testCase "genSym" $
                        ( genSym
                            ( Left (ssymBool "a") ::
                                Either SymBool SymBool
                            )
                            "a" ::
                            UnionM (Either SymBool SymBool)
                        )
                          @?= mrgPure (Left (isymBool "a" 0)),
                      testCase "genSymSimple" $
                        ( genSymSimple
                            ( Left (ssymBool "a") ::
                                Either SymBool SymBool
                            )
                            "a" ::
                            Either SymBool SymBool
                        )
                          @?= Left (isymBool "a" 0)
                    ],
                  testGroup
                    "Right v"
                    [ testCase "genSym" $
                        ( genSym
                            ( Right (ssymBool "a") ::
                                Either SymBool SymBool
                            )
                            "a" ::
                            UnionM (Either SymBool SymBool)
                        )
                          @?= mrgPure (Right (isymBool "a" 0)),
                      testCase "genSymSimple" $
                        ( genSymSimple
                            ( Right (ssymBool "a") ::
                                Either SymBool SymBool
                            )
                            "a" ::
                            Either SymBool SymBool
                        )
                          @?= Right (isymBool "a" 0)
                    ]
                ],
              testCase "() spec" $ do
                (genSym () "a" :: UnionM (Either SymBool SymBool))
                  @?= mrgIf
                    (isymBool "a" 0)
                    (mrgPure $ Left $ isymBool "a" 1)
                    (mrgPure $ Right $ isymBool "a" 2)
            ],
          testGroup
            "lists"
            [ testGroup
                "Max length spec"
                [ testCase "max length = 0" $
                    (genSym (0 :: Integer) "a" :: UnionM [SymBool])
                      @?= mrgPure [],
                  testCase "max length = 3" $
                    (genSym (3 :: Integer) "a" :: UnionM [SymBool])
                      @?= mrgIf
                        (isymBool "a" 3)
                        (mrgPure [])
                        ( mrgIf
                            (isymBool "a" 4)
                            (mrgPure [isymBool "a" 2])
                            ( mrgIf
                                (isymBool "a" 5)
                                (mrgPure [isymBool "a" 1, isymBool "a" 2])
                                ( mrgPure
                                    [ isymBool "a" 0,
                                      isymBool "a" 1,
                                      isymBool "a" 2
                                    ]
                                )
                            )
                        )
                ],
              testGroup
                "Min & max length spec"
                [ testCase "min length = 1, max length = 3" $
                    (genSym (ListSpec 1 3 ()) "a" :: UnionM [SymBool])
                      @?= mrgIf
                        (isymBool "a" 3)
                        (mrgPure [isymBool "a" 2])
                        ( mrgIf
                            (isymBool "a" 4)
                            (mrgPure [isymBool "a" 1, isymBool "a" 2])
                            ( mrgPure
                                [ isymBool "a" 0,
                                  isymBool "a" 1,
                                  isymBool "a" 2
                                ]
                            )
                        ),
                  testCase "min length = 1, max length = 2, nested" $
                    ( genSym (ListSpec 1 2 (ListSpec 1 2 ())) "a" ::
                        UnionM [UnionM [SymBool]]
                    )
                      @?= mrgIf
                        (isymBool "a" 6)
                        ( mrgPure
                            [ mrgIf
                                (isymBool "a" 5)
                                (mrgPure [isymBool "a" 4])
                                (mrgPure [isymBool "a" 3, isymBool "a" 4])
                            ]
                        )
                        ( mrgPure
                            [ mrgIf
                                (isymBool "a" 2)
                                (mrgPure [isymBool "a" 1])
                                (mrgPure [isymBool "a" 0, isymBool "a" 1]),
                              mrgIf
                                (isymBool "a" 5)
                                (mrgPure [isymBool "a" 4])
                                (mrgPure [isymBool "a" 3, isymBool "a" 4])
                            ]
                        )
                ],
              testGroup
                "Exact length spec"
                [ testGroup
                    "length = 2"
                    [ testCase "genSym" $
                        (genSym (SimpleListSpec 2 ()) "a" :: UnionM [SymBool])
                          @?= mrgPure [isymBool "a" 0, isymBool "a" 1],
                      testCase "genSymSimple" $
                        (genSymSimple (SimpleListSpec 2 ()) "a" :: [SymBool])
                          @?= [isymBool "a" 0, isymBool "a" 1]
                    ],
                  testGroup
                    "length = 2, nested"
                    [ testCase "genSym" $
                        ( genSym
                            (SimpleListSpec 2 (SimpleListSpec 2 ()))
                            "a" ::
                            UnionM [[SymBool]]
                        )
                          @?= mrgPure
                            [ [isymBool "a" 0, isymBool "a" 1],
                              [isymBool "a" 2, isymBool "a" 3]
                            ],
                      testCase "genSymSimple" $
                        ( genSymSimple
                            (SimpleListSpec 2 (SimpleListSpec 2 ()))
                            "a" ::
                            [[SymBool]]
                        )
                          @?= [ [isymBool "a" 0, isymBool "a" 1],
                                [isymBool "a" 2, isymBool "a" 3]
                              ]
                    ]
                ],
              testGroup
                "List with same shape spec"
                [ testCase "genSym" $
                    ( genSym
                        [[conBool True], [ssymBool "a", ssymBool "b"]]
                        "a" ::
                        UnionM [[SymBool]]
                    )
                      @?= mrgPure
                        [ [isymBool "a" 0],
                          [isymBool "a" 1, isymBool "a" 2]
                        ],
                  testCase "genSymSimple" $
                    ( genSymSimple
                        [ [conBool True],
                          [ssymBool "a", ssymBool "b"]
                        ]
                        "a" ::
                        [[SymBool]]
                    )
                      @?= [[isymBool "a" 0], [isymBool "a" 1, isymBool "a" 2]]
                ]
            ],
          testGroup
            "()"
            [ testCase "() spec" $ do
                (genSym () "a" :: UnionM ()) @?= mrgPure ()
                (genSymSimple () "a" :: ()) @?= ()
            ],
          testGroup
            "(,)"
            [ testGroup
                "Some spec"
                [ testCase "genSym" $
                    ( genSym
                        ( EnumGenUpperBound @Integer 2,
                          EnumGenUpperBound @Integer 2
                        )
                        "a" ::
                        UnionM (Integer, Integer)
                    )
                      @?= do
                        x1 <- mrgIf (isymBool "a" 0) (mrgPure 0) (mrgPure 1)
                        x2 <- mrgIf (isymBool "a" 1) (mrgPure 0) (mrgPure 1)
                        mrgPure (x1, x2),
                  testCase "genSymSimple" $
                    ( genSymSimple
                        ( (),
                          [ [ssymBool "b"],
                            [ssymBool "b", ssymBool "c"]
                          ]
                        )
                        "a" ::
                        (SymBool, [[SymBool]])
                    )
                      @?= ( isymBool "a" 0,
                            [ [isymBool "a" 1],
                              [isymBool "a" 2, isymBool "a" 3]
                            ]
                          )
                ],
              testGroup
                "No spec"
                [ testCase "genSym" $
                    (genSym () "a" :: UnionM (SymBool, SymBool))
                      @?= mrgPure (isymBool "a" 0, isymBool "a" 1),
                  testCase "genSymSimple" $
                    (genSymSimple () "a" :: (SymBool, SymBool))
                      @?= (isymBool "a" 0, isymBool "a" 1)
                ]
            ],
          testGroup
            "(,,)"
            [ testGroup
                "Some spec"
                [ testCase "genSym" $
                    ( genSym
                        ( EnumGenUpperBound @Integer 2,
                          EnumGenUpperBound @Integer 2,
                          EnumGenUpperBound @Integer 2
                        )
                        "a" ::
                        UnionM (Integer, Integer, Integer)
                    )
                      @?= do
                        x1 <- mrgIf (isymBool "a" 0) (mrgPure 0) (mrgPure 1)
                        x2 <- mrgIf (isymBool "a" 1) (mrgPure 0) (mrgPure 1)
                        x3 <- mrgIf (isymBool "a" 2) (mrgPure 0) (mrgPure 1)
                        mrgPure (x1, x2, x3),
                  testCase "genSymSimple" $
                    ( genSymSimple
                        ((), [[ssymBool "b"], [ssymBool "b", ssymBool "c"]], ())
                        "a" ::
                        (SymBool, [[SymBool]], SymBool)
                    )
                      @?= ( isymBool "a" 0,
                            [ [isymBool "a" 1],
                              [isymBool "a" 2, isymBool "a" 3]
                            ],
                            isymBool "a" 4
                          )
                ],
              testGroup
                "No spec"
                [ testCase "genSym" $
                    (genSym () "a" :: UnionM (SymBool, SymBool, SymBool))
                      @?= mrgPure
                        (isymBool "a" 0, isymBool "a" 1, isymBool "a" 2),
                  testCase "genSymSimple" $
                    (genSymSimple () "a" :: (SymBool, SymBool, SymBool))
                      @?= (isymBool "a" 0, isymBool "a" 1, isymBool "a" 2)
                ]
            ],
          testGroup
            "(,,,)"
            [ testGroup
                "Some spec"
                [ testCase "genSym" $
                    ( genSym
                        ( EnumGenUpperBound @Integer 2,
                          EnumGenUpperBound @Integer 2,
                          EnumGenUpperBound @Integer 2,
                          EnumGenUpperBound @Integer 2
                        )
                        "a" ::
                        UnionM (Integer, Integer, Integer, Integer)
                    )
                      @?= do
                        x1 <- mrgIf (isymBool "a" 0) (mrgPure 0) (mrgPure 1)
                        x2 <- mrgIf (isymBool "a" 1) (mrgPure 0) (mrgPure 1)
                        x3 <- mrgIf (isymBool "a" 2) (mrgPure 0) (mrgPure 1)
                        x4 <- mrgIf (isymBool "a" 3) (mrgPure 0) (mrgPure 1)
                        mrgPure (x1, x2, x3, x4),
                  testCase "genSymSimple" $
                    ( genSymSimple
                        ( (),
                          [[ssymBool "b"], [ssymBool "b", ssymBool "c"]],
                          (),
                          ()
                        )
                        "a" ::
                        (SymBool, [[SymBool]], SymBool, SymBool)
                    )
                      @?= ( isymBool "a" 0,
                            [ [isymBool "a" 1],
                              [isymBool "a" 2, isymBool "a" 3]
                            ],
                            isymBool "a" 4,
                            isymBool "a" 5
                          )
                ],
              testGroup
                "No spec"
                [ testCase "genSym" $
                    ( genSym () "a" ::
                        UnionM (SymBool, SymBool, SymBool, SymBool)
                    )
                      @?= mrgPure
                        ( isymBool "a" 0,
                          isymBool "a" 1,
                          isymBool "a" 2,
                          isymBool "a" 3
                        ),
                  testCase "genSymSimple" $
                    ( genSymSimple () "a" ::
                        (SymBool, SymBool, SymBool, SymBool)
                    )
                      @?= ( isymBool "a" 0,
                            isymBool "a" 1,
                            isymBool "a" 2,
                            isymBool "a" 3
                          )
                ]
            ],
          testGroup
            "(,,,,)"
            [ testGroup
                "Some spec"
                [ testCase "genSym" $
                    ( genSym
                        ( EnumGenUpperBound @Integer 2,
                          EnumGenUpperBound @Integer 2,
                          EnumGenUpperBound @Integer 2,
                          EnumGenUpperBound @Integer 2,
                          EnumGenUpperBound @Integer 2
                        )
                        "a" ::
                        UnionM (Integer, Integer, Integer, Integer, Integer)
                    )
                      @?= do
                        x1 <- mrgIf (isymBool "a" 0) (mrgPure 0) (mrgPure 1)
                        x2 <- mrgIf (isymBool "a" 1) (mrgPure 0) (mrgPure 1)
                        x3 <- mrgIf (isymBool "a" 2) (mrgPure 0) (mrgPure 1)
                        x4 <- mrgIf (isymBool "a" 3) (mrgPure 0) (mrgPure 1)
                        x5 <- mrgIf (isymBool "a" 4) (mrgPure 0) (mrgPure 1)
                        mrgPure (x1, x2, x3, x4, x5),
                  testCase "genSymSimple" $
                    ( genSymSimple
                        ( (),
                          [ [ssymBool "b"],
                            [ssymBool "b", ssymBool "c"]
                          ],
                          (),
                          (),
                          ()
                        )
                        "a" ::
                        (SymBool, [[SymBool]], SymBool, SymBool, SymBool)
                    )
                      @?= ( isymBool "a" 0,
                            [ [isymBool "a" 1],
                              [isymBool "a" 2, isymBool "a" 3]
                            ],
                            isymBool "a" 4,
                            isymBool "a" 5,
                            isymBool "a" 6
                          )
                ],
              testGroup
                "No spec"
                [ testCase "genSym" $
                    ( genSym () "a" ::
                        UnionM (SymBool, SymBool, SymBool, SymBool, SymBool)
                    )
                      @?= mrgPure
                        ( isymBool "a" 0,
                          isymBool "a" 1,
                          isymBool "a" 2,
                          isymBool "a" 3,
                          isymBool "a" 4
                        ),
                  testCase "genSymSimple" $
                    ( genSymSimple () "a" ::
                        (SymBool, SymBool, SymBool, SymBool, SymBool)
                    )
                      @?= ( isymBool "a" 0,
                            isymBool "a" 1,
                            isymBool "a" 2,
                            isymBool "a" 3,
                            isymBool "a" 4
                          )
                ]
            ],
          testGroup
            "(,,,,,)"
            [ testGroup
                "Some spec"
                [ testCase "genSym" $
                    ( genSym
                        ( EnumGenUpperBound @Integer 2,
                          EnumGenUpperBound @Integer 2,
                          EnumGenUpperBound @Integer 2,
                          EnumGenUpperBound @Integer 2,
                          EnumGenUpperBound @Integer 2,
                          EnumGenUpperBound @Integer 2
                        )
                        "a" ::
                        UnionM
                          ( Integer,
                            Integer,
                            Integer,
                            Integer,
                            Integer,
                            Integer
                          )
                    )
                      @?= do
                        x1 <- mrgIf (isymBool "a" 0) (mrgPure 0) (mrgPure 1)
                        x2 <- mrgIf (isymBool "a" 1) (mrgPure 0) (mrgPure 1)
                        x3 <- mrgIf (isymBool "a" 2) (mrgPure 0) (mrgPure 1)
                        x4 <- mrgIf (isymBool "a" 3) (mrgPure 0) (mrgPure 1)
                        x5 <- mrgIf (isymBool "a" 4) (mrgPure 0) (mrgPure 1)
                        x6 <- mrgIf (isymBool "a" 5) (mrgPure 0) (mrgPure 1)
                        mrgPure (x1, x2, x3, x4, x5, x6),
                  testCase "genSymSimple" $
                    ( genSymSimple
                        ( (),
                          [ [ssymBool "b"],
                            [ssymBool "b", ssymBool "c"]
                          ],
                          (),
                          (),
                          (),
                          ()
                        )
                        "a" ::
                        ( SymBool,
                          [[SymBool]],
                          SymBool,
                          SymBool,
                          SymBool,
                          SymBool
                        )
                    )
                      @?= ( isymBool "a" 0,
                            [ [isymBool "a" 1],
                              [isymBool "a" 2, isymBool "a" 3]
                            ],
                            isymBool "a" 4,
                            isymBool "a" 5,
                            isymBool "a" 6,
                            isymBool "a" 7
                          )
                ],
              testGroup
                "No spec"
                [ testCase "genSym" $
                    ( genSym () "a" ::
                        UnionM
                          ( SymBool,
                            SymBool,
                            SymBool,
                            SymBool,
                            SymBool,
                            SymBool
                          )
                    )
                      @?= mrgPure
                        ( isymBool "a" 0,
                          isymBool "a" 1,
                          isymBool "a" 2,
                          isymBool "a" 3,
                          isymBool "a" 4,
                          isymBool "a" 5
                        ),
                  testCase "genSymSimple" $
                    ( genSymSimple () "a" ::
                        ( SymBool,
                          SymBool,
                          SymBool,
                          SymBool,
                          SymBool,
                          SymBool
                        )
                    )
                      @?= ( isymBool "a" 0,
                            isymBool "a" 1,
                            isymBool "a" 2,
                            isymBool "a" 3,
                            isymBool "a" 4,
                            isymBool "a" 5
                          )
                ]
            ],
          testGroup
            "(,,,,,,)"
            [ testGroup
                "Some spec"
                [ testCase "genSym" $
                    ( genSym
                        ( EnumGenUpperBound @Integer 2,
                          EnumGenUpperBound @Integer 2,
                          EnumGenUpperBound @Integer 2,
                          EnumGenUpperBound @Integer 2,
                          EnumGenUpperBound @Integer 2,
                          EnumGenUpperBound @Integer 2,
                          EnumGenUpperBound @Integer 2
                        )
                        "a" ::
                        UnionM
                          ( Integer,
                            Integer,
                            Integer,
                            Integer,
                            Integer,
                            Integer,
                            Integer
                          )
                    )
                      @?= do
                        x1 <- mrgIf (isymBool "a" 0) (mrgPure 0) (mrgPure 1)
                        x2 <- mrgIf (isymBool "a" 1) (mrgPure 0) (mrgPure 1)
                        x3 <- mrgIf (isymBool "a" 2) (mrgPure 0) (mrgPure 1)
                        x4 <- mrgIf (isymBool "a" 3) (mrgPure 0) (mrgPure 1)
                        x5 <- mrgIf (isymBool "a" 4) (mrgPure 0) (mrgPure 1)
                        x6 <- mrgIf (isymBool "a" 5) (mrgPure 0) (mrgPure 1)
                        x7 <- mrgIf (isymBool "a" 6) (mrgPure 0) (mrgPure 1)
                        mrgPure (x1, x2, x3, x4, x5, x6, x7),
                  testCase "genSymSimple" $
                    ( genSymSimple
                        ( (),
                          [ [ssymBool "b"],
                            [ssymBool "b", ssymBool "c"]
                          ],
                          (),
                          (),
                          (),
                          (),
                          ()
                        )
                        "a" ::
                        ( SymBool,
                          [[SymBool]],
                          SymBool,
                          SymBool,
                          SymBool,
                          SymBool,
                          SymBool
                        )
                    )
                      @?= ( isymBool "a" 0,
                            [ [isymBool "a" 1],
                              [isymBool "a" 2, isymBool "a" 3]
                            ],
                            isymBool "a" 4,
                            isymBool "a" 5,
                            isymBool "a" 6,
                            isymBool "a" 7,
                            isymBool "a" 8
                          )
                ],
              testGroup
                "No spec"
                [ testCase "genSym" $
                    ( genSym () "a" ::
                        UnionM
                          ( SymBool,
                            SymBool,
                            SymBool,
                            SymBool,
                            SymBool,
                            SymBool,
                            SymBool
                          )
                    )
                      @?= mrgPure
                        ( isymBool "a" 0,
                          isymBool "a" 1,
                          isymBool "a" 2,
                          isymBool "a" 3,
                          isymBool "a" 4,
                          isymBool "a" 5,
                          isymBool "a" 6
                        ),
                  testCase "genSymSimple" $
                    ( genSymSimple () "a" ::
                        ( SymBool,
                          SymBool,
                          SymBool,
                          SymBool,
                          SymBool,
                          SymBool,
                          SymBool
                        )
                    )
                      @?= ( isymBool "a" 0,
                            isymBool "a" 1,
                            isymBool "a" 2,
                            isymBool "a" 3,
                            isymBool "a" 4,
                            isymBool "a" 5,
                            isymBool "a" 6
                          )
                ]
            ],
          testGroup
            "(,,,,,,,)"
            [ testGroup
                "Some spec"
                [ testCase "genSym" $
                    ( genSym
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
                        UnionM
                          ( Integer,
                            Integer,
                            Integer,
                            Integer,
                            Integer,
                            Integer,
                            Integer,
                            Integer
                          )
                    )
                      @?= do
                        x1 <- mrgIf (isymBool "a" 0) (mrgPure 0) (mrgPure 1)
                        x2 <- mrgIf (isymBool "a" 1) (mrgPure 0) (mrgPure 1)
                        x3 <- mrgIf (isymBool "a" 2) (mrgPure 0) (mrgPure 1)
                        x4 <- mrgIf (isymBool "a" 3) (mrgPure 0) (mrgPure 1)
                        x5 <- mrgIf (isymBool "a" 4) (mrgPure 0) (mrgPure 1)
                        x6 <- mrgIf (isymBool "a" 5) (mrgPure 0) (mrgPure 1)
                        x7 <- mrgIf (isymBool "a" 6) (mrgPure 0) (mrgPure 1)
                        x8 <- mrgIf (isymBool "a" 7) (mrgPure 0) (mrgPure 1)
                        mrgPure (x1, x2, x3, x4, x5, x6, x7, x8),
                  testCase "genSymSimple" $
                    ( genSymSimple
                        ( (),
                          [ [ssymBool "b"],
                            [ssymBool "b", ssymBool "c"]
                          ],
                          (),
                          (),
                          (),
                          (),
                          (),
                          ()
                        )
                        "a" ::
                        ( SymBool,
                          [[SymBool]],
                          SymBool,
                          SymBool,
                          SymBool,
                          SymBool,
                          SymBool,
                          SymBool
                        )
                    )
                      @?= ( isymBool "a" 0,
                            [ [isymBool "a" 1],
                              [isymBool "a" 2, isymBool "a" 3]
                            ],
                            isymBool "a" 4,
                            isymBool "a" 5,
                            isymBool "a" 6,
                            isymBool "a" 7,
                            isymBool "a" 8,
                            isymBool "a" 9
                          )
                ],
              testGroup
                "No spec"
                [ testCase "genSym" $
                    ( genSym () "a" ::
                        UnionM
                          ( SymBool,
                            SymBool,
                            SymBool,
                            SymBool,
                            SymBool,
                            SymBool,
                            SymBool,
                            SymBool
                          )
                    )
                      @?= mrgPure
                        ( isymBool "a" 0,
                          isymBool "a" 1,
                          isymBool "a" 2,
                          isymBool "a" 3,
                          isymBool "a" 4,
                          isymBool "a" 5,
                          isymBool "a" 6,
                          isymBool "a" 7
                        ),
                  testCase "genSymSimple" $
                    ( genSymSimple () "a" ::
                        ( SymBool,
                          SymBool,
                          SymBool,
                          SymBool,
                          SymBool,
                          SymBool,
                          SymBool,
                          SymBool
                        )
                    )
                      @?= ( isymBool "a" 0,
                            isymBool "a" 1,
                            isymBool "a" 2,
                            isymBool "a" 3,
                            isymBool "a" 4,
                            isymBool "a" 5,
                            isymBool "a" 6,
                            isymBool "a" 7
                          )
                ]
            ],
          testGroup
            "MaybeT Maybe SymBool"
            [ testGroup
                "Same shape spec"
                [ testGroup
                    "MaybeT Nothing"
                    [ testCase "genSym" $
                        ( genSym (MaybeT Nothing :: MaybeT Maybe SymBool) "a" ::
                            UnionM (MaybeT Maybe SymBool)
                        )
                          @?= mrgPure (MaybeT Nothing),
                      testCase "genSymSimple" $
                        ( genSymSimple
                            (MaybeT Nothing :: MaybeT Maybe SymBool)
                            "a" ::
                            MaybeT Maybe SymBool
                        )
                          @?= MaybeT Nothing
                    ],
                  testGroup
                    "MaybeT (Just Nothing)"
                    [ testCase "genSym" $
                        ( genSym
                            ( MaybeT (Just Nothing) ::
                                MaybeT Maybe SymBool
                            )
                            "a" ::
                            UnionM (MaybeT Maybe SymBool)
                        )
                          @?= mrgPure (MaybeT (Just Nothing)),
                      testCase "genSymSimple" $
                        ( genSymSimple
                            ( MaybeT (Just (Just $ ssymBool "a")) ::
                                MaybeT Maybe SymBool
                            )
                            "a" ::
                            MaybeT Maybe SymBool
                        )
                          @?= MaybeT (Just (Just $ isymBool "a" 0))
                    ],
                  testGroup
                    "MaybeT (Just (Just v))"
                    [ testCase "genSym" $
                        ( genSym
                            ( MaybeT (Just (Just $ ssymBool "a")) ::
                                MaybeT Maybe SymBool
                            )
                            "a" ::
                            UnionM (MaybeT Maybe SymBool)
                        )
                          @?= mrgPure (MaybeT (Just (Just $ isymBool "a" 0))),
                      testCase "genSymSimple" $
                        ( genSymSimple
                            ( MaybeT (Just (Just $ ssymBool "a")) ::
                                MaybeT Maybe SymBool
                            )
                            "a" ::
                            MaybeT Maybe SymBool
                        )
                          @?= MaybeT (Just (Just $ isymBool "a" 0))
                    ]
                ],
              testCase "No spec" $
                (genSym () "a" :: UnionM (MaybeT Maybe SymBool))
                  @?= mrgIf
                    (isymBool "a" 0)
                    (mrgPure $ MaybeT Nothing)
                    ( mrgIf
                        (isymBool "a" 1)
                        (mrgPure $ MaybeT $ Just Nothing)
                        (mrgPure $ MaybeT $ Just $ Just $ isymBool "a" 2)
                    ),
              testGroup
                "Maybe (Maybe SymBool) spec"
                [ testGroup
                    "Nothing"
                    [ testCase "genSym" $
                        ( genSym (Nothing :: Maybe (Maybe SymBool)) "a" ::
                            UnionM (MaybeT Maybe SymBool)
                        )
                          @?= mrgPure (MaybeT Nothing),
                      testCase "genSymSimple" $
                        ( genSymSimple (Nothing :: Maybe (Maybe SymBool)) "a" ::
                            MaybeT Maybe SymBool
                        )
                          @?= MaybeT Nothing
                    ],
                  testGroup
                    "Just Nothing"
                    [ testCase "genSym" $
                        ( genSym (Just Nothing :: Maybe (Maybe SymBool)) "a" ::
                            UnionM (MaybeT Maybe SymBool)
                        )
                          @?= mrgPure (MaybeT (Just Nothing)),
                      testCase "genSymSimple" $
                        ( genSymSimple
                            (Just Nothing :: Maybe (Maybe SymBool))
                            "a" ::
                            MaybeT Maybe SymBool
                        )
                          @?= MaybeT (Just Nothing)
                    ],
                  testGroup
                    "Just (Just v)"
                    [ testCase "genSym" $
                        ( genSym
                            ( Just $ Just $ ssymBool "a" ::
                                Maybe (Maybe SymBool)
                            )
                            "a" ::
                            UnionM (MaybeT Maybe SymBool)
                        )
                          @?= mrgPure (MaybeT (Just (Just $ isymBool "a" 0))),
                      testCase "genSymSimple" $
                        ( genSymSimple
                            ( Just $ Just $ ssymBool "a" ::
                                Maybe (Maybe SymBool)
                            )
                            "a" ::
                            MaybeT Maybe SymBool
                        )
                          @?= MaybeT (Just (Just $ isymBool "a" 0))
                    ]
                ]
            ],
          testGroup
            "ExceptT SymBool Maybe SymBool"
            [ testGroup
                "Same shape spec"
                [ testGroup
                    "ExceptT Nothing"
                    [ testCase "genSym" $
                        ( genSym
                            ( ExceptT Nothing ::
                                ExceptT SymBool Maybe SymBool
                            )
                            "a" ::
                            UnionM (ExceptT SymBool Maybe SymBool)
                        )
                          @?= mrgPure (ExceptT Nothing),
                      testCase "genSymSimple" $
                        ( genSymSimple
                            ( ExceptT Nothing ::
                                ExceptT SymBool Maybe SymBool
                            )
                            "a" ::
                            ExceptT SymBool Maybe SymBool
                        )
                          @?= ExceptT Nothing
                    ],
                  testGroup
                    "ExceptT (Just (Left v))"
                    [ testCase "genSym" $
                        ( genSym
                            ( ExceptT $ Just $ Left $ ssymBool "a" ::
                                ExceptT SymBool Maybe SymBool
                            )
                            "a" ::
                            UnionM (ExceptT SymBool Maybe SymBool)
                        )
                          @?= mrgPure
                            (ExceptT $ Just $ Left $ isymBool "a" 0),
                      testCase "genSymSimple" $
                        ( genSymSimple
                            ( ExceptT $ Just $ Left $ ssymBool "a" ::
                                ExceptT SymBool Maybe SymBool
                            )
                            "a" ::
                            ExceptT SymBool Maybe SymBool
                        )
                          @?= ExceptT (Just $ Left $ isymBool "a" 0)
                    ],
                  testGroup
                    "ExceptT (Just (Right v))"
                    [ testCase "genSym" $
                        ( genSym
                            ( ExceptT $ Just $ Right $ ssymBool "a" ::
                                ExceptT SymBool Maybe SymBool
                            )
                            "a" ::
                            UnionM (ExceptT SymBool Maybe SymBool)
                        )
                          @?= mrgPure
                            (ExceptT $ Just $ Right $ isymBool "a" 0),
                      testCase "genSymSimple" $
                        ( genSymSimple
                            ( ExceptT $ Just $ Right $ ssymBool "a" ::
                                ExceptT SymBool Maybe SymBool
                            )
                            "a" ::
                            ExceptT SymBool Maybe SymBool
                        )
                          @?= ExceptT (Just $ Right $ isymBool "a" 0)
                    ]
                ],
              testCase "() spec" $ do
                (genSym () "a" :: UnionM (ExceptT SymBool Maybe SymBool))
                  @?= mrgIf
                    (isymBool "a" 0)
                    (mrgPure $ ExceptT Nothing)
                    ( mrgIf
                        (isymBool "a" 1)
                        (mrgPure $ ExceptT $ Just $ Left $ isymBool "a" 2)
                        (mrgPure $ ExceptT $ Just $ Right $ isymBool "a" 3)
                    ),
              testGroup
                "Maybe (Either SymBool SymBool) spec"
                [ testGroup
                    "Nothing"
                    [ testCase "genSym" $
                        ( genSym
                            (Nothing :: Maybe (Either SymBool SymBool))
                            "a" ::
                            UnionM (ExceptT SymBool Maybe SymBool)
                        )
                          @?= mrgPure (ExceptT Nothing),
                      testCase "genSymSimple" $
                        ( genSymSimple
                            (Nothing :: Maybe (Either SymBool SymBool))
                            "a" ::
                            ExceptT SymBool Maybe SymBool
                        )
                          @?= ExceptT Nothing
                    ],
                  testGroup
                    "Just (left v)"
                    [ testCase "genSym" $
                        ( genSym
                            ( Just $ Left $ ssymBool "a" ::
                                Maybe (Either SymBool SymBool)
                            )
                            "a" ::
                            UnionM (ExceptT SymBool Maybe SymBool)
                        )
                          @?= mrgPure
                            (ExceptT (Just (Left $ isymBool "a" 0))),
                      testCase "genSymSimple" $
                        ( genSymSimple
                            ( Just $ Left $ ssymBool "a" ::
                                Maybe (Either SymBool SymBool)
                            )
                            "a" ::
                            ExceptT SymBool Maybe SymBool
                        )
                          @?= ExceptT (Just (Left $ isymBool "a" 0))
                    ],
                  testGroup
                    "Just (left v)"
                    [ testCase "genSym" $
                        ( genSym
                            ( Just $ Right $ ssymBool "a" ::
                                Maybe (Either SymBool SymBool)
                            )
                            "a" ::
                            UnionM (ExceptT SymBool Maybe SymBool)
                        )
                          @?= mrgPure
                            (ExceptT (Just (Right $ isymBool "a" 0))),
                      testCase "genSymSimple" $
                        ( genSymSimple
                            ( Just $ Right $ ssymBool "a" ::
                                Maybe (Either SymBool SymBool)
                            )
                            "a" ::
                            ExceptT SymBool Maybe SymBool
                        )
                          @?= ExceptT (Just (Right $ isymBool "a" 0))
                    ]
                ]
            ]
        ],
      testGroup
        "choose*"
        [ testCase "chooseFresh" $ do
            (runFresh (chooseFresh [1, 2, 3]) "a" :: UnionM Int)
              @?= mrgIf
                (isymBool "a" 0)
                (mrgPure 1)
                (mrgIf (isymBool "a" 1) (mrgPure 2) (mrgPure 3)),
          testCase "choose" $ do
            (choose [1, 2, 3] "a" :: UnionM Int)
              @?= mrgIf
                (isymBool "a" 0)
                (mrgPure 1)
                (mrgIf (isymBool "a" 1) (mrgPure 2) (mrgPure 3)),
          testCase "chooseSimpleFresh" $ do
            (runFresh (chooseSimpleFresh ["x", "y", "z"]) "a" :: SymBool)
              @?= symIte
                (isymBool "a" 0)
                (ssymBool "x")
                (symIte (isymBool "a" 1) (ssymBool "y") (ssymBool "z")),
          testCase "chooseSimple" $ do
            (chooseSimple ["x", "y", "z"] "a" :: SymBool)
              @?= symIte
                (isymBool "a" 0)
                (ssymBool "x")
                (symIte (isymBool "a" 1) (ssymBool "y") (ssymBool "z")),
          testCase "chooseUnionFresh" $ do
            ( runFresh
                ( chooseUnionFresh
                    [ mrgIf (ssymBool "x") 1 2,
                      mrgIf (ssymBool "x") 2 3,
                      mrgIf (ssymBool "x") 3 4
                    ]
                )
                "a" ::
                UnionM Int
              )
              @?= mrgIf
                (isymBool "a" 0)
                (mrgIf (ssymBool "x") 1 2)
                ( mrgIf
                    (isymBool "a" 1)
                    (mrgIf (ssymBool "x") 2 3)
                    (mrgIf (ssymBool "x") 3 4)
                ),
          testCase "chooseUnion" $ do
            ( chooseUnion
                [ mrgIf (ssymBool "x") 1 2,
                  mrgIf (ssymBool "x") 2 3,
                  mrgIf (ssymBool "x") 3 4
                ]
                "a" ::
                UnionM Int
              )
              @?= mrgIf
                (isymBool "a" 0)
                (mrgIf (ssymBool "x") 1 2)
                ( mrgIf
                    (isymBool "a" 1)
                    (mrgIf (ssymBool "x") 2 3)
                    (mrgIf (ssymBool "x") 3 4)
                ),
          testCase "liftFresh" $ do
            let orig = simpleFresh () :: Fresh (SymBool, SymBool)
            let actual = flip runFreshT "a" $ do
                  r1 <- liftFresh orig
                  r2 <- liftFresh orig
                  return (r1, r2) ::
                    FreshT UnionM ((SymBool, SymBool), (SymBool, SymBool))
            let expected =
                  return
                    ( (isymBool "a" 0, isymBool "a" 1),
                      (isymBool "a" 2, isymBool "a" 3)
                    )
            actual @?= expected
        ]
    ]
