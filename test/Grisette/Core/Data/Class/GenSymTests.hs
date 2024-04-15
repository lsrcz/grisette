{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Grisette.Core.Data.Class.GenSymTests (genSymTests) where

import Control.Monad (replicateM)
import Control.Monad.Except (ExceptT (ExceptT))
import Control.Monad.Trans.Maybe (MaybeT (MaybeT))
import qualified Data.Text as T
import Grisette.Core.Control.Monad.UnionM (UnionM)
import Grisette.Core.Data.Class.GenSym
  ( EnumGenBound (EnumGenBound),
    EnumGenUpperBound (EnumGenUpperBound),
    Fresh,
    FreshT,
    GenSymSimple (simpleFresh),
    ListSpec (ListSpec),
    MonadFresh (localIdentifier),
    SimpleListSpec (SimpleListSpec),
    choose,
    chooseFresh,
    chooseSimple,
    chooseSimpleFresh,
    chooseUnion,
    chooseUnionFresh,
    freshString,
    genSym,
    genSymSimple,
    liftFresh,
    runFresh,
    runFreshT,
  )
import Grisette.Core.Data.Class.ITEOp (ITEOp (symIte))
import Grisette.Core.Data.Class.SimpleMergeable (mrgIf)
import Grisette.Core.Data.Class.TestValues (conBool, isymBool, ssymBool)
import Grisette.Core.Data.Class.TryMerge (mrgSingle)
import Grisette.Core.Data.Symbol (withInfo)
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
                      @?= mrgSingle (isymBool "a" 0),
                  testCase "genSymSimple" $
                    (genSymSimple () "a" :: SymBool)
                      @?= isymBool "a" 0
                ],
              testGroup
                "SymBool spec"
                [ testCase "genSym" $
                    (genSym (conBool True) "a" :: UnionM SymBool)
                      @?= mrgSingle (isymBool "a" 0),
                  testCase "genSymSimple" $
                    (genSymSimple (conBool True) "a" :: SymBool)
                      @?= isymBool "a" 0
                ]
            ],
          testGroup
            "Bool"
            [ testCase "() spec" $
                (genSym () "a" :: UnionM Bool)
                  @?= mrgIf (isymBool "a" 0) (mrgSingle False) (mrgSingle True),
              testGroup
                "Bool spec"
                [ testGroup
                    "genSym"
                    [ testCase "True" $
                        (genSym True "a" :: UnionM Bool)
                          @?= mrgSingle True,
                      testCase "False" $
                        (genSym False "a" :: UnionM Bool)
                          @?= mrgSingle False
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
                      @?= mrgSingle 1,
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
                    (mrgSingle 0)
                    (mrgIf (isymBool "a" 1) (mrgSingle 1) (mrgSingle 2)),
              testCase "Bound spec" $
                (genSym (EnumGenBound (-1 :: Integer) 2) "a" :: UnionM Integer)
                  @?= mrgIf
                    (isymBool "a" 0)
                    (mrgSingle (-1))
                    (mrgIf (isymBool "a" 1) (mrgSingle 0) (mrgSingle 1))
            ],
          testGroup
            "Char"
            [ testGroup
                "Char spec"
                [ testCase "genSym" $
                    (genSym 'x' "a" :: UnionM Char)
                      @?= mrgSingle 'x',
                  testCase "genSymSimple" $
                    (genSymSimple 'x' "a" :: Char) @?= 'x'
                ],
              testCase "Upper bound spec" $
                (genSym (EnumGenUpperBound @Char (toEnum 3)) "a" :: UnionM Char)
                  @?= mrgIf
                    (isymBool "a" 0)
                    (mrgSingle $ toEnum 0)
                    ( mrgIf
                        (isymBool "a" 1)
                        (mrgSingle $ toEnum 1)
                        (mrgSingle $ toEnum 2)
                    ),
              testCase "Bound spec" $
                (genSym (EnumGenBound 'a' 'd') "a" :: UnionM Char)
                  @?= mrgIf
                    (isymBool "a" 0)
                    (mrgSingle 'a')
                    (mrgIf (isymBool "a" 1) (mrgSingle 'b') (mrgSingle 'c'))
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
                          @?= mrgSingle Nothing,
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
                          @?= mrgSingle (Just (isymBool "a" 0)),
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
                    (mrgSingle Nothing)
                    (mrgSingle (Just (isymBool "a" 1)))
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
                          @?= mrgSingle (Left (isymBool "a" 0)),
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
                          @?= mrgSingle (Right (isymBool "a" 0)),
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
                    (mrgSingle $ Left $ isymBool "a" 1)
                    (mrgSingle $ Right $ isymBool "a" 2)
            ],
          testGroup
            "lists"
            [ testGroup
                "Max length spec"
                [ testCase "max length = 0" $
                    (genSym (0 :: Integer) "a" :: UnionM [SymBool])
                      @?= mrgSingle [],
                  testCase "max length = 3" $
                    (genSym (3 :: Integer) "a" :: UnionM [SymBool])
                      @?= mrgIf
                        (isymBool "a" 3)
                        (mrgSingle [])
                        ( mrgIf
                            (isymBool "a" 4)
                            (mrgSingle [isymBool "a" 2])
                            ( mrgIf
                                (isymBool "a" 5)
                                (mrgSingle [isymBool "a" 1, isymBool "a" 2])
                                ( mrgSingle
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
                        (mrgSingle [isymBool "a" 2])
                        ( mrgIf
                            (isymBool "a" 4)
                            (mrgSingle [isymBool "a" 1, isymBool "a" 2])
                            ( mrgSingle
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
                        ( mrgSingle
                            [ mrgIf
                                (isymBool "a" 5)
                                (mrgSingle [isymBool "a" 4])
                                (mrgSingle [isymBool "a" 3, isymBool "a" 4])
                            ]
                        )
                        ( mrgSingle
                            [ mrgIf
                                (isymBool "a" 2)
                                (mrgSingle [isymBool "a" 1])
                                (mrgSingle [isymBool "a" 0, isymBool "a" 1]),
                              mrgIf
                                (isymBool "a" 5)
                                (mrgSingle [isymBool "a" 4])
                                (mrgSingle [isymBool "a" 3, isymBool "a" 4])
                            ]
                        )
                ],
              testGroup
                "Exact length spec"
                [ testGroup
                    "length = 2"
                    [ testCase "genSym" $
                        (genSym (SimpleListSpec 2 ()) "a" :: UnionM [SymBool])
                          @?= mrgSingle [isymBool "a" 0, isymBool "a" 1],
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
                          @?= mrgSingle
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
                      @?= mrgSingle
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
                (genSym () "a" :: UnionM ()) @?= mrgSingle ()
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
                        x1 <- mrgIf (isymBool "a" 0) (mrgSingle 0) (mrgSingle 1)
                        x2 <- mrgIf (isymBool "a" 1) (mrgSingle 0) (mrgSingle 1)
                        mrgSingle (x1, x2),
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
                      @?= mrgSingle (isymBool "a" 0, isymBool "a" 1),
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
                        x1 <- mrgIf (isymBool "a" 0) (mrgSingle 0) (mrgSingle 1)
                        x2 <- mrgIf (isymBool "a" 1) (mrgSingle 0) (mrgSingle 1)
                        x3 <- mrgIf (isymBool "a" 2) (mrgSingle 0) (mrgSingle 1)
                        mrgSingle (x1, x2, x3),
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
                      @?= mrgSingle
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
                        x1 <- mrgIf (isymBool "a" 0) (mrgSingle 0) (mrgSingle 1)
                        x2 <- mrgIf (isymBool "a" 1) (mrgSingle 0) (mrgSingle 1)
                        x3 <- mrgIf (isymBool "a" 2) (mrgSingle 0) (mrgSingle 1)
                        x4 <- mrgIf (isymBool "a" 3) (mrgSingle 0) (mrgSingle 1)
                        mrgSingle (x1, x2, x3, x4),
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
                      @?= mrgSingle
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
                        x1 <- mrgIf (isymBool "a" 0) (mrgSingle 0) (mrgSingle 1)
                        x2 <- mrgIf (isymBool "a" 1) (mrgSingle 0) (mrgSingle 1)
                        x3 <- mrgIf (isymBool "a" 2) (mrgSingle 0) (mrgSingle 1)
                        x4 <- mrgIf (isymBool "a" 3) (mrgSingle 0) (mrgSingle 1)
                        x5 <- mrgIf (isymBool "a" 4) (mrgSingle 0) (mrgSingle 1)
                        mrgSingle (x1, x2, x3, x4, x5),
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
                      @?= mrgSingle
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
                        x1 <- mrgIf (isymBool "a" 0) (mrgSingle 0) (mrgSingle 1)
                        x2 <- mrgIf (isymBool "a" 1) (mrgSingle 0) (mrgSingle 1)
                        x3 <- mrgIf (isymBool "a" 2) (mrgSingle 0) (mrgSingle 1)
                        x4 <- mrgIf (isymBool "a" 3) (mrgSingle 0) (mrgSingle 1)
                        x5 <- mrgIf (isymBool "a" 4) (mrgSingle 0) (mrgSingle 1)
                        x6 <- mrgIf (isymBool "a" 5) (mrgSingle 0) (mrgSingle 1)
                        mrgSingle (x1, x2, x3, x4, x5, x6),
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
                      @?= mrgSingle
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
                        x1 <- mrgIf (isymBool "a" 0) (mrgSingle 0) (mrgSingle 1)
                        x2 <- mrgIf (isymBool "a" 1) (mrgSingle 0) (mrgSingle 1)
                        x3 <- mrgIf (isymBool "a" 2) (mrgSingle 0) (mrgSingle 1)
                        x4 <- mrgIf (isymBool "a" 3) (mrgSingle 0) (mrgSingle 1)
                        x5 <- mrgIf (isymBool "a" 4) (mrgSingle 0) (mrgSingle 1)
                        x6 <- mrgIf (isymBool "a" 5) (mrgSingle 0) (mrgSingle 1)
                        x7 <- mrgIf (isymBool "a" 6) (mrgSingle 0) (mrgSingle 1)
                        mrgSingle (x1, x2, x3, x4, x5, x6, x7),
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
                      @?= mrgSingle
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
                        x1 <- mrgIf (isymBool "a" 0) (mrgSingle 0) (mrgSingle 1)
                        x2 <- mrgIf (isymBool "a" 1) (mrgSingle 0) (mrgSingle 1)
                        x3 <- mrgIf (isymBool "a" 2) (mrgSingle 0) (mrgSingle 1)
                        x4 <- mrgIf (isymBool "a" 3) (mrgSingle 0) (mrgSingle 1)
                        x5 <- mrgIf (isymBool "a" 4) (mrgSingle 0) (mrgSingle 1)
                        x6 <- mrgIf (isymBool "a" 5) (mrgSingle 0) (mrgSingle 1)
                        x7 <- mrgIf (isymBool "a" 6) (mrgSingle 0) (mrgSingle 1)
                        x8 <- mrgIf (isymBool "a" 7) (mrgSingle 0) (mrgSingle 1)
                        mrgSingle (x1, x2, x3, x4, x5, x6, x7, x8),
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
                      @?= mrgSingle
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
                          @?= mrgSingle (MaybeT Nothing),
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
                          @?= mrgSingle (MaybeT (Just Nothing)),
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
                          @?= mrgSingle (MaybeT (Just (Just $ isymBool "a" 0))),
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
                    (mrgSingle $ MaybeT Nothing)
                    ( mrgIf
                        (isymBool "a" 1)
                        (mrgSingle $ MaybeT $ Just Nothing)
                        (mrgSingle $ MaybeT $ Just $ Just $ isymBool "a" 2)
                    ),
              testGroup
                "Maybe (Maybe SymBool) spec"
                [ testGroup
                    "Nothing"
                    [ testCase "genSym" $
                        ( genSym (Nothing :: Maybe (Maybe SymBool)) "a" ::
                            UnionM (MaybeT Maybe SymBool)
                        )
                          @?= mrgSingle (MaybeT Nothing),
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
                          @?= mrgSingle (MaybeT (Just Nothing)),
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
                          @?= mrgSingle (MaybeT (Just (Just $ isymBool "a" 0))),
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
                          @?= mrgSingle (ExceptT Nothing),
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
                          @?= mrgSingle
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
                          @?= mrgSingle
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
                    (mrgSingle $ ExceptT Nothing)
                    ( mrgIf
                        (isymBool "a" 1)
                        (mrgSingle $ ExceptT $ Just $ Left $ isymBool "a" 2)
                        (mrgSingle $ ExceptT $ Just $ Right $ isymBool "a" 3)
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
                          @?= mrgSingle (ExceptT Nothing),
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
                          @?= mrgSingle
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
                          @?= mrgSingle
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
                (mrgSingle 1)
                (mrgIf (isymBool "a" 1) (mrgSingle 2) (mrgSingle 3)),
          testCase "choose" $ do
            (choose [1, 2, 3] "a" :: UnionM Int)
              @?= mrgIf
                (isymBool "a" 0)
                (mrgSingle 1)
                (mrgIf (isymBool "a" 1) (mrgSingle 2) (mrgSingle 3)),
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
        ],
      testCase "freshString" $ do
        runFresh (replicateM 2 $ freshString "a") "b" @?= ["b@0[a]", "b@1[a]"],
      testCase "localIdentifier" $ do
        let computation = do
              a <- simpleFresh ()
              (b1, b2) <- localIdentifier (`withInfo` ("b" :: T.Text)) $ do
                b1 <- simpleFresh ()
                b2 <- simpleFresh ()
                return (b1, b2)
              c <- simpleFresh ()
              return [a, b1, b2, c :: SymBool]
        let actual = runFresh computation "c"
        actual
          @?= [ isymBool "c" 0,
                isymBool (withInfo "c" ("b" :: T.Text)) 0,
                isymBool (withInfo "c" ("b" :: T.Text)) 1,
                isymBool "c" 1
              ]
    ]
