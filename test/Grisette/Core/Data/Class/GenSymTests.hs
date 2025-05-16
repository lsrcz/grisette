{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Grisette.Core.Data.Class.GenSymTests (genSymTests) where

import Control.Monad (replicateM)
import Control.Monad.Except (ExceptT (ExceptT))
import Control.Monad.Trans.Maybe (MaybeT (MaybeT))
import qualified Data.Text as T
import Grisette
  ( AsKey,
    EnumGenBound (EnumGenBound),
    EnumGenUpperBound (EnumGenUpperBound),
    Fresh,
    FreshT,
    GenSymSimple (simpleFresh),
    ITEOp (symIte),
    ListSpec (ListSpec),
    MonadFresh (localIdentifier),
    SExpr (Atom),
    SimpleListSpec (SimpleListSpec),
    Solvable (con, isym, ssym),
    SymBool,
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
    mapMetadata,
    mrgIf,
    mrgSingle,
    runFresh,
    runFreshT,
    withMetadata,
  )
import Grisette.Internal.Core.Control.Monad.Union (Union)
import Grisette.Internal.Core.Data.Class.AsKey (AsKey1 (AsKey1))
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
                    AsKey1 (genSym () "a" :: Union (AsKey SymBool))
                      @?= mrgSingle (isym "a" 0),
                  testCase "genSymSimple" $
                    (genSymSimple () "a" :: AsKey SymBool) @?= isym "a" 0
                ],
              testGroup
                "SymBool spec"
                [ testCase "genSym" $
                    AsKey1 (genSym (con True :: AsKey SymBool) "a" :: Union (AsKey SymBool))
                      @?= mrgSingle (isym "a" 0),
                  testCase "genSymSimple" $
                    (genSymSimple (con True :: AsKey SymBool) "a" :: AsKey SymBool)
                      @?= isym "a" 0
                ]
            ],
          testGroup
            "Bool"
            [ testCase "() spec" $
                AsKey1 (genSym () "a" :: Union Bool)
                  @?= AsKey1 (mrgIf (isym "a" 0) (mrgSingle False) (mrgSingle True)),
              testGroup
                "Bool spec"
                [ testGroup
                    "genSym"
                    [ testCase "True" $
                        AsKey1 (genSym True "a" :: Union Bool)
                          @?= mrgSingle True,
                      testCase "False" $
                        AsKey1 (genSym False "a" :: Union Bool)
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
                    AsKey1 (genSym (1 :: Integer) "a" :: Union Integer)
                      @?= mrgSingle 1,
                  testCase "genSymSimple" $
                    (genSymSimple (1 :: Integer) "a" :: Integer)
                      @?= 1
                ],
              testCase "Upper bound spec" $
                AsKey1 (genSym (EnumGenUpperBound (3 :: Integer)) "a" :: Union Integer)
                  @?= mrgIf
                    (isym "a" 0)
                    (mrgSingle 0)
                    (mrgIf (isym "a" 1) (mrgSingle 1) (mrgSingle 2)),
              testCase "Bound spec" $
                AsKey1 (genSym (EnumGenBound (-1 :: Integer) 2) "a" :: Union Integer)
                  @?= mrgIf
                    (isym "a" 0)
                    (mrgSingle (-1))
                    (mrgIf (isym "a" 1) (mrgSingle 0) (mrgSingle 1))
            ],
          testGroup
            "Char"
            [ testGroup
                "Char spec"
                [ testCase "genSym" $
                    AsKey1 (genSym 'x' "a" :: Union Char)
                      @?= mrgSingle 'x',
                  testCase "genSymSimple" $
                    (genSymSimple 'x' "a" :: Char) @?= 'x'
                ],
              testCase "Upper bound spec" $
                AsKey1 (genSym (EnumGenUpperBound @Char (toEnum 3)) "a" :: Union Char)
                  @?= mrgIf
                    (isym "a" 0)
                    (mrgSingle $ toEnum 0)
                    ( mrgIf
                        (isym "a" 1)
                        (mrgSingle $ toEnum 1)
                        (mrgSingle $ toEnum 2)
                    ),
              testCase "Bound spec" $
                AsKey1 (genSym (EnumGenBound 'a' 'd') "a" :: Union Char)
                  @?= mrgIf
                    (isym "a" 0)
                    (mrgSingle 'a')
                    (mrgIf (isym "a" 1) (mrgSingle 'b') (mrgSingle 'c'))
            ],
          testGroup
            "Maybe SymBool"
            [ testGroup
                "Maybe SymBool spec"
                [ testGroup
                    "Nothing"
                    [ testCase "genSym" $
                        AsKey1
                          ( genSym (Nothing :: Maybe (AsKey SymBool)) "a" ::
                              Union (Maybe (AsKey SymBool))
                          )
                          @?= mrgSingle Nothing,
                      testCase "genSymSimple" $
                        ( genSymSimple (Nothing :: Maybe (AsKey SymBool)) "a" ::
                            Maybe (AsKey SymBool)
                        )
                          @?= Nothing
                    ],
                  testGroup
                    "Just v"
                    [ testCase "genSym" $
                        AsKey1
                          ( genSym (Just (ssym "a" :: AsKey SymBool)) "a" ::
                              Union (Maybe (AsKey SymBool))
                          )
                          @?= mrgSingle (Just (isym "a" 0)),
                      testCase "genSymSimple" $
                        ( genSymSimple (Just (ssym "a" :: AsKey SymBool)) "a" ::
                            Maybe (AsKey SymBool)
                        )
                          @?= Just (isym "a" 0)
                    ]
                ],
              testCase "() spec" $
                AsKey1 (genSym () "a" :: Union (Maybe (AsKey SymBool)))
                  @?= mrgIf
                    (isym "a" 0)
                    (mrgSingle Nothing)
                    (mrgSingle (Just (isym "a" 1)))
            ],
          testGroup
            "Either SymBool SymBool"
            [ testGroup
                "Either SymBool SymBool spec"
                [ testGroup
                    "Left v"
                    [ testCase "genSym" $
                        ( AsKey1 $
                            genSym
                              ( Left (ssym "a") ::
                                  Either (AsKey SymBool) (AsKey SymBool)
                              )
                              "a" ::
                            AsKey1 Union (Either (AsKey SymBool) (AsKey SymBool))
                        )
                          @?= mrgSingle (Left (isym "a" 0)),
                      testCase "genSymSimple" $
                        ( genSymSimple
                            ( Left (ssym "a") ::
                                Either (AsKey SymBool) (AsKey SymBool)
                            )
                            "a" ::
                            Either (AsKey SymBool) (AsKey SymBool)
                        )
                          @?= Left (isym "a" 0)
                    ],
                  testGroup
                    "Right v"
                    [ testCase "genSym" $
                        ( AsKey1 $
                            genSym
                              ( Right (ssym "a") ::
                                  Either (AsKey SymBool) (AsKey SymBool)
                              )
                              "a" ::
                            AsKey1 Union (Either (AsKey SymBool) (AsKey SymBool))
                        )
                          @?= mrgSingle (Right (isym "a" 0)),
                      testCase "genSymSimple" $
                        ( genSymSimple
                            ( Right (ssym "a") ::
                                Either (AsKey SymBool) (AsKey SymBool)
                            )
                            "a" ::
                            Either (AsKey SymBool) (AsKey SymBool)
                        )
                          @?= Right (isym "a" 0)
                    ]
                ],
              testCase "() spec" $ do
                (AsKey1 (genSym () "a" :: Union (Either (AsKey SymBool) (AsKey SymBool))))
                  @?= mrgIf
                    (isym "a" 0)
                    (mrgSingle $ Left $ isym "a" 1)
                    (mrgSingle $ Right $ isym "a" 2)
            ],
          testGroup
            "lists"
            [ testGroup
                "Max length spec"
                [ testCase "max length = 0" $
                    AsKey1 (genSym (0 :: Integer) "a" :: Union [AsKey SymBool])
                      @?= mrgSingle [],
                  testCase "max length = 3" $
                    AsKey1 (genSym (3 :: Integer) "a" :: Union [AsKey SymBool])
                      @?= mrgIf
                        (isym "a" 3)
                        (mrgSingle [])
                        ( mrgIf
                            (isym "a" 4)
                            (mrgSingle [isym "a" 2])
                            ( mrgIf
                                (isym "a" 5)
                                (mrgSingle [isym "a" 1, isym "a" 2])
                                ( mrgSingle
                                    [ isym "a" 0,
                                      isym "a" 1,
                                      isym "a" 2
                                    ]
                                )
                            )
                        )
                ],
              testGroup
                "Min & max length spec"
                [ testCase "min length = 1, max length = 3" $
                    AsKey1 (genSym (ListSpec 1 3 ()) "a" :: Union [AsKey SymBool])
                      @?= mrgIf
                        (isym "a" 3)
                        (mrgSingle [isym "a" 2])
                        ( mrgIf
                            (isym "a" 4)
                            (mrgSingle [isym "a" 1, isym "a" 2])
                            ( mrgSingle
                                [ isym "a" 0,
                                  isym "a" 1,
                                  isym "a" 2
                                ]
                            )
                        ),
                  testCase "min length = 1, max length = 2, nested" $
                    AsKey1
                      ( genSym (ListSpec 1 2 (ListSpec 1 2 ())) "a" ::
                          Union [AsKey1 Union [AsKey SymBool]]
                      )
                      @?= mrgIf
                        (isym "a" 6)
                        ( mrgSingle
                            [ mrgIf
                                (isym "a" 5)
                                (mrgSingle [isym "a" 4])
                                (mrgSingle [isym "a" 3, isym "a" 4])
                            ]
                        )
                        ( mrgSingle
                            [ mrgIf
                                (isym "a" 2)
                                (mrgSingle [isym "a" 1])
                                (mrgSingle [isym "a" 0, isym "a" 1]),
                              mrgIf
                                (isym "a" 5)
                                (mrgSingle [isym "a" 4])
                                (mrgSingle [isym "a" 3, isym "a" 4])
                            ]
                        )
                ],
              testGroup
                "Exact length spec"
                [ testGroup
                    "length = 2"
                    [ testCase "genSym" $
                        AsKey1
                          (genSym (SimpleListSpec 2 ()) "a" :: Union [AsKey SymBool])
                          @?= mrgSingle [isym "a" 0, isym "a" 1],
                      testCase "genSymSimple" $
                        (genSymSimple (SimpleListSpec 2 ()) "a" :: [AsKey SymBool])
                          @?= [isym "a" 0, isym "a" 1]
                    ],
                  testGroup
                    "length = 2, nested"
                    [ testCase "genSym" $
                        AsKey1
                          ( genSym
                              (SimpleListSpec 2 (SimpleListSpec 2 ()))
                              "a" ::
                              Union [[AsKey SymBool]]
                          )
                          @?= mrgSingle
                            [ [isym "a" 0, isym "a" 1],
                              [isym "a" 2, isym "a" 3]
                            ],
                      testCase "genSymSimple" $
                        ( genSymSimple
                            (SimpleListSpec 2 (SimpleListSpec 2 ()))
                            "a" ::
                            [[AsKey SymBool]]
                        )
                          @?= [ [isym "a" 0, isym "a" 1],
                                [isym "a" 2, isym "a" 3]
                              ]
                    ]
                ],
              testGroup
                "List with same shape spec"
                [ testCase "genSym" $
                    AsKey1
                      ( genSym
                          [[con True :: AsKey SymBool], [ssym "a", ssym "b"]]
                          "a" ::
                          Union [[AsKey SymBool]]
                      )
                      @?= mrgSingle
                        [ [isym "a" 0],
                          [isym "a" 1, isym "a" 2]
                        ],
                  testCase "genSymSimple" $
                    ( genSymSimple
                        [ [con True :: AsKey SymBool],
                          [ssym "a", ssym "b"]
                        ]
                        "a" ::
                        [[AsKey SymBool]]
                    )
                      @?= [[isym "a" 0], [isym "a" 1, isym "a" 2]]
                ]
            ],
          testGroup
            "()"
            [ testCase "() spec" $ do
                AsKey1 (genSym () "a" :: Union ()) @?= mrgSingle ()
                (genSymSimple () "a" :: ()) @?= ()
            ],
          testGroup
            "(,)"
            [ testGroup
                "Some spec"
                [ testCase "genSym" $
                    AsKey1
                      ( genSym
                          ( EnumGenUpperBound @Integer 2,
                            EnumGenUpperBound @Integer 2
                          )
                          "a" ::
                          Union (Integer, Integer)
                      )
                      @?= do
                        x1 <- mrgIf (isym "a" 0) (mrgSingle 0) (mrgSingle 1)
                        x2 <- mrgIf (isym "a" 1) (mrgSingle 0) (mrgSingle 1)
                        mrgSingle (x1, x2),
                  testCase "genSymSimple" $
                    ( genSymSimple
                        ( (),
                          [ [ssym "b" :: AsKey SymBool],
                            [ssym "b", ssym "c"]
                          ]
                        )
                        "a" ::
                        (AsKey SymBool, [[AsKey SymBool]])
                    )
                      @?= ( isym "a" 0,
                            [ [isym "a" 1],
                              [isym "a" 2, isym "a" 3]
                            ]
                          )
                ],
              testGroup
                "No spec"
                [ testCase "genSym" $
                    AsKey1
                      (genSym () "a" :: Union (AsKey SymBool, AsKey SymBool))
                      @?= mrgSingle (isym "a" 0, isym "a" 1),
                  testCase "genSymSimple" $
                    (genSymSimple () "a" :: (AsKey SymBool, AsKey SymBool))
                      @?= (isym "a" 0, isym "a" 1)
                ]
            ],
          testGroup
            "(,,)"
            [ testGroup
                "Some spec"
                [ testCase "genSym" $
                    AsKey1
                      ( genSym
                          ( EnumGenUpperBound @Integer 2,
                            EnumGenUpperBound @Integer 2,
                            EnumGenUpperBound @Integer 2
                          )
                          "a" ::
                          Union (Integer, Integer, Integer)
                      )
                      @?= do
                        x1 <- mrgIf (isym "a" 0) (mrgSingle 0) (mrgSingle 1)
                        x2 <- mrgIf (isym "a" 1) (mrgSingle 0) (mrgSingle 1)
                        x3 <- mrgIf (isym "a" 2) (mrgSingle 0) (mrgSingle 1)
                        mrgSingle (x1, x2, x3),
                  testCase "genSymSimple" $
                    ( genSymSimple
                        ((), [[ssym "b" :: AsKey SymBool], [ssym "b", ssym "c"]], ())
                        "a" ::
                        (AsKey SymBool, [[AsKey SymBool]], AsKey SymBool)
                    )
                      @?= ( isym "a" 0,
                            [ [isym "a" 1],
                              [isym "a" 2, isym "a" 3]
                            ],
                            isym "a" 4
                          )
                ],
              testGroup
                "No spec"
                [ testCase "genSym" $
                    AsKey1
                      (genSym () "a" :: Union (AsKey SymBool, AsKey SymBool, AsKey SymBool))
                      @?= mrgSingle
                        (isym "a" 0, isym "a" 1, isym "a" 2),
                  testCase "genSymSimple" $
                    (genSymSimple () "a" :: (AsKey SymBool, AsKey SymBool, AsKey SymBool))
                      @?= (isym "a" 0, isym "a" 1, isym "a" 2)
                ]
            ],
          testGroup
            "(,,,)"
            [ testGroup
                "Some spec"
                [ testCase "genSym" $
                    AsKey1
                      ( genSym
                          ( EnumGenUpperBound @Integer 2,
                            EnumGenUpperBound @Integer 2,
                            EnumGenUpperBound @Integer 2,
                            EnumGenUpperBound @Integer 2
                          )
                          "a" ::
                          Union (Integer, Integer, Integer, Integer)
                      )
                      @?= do
                        x1 <- mrgIf (isym "a" 0) (mrgSingle 0) (mrgSingle 1)
                        x2 <- mrgIf (isym "a" 1) (mrgSingle 0) (mrgSingle 1)
                        x3 <- mrgIf (isym "a" 2) (mrgSingle 0) (mrgSingle 1)
                        x4 <- mrgIf (isym "a" 3) (mrgSingle 0) (mrgSingle 1)
                        mrgSingle (x1, x2, x3, x4),
                  testCase "genSymSimple" $
                    ( genSymSimple
                        ( (),
                          [[ssym "b" :: AsKey SymBool], [ssym "b", ssym "c"]],
                          (),
                          ()
                        )
                        "a" ::
                        (AsKey SymBool, [[AsKey SymBool]], AsKey SymBool, AsKey SymBool)
                    )
                      @?= ( isym "a" 0,
                            [ [isym "a" 1],
                              [isym "a" 2, isym "a" 3]
                            ],
                            isym "a" 4,
                            isym "a" 5
                          )
                ],
              testGroup
                "No spec"
                [ testCase "genSym" $
                    AsKey1
                      ( genSym () "a" ::
                          Union (AsKey SymBool, AsKey SymBool, AsKey SymBool, AsKey SymBool)
                      )
                      @?= mrgSingle
                        ( isym "a" 0,
                          isym "a" 1,
                          isym "a" 2,
                          isym "a" 3
                        ),
                  testCase "genSymSimple" $
                    ( genSymSimple () "a" ::
                        (AsKey SymBool, AsKey SymBool, AsKey SymBool, AsKey SymBool)
                    )
                      @?= ( isym "a" 0,
                            isym "a" 1,
                            isym "a" 2,
                            isym "a" 3
                          )
                ]
            ],
          testGroup
            "(,,,,)"
            [ testGroup
                "Some spec"
                [ testCase "genSym" $
                    AsKey1
                      ( genSym
                          ( EnumGenUpperBound @Integer 2,
                            EnumGenUpperBound @Integer 2,
                            EnumGenUpperBound @Integer 2,
                            EnumGenUpperBound @Integer 2,
                            EnumGenUpperBound @Integer 2
                          )
                          "a" ::
                          Union (Integer, Integer, Integer, Integer, Integer)
                      )
                      @?= do
                        x1 <- mrgIf (isym "a" 0) (mrgSingle 0) (mrgSingle 1)
                        x2 <- mrgIf (isym "a" 1) (mrgSingle 0) (mrgSingle 1)
                        x3 <- mrgIf (isym "a" 2) (mrgSingle 0) (mrgSingle 1)
                        x4 <- mrgIf (isym "a" 3) (mrgSingle 0) (mrgSingle 1)
                        x5 <- mrgIf (isym "a" 4) (mrgSingle 0) (mrgSingle 1)
                        mrgSingle (x1, x2, x3, x4, x5),
                  testCase "genSymSimple" $
                    ( genSymSimple
                        ( (),
                          [ [ssym "b" :: AsKey SymBool],
                            [ssym "b", ssym "c"]
                          ],
                          (),
                          (),
                          ()
                        )
                        "a" ::
                        (AsKey SymBool, [[AsKey SymBool]], AsKey SymBool, AsKey SymBool, AsKey SymBool)
                    )
                      @?= ( isym "a" 0,
                            [ [isym "a" 1],
                              [isym "a" 2, isym "a" 3]
                            ],
                            isym "a" 4,
                            isym "a" 5,
                            isym "a" 6
                          )
                ],
              testGroup
                "No spec"
                [ testCase "genSym" $
                    AsKey1
                      ( genSym () "a" ::
                          Union (AsKey SymBool, AsKey SymBool, AsKey SymBool, AsKey SymBool, AsKey SymBool)
                      )
                      @?= mrgSingle
                        ( isym "a" 0,
                          isym "a" 1,
                          isym "a" 2,
                          isym "a" 3,
                          isym "a" 4
                        ),
                  testCase "genSymSimple" $
                    ( genSymSimple () "a" ::
                        (AsKey SymBool, AsKey SymBool, AsKey SymBool, AsKey SymBool, AsKey SymBool)
                    )
                      @?= ( isym "a" 0,
                            isym "a" 1,
                            isym "a" 2,
                            isym "a" 3,
                            isym "a" 4
                          )
                ]
            ],
          testGroup
            "(,,,,,)"
            [ testGroup
                "Some spec"
                [ testCase "genSym" $
                    AsKey1
                      ( genSym
                          ( EnumGenUpperBound @Integer 2,
                            EnumGenUpperBound @Integer 2,
                            EnumGenUpperBound @Integer 2,
                            EnumGenUpperBound @Integer 2,
                            EnumGenUpperBound @Integer 2,
                            EnumGenUpperBound @Integer 2
                          )
                          "a" ::
                          Union
                            ( Integer,
                              Integer,
                              Integer,
                              Integer,
                              Integer,
                              Integer
                            )
                      )
                      @?= do
                        x1 <- mrgIf (isym "a" 0) (mrgSingle 0) (mrgSingle 1)
                        x2 <- mrgIf (isym "a" 1) (mrgSingle 0) (mrgSingle 1)
                        x3 <- mrgIf (isym "a" 2) (mrgSingle 0) (mrgSingle 1)
                        x4 <- mrgIf (isym "a" 3) (mrgSingle 0) (mrgSingle 1)
                        x5 <- mrgIf (isym "a" 4) (mrgSingle 0) (mrgSingle 1)
                        x6 <- mrgIf (isym "a" 5) (mrgSingle 0) (mrgSingle 1)
                        mrgSingle (x1, x2, x3, x4, x5, x6),
                  testCase "genSymSimple" $
                    ( genSymSimple
                        ( (),
                          [ [ssym "b" :: AsKey SymBool],
                            [ssym "b", ssym "c"]
                          ],
                          (),
                          (),
                          (),
                          ()
                        )
                        "a" ::
                        ( AsKey SymBool,
                          [[AsKey SymBool]],
                          AsKey SymBool,
                          AsKey SymBool,
                          AsKey SymBool,
                          AsKey SymBool
                        )
                    )
                      @?= ( isym "a" 0,
                            [ [isym "a" 1],
                              [isym "a" 2, isym "a" 3]
                            ],
                            isym "a" 4,
                            isym "a" 5,
                            isym "a" 6,
                            isym "a" 7
                          )
                ],
              testGroup
                "No spec"
                [ testCase "genSym" $
                    AsKey1
                      ( genSym () "a" ::
                          Union
                            ( AsKey SymBool,
                              AsKey SymBool,
                              AsKey SymBool,
                              AsKey SymBool,
                              AsKey SymBool,
                              AsKey SymBool
                            )
                      )
                      @?= mrgSingle
                        ( isym "a" 0,
                          isym "a" 1,
                          isym "a" 2,
                          isym "a" 3,
                          isym "a" 4,
                          isym "a" 5
                        ),
                  testCase "genSymSimple" $
                    ( genSymSimple () "a" ::
                        ( AsKey SymBool,
                          AsKey SymBool,
                          AsKey SymBool,
                          AsKey SymBool,
                          AsKey SymBool,
                          AsKey SymBool
                        )
                    )
                      @?= ( isym "a" 0,
                            isym "a" 1,
                            isym "a" 2,
                            isym "a" 3,
                            isym "a" 4,
                            isym "a" 5
                          )
                ]
            ],
          testGroup
            "(,,,,,,)"
            [ testGroup
                "Some spec"
                [ testCase "genSym" $
                    AsKey1
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
                          Union
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
                        x1 <- mrgIf (isym "a" 0) (mrgSingle 0) (mrgSingle 1)
                        x2 <- mrgIf (isym "a" 1) (mrgSingle 0) (mrgSingle 1)
                        x3 <- mrgIf (isym "a" 2) (mrgSingle 0) (mrgSingle 1)
                        x4 <- mrgIf (isym "a" 3) (mrgSingle 0) (mrgSingle 1)
                        x5 <- mrgIf (isym "a" 4) (mrgSingle 0) (mrgSingle 1)
                        x6 <- mrgIf (isym "a" 5) (mrgSingle 0) (mrgSingle 1)
                        x7 <- mrgIf (isym "a" 6) (mrgSingle 0) (mrgSingle 1)
                        mrgSingle (x1, x2, x3, x4, x5, x6, x7),
                  testCase "genSymSimple" $
                    ( genSymSimple
                        ( (),
                          [ [ssym "b" :: AsKey SymBool],
                            [ssym "b", ssym "c"]
                          ],
                          (),
                          (),
                          (),
                          (),
                          ()
                        )
                        "a" ::
                        ( AsKey SymBool,
                          [[AsKey SymBool]],
                          AsKey SymBool,
                          AsKey SymBool,
                          AsKey SymBool,
                          AsKey SymBool,
                          AsKey SymBool
                        )
                    )
                      @?= ( isym "a" 0,
                            [ [isym "a" 1],
                              [isym "a" 2, isym "a" 3]
                            ],
                            isym "a" 4,
                            isym "a" 5,
                            isym "a" 6,
                            isym "a" 7,
                            isym "a" 8
                          )
                ],
              testGroup
                "No spec"
                [ testCase "genSym" $
                    AsKey1
                      ( genSym () "a" ::
                          Union
                            ( AsKey SymBool,
                              AsKey SymBool,
                              AsKey SymBool,
                              AsKey SymBool,
                              AsKey SymBool,
                              AsKey SymBool,
                              AsKey SymBool
                            )
                      )
                      @?= mrgSingle
                        ( isym "a" 0,
                          isym "a" 1,
                          isym "a" 2,
                          isym "a" 3,
                          isym "a" 4,
                          isym "a" 5,
                          isym "a" 6
                        ),
                  testCase "genSymSimple" $
                    ( genSymSimple () "a" ::
                        ( AsKey SymBool,
                          AsKey SymBool,
                          AsKey SymBool,
                          AsKey SymBool,
                          AsKey SymBool,
                          AsKey SymBool,
                          AsKey SymBool
                        )
                    )
                      @?= ( isym "a" 0,
                            isym "a" 1,
                            isym "a" 2,
                            isym "a" 3,
                            isym "a" 4,
                            isym "a" 5,
                            isym "a" 6
                          )
                ]
            ],
          testGroup
            "(,,,,,,,)"
            [ testGroup
                "Some spec"
                [ testCase "genSym" $
                    AsKey1
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
                          Union
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
                        x1 <- mrgIf (isym "a" 0) (mrgSingle 0) (mrgSingle 1)
                        x2 <- mrgIf (isym "a" 1) (mrgSingle 0) (mrgSingle 1)
                        x3 <- mrgIf (isym "a" 2) (mrgSingle 0) (mrgSingle 1)
                        x4 <- mrgIf (isym "a" 3) (mrgSingle 0) (mrgSingle 1)
                        x5 <- mrgIf (isym "a" 4) (mrgSingle 0) (mrgSingle 1)
                        x6 <- mrgIf (isym "a" 5) (mrgSingle 0) (mrgSingle 1)
                        x7 <- mrgIf (isym "a" 6) (mrgSingle 0) (mrgSingle 1)
                        x8 <- mrgIf (isym "a" 7) (mrgSingle 0) (mrgSingle 1)
                        mrgSingle (x1, x2, x3, x4, x5, x6, x7, x8),
                  testCase "genSymSimple" $
                    ( genSymSimple
                        ( (),
                          [ [ssym "b" :: AsKey SymBool],
                            [ssym "b", ssym "c"]
                          ],
                          (),
                          (),
                          (),
                          (),
                          (),
                          ()
                        )
                        "a" ::
                        ( AsKey SymBool,
                          [[AsKey SymBool]],
                          AsKey SymBool,
                          AsKey SymBool,
                          AsKey SymBool,
                          AsKey SymBool,
                          AsKey SymBool,
                          AsKey SymBool
                        )
                    )
                      @?= ( isym "a" 0,
                            [ [isym "a" 1],
                              [isym "a" 2, isym "a" 3]
                            ],
                            isym "a" 4,
                            isym "a" 5,
                            isym "a" 6,
                            isym "a" 7,
                            isym "a" 8,
                            isym "a" 9
                          )
                ],
              testGroup
                "No spec"
                [ testCase "genSym" $
                    AsKey1
                      ( genSym () "a" ::
                          Union
                            ( AsKey SymBool,
                              AsKey SymBool,
                              AsKey SymBool,
                              AsKey SymBool,
                              AsKey SymBool,
                              AsKey SymBool,
                              AsKey SymBool,
                              AsKey SymBool
                            )
                      )
                      @?= mrgSingle
                        ( isym "a" 0,
                          isym "a" 1,
                          isym "a" 2,
                          isym "a" 3,
                          isym "a" 4,
                          isym "a" 5,
                          isym "a" 6,
                          isym "a" 7
                        ),
                  testCase "genSymSimple" $
                    ( genSymSimple () "a" ::
                        ( AsKey SymBool,
                          AsKey SymBool,
                          AsKey SymBool,
                          AsKey SymBool,
                          AsKey SymBool,
                          AsKey SymBool,
                          AsKey SymBool,
                          AsKey SymBool
                        )
                    )
                      @?= ( isym "a" 0,
                            isym "a" 1,
                            isym "a" 2,
                            isym "a" 3,
                            isym "a" 4,
                            isym "a" 5,
                            isym "a" 6,
                            isym "a" 7
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
                        AsKey1
                          ( genSym
                              (MaybeT Nothing :: MaybeT Maybe (AsKey SymBool))
                              "a" ::
                              Union (MaybeT Maybe (AsKey SymBool))
                          )
                          @?= mrgSingle (MaybeT Nothing),
                      testCase "genSymSimple" $
                        ( genSymSimple
                            (MaybeT Nothing :: MaybeT Maybe (AsKey SymBool))
                            "a" ::
                            MaybeT Maybe (AsKey SymBool)
                        )
                          @?= MaybeT Nothing
                    ],
                  testGroup
                    "MaybeT (Just Nothing)"
                    [ testCase "genSym" $
                        AsKey1
                          ( genSym
                              ( MaybeT (Just Nothing) ::
                                  MaybeT Maybe (AsKey SymBool)
                              )
                              "a" ::
                              Union (MaybeT Maybe (AsKey SymBool))
                          )
                          @?= mrgSingle (MaybeT (Just Nothing)),
                      testCase "genSymSimple" $
                        ( genSymSimple
                            ( MaybeT (Just (Just $ ssym "a")) ::
                                MaybeT Maybe (AsKey SymBool)
                            )
                            "a" ::
                            MaybeT Maybe (AsKey SymBool)
                        )
                          @?= MaybeT (Just (Just $ isym "a" 0))
                    ],
                  testGroup
                    "MaybeT (Just (Just v))"
                    [ testCase "genSym" $
                        AsKey1
                          ( genSym
                              ( MaybeT (Just (Just $ ssym "a")) ::
                                  MaybeT Maybe (AsKey SymBool)
                              )
                              "a" ::
                              Union (MaybeT Maybe (AsKey SymBool))
                          )
                          @?= mrgSingle (MaybeT (Just (Just $ isym "a" 0))),
                      testCase "genSymSimple" $
                        ( genSymSimple
                            ( MaybeT (Just (Just $ ssym "a")) ::
                                MaybeT Maybe (AsKey SymBool)
                            )
                            "a" ::
                            MaybeT Maybe (AsKey SymBool)
                        )
                          @?= MaybeT (Just (Just $ isym "a" 0))
                    ]
                ],
              testCase "No spec" $
                AsKey1
                  (genSym () "a" :: Union (MaybeT Maybe (AsKey SymBool)))
                  @?= mrgIf
                    (isym "a" 0)
                    (mrgSingle $ MaybeT Nothing)
                    ( mrgIf
                        (isym "a" 1)
                        (mrgSingle $ MaybeT $ Just Nothing)
                        (mrgSingle $ MaybeT $ Just $ Just $ isym "a" 2)
                    ),
              testGroup
                "Maybe (Maybe SymBool) spec"
                [ testGroup
                    "Nothing"
                    [ testCase "genSym" $
                        AsKey1
                          ( genSym
                              (Nothing :: Maybe (Maybe (AsKey SymBool)))
                              "a" ::
                              Union (MaybeT Maybe (AsKey SymBool))
                          )
                          @?= mrgSingle (MaybeT Nothing),
                      testCase "genSymSimple" $
                        ( genSymSimple
                            (Nothing :: Maybe (Maybe (AsKey SymBool)))
                            "a" ::
                            MaybeT Maybe (AsKey SymBool)
                        )
                          @?= MaybeT Nothing
                    ],
                  testGroup
                    "Just Nothing"
                    [ testCase "genSym" $
                        AsKey1
                          ( genSym
                              (Just Nothing :: Maybe (Maybe (AsKey SymBool)))
                              "a" ::
                              Union (MaybeT Maybe (AsKey SymBool))
                          )
                          @?= mrgSingle (MaybeT (Just Nothing)),
                      testCase "genSymSimple" $
                        ( genSymSimple
                            (Just Nothing :: Maybe (Maybe (AsKey SymBool)))
                            "a" ::
                            MaybeT Maybe (AsKey SymBool)
                        )
                          @?= MaybeT (Just Nothing)
                    ],
                  testGroup
                    "Just (Just v)"
                    [ testCase "genSym" $
                        AsKey1
                          ( genSym
                              ( Just $ Just $ ssym "a" ::
                                  Maybe (Maybe (AsKey SymBool))
                              )
                              "a" ::
                              Union (MaybeT Maybe (AsKey SymBool))
                          )
                          @?= mrgSingle (MaybeT (Just (Just $ isym "a" 0))),
                      testCase "genSymSimple" $
                        ( genSymSimple
                            ( Just $ Just $ ssym "a" ::
                                Maybe (Maybe (AsKey SymBool))
                            )
                            "a" ::
                            MaybeT Maybe (AsKey SymBool)
                        )
                          @?= MaybeT (Just (Just $ isym "a" 0))
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
                        AsKey1
                          ( genSym
                              ( ExceptT Nothing ::
                                  ExceptT (AsKey SymBool) Maybe (AsKey SymBool)
                              )
                              "a" ::
                              Union (ExceptT (AsKey SymBool) Maybe (AsKey SymBool))
                          )
                          @?= mrgSingle (ExceptT Nothing),
                      testCase "genSymSimple" $
                        ( genSymSimple
                            ( ExceptT Nothing ::
                                ExceptT (AsKey SymBool) Maybe (AsKey SymBool)
                            )
                            "a" ::
                            ExceptT (AsKey SymBool) Maybe (AsKey SymBool)
                        )
                          @?= ExceptT Nothing
                    ],
                  testGroup
                    "ExceptT (Just (Left v))"
                    [ testCase "genSym" $
                        AsKey1
                          ( genSym
                              ( ExceptT $ Just $ Left $ ssym "a" ::
                                  ExceptT (AsKey SymBool) Maybe (AsKey SymBool)
                              )
                              "a" ::
                              Union (ExceptT (AsKey SymBool) Maybe (AsKey SymBool))
                          )
                          @?= mrgSingle
                            (ExceptT $ Just $ Left $ isym "a" 0),
                      testCase "genSymSimple" $
                        ( genSymSimple
                            ( ExceptT $ Just $ Left $ ssym "a" ::
                                ExceptT (AsKey SymBool) Maybe (AsKey SymBool)
                            )
                            "a" ::
                            ExceptT (AsKey SymBool) Maybe (AsKey SymBool)
                        )
                          @?= ExceptT (Just $ Left $ isym "a" 0)
                    ],
                  testGroup
                    "ExceptT (Just (Right v))"
                    [ testCase "genSym" $
                        AsKey1
                          ( genSym
                              ( ExceptT $ Just $ Right $ ssym "a" ::
                                  ExceptT (AsKey SymBool) Maybe (AsKey SymBool)
                              )
                              "a" ::
                              Union (ExceptT (AsKey SymBool) Maybe (AsKey SymBool))
                          )
                          @?= mrgSingle
                            (ExceptT $ Just $ Right $ isym "a" 0),
                      testCase "genSymSimple" $
                        ( genSymSimple
                            ( ExceptT $ Just $ Right $ ssym "a" ::
                                ExceptT (AsKey SymBool) Maybe (AsKey SymBool)
                            )
                            "a" ::
                            ExceptT (AsKey SymBool) Maybe (AsKey SymBool)
                        )
                          @?= ExceptT (Just $ Right $ isym "a" 0)
                    ]
                ],
              testCase "() spec" $ do
                AsKey1
                  (genSym () "a" :: Union (ExceptT (AsKey SymBool) Maybe (AsKey SymBool)))
                  @?= mrgIf
                    (isym "a" 0)
                    (mrgSingle $ ExceptT Nothing)
                    ( mrgIf
                        (isym "a" 1)
                        (mrgSingle $ ExceptT $ Just $ Left $ isym "a" 2)
                        (mrgSingle $ ExceptT $ Just $ Right $ isym "a" 3)
                    ),
              testGroup
                "Maybe (Either SymBool SymBool) spec"
                [ testGroup
                    "Nothing"
                    [ testCase "genSym" $
                        AsKey1
                          ( genSym
                              (Nothing :: Maybe (Either (AsKey SymBool) (AsKey SymBool)))
                              "a" ::
                              Union (ExceptT (AsKey SymBool) Maybe (AsKey SymBool))
                          )
                          @?= mrgSingle (ExceptT Nothing),
                      testCase "genSymSimple" $
                        ( genSymSimple
                            (Nothing :: Maybe (Either (AsKey SymBool) (AsKey SymBool)))
                            "a" ::
                            ExceptT (AsKey SymBool) Maybe (AsKey SymBool)
                        )
                          @?= ExceptT Nothing
                    ],
                  testGroup
                    "Just (left v)"
                    [ testCase "genSym" $
                        AsKey1
                          ( genSym
                              ( Just $ Left $ ssym "a" ::
                                  Maybe (Either (AsKey SymBool) (AsKey SymBool))
                              )
                              "a" ::
                              Union (ExceptT (AsKey SymBool) Maybe (AsKey SymBool))
                          )
                          @?= mrgSingle
                            (ExceptT (Just (Left $ isym "a" 0))),
                      testCase "genSymSimple" $
                        ( genSymSimple
                            ( Just $ Left $ ssym "a" ::
                                Maybe (Either (AsKey SymBool) (AsKey SymBool))
                            )
                            "a" ::
                            ExceptT (AsKey SymBool) Maybe (AsKey SymBool)
                        )
                          @?= ExceptT (Just (Left $ isym "a" 0))
                    ],
                  testGroup
                    "Just (left v)"
                    [ testCase "genSym" $
                        AsKey1
                          ( genSym
                              ( Just $ Right $ ssym "a" ::
                                  Maybe (Either (AsKey SymBool) (AsKey SymBool))
                              )
                              "a" ::
                              Union (ExceptT (AsKey SymBool) Maybe (AsKey SymBool))
                          )
                          @?= mrgSingle
                            (ExceptT (Just (Right $ isym "a" 0))),
                      testCase "genSymSimple" $
                        ( genSymSimple
                            ( Just $ Right $ ssym "a" ::
                                Maybe (Either (AsKey SymBool) (AsKey SymBool))
                            )
                            "a" ::
                            ExceptT (AsKey SymBool) Maybe (AsKey SymBool)
                        )
                          @?= ExceptT (Just (Right $ isym "a" 0))
                    ]
                ]
            ]
        ],
      testGroup
        "choose*"
        [ testCase "chooseFresh" $ do
            (AsKey1 (runFresh (chooseFresh [1, 2, 3]) "a" :: Union Int))
              @?= mrgIf
                (isym "a" 0)
                (mrgSingle 1)
                (mrgIf (isym "a" 1) (mrgSingle 2) (mrgSingle 3)),
          testCase "choose" $ do
            (AsKey1 (choose [1, 2, 3] "a" :: Union Int))
              @?= mrgIf
                (isym "a" 0)
                (mrgSingle 1)
                (mrgIf (isym "a" 1) (mrgSingle 2) (mrgSingle 3)),
          testCase "chooseSimpleFresh" $ do
            (runFresh (chooseSimpleFresh ["x", "y", "z"]) "a" :: AsKey SymBool)
              @?= symIte
                (isym "a" 0)
                (ssym "x")
                (symIte (isym "a" 1) (ssym "y") (ssym "z")),
          testCase "chooseSimple" $ do
            (chooseSimple ["x", "y", "z"] "a" :: AsKey SymBool)
              @?= symIte
                (isym "a" 0)
                (ssym "x")
                (symIte (isym "a" 1) (ssym "y") (ssym "z")),
          testCase "chooseUnionFresh" $ do
            AsKey1
              ( runFresh
                  ( chooseUnionFresh
                      [ mrgIf (ssym "x") 1 2,
                        mrgIf (ssym "x") 2 3,
                        mrgIf (ssym "x") 3 4
                      ]
                  )
                  "a" ::
                  Union Int
              )
              @?= mrgIf
                (isym "a" 0)
                (mrgIf (ssym "x") 1 2)
                ( mrgIf
                    (isym "a" 1)
                    (mrgIf (ssym "x") 2 3)
                    (mrgIf (ssym "x") 3 4)
                ),
          testCase "chooseUnion" $ do
            AsKey1
              ( chooseUnion
                  [ mrgIf (ssym "x") 1 2,
                    mrgIf (ssym "x") 2 3,
                    mrgIf (ssym "x") 3 4
                  ]
                  "a" ::
                  Union Int
              )
              @?= mrgIf
                (isym "a" 0)
                (mrgIf (ssym "x") 1 2)
                ( mrgIf
                    (isym "a" 1)
                    (mrgIf (ssym "x") 2 3)
                    (mrgIf (ssym "x") 3 4)
                ),
          testCase "liftFresh" $ do
            let orig = simpleFresh () :: Fresh (AsKey SymBool, AsKey SymBool)
            let actual = flip runFreshT "a" $ do
                  r1 <- liftFresh orig
                  r2 <- liftFresh orig
                  return (r1, r2) ::
                    FreshT (AsKey1 Union) ((AsKey SymBool, AsKey SymBool), (AsKey SymBool, AsKey SymBool))
            let expected =
                  return
                    ( (isym "a" 0, isym "a" 1),
                      (isym "a" 2, isym "a" 3)
                    )
            actual @?= expected
        ],
      testCase "freshString" $ do
        runFresh (replicateM 2 $ freshString "a") "b" @?= ["b@0[a]", "b@1[a]"],
      testCase "localIdentifier" $ do
        let computation = do
              a <- simpleFresh ()
              (b1, b2) <-
                localIdentifier (mapMetadata (const $ Atom ("b" :: T.Text))) $ do
                  b1 <- simpleFresh ()
                  b2 <- simpleFresh ()
                  return (b1, b2)
              c <- simpleFresh ()
              return [a, b1, b2, c :: AsKey SymBool]
        let actual = runFresh computation "c"
        actual
          @?= [ isym "c" 0,
                isym (withMetadata "c" (Atom ("b" :: T.Text))) 0,
                isym (withMetadata "c" (Atom ("b" :: T.Text))) 1,
                isym "c" 1
              ]
    ]
