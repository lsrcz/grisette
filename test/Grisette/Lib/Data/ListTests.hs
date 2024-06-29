{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use elemIndex" #-}
{-# HLINT ignore "Use elemIndices" #-}
{-# HLINT ignore "Use nub" #-}
{-# HLINT ignore "Use union" #-}
{-# HLINT ignore "Use intersect" #-}
{-# HLINT ignore "Use insert" #-}
{-# HLINT ignore "Use group" #-}

module Grisette.Lib.Data.ListTests (listTests) where

import Data.List
  ( delete,
    deleteBy,
    deleteFirstsBy,
    dropWhileEnd,
    elemIndex,
    elemIndices,
    findIndex,
    findIndices,
    group,
    groupBy,
    insert,
    insertBy,
    intersect,
    intersectBy,
    isInfixOf,
    isPrefixOf,
    isSubsequenceOf,
    isSuffixOf,
    nub,
    nubBy,
    partition,
    stripPrefix,
    union,
    unionBy,
    (\\),
  )
import Grisette
  ( ITEOp (symIte),
    LogicalOp (symNot, (.&&), (.||)),
    Solvable (con),
    SymBool,
    SymEq ((./=), (.==)),
    SymOrd (symCompare, (.<=), (.>=)),
    Union,
    mrgGroupBy,
    mrgIf,
  )
import Grisette.Lib.Control.Applicative (mrgPure)
import Grisette.Lib.Data.List
  ( mrgBreak,
    mrgDelete,
    mrgDeleteBy,
    mrgDeleteFirstsBy,
    mrgDrop,
    mrgDropWhile,
    mrgDropWhileEnd,
    mrgElemIndex,
    mrgElemIndices,
    mrgFilter,
    mrgFindIndex,
    mrgFindIndices,
    mrgGroup,
    mrgInsert,
    mrgInsertBy,
    mrgIntersect,
    mrgIntersectBy,
    mrgLookup,
    mrgNub,
    mrgNubBy,
    mrgPartition,
    mrgSpan,
    mrgSplitAt,
    mrgStripPrefix,
    mrgTake,
    mrgTakeWhile,
    mrgUnion,
    mrgUnionBy,
    symIsInfixOf,
    symIsPrefixOf,
    symIsSubsequenceOf,
    symIsSuffixOf,
    (.!?),
    (.\\),
  )
import Grisette.SymPrim (SymInteger)
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.HUnit ((@?=))
import Test.QuickCheck (Gen, forAll, ioProperty, listOf, oneof)

aint, bint, cint, dint, eint, fint :: SymInteger
aint = "a"
bint = "b"
cint = "c"
dint = "d"
eint = "e"
fint = "f"

ranint :: Gen SymInteger
ranint = oneof $ return <$> [aint, bint, cint, dint, eint, fint, -1, 0, 1]

ranilist :: Gen [SymInteger]
ranilist = listOf ranint

listTests :: Test
listTests =
  testGroup
    "List"
    [ testGroup
        "mrgTake"
        [ testProperty "concrete int" $
            \(n :: Integer) -> forAll ranilist $ \ranilist -> ioProperty $ do
              let actual = mrgTake n ranilist :: Union [SymInteger]
              let expected = mrgPure $ take (fromInteger n) ranilist
              actual @?= expected,
          testCase "symbolic int" $ do
            let actual = mrgTake aint [bint, cint, dint] :: Union [SymInteger]
            let expected =
                  mrgIf (aint .<= 0) (return []) $
                    mrgIf (aint .== 1) (return [bint]) $
                      mrgIf (aint .== 2) (return [bint, cint]) $
                        return [bint, cint, dint]
            actual @?= expected
        ],
      testGroup
        "mrgDrop"
        [ testProperty "concrete int" $
            \(n :: Integer) -> forAll ranilist $ \ranilist -> ioProperty $ do
              let actual = mrgDrop n ranilist :: Union [SymInteger]
              let expected = mrgPure $ drop (fromInteger n) ranilist
              actual @?= expected,
          testCase "symbolic int" $ do
            let actual = mrgDrop aint [bint, cint, dint] :: Union [SymInteger]
            let expected =
                  mrgIf (aint .>= 3) (return []) $
                    mrgIf (aint .== 2) (return [dint]) $
                      mrgIf (aint .== 1) (return [cint, dint]) $
                        return [bint, cint, dint]
            actual @?= expected
        ],
      testGroup
        "mrgSplitAt"
        [ testProperty "concrete int" $
            \(n :: Integer) -> forAll ranilist $ \ranilist -> ioProperty $ do
              let actual =
                    mrgSplitAt n ranilist ::
                      Union ([SymInteger], [SymInteger])
              let expected = mrgPure $ splitAt (fromInteger n) ranilist
              actual @?= expected,
          testCase "symbolic int" $ do
            let actual =
                  mrgSplitAt aint [bint, cint, dint] ::
                    Union ([SymInteger], [SymInteger])
            let expected =
                  mrgIf (aint .<= 0) (return ([], [bint, cint, dint])) $
                    mrgIf (aint .== 1) (return ([bint], [cint, dint])) $
                      mrgIf (aint .== 2) (return ([bint, cint], [dint])) $
                        return ([bint, cint, dint], [])
            actual @?= expected
        ],
      testGroup
        "mrgTakeWhile"
        [ testProperty "concrete int" $
            \ranilist -> ioProperty $ do
              let actual = mrgTakeWhile (.== 0) ranilist :: Union [Integer]
              let expected = mrgPure $ takeWhile (== 0) ranilist
              actual @?= expected,
          testCase "symbolic int" $ do
            let actual =
                  mrgTakeWhile (.== 0) [aint, bint, cint] :: Union [SymInteger]
            let expected =
                  mrgIf (aint ./= 0) (return []) $
                    mrgIf (bint ./= 0) (return [aint]) $
                      mrgIf (cint ./= 0) (return ([aint, bint])) $
                        return [aint, bint, cint]
            actual @?= expected
        ],
      testGroup
        "mrgDropWhile"
        [ testProperty "concrete int" $
            \ranilist -> ioProperty $ do
              let actual = mrgDropWhile (.== 0) ranilist :: Union [Integer]
              let expected = mrgPure $ dropWhile (== 0) ranilist
              actual @?= expected,
          testCase "symbolic int" $ do
            let actual =
                  mrgDropWhile (.== 0) [aint, bint, cint] :: Union [SymInteger]
            let expected =
                  mrgIf
                    ((aint .== 0 .&& bint .== 0) .&& cint .== 0)
                    (return [])
                    $ mrgIf (aint .== 0 .&& bint .== 0) (return [cint])
                    $ mrgIf (aint .== 0) (return ([bint, cint]))
                    $ return [aint, bint, cint]
            actual @?= expected
        ],
      testGroup
        "mrgDropWhileEnd"
        [ testProperty "concrete int" $
            \ranilist -> ioProperty $ do
              let actual = mrgDropWhileEnd (.== 0) ranilist :: Union [Integer]
              let expected = mrgPure $ dropWhileEnd (== 0) ranilist
              actual @?= expected,
          testCase "symbolic int" $ do
            let actual =
                  mrgDropWhileEnd (.== 0) [aint, bint, cint] ::
                    Union [SymInteger]
            let expected =
                  mrgIf
                    ((cint .== 0 .&& bint .== 0) .&& aint .== 0)
                    (return [])
                    $ mrgIf (cint .== 0 .&& bint .== 0) (return [aint])
                    $ mrgIf (cint .== 0) (return ([aint, bint]))
                    $ return [aint, bint, cint]
            actual @?= expected
        ],
      testGroup
        "mrgSpan"
        [ testProperty "concrete int" $
            \ranilist -> ioProperty $ do
              let actual =
                    mrgSpan (.== 0) ranilist :: Union ([Integer], [Integer])
              let expected = mrgPure $ span (== 0) ranilist
              actual @?= expected,
          testCase "symbolic int" $ do
            let actual =
                  mrgSpan (.== 0) [aint, bint, cint] ::
                    Union ([SymInteger], [SymInteger])
            let expected =
                  mrgIf (aint ./= 0) (return ([], [aint, bint, cint])) $
                    mrgIf (bint ./= 0) (return ([aint], [bint, cint])) $
                      mrgIf (cint ./= 0) (return ([aint, bint], [cint])) $
                        return ([aint, bint, cint], [])
            actual @?= expected
        ],
      testGroup
        "mrgBreak"
        [ testProperty "concrete int" $
            \ranilist -> ioProperty $ do
              let actual =
                    mrgBreak (.== 0) ranilist :: Union ([Integer], [Integer])
              let expected = mrgPure $ break (== 0) ranilist
              actual @?= expected,
          testCase "symbolic int" $ do
            let actual =
                  mrgBreak (.== 0) [aint, bint, cint] ::
                    Union ([SymInteger], [SymInteger])
            let expected =
                  mrgIf (aint .== 0) (return ([], [aint, bint, cint])) $
                    mrgIf (bint .== 0) (return ([aint], [bint, cint])) $
                      mrgIf (cint .== 0) (return ([aint, bint], [cint])) $
                        return ([aint, bint, cint], [])
            actual @?= expected
        ],
      testGroup
        "mrgStripPrefix"
        [ testProperty "concrete int" $
            \l1 l2 -> ioProperty $ do
              let actual = mrgStripPrefix l1 l2 :: Union (Maybe [Integer])
              let expected = mrgPure $ stripPrefix l1 l2
              actual @?= expected,
          testCase "symbolic int" $ do
            let actual =
                  mrgStripPrefix [aint, bint] [cint, dint, eint] ::
                    Union (Maybe [SymInteger])
            let expected =
                  mrgIf
                    (symNot ((aint .== cint) .&& (bint .== dint)))
                    (return Nothing)
                    (return $ Just [eint])
            actual @?= expected
        ],
      testGroup
        "mrgGroup"
        [ testProperty "concrete int" $
            \l -> ioProperty $ do
              let actual = mrgGroup l :: Union [[Integer]]
              let expected = mrgPure $ group l
              actual @?= expected,
          testCase "symbolic int" $ do
            let actual = mrgGroup [aint, bint, cint] :: Union [[SymInteger]]
            let expected =
                  mrgIf
                    (aint .== bint .&& aint .== cint)
                    (return [[aint, bint, cint]])
                    $ mrgIf
                      (aint .== bint .|| bint .== cint)
                      ( mrgIf (aint .== bint) (return [[aint, bint], [cint]]) $
                          return [[aint], [bint, cint]]
                      )
                    $ return [[aint], [bint], [cint]]

            actual @?= expected
        ],
      testGroup
        "symIsPrefixOf"
        [ testProperty "concrete int" $
            \(l1 :: [Int]) l2 -> ioProperty $ do
              let actual = symIsPrefixOf l1 l2 :: SymBool
              let expected = con $ isPrefixOf l1 l2
              actual @?= expected,
          testCase "symbolic int" $ do
            let actual = symIsPrefixOf [aint, bint] [cint, dint, eint]
            actual @?= (aint .== cint .&& bint .== dint),
          testCase "symbolic int, not long enough" $ do
            let actual = symIsPrefixOf [cint, dint, eint] [aint, bint]
            actual @?= con False
        ],
      testGroup
        "symIsSuffixOf"
        [ testProperty "concrete int" $
            \(l1 :: [Int]) l2 -> ioProperty $ do
              let actual = symIsSuffixOf l1 l2 :: SymBool
              let expected = con $ isSuffixOf l1 l2
              actual @?= expected,
          testCase "symbolic int" $ do
            let actual = symIsSuffixOf [aint, bint] [cint, dint, eint]
            actual @?= (bint .== eint .&& aint .== dint),
          testCase "symbolic int, not long enough" $ do
            let actual = symIsSuffixOf [cint, dint, eint] [aint, bint]
            actual @?= con False
        ],
      testGroup
        "symIsInfixOf"
        [ testProperty "concrete int" $
            \(l1 :: [Int]) l2 -> ioProperty $ do
              let actual = symIsInfixOf l1 l2 :: SymBool
              let expected = con $ isInfixOf l1 l2
              actual @?= expected,
          testCase "symbolic int" $ do
            let actual = symIsInfixOf [aint, bint] [cint, dint, eint, fint]
            actual
              @?= ( (aint .== cint .&& bint .== dint)
                      .|| (aint .== dint .&& bint .== eint)
                  )
              .|| (aint .== eint .&& bint .== fint),
          testCase "symbolic int, not long enough" $ do
            let actual = symIsInfixOf [cint, dint, eint] [aint, bint]
            actual @?= con False
        ],
      testGroup
        "symIsSubsequenceOf"
        [ testProperty "concrete int" $
            \(l1 :: [Int]) l2 -> ioProperty $ do
              let actual = symIsSubsequenceOf l1 l2 :: SymBool
              let expected = con $ isSubsequenceOf l1 l2
              actual @?= expected,
          testCase "symbolic int" $ do
            let actual =
                  symIsSubsequenceOf [aint, bint] [cint, dint, eint, fint]
            actual
              @?= symIte
                (aint .== cint)
                (bint .== dint .|| (bint .== eint .|| bint .== fint))
                ( symIte
                    (aint .== dint)
                    (bint .== eint .|| bint .== fint)
                    (aint .== eint .&& bint .== fint)
                ),
          testCase "symbolic int, not long enough" $ do
            let actual = symIsSubsequenceOf [cint, dint, eint] [aint, bint]
            actual @?= con False
        ],
      testGroup
        "mrgLookup"
        [ testProperty "concrete int" $
            \v l -> ioProperty $ do
              let actual = mrgLookup (v :: Integer) l :: Union (Maybe Integer)
              let expected = mrgPure $ lookup v l
              actual @?= expected,
          testCase "symbolic int" $ do
            let actual =
                  mrgLookup
                    aint
                    [ (bint, Just cint),
                      (dint, Nothing),
                      (fint, Just eint)
                    ] ::
                    Union (Maybe (Maybe SymInteger))
            let expected =
                  mrgIf
                    ((aint ./= bint .&& aint ./= dint) .&& aint ./= fint)
                    (return Nothing)
                    $ mrgIf (aint .== bint) (return $ Just $ Just cint)
                    $ mrgIf
                      (aint .== dint)
                      (return $ Just Nothing)
                      (return $ Just $ Just eint)
            actual @?= expected
        ],
      testGroup
        "mrgFilter"
        [ testProperty "concrete int" $
            \l -> ioProperty $ do
              let actual = mrgFilter (.== 0) l :: Union [Integer]
              let expected = mrgPure $ filter (== 0) l
              actual @?= expected,
          testCase "symbolic int" $ do
            let actual =
                  mrgFilter (.== 0) [aint, bint] :: Union [SymInteger]
            let expected =
                  mrgIf (symNot $ aint .== 0 .|| bint .== 0) (return []) $
                    mrgIf
                      (symNot $ aint .== 0 .&& bint .== 0)
                      (return [symIte (aint .== 0) aint bint])
                      (return [aint, bint])
            actual @?= expected
        ],
      testGroup
        "mrgPartition"
        [ testProperty "concrete int" $
            \l -> ioProperty $ do
              let actual =
                    mrgPartition (.== 0) l ::
                      Union ([Integer], [Integer])
              let expected = mrgPure $ partition (== 0) l
              actual @?= expected,
          testCase "symbolic int" $ do
            let actual =
                  mrgPartition (.== 0) [aint, bint] ::
                    Union ([SymInteger], [SymInteger])
            let expected =
                  mrgIf
                    (symNot $ aint .== 0 .|| bint .== 0)
                    (return ([], [aint, bint]))
                    $ mrgIf
                      (symNot $ aint .== 0 .&& bint .== 0)
                      ( return
                          ( [symIte (aint .== 0) aint bint],
                            [symIte (aint .== 0) bint aint]
                          )
                      )
                      (return ([aint, bint], []))
            actual @?= expected
        ],
      testGroup
        ".!?"
        [ testProperty "concrete int" $
            \l (i :: Int) -> ioProperty $ do
              let actual = l .!? i :: Union (Maybe Integer)
              let expected =
                    mrgPure $
                      if i < 0 || i >= length l then Nothing else Just $ l !! i
              actual @?= expected,
          testCase "symbolic int" $ do
            let actual =
                  [aint, bint, cint] .!? dint :: Union (Maybe SymInteger)
            let expected =
                  mrgIf
                    (symNot (dint .== 0 .|| dint .== 1 .|| dint .== 2))
                    (return Nothing)
                    ( return $
                        Just $
                          symIte (dint .== 0) aint $
                            symIte (dint .== 1) bint cint
                    )
            actual @?= expected
        ],
      testGroup
        "mrgElemIndex"
        [ testProperty "concrete int" $
            \(v :: Integer) l -> ioProperty $ do
              let actual = mrgElemIndex v l :: Union (Maybe Int)
              let expected = mrgPure $ elemIndex v l
              actual @?= expected,
          testCase "symbolic int" $ do
            let actual =
                  mrgElemIndex aint [bint, cint, dint] ::
                    Union (Maybe SymInteger)
            let expected =
                  mrgIf
                    (symNot $ aint .== bint .|| aint .== cint .|| aint .== dint)
                    (mrgPure Nothing)
                    ( mrgPure $
                        Just $
                          symIte (aint .== bint) 0 (symIte (aint .== cint) 1 2)
                    )
            actual @?= expected
        ],
      testGroup
        "mrgElemIndices"
        [ testProperty "concrete int" $
            \(v :: Integer) l -> ioProperty $ do
              let actual = mrgElemIndices v l :: Union [Int]
              let expected = mrgPure $ elemIndices v l
              actual @?= expected
        ],
      testGroup
        "mrgFindIndex"
        [ testProperty "concrete int" $
            \(l :: [Integer]) -> ioProperty $ do
              let actual = mrgFindIndex (.== 0) l :: Union (Maybe Int)
              let expected = mrgPure $ findIndex (== 0) l
              actual @?= expected,
          testCase "symbolic int" $ do
            let actual =
                  mrgFindIndex (.== 0) [aint, bint, cint] ::
                    Union (Maybe SymInteger)
            let expected =
                  mrgIf
                    (symNot $ aint .== 0 .|| bint .== 0 .|| cint .== 0)
                    (mrgPure Nothing)
                    ( mrgPure $
                        Just $
                          symIte (aint .== 0) 0 (symIte (bint .== 0) 1 2)
                    )
            actual @?= expected
        ],
      testGroup
        "mrgFindIndices"
        [ testProperty "concrete int" $
            \(l :: [Integer]) -> ioProperty $ do
              let actual = mrgFindIndices (.== 0) l :: Union [Int]
              let expected = mrgPure $ findIndices (== 0) l
              actual @?= expected
        ],
      testGroup
        "mrgNub"
        [ testProperty "concrete int" $
            \l -> ioProperty $ do
              let actual = mrgNub l :: Union [Integer]
              let expected = mrgPure $ nub l
              actual @?= expected,
          testCase "symbolic int" $ do
            let actual = mrgNub [aint, bint, cint] :: Union [SymInteger]
            let expected =
                  mrgIf
                    (bint .== aint .&& cint .== aint)
                    (return [aint])
                    $ mrgIf
                      (bint .== aint .|| cint .== bint .|| cint .== aint)
                      (return [aint, symIte (bint .== aint) cint bint])
                      (return [aint, bint, cint])
            actual @?= expected
        ],
      testGroup
        "mrgDelete"
        [ testProperty "concrete int" $
            \i l -> ioProperty $ do
              let actual = mrgDelete i l :: Union [Integer]
              let expected = mrgPure $ delete i l
              actual @?= expected,
          testCase "symbolic int" $ do
            let actual =
                  mrgDelete aint [bint, cint, dint] :: Union [SymInteger]
            let expected =
                  mrgIf
                    (aint .== bint .|| aint .== cint .|| aint .== dint)
                    ( return
                        [ symIte (aint .== bint) cint bint,
                          symIte (aint .== bint .|| aint .== cint) dint cint
                        ]
                    )
                    (return [bint, cint, dint])
            actual @?= expected
        ],
      testGroup
        ".\\\\"
        [ testProperty "concrete int" $
            \l1 l2 -> ioProperty $ do
              let actual = l1 .\\ l2 :: Union [Integer]
              let expected = mrgPure $ l1 \\ l2
              actual @?= expected
        ],
      testGroup
        "mrgUnion"
        [ testProperty "concrete int" $
            \l1 l2 -> ioProperty $ do
              let actual = mrgUnion l1 l2 :: Union [Integer]
              let expected = mrgPure $ union l1 l2
              actual @?= expected
        ],
      testGroup
        "mrgIntersect"
        [ testProperty "concrete int" $
            \l1 l2 -> ioProperty $ do
              let actual = mrgIntersect l1 l2 :: Union [Integer]
              let expected = mrgPure $ intersect l1 l2
              actual @?= expected,
          testCase "symbolic int" $ do
            let actual =
                  mrgIntersect [aint, bint] [cint, dint] :: Union [SymInteger]
            let expected =
                  mrgIf
                    ( symNot $
                        (bint .== cint .|| bint .== dint)
                          .|| (aint .== cint .|| aint .== dint)
                    )
                    (return [])
                    ( mrgIf
                        ( symNot $
                            (bint .== cint .|| bint .== dint)
                              .&& (aint .== cint .|| aint .== dint)
                        )
                        ( return
                            [symIte (bint .== cint .|| bint .== dint) bint aint]
                        )
                        (return [aint, bint])
                    )
            actual @?= expected
        ],
      testGroup
        "mrgNubBy"
        [ testProperty "concrete int" $
            \l -> ioProperty $ do
              let actual = mrgNubBy (.==) l :: Union [Integer]
              let expected = mrgPure $ nubBy (==) l
              actual @?= expected,
          testCase "symbolic int" $ do
            let actual =
                  mrgNubBy (.==) [aint, bint, cint] :: Union [SymInteger]
            let expected =
                  mrgIf
                    (bint .== aint .&& cint .== aint)
                    (return [aint])
                    $ mrgIf
                      (bint .== aint .|| cint .== bint .|| cint .== aint)
                      (return [aint, symIte (bint .== aint) cint bint])
                      (return [aint, bint, cint])
            actual @?= expected
        ],
      testGroup
        "mrgDeleteBy"
        [ testProperty "concrete int" $
            \i l -> ioProperty $ do
              let actual = mrgDeleteBy (./=) i l :: Union [Integer]
              let expected = mrgPure $ deleteBy (/=) i l
              actual @?= expected,
          testCase "symbolic int" $ do
            let actual =
                  mrgDeleteBy (./=) aint [bint, cint, dint] ::
                    Union [SymInteger]
            let expected =
                  mrgIf
                    ( symNot $
                        aint .== bint .&& aint .== cint .&& aint .== dint
                    )
                    ( return
                        [ symIte (aint .== bint) bint cint,
                          symIte (aint .== bint .&& aint .== cint) cint dint
                        ]
                    )
                    (return [bint, cint, dint])
            actual @?= expected
        ],
      testGroup
        "mrgDeleteFirstsBy"
        [ testProperty "concrete int" $
            \l1 l2 -> ioProperty $ do
              let actual = mrgDeleteFirstsBy (./=) l1 l2 :: Union [Integer]
              let expected = mrgPure $ deleteFirstsBy (/=) l1 l2
              actual @?= expected
        ],
      testGroup
        "mrgUnionBy"
        [ testProperty "concrete int" $
            \l1 l2 -> ioProperty $ do
              let actual = mrgUnionBy (.==) l1 l2 :: Union [Integer]
              let expected = mrgPure $ unionBy (==) l1 l2
              actual @?= expected
        ],
      testGroup
        "mrgIntersectBy"
        [ testProperty "concrete int" $
            \l1 l2 -> ioProperty $ do
              let actual = mrgIntersectBy (.==) l1 l2 :: Union [Integer]
              let expected = mrgPure $ intersectBy (==) l1 l2
              actual @?= expected,
          testCase "symbolic int" $ do
            let actual =
                  mrgIntersectBy (.==) [aint, bint] [cint, dint] ::
                    Union [SymInteger]
            let expected =
                  mrgIf
                    ( symNot $
                        (bint .== cint .|| bint .== dint)
                          .|| (aint .== cint .|| aint .== dint)
                    )
                    (return [])
                    ( mrgIf
                        ( symNot $
                            (bint .== cint .|| bint .== dint)
                              .&& (aint .== cint .|| aint .== dint)
                        )
                        ( return
                            [symIte (bint .== cint .|| bint .== dint) bint aint]
                        )
                        (return [aint, bint])
                    )
            actual @?= expected
        ],
      testGroup
        "mrgGroupBy"
        [ testProperty "concrete int" $
            \l -> ioProperty $ do
              let actual = mrgGroupBy (.==) l :: Union [[Integer]]
              let expected = mrgPure $ groupBy (==) l
              actual @?= expected,
          testCase "symbolic int" $ do
            let actual =
                  mrgGroupBy (.==) [aint, bint, cint] :: Union [[SymInteger]]
            let expected =
                  mrgIf
                    (aint .== bint .&& aint .== cint)
                    (return [[aint, bint, cint]])
                    $ mrgIf
                      (aint .== bint .|| bint .== cint)
                      ( mrgIf (aint .== bint) (return [[aint, bint], [cint]]) $
                          return [[aint], [bint, cint]]
                      )
                    $ return [[aint], [bint], [cint]]

            actual @?= expected
        ],
      testGroup
        "mrgInsert"
        [ testProperty "concrete int" $
            \i l -> ioProperty $ do
              let actual = mrgInsert i l :: Union [Integer]
              let expected = mrgPure $ insert i l
              actual @?= expected,
          testCase "symbolic int" $ do
            let actual =
                  mrgInsert aint [bint, cint, dint] :: Union [SymInteger]
            let expected =
                  mrgPure
                    [ symIte (aint .<= bint) aint bint,
                      symIte (aint .<= bint) bint $
                        symIte (aint .<= cint) aint cint,
                      symIte (aint .<= bint .|| aint .<= cint) cint $
                        symIte (aint .<= dint) aint dint,
                      symIte
                        (aint .<= bint .|| aint .<= cint .|| aint .<= dint)
                        dint
                        aint
                    ]
            actual @?= expected
        ],
      testGroup
        "mrgInsertBy"
        [ testProperty "concrete int" $
            \i l -> ioProperty $ do
              let actual = mrgInsertBy symCompare i l :: Union [Integer]
              let expected = mrgPure $ insertBy compare i l
              actual @?= expected
        ]
    ]
