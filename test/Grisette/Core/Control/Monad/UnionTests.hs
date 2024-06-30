{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <$>" #-}

module Grisette.Core.Control.Monad.UnionTests (unionTests) where

import Control.Monad.Except (ExceptT)
import Control.Monad.Identity (Identity (Identity))
import qualified Data.Text as T
import Grisette
  ( EvalSym (evalSym),
    ExtractSym (extractSym),
    Format (format),
    Function ((#)),
    ITEOp (symIte),
    LogicalOp ((.&&)),
    Mergeable (rootStrategy),
    ModelOps (emptyModel),
    ModelRep (buildModel),
    ModelValuePair ((::=)),
    PlainUnion (ifView, singleView),
    SimpleMergeable (mrgIte),
    Solvable (con, conView, isym, ssym),
    SubstSym (substSym),
    SymBool,
    SymBranching (mrgIfPropagatedStrategy, mrgIfWithStrategy),
    SymEq ((.==)),
    SymInteger,
    SymOrd ((.<=)),
    SymbolSetRep (buildSymbolSet),
    ToCon (toCon),
    ToSym (toSym),
    TryMerge (tryMergeWithStrategy),
    TypedSymbol,
    mrgIf,
    mrgIte1,
    mrgSingle,
    tryMerge,
  )
import Grisette.Internal.Core.Control.Monad.Union
  ( Union (UAny, UMrg),
    isMerged,
    liftToMonadUnion,
    liftUnion,
    unionBase,
    unionBinOp,
    unionSize,
    unionUnaryOp,
  )
import Grisette.Internal.Core.Data.UnionBase
  ( UnionBase (UnionSingle),
    ifWithLeftMost,
  )
import Grisette.TestUtil.PrettyPrint (compactRenderedAs, renderedAs)
import Grisette.TestUtil.SymbolicAssertion ((.@?=))
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit ((@?=))

union1 :: Union (Either SymBool SymInteger)
union1 = mrgIfPropagatedStrategy "u1c" (return $ Left "u1a") (return $ Right "u1b")

union2 :: Union (Either SymBool SymInteger)
union2 = mrgIfPropagatedStrategy "u2c" (return $ Left "u2a") (return $ Right "u2b")

union12 :: Union (Either SymBool SymInteger)
union12 = mrgIfPropagatedStrategy "u12c" union1 union2

unionBase12Merged :: UnionBase (Either SymBool SymInteger)
unionBase12Merged =
  ifWithLeftMost
    True
    (symIte "u12c" "u1c" "u2c")
    (UnionSingle (Left (symIte "u12c" "u1a" "u2a")))
    (UnionSingle (Right (symIte "u12c" "u1b" "u2b")))

union12Merged :: Union (Either SymBool SymInteger)
union12Merged = UMrg rootStrategy unionBase12Merged

unionSimple1 :: Union SymInteger
unionSimple1 = mrgIfPropagatedStrategy "u1c" (return "u1a") (return "u1b")

unionSimple1Plus1 :: Union SymInteger
unionSimple1Plus1 =
  mrgIfPropagatedStrategy
    "u1c"
    (return $ "u1a" + 1)
    (return $ "u1b" + 1)

unionSimple2 :: Union SymInteger
unionSimple2 = mrgIfPropagatedStrategy "u2c" (return "u2a") (return "u2b")

unionSimple12Merged :: Union SymInteger
unionSimple12Merged =
  UMrg
    rootStrategy
    ( UnionSingle
        (symIte "u12c" (symIte "u1c" "u1a" "u1b") (symIte "u2c" "u2a" "u2b"))
    )

unionTests :: Test
unionTests =
  testGroup
    "Union"
    [ testCase "unionBase" $
        unionBase union12Merged @?= unionBase12Merged,
      testCase "isMerged" $ do
        isMerged union12 @?= False
        isMerged union12Merged @?= True,
      testCase "liftUnion & liftToMonadUnion" $ do
        let expected =
              mrgSingle (symIte "u1c" "u1a" "u1b") :: ExceptT () Union SymInteger
        liftUnion unionSimple1 @?= expected
        liftToMonadUnion unionSimple1 @?= expected,
      testCase "unionSize" $
        unionSize union12Merged @?= 2,
      testCase "unaryOp" $
        unionUnaryOp (+ 1) unionSimple1 .@?= unionSimple1Plus1,
      testCase "binOp" $ do
        let actual = unionBinOp (+) unionSimple1 unionSimple2
        let expected =
              mrgSingle (symIte "u1c" "u1a" "u1b" + symIte "u2c" "u2a" "u2b")
        actual .@?= expected,
      testCase "Mergeable & TryMerge" $
        tryMergeWithStrategy rootStrategy union12 @?= union12Merged,
      testCase "SimpleMerge" $
        mrgIte "u12c" union1 union2 @?= union12Merged,
      testCase "SymBranching" $ do
        let actual = mrgIfWithStrategy rootStrategy "u12c" union1 union2
        actual @?= union12Merged,
      testCase "SimpleMergeable1" $
        mrgIte1 "u12c" unionSimple1 unionSimple2 @?= unionSimple12Merged,
      testGroup
        "PlainUnion"
        [ testGroup
            "SingleView"
            [ testCase "is single" $ do
                let actual = singleView (tryMerge unionSimple1)
                let expected = Just (symIte "u1c" "u1a" "u1b")
                actual @?= expected,
              testCase "is not single" $
                singleView unionSimple1 @?= Nothing
            ],
          testGroup
            "IfView"
            [ testCase "is single" $ do
                let actual = ifView (tryMerge unionSimple1)
                let expected = Nothing
                actual @?= expected,
              testCase "is not single (unmerged)" $ do
                let actual = ifView unionSimple1
                let expected = Just ("u1c", return "u1a", return "u1b")
                actual @?= expected,
              testCase "is not single (merged)" $ do
                let actual = ifView (tryMerge union1)
                let expected =
                      Just ("u1c", mrgSingle $ Left "u1a", mrgSingle $ Right "u1b")
                actual @?= expected
            ]
        ],
      testGroup
        "Show"
        [ testCase "Merged" $ do
            let expected =
                  "{If (ite u12c u1c u2c) (Left (ite u12c u1a u2a)) "
                    ++ "(Right (ite u12c u1b u2b))}"
            show union12Merged @?= expected,
          testCase "Not merged" $ do
            let expected = "<If u1c u1a u1b>"
            show unionSimple1 @?= expected
        ],
      testGroup
        "Format"
        [ testCase "Merged" $ do
            format union12Merged
              `renderedAs` ( "{If (ite u12c u1c u2c) (Left (ite u12c u1a u2a)) "
                               <> "(Right (ite u12c u1b u2b))}"
                           )
            format union12Merged
              `compactRenderedAs` ( T.intercalate
                                      "\n"
                                      [ "{ If",
                                        "    ...",
                                        "    ( Left",
                                        "        ...",
                                        "    )",
                                        "    ( Right",
                                        "        ...",
                                        "    )",
                                        "}"
                                      ]
                                  ),
          testCase "Not merged" $ do
            format union1 `renderedAs` "<If u1c (Left u1a) (Right u1b)>"
        ],
      testGroup
        "Functor"
        [ testCase "fmap should work" $ do
            (+ 1) <$> unionSimple1 @?= unionSimple1Plus1
        ],
      testGroup
        "Applicative"
        [ testCase "pure should work" $
            (pure 1 :: Union Int) @?= UAny (UnionSingle 1),
          testCase "<*> should work" $
            pure (+ 1) <*> unionSimple1 @?= unionSimple1Plus1
        ],
      testGroup
        "Monad"
        [ testCase "return should work" $
            (return 1 :: Union Int) @?= UAny (UnionSingle 1),
          testCase ">>= should work" $
            (unionSimple1 >>= (\i -> return (i + 1))) @?= unionSimple1Plus1,
          testCase ">>= should propagate merge strategy" $ do
            let actual = unionSimple1 >>= (\i -> mrgSingle (i + 1))
            let expected = mrgSingle (symIte "u1c" ("u1a" + 1) ("u1b" + 1))
            actual @?= expected
        ],
      testCase "SymEq" $ do
        let actual = union1 .== union2
        let expected =
              (("u1c" :: SymBool) .== "u2c")
                .&& ( symIte
                        "u1c"
                        (("u1a" :: SymBool) .== "u2a")
                        (("u1b" :: SymInteger) .== "u2b")
                    )
        actual .@?= expected,
      testCase "SymOrd" $ do
        let actual = union1 .<= union2
        let expected =
              symIte
                (("u1c" :: SymBool) .== "u2c")
                ( symIte
                    "u1c"
                    (("u1a" :: SymBool) .<= "u2a")
                    (("u1b" :: SymInteger) .<= "u2b")
                )
                "u1c"
        actual .@?= expected,
      testCase "ToSym a (Union b)" $ do
        let actual = toSym True :: Union SymBool
        let expected = mrgSingle (con True)
        actual @?= expected,
      testCase "ToSym (Identity a) (Union b)" $ do
        let actual = toSym $ Identity True :: Union SymBool
        let expected = mrgSingle (con True)
        actual @?= expected,
      testCase "ToSym (Union a) (Union b)" $ do
        let actual = toSym (mrgSingle True :: Union Bool) :: Union SymBool
        let expected = return (con True)
        actual @?= expected,
      testCase "ToSym (Union Integer) SymInteger" $ do
        let actual = toSym (mrgIf "a" 1 2 :: Union Integer)
        let expected = symIte "a" 1 2 :: SymInteger
        actual @?= expected,
      testGroup
        "ToCon (Union a) b"
        [ testCase "Const" $ do
            let actual = mrgSingle (con True) :: Union SymBool
            let expected = Just True :: Maybe Bool
            toCon actual @?= expected,
          testCase "Not const" $ do
            let actual = mrgSingle "a" :: Union SymBool
            let expected = Nothing :: Maybe Bool
            toCon actual @?= expected
        ],
      testGroup
        "ToCon (Union a) (Identity b)"
        [ testCase "Const" $ do
            let actual = mrgSingle (con True) :: Union SymBool
            let expected = Just (Identity True) :: Maybe (Identity Bool)
            toCon actual @?= expected,
          testCase "Not const" $ do
            let actual = mrgSingle "a" :: Union SymBool
            let expected = Nothing :: Maybe (Identity Bool)
            toCon actual @?= expected
        ],
      testGroup
        "ToCon (Union a) (Union b)"
        [ testCase "Const" $ do
            let actual = mrgSingle (con True) :: Union SymBool
            let expected = Just (return True) :: Maybe (Union Bool)
            toCon actual @?= expected,
          testCase "Not const" $ do
            let actual = mrgSingle "a" :: Union SymBool
            let expected = Nothing :: Maybe (Union Bool)
            toCon actual @?= expected
        ],
      testGroup "EvalSym" $ do
        let model = buildModel ("a" ::= True, "b" ::= False, "c" ::= True)
        [ testCase "EmptyModel with no fill default" $ do
            let actual = evalSym False emptyModel (mrgSingle "a")
            let expected = mrgSingle "a" :: Union SymBool
            actual @?= expected,
          testCase "EmptyModel with filling default" $ do
            let actual = evalSym True emptyModel (mrgSingle "a")
            let expected = mrgSingle $ con False :: Union SymBool
            actual @?= expected,
          testCase "non-empty model, simple test" $ do
            let actual = evalSym False model (mrgSingle "a")
            let expected = mrgSingle $ con True :: Union SymBool
            actual @?= expected,
          testCase "non-empty model, complex test" $ do
            let actual =
                  evalSym
                    False
                    model
                    ( mrgIf
                        "d"
                        (mrgIf "a" (mrgSingle $ Left "b") (mrgSingle $ Right "e"))
                        (mrgSingle $ Right "f")
                    ) ::
                    Union (Either SymBool SymBool)
            let expected =
                  mrgIf "d" (mrgSingle $ Left (con False)) (mrgSingle $ Right "f")
            actual .@?= expected
          ],
      testCase "SubstSym" $ do
        let actual =
              substSym
                ("a" :: TypedSymbol Bool)
                "b"
                ( mrgIf "a" (return $ Left "a") (return $ Right "c") ::
                    Union (Either SymBool SymBool)
                )
        let expected = mrgIf "b" (return $ Left "b") (return $ Right "c")
        actual @?= expected,
      testCase "ExtractSym" $ do
        let actual = extractSym union1
        let expected =
              buildSymbolSet
                ( "u1c" :: TypedSymbol Bool,
                  "u1a" :: TypedSymbol Bool,
                  "u1b" :: TypedSymbol Integer
                )
        actual @?= expected,
      testGroup
        "Solvable"
        [ testCase "con" $ (con True :: Union SymBool) @?= mrgSingle (con True),
          testCase "sym" $ (ssym "a" :: Union SymBool) @?= mrgSingle (ssym "a"),
          testCase "isym" $
            (isym "a" 1 :: Union SymBool) @?= mrgSingle (isym "a" 1),
          testGroup
            "conView"
            [ testCase "is concrete" $ do
                let value =
                      mrgIfPropagatedStrategy
                        "a"
                        (return $ con True)
                        (return $ con True)
                conView (value :: Union SymBool) @?= Just True,
              testCase "not concrete" $
                conView (ssym "a" :: Union SymBool) @?= Nothing
            ]
        ],
      testCase "Function" $ do
        let f = mrgSingle (+ 1) :: Union (SymInteger -> SymInteger)
        f # 1 @?= 2
    ]
