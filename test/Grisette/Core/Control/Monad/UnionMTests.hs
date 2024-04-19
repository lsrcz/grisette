{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <$>" #-}

module Grisette.Core.Control.Monad.UnionMTests (unionMTests) where

import Control.Monad.Except (ExceptT)
import qualified Data.Text as T
import Grisette
  ( EvaluateSym (evaluateSym),
    ExtractSymbolics (extractSymbolics),
    Function ((#)),
    GPretty (gpretty),
    ITEOp (symIte),
    LogicalOp ((.&&)),
    Mergeable (rootStrategy),
    ModelOps (emptyModel),
    ModelRep (buildModel),
    ModelValuePair ((::=)),
    PlainUnion (ifView, singleView),
    SEq ((.==)),
    SOrd ((.<=)),
    SimpleMergeable (mrgIte),
    Solvable (con, conView, isym, ssym),
    SubstituteSym (substituteSym),
    SymBool,
    SymInteger,
    SymbolSetRep (buildSymbolSet),
    ToCon (toCon),
    ToSym (toSym),
    TryMerge (tryMergeWithStrategy),
    TypedSymbol,
    UnionMergeable1 (mrgIfPropagatedStrategy, mrgIfWithStrategy),
    mrgIf,
    mrgIte1,
    mrgSingle,
    tryMerge,
  )
import Grisette.Internal.Core.Control.Monad.UnionM
  ( UnionM (UAny, UMrg),
    isMerged,
    liftToMonadUnion,
    liftUnionM,
    underlyingUnion,
    unionMBinOp,
    unionMUnaryOp,
    unionSize,
  )
import Grisette.Internal.Core.Data.Union (Union (UnionSingle), ifWithLeftMost)
import Grisette.TestUtil.PrettyPrint (compactRenderedAs, renderedAs)
import Grisette.TestUtil.SymbolicAssertion ((.@?=))
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit ((@?=))

unionM1 :: UnionM (Either SymBool SymInteger)
unionM1 = mrgIfPropagatedStrategy "u1c" (return $ Left "u1a") (return $ Right "u1b")

unionM2 :: UnionM (Either SymBool SymInteger)
unionM2 = mrgIfPropagatedStrategy "u2c" (return $ Left "u2a") (return $ Right "u2b")

unionM12 :: UnionM (Either SymBool SymInteger)
unionM12 = mrgIfPropagatedStrategy "u12c" unionM1 unionM2

union12Merged :: Union (Either SymBool SymInteger)
union12Merged =
  ifWithLeftMost
    True
    (symIte "u12c" "u1c" "u2c")
    (UnionSingle (Left (symIte "u12c" "u1a" "u2a")))
    (UnionSingle (Right (symIte "u12c" "u1b" "u2b")))

unionM12Merged :: UnionM (Either SymBool SymInteger)
unionM12Merged = UMrg rootStrategy union12Merged

unionMSimple1 :: UnionM SymInteger
unionMSimple1 = mrgIfPropagatedStrategy "u1c" (return "u1a") (return "u1b")

unionMSimple1Plus1 :: UnionM SymInteger
unionMSimple1Plus1 =
  mrgIfPropagatedStrategy
    "u1c"
    (return $ "u1a" + 1)
    (return $ "u1b" + 1)

unionMSimple2 :: UnionM SymInteger
unionMSimple2 = mrgIfPropagatedStrategy "u2c" (return "u2a") (return "u2b")

unionMSimple12Merged :: UnionM SymInteger
unionMSimple12Merged =
  UMrg
    rootStrategy
    ( UnionSingle
        (symIte "u12c" (symIte "u1c" "u1a" "u1b") (symIte "u2c" "u2a" "u2b"))
    )

unionMTests :: Test
unionMTests =
  testGroup
    "UnionM"
    [ testCase "underlyingUnion" $
        underlyingUnion unionM12Merged @?= union12Merged,
      testCase "isMerged" $ do
        isMerged unionM12 @?= False
        isMerged unionM12Merged @?= True,
      testCase "liftUnionM & liftToMonadUnion" $ do
        let expected =
              mrgSingle (symIte "u1c" "u1a" "u1b") :: ExceptT () UnionM SymInteger
        liftUnionM unionMSimple1 @?= expected
        liftToMonadUnion unionMSimple1 @?= expected,
      testCase "unionSize" $
        unionSize unionM12Merged @?= 2,
      testCase "unaryOp" $
        unionMUnaryOp (+ 1) unionMSimple1 .@?= unionMSimple1Plus1,
      testCase "binOp" $ do
        let actual = unionMBinOp (+) unionMSimple1 unionMSimple2
        let expected =
              mrgSingle (symIte "u1c" "u1a" "u1b" + symIte "u2c" "u2a" "u2b")
        actual .@?= expected,
      testCase "Mergeable & TryMerge" $
        tryMergeWithStrategy rootStrategy unionM12 @?= unionM12Merged,
      testCase "SimpleMerge" $
        mrgIte "u12c" unionM1 unionM2 @?= unionM12Merged,
      testCase "UnionMergeable1" $ do
        let actual = mrgIfWithStrategy rootStrategy "u12c" unionM1 unionM2
        actual @?= unionM12Merged,
      testCase "SimpleMergeable1" $
        mrgIte1 "u12c" unionMSimple1 unionMSimple2 @?= unionMSimple12Merged,
      testGroup
        "PlainUnion"
        [ testGroup
            "SingleView"
            [ testCase "is single" $ do
                let actual = singleView (tryMerge unionMSimple1)
                let expected = Just (symIte "u1c" "u1a" "u1b")
                actual @?= expected,
              testCase "is not single" $
                singleView unionMSimple1 @?= Nothing
            ],
          testGroup
            "IfView"
            [ testCase "is single" $ do
                let actual = ifView (tryMerge unionMSimple1)
                let expected = Nothing
                actual @?= expected,
              testCase "is not single (unmerged)" $ do
                let actual = ifView unionMSimple1
                let expected = Just ("u1c", return "u1a", return "u1b")
                actual @?= expected,
              testCase "is not single (merged)" $ do
                let actual = ifView (tryMerge unionM1)
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
            show unionM12Merged @?= expected,
          testCase "Not merged" $ do
            let expected = "<If u1c u1a u1b>"
            show unionMSimple1 @?= expected
        ],
      testGroup
        "GPretty"
        [ testCase "Merged" $ do
            gpretty unionM12Merged
              `renderedAs` ( "{If (ite u12c u1c u2c) (Left (ite u12c u1a u2a)) "
                               <> "(Right (ite u12c u1b u2b))}"
                           )
            gpretty unionM12Merged
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
            gpretty unionM1 `renderedAs` "<If u1c (Left u1a) (Right u1b)>"
        ],
      testGroup
        "Functor"
        [ testCase "fmap should work" $ do
            (+ 1) <$> unionMSimple1 @?= unionMSimple1Plus1
        ],
      testGroup
        "Applicative"
        [ testCase "pure should work" $
            (pure 1 :: UnionM Int) @?= UAny (UnionSingle 1),
          testCase "<*> should work" $
            pure (+ 1) <*> unionMSimple1 @?= unionMSimple1Plus1
        ],
      testGroup
        "Monad"
        [ testCase "return should work" $
            (return 1 :: UnionM Int) @?= UAny (UnionSingle 1),
          testCase ">>= should work" $
            (unionMSimple1 >>= (\i -> return (i + 1))) @?= unionMSimple1Plus1,
          testCase ">>= should propagate merge strategy" $ do
            let actual = unionMSimple1 >>= (\i -> mrgSingle (i + 1))
            let expected = mrgSingle (symIte "u1c" ("u1a" + 1) ("u1b" + 1))
            actual @?= expected
        ],
      testCase "SEq" $ do
        let actual = unionM1 .== unionM2
        let expected =
              (("u1c" :: SymBool) .== "u2c")
                .&& ( symIte
                        "u1c"
                        (("u1a" :: SymBool) .== "u2a")
                        (("u1b" :: SymInteger) .== "u2b")
                    )
        actual .@?= expected,
      testCase "SOrd" $ do
        let actual = unionM1 .<= unionM2
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
      testCase "ToSym a (UnionM b)" $ do
        let actual = toSym True :: UnionM SymBool
        let expected = mrgSingle (con True)
        actual @?= expected,
      testCase "ToSym (UnionM a) (UnionM b)" $ do
        let actual = toSym (mrgSingle True :: UnionM Bool) :: UnionM SymBool
        let expected = mrgSingle (con True)
        actual @?= expected,
      testCase "ToSym (UnionM Integer) SymInteger" $ do
        let actual = toSym (mrgIf "a" 1 2 :: UnionM Integer)
        let expected = symIte "a" 1 2 :: SymInteger
        actual @?= expected,
      testGroup
        "ToCon (UnionM a) b"
        [ testCase "Const" $ do
            let actual = mrgSingle (con True) :: UnionM SymBool
            let expected = Just True :: Maybe Bool
            toCon actual @?= expected,
          testCase "Not const" $ do
            let actual = mrgSingle "a" :: UnionM SymBool
            let expected = Nothing :: Maybe Bool
            toCon actual @?= expected
        ],
      testGroup
        "ToCon (UnionM a) (UnionM b)"
        [ testCase "Const" $ do
            let actual = mrgSingle (con True) :: UnionM SymBool
            let expected = Just (mrgSingle True) :: Maybe (UnionM Bool)
            toCon actual @?= expected,
          testCase "Not const" $ do
            let actual = mrgSingle "a" :: UnionM SymBool
            let expected = Nothing :: Maybe (UnionM Bool)
            toCon actual @?= expected
        ],
      testGroup "EvaluateSym" $ do
        let model = buildModel ("a" ::= True, "b" ::= False, "c" ::= True)
        [ testCase "EmptyModel with no fill default" $ do
            let actual = evaluateSym False emptyModel (return "a")
            let expected = mrgSingle "a" :: UnionM SymBool
            actual @?= expected,
          testCase "EmptyModel with filling default" $ do
            let actual = evaluateSym True emptyModel (return "a")
            let expected = mrgSingle $ con False :: UnionM SymBool
            actual @?= expected,
          testCase "non-empty model, simple test" $ do
            let actual = evaluateSym False model (return "a")
            let expected = mrgSingle $ con True :: UnionM SymBool
            actual @?= expected,
          testCase "non-empty model, complex test" $ do
            let actual =
                  evaluateSym
                    False
                    model
                    ( mrgIf
                        "d"
                        (mrgIf "a" (mrgSingle $ Left "b") (mrgSingle $ Right "e"))
                        (mrgSingle $ Right "f")
                    ) ::
                    UnionM (Either SymBool SymBool)
            let expected =
                  mrgIf "d" (mrgSingle $ Left (con False)) (mrgSingle $ Right "f")
            actual .@?= expected
          ],
      testCase "SubstituteSym" $ do
        let actual =
              substituteSym
                ("a" :: TypedSymbol Bool)
                "b"
                ( mrgIf "a" (return $ Left "a") (return $ Right "c") ::
                    UnionM (Either SymBool SymBool)
                )
        let expected = mrgIf "b" (return $ Left "b") (return $ Right "c")
        actual @?= expected,
      testCase "ExtractSymbolics" $ do
        let actual = extractSymbolics unionM1
        let expected =
              buildSymbolSet
                ( "u1c" :: TypedSymbol Bool,
                  "u1a" :: TypedSymbol Bool,
                  "u1b" :: TypedSymbol Integer
                )
        actual @?= expected,
      testGroup
        "Solvable"
        [ testCase "con" $ (con True :: UnionM SymBool) @?= mrgSingle (con True),
          testCase "sym" $ (ssym "a" :: UnionM SymBool) @?= mrgSingle (ssym "a"),
          testCase "isym" $
            (isym "a" 1 :: UnionM SymBool) @?= mrgSingle (isym "a" 1),
          testGroup
            "conView"
            [ testCase "is concrete" $ do
                let value =
                      mrgIfPropagatedStrategy
                        "a"
                        (return $ con True)
                        (return $ con True)
                conView (value :: UnionM SymBool) @?= Just True,
              testCase "not concrete" $
                conView (ssym "a" :: UnionM SymBool) @?= Nothing
            ]
        ],
      testCase "Function" $ do
        let f = mrgSingle (+ 1) :: UnionM (SymInteger -> SymInteger)
        f # 1 @?= 2
    ]
