{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <$>" #-}

module Grisette.Core.Control.Monad.UnionTests (unionTests) where

import Control.Monad.Except (ExceptT)
import Control.Monad.Identity (Identity (Identity))
import Data.Maybe (isNothing)
import qualified Data.Text as T
import Grisette
  ( AsKey (AsKey),
    EvalSym (evalSym),
    ExtractSym (extractSym),
    Function ((#)),
    ITEOp (symIte),
    IfViewResult (IfViewResult),
    LogicalOp ((.&&)),
    Mergeable (rootStrategy),
    ModelOps (emptyModel),
    ModelRep (buildModel),
    ModelValuePair ((::=)),
    PPrint (pformat),
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
    TypedAnySymbol,
    TypedConstantSymbol,
    UnionView (ifView, singleView),
    mrgIf,
    mrgIte1,
    mrgSingle,
    toUnionSym,
    tryMerge,
    unionToCon,
  )
import Grisette.Internal.Core.Control.Monad.Union
  ( Union (Union),
    isMerged,
    unionBase,
    unionBinOp,
    unionSize,
    unionUnaryOp,
  )
import Grisette.Internal.Core.Data.Class.AsKey (AsKey1 (AsKey1, getAsKey1))
import Grisette.Internal.Core.Data.Class.UnionView (liftToMonadUnion, liftUnion)
import Grisette.Internal.Core.Data.UnionBase
  ( UnionBase (UnionSingle),
    ifWithLeftMost,
  )
import Grisette.TestUtil.PrettyPrint (compactRenderedAs, renderedAs)
import Grisette.TestUtil.SymbolicAssertion ((.@?=))
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (assertBool, (@?=))

union1 :: AsKey1 Union (Either (AsKey SymBool) (AsKey SymInteger))
union1 = AsKey1 $ mrgIfPropagatedStrategy "u1c" (return $ Left "u1a") (return $ Right "u1b")

union2 :: AsKey1 Union (Either (AsKey SymBool) (AsKey SymInteger))
union2 = AsKey1 $ mrgIfPropagatedStrategy "u2c" (return $ Left "u2a") (return $ Right "u2b")

union12 :: AsKey1 Union (Either (AsKey SymBool) (AsKey SymInteger))
union12 = mrgIfPropagatedStrategy "u12c" union1 union2

unionBase12Merged :: UnionBase (Either (AsKey SymBool) (AsKey SymInteger))
unionBase12Merged =
  ifWithLeftMost
    True
    (symIte "u12c" "u1c" "u2c")
    (UnionSingle (Left (symIte "u12c" "u1a" "u2a")))
    (UnionSingle (Right (symIte "u12c" "u1b" "u2b")))

union12Merged :: AsKey1 Union (Either (AsKey SymBool) (AsKey SymInteger))
union12Merged = AsKey1 $ Union (Just rootStrategy) unionBase12Merged

unionSimple1 :: AsKey1 Union (AsKey SymInteger)
unionSimple1 = mrgIfPropagatedStrategy "u1c" (return "u1a") (return "u1b")

unionSimple1Plus1 :: AsKey1 Union (AsKey SymInteger)
unionSimple1Plus1 =
  mrgIfPropagatedStrategy
    "u1c"
    (return $ "u1a" + 1)
    (return $ "u1b" + 1)

unionSimple2 :: AsKey1 Union (AsKey SymInteger)
unionSimple2 = AsKey1 $ mrgIfPropagatedStrategy "u2c" (return "u2a") (return "u2b")

unionSimple12Merged :: AsKey1 Union (AsKey SymInteger)
unionSimple12Merged =
  AsKey1 $
    Union
      (Just rootStrategy)
      ( UnionSingle
          (symIte "u12c" (symIte "u1c" "u1a" "u1b") (symIte "u2c" "u2a" "u2b"))
      )

unionTests :: Test
unionTests =
  testGroup
    "Union"
    [ testCase "unionBase" $
        AsKey1 (unionBase (getAsKey1 union12Merged)) @?= AsKey1 unionBase12Merged,
      testCase "isMerged" $ do
        isMerged (getAsKey1 union12) @?= False
        isMerged (getAsKey1 union12Merged) @?= True,
      testCase "liftUnion & liftToMonadUnion" $ do
        let expected =
              mrgSingle (symIte "u1c" "u1a" "u1b") :: ExceptT () (AsKey1 Union) (AsKey SymInteger)
        liftUnion (getAsKey1 unionSimple1) @?= expected
        liftToMonadUnion (getAsKey1 unionSimple1) @?= expected,
      testCase "unionSize" $
        unionSize (getAsKey1 union12Merged) @?= 2,
      testCase "unaryOp" $
        unionUnaryOp (+ 1) (getAsKey1 unionSimple1) .@?= (getAsKey1 unionSimple1Plus1),
      testCase "binOp" $ do
        let actual = unionBinOp (+) (getAsKey1 unionSimple1) (getAsKey1 unionSimple2)
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
        "UnionView"
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
                assertBool "ifView should return Nothing" (isNothing actual),
              testCase "is not single (unmerged)" $ do
                let Just (IfViewResult cond left right) = ifView unionSimple1
                let expected =
                      ( "u1c",
                        return "u1a",
                        return "u1b"
                      )
                (AsKey cond, left, right) @?= expected,
              testCase "is not single (merged)" $ do
                let Just (IfViewResult cond left right) = ifView (tryMerge union1)
                let expected =
                      ( "u1c",
                        mrgSingle $ Left "u1a",
                        mrgSingle $ Right "u1b"
                      )
                (AsKey cond, left, right) @?= expected
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
        "PPrint"
        [ testCase "Merged" $ do
            pformat union12Merged
              `renderedAs` ( "{If (ite u12c u1c u2c) (Left (ite u12c u1a u2a)) "
                               <> "(Right (ite u12c u1b u2b))}"
                           )
            pformat union12Merged
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
            pformat union1 `renderedAs` "<If u1c (Left u1a) (Right u1b)>"
        ],
      testGroup
        "Functor"
        [ testCase "fmap should work" $ do
            (+ 1) <$> unionSimple1 @?= unionSimple1Plus1
        ],
      testGroup
        "Applicative"
        [ testCase "pure should work" $
            (pure 1 :: AsKey1 Union Int) @?= AsKey1 (Union Nothing (UnionSingle 1)),
          testCase "<*> should work" $
            pure (+ 1) <*> unionSimple1 @?= unionSimple1Plus1
        ],
      testGroup
        "Monad"
        [ testCase "return should work" $
            (return 1 :: AsKey1 Union Int) @?= AsKey1 (Union Nothing (UnionSingle 1)),
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
      testCase "ToSym a (Union b) should be done with toUnionSym" $ do
        let actual = toUnionSym True :: Union (AsKey SymBool)
        let expected = mrgSingle (con True)
        AsKey actual @?= AsKey expected,
      testCase "toUnionSym (Identity a) (Union b)" $ do
        let actual = toUnionSym $ Identity True :: Union (AsKey SymBool)
        let expected = mrgSingle (con True)
        AsKey actual @?= AsKey expected,
      testCase "toUnionSym (Union a) (Union b)" $ do
        let actual = toUnionSym (mrgSingle True :: Union Bool) :: Union (AsKey SymBool)
        let expected = mrgSingle (con True)
        AsKey actual @?= AsKey expected,
      testCase "ToSym (Identity a) (Union b)" $ do
        let actual = toSym $ Identity True :: Union (AsKey SymBool)
        let expected = return $ con True
        AsKey actual @?= AsKey expected,
      testCase "ToSym (Union a) (Union b)" $ do
        let actual = toSym (mrgSingle True :: Union Bool) :: Union (AsKey SymBool)
        let expected = return $ con True
        AsKey actual @?= AsKey expected,
      testCase "ToSym (Union Integer) SymInteger" $ do
        let actual = toSym (mrgIf "a" 1 2 :: Union Integer)
        let expected = symIte "a" 1 2 :: SymInteger
        AsKey actual @?= AsKey expected,
      testGroup
        "ToCon (Union a) b should be done with unionToCon"
        [ testCase "Const" $ do
            let actual = mrgSingle (con True) :: Union SymBool
            let expected = Just True :: Maybe Bool
            unionToCon actual @?= expected,
          testCase "Not const" $ do
            let actual = mrgSingle "a" :: Union SymBool
            let expected = Nothing :: Maybe Bool
            unionToCon actual @?= expected
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
            let expected = Just (return True) :: Maybe (AsKey1 Union Bool)
            toCon actual @?= expected,
          testCase "Not const" $ do
            let actual = mrgSingle "a" :: Union SymBool
            let expected = Nothing :: Maybe (Union Bool)
            (AsKey <$> (toCon actual)) @?= (AsKey <$> expected)
        ],
      testGroup "EvalSym" $ do
        let model = buildModel ("a" ::= True, "b" ::= False, "c" ::= True)
        [ testCase "EmptyModel with no fill default" $ do
            let actual = evalSym False emptyModel (mrgSingle "a")
            let expected = mrgSingle "a" :: AsKey1 Union (AsKey SymBool)
            actual @?= expected,
          testCase "EmptyModel with filling default" $ do
            let actual = evalSym True emptyModel (mrgSingle "a")
            let expected = mrgSingle $ con False :: AsKey1 Union (AsKey SymBool)
            actual @?= expected,
          testCase "non-empty model, simple test" $ do
            let actual = evalSym False model (mrgSingle "a")
            let expected = mrgSingle $ con True :: AsKey1 Union (AsKey SymBool)
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
                    AsKey1 Union (Either (AsKey SymBool) (AsKey SymBool))
            let expected =
                  mrgIf "d" (mrgSingle $ Left (con False)) (mrgSingle $ Right "f")
            actual .@?= expected
          ],
      testCase "SubstSym" $ do
        let actual =
              substSym
                ("a" :: TypedConstantSymbol Bool)
                "b"
                ( mrgIf "a" (return $ Left "a") (return $ Right "c") ::
                    AsKey1 Union (Either (AsKey SymBool) (AsKey SymBool))
                )
        let expected = mrgIf "b" (return $ Left "b") (return $ Right "c")
        actual @?= expected,
      testCase "ExtractSym" $ do
        let actual = extractSym union1
        let expected =
              buildSymbolSet
                ( "u1c" :: TypedAnySymbol Bool,
                  "u1a" :: TypedAnySymbol Bool,
                  "u1b" :: TypedAnySymbol Integer
                )
        actual @?= expected,
      testGroup
        "Solvable"
        [ testCase "con" $ (con True :: AsKey1 Union (AsKey SymBool)) @?= mrgSingle (con True),
          testCase "sym" $ (ssym "a" :: AsKey1 Union (AsKey SymBool)) @?= mrgSingle (ssym "a"),
          testCase "isym" $
            (isym "a" 1 :: AsKey1 Union (AsKey SymBool)) @?= mrgSingle (isym "a" 1),
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
        let f = mrgSingle (+ 1) :: AsKey1 Union (AsKey SymInteger -> AsKey SymInteger)
        f # 1 @?= 2
    ]
