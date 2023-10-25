{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Grisette.Core.Control.Monad.UnionMTests (unionMTests) where

import qualified Data.ByteString as B
import qualified Data.HashMap.Lazy as ML
import Grisette.Core.BuiltinUnionWrappers (mrgLeft, mrgRight)
import Grisette.Core.Control.Monad.UnionM
  ( UnionM,
    isMerged,
    underlyingUnion,
    unionSize,
  )
import Grisette.Core.Data.Class.Bool
  ( ITEOp (ites),
    LogicalOp (implies, nots, xors, (&&~), (||~)),
    SEq ((==~)),
  )
import Grisette.Core.Data.Class.Evaluate
  ( EvaluateSym (evaluateSym),
  )
import Grisette.Core.Data.Class.ExtractSymbolics
  ( ExtractSymbolics (extractSymbolics),
  )
import Grisette.Core.Data.Class.Function (Function ((#)))
import Grisette.Core.Data.Class.GenSym
  ( ListSpec (ListSpec),
    choose,
    genSym,
    genSymSimple,
  )
import Grisette.Core.Data.Class.ModelOps
  ( ModelOps (emptyModel),
    ModelRep (buildModel),
    SymbolSetRep (buildSymbolSet),
  )
import Grisette.Core.Data.Class.SOrd
  ( SOrd (symCompare, (<=~), (<~), (>=~), (>~)),
  )
import Grisette.Core.Data.Class.SimpleMergeable
  ( SimpleMergeable (mrgIte),
    UnionLike (single, unionIf),
    UnionPrjOp (ifView, leftMost, singleView, toGuardedList),
    merge,
    mrgIf,
    mrgIte1,
    mrgSingle,
    (#~),
    pattern If,
    pattern Single,
  )
import Grisette.Core.Data.Class.Solvable (Solvable (con, conView, isym, ssym))
import Grisette.Core.Data.Class.Substitute (SubstituteSym (substituteSym))
import Grisette.Core.Data.Class.ToCon (ToCon (toCon))
import Grisette.Core.Data.Class.ToSym (ToSym (toSym))
import Grisette.Core.Data.Union (Union (UnionIf, UnionSingle))
import Grisette.IR.SymPrim.Data.Prim.InternedTerm.Term (TypedSymbol)
import Grisette.IR.SymPrim.Data.Prim.Model
  ( ModelValuePair ((::=)),
  )
import Grisette.IR.SymPrim.Data.SymPrim (SymBool)
import Grisette.TestUtil.SymbolicAssertion ((@?=~))
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (assertFailure, (@?=))

unionMTests :: Test
unionMTests =
  testGroup
    "UnionM"
    [ testCase "Mergeable" $
        do
          let r =
                ( mrgIf
                    "a"
                    ( mrgSingle
                        ( mrgIf
                            "b"
                            (mrgSingle $ Left "c")
                            (mrgSingle $ Right "d")
                        )
                    )
                    ( mrgSingle
                        ( mrgIf
                            "e"
                            (mrgSingle $ Left "f")
                            (mrgSingle $ Right "g")
                        )
                    ) ::
                    UnionM (UnionM (Either SymBool SymBool))
                )
          isMerged r @?= True
          underlyingUnion (underlyingUnion <$> r)
            @?= UnionSingle
              ( UnionIf
                  (Left $ ites "a" "c" "f")
                  True
                  (ites "a" "b" "e")
                  (UnionSingle $ Left $ ites "a" "c" "f")
                  (UnionSingle $ Right $ ites "a" "d" "g")
              ),
      testCase "SimpleMergeable" $ do
        let l :: UnionM (Either SymBool SymBool) =
              mrgIf "b" (mrgSingle $ Left "c") (mrgSingle $ Right "d")
        let r = mrgIf "e" (mrgSingle $ Left "f") (mrgSingle $ Right "g")
        let res = mrgIte "a" l r
        let ref =
              UnionIf
                (Left $ ites "a" "c" "f")
                True
                (ites "a" "b" "e")
                (UnionSingle $ Left $ ites "a" "c" "f")
                (UnionSingle $ Right $ ites "a" "d" "g")
        isMerged res @?= True
        underlyingUnion res @?= ref,
      testCase "SimpleMergeable1" $ do
        let l :: UnionM SymBool = mrgIf "b" (mrgSingle "c") (mrgSingle "d")
        let r :: UnionM SymBool = mrgIf "e" (mrgSingle "f") (mrgSingle "g")
        let res = mrgIte1 "a" l r
        isMerged res @?= True
        underlyingUnion res
          @?= UnionSingle
            ( ites
                "a"
                (ites "b" "c" "d")
                (ites "e" "f" "g")
            ),
      testGroup
        "Functor"
        [ testCase "fmap should work but would strip mergeable knowledge" $ do
            let x :: UnionM Integer =
                  (+ 1) <$> mrgIf "a" (mrgSingle 1) (mrgSingle 2)
            x @?= unionIf "a" (return 2) (return 3)
        ],
      testGroup
        "Applicative"
        [ testCase "pure should work but won't give us mergeable knowledge" $
            (pure 1 :: UnionM Integer) @?= single 1,
          testCase "<*> should work but won't give us mergeable knowledge" $ do
            let f :: UnionM (Integer -> Integer) =
                  mrgIf "a" (mrgSingle id) (mrgSingle (+ 1))
            let v :: UnionM Integer = mrgIf "b" (mrgSingle 1) (mrgSingle 3)
            f
              <*> v
              @?= unionIf
                "a"
                (unionIf "b" (single 1) (single 3))
                (unionIf "b" (single 2) (single 4))
        ],
      testGroup
        "Monad"
        [ testCase "return should work but won't give us mergeable knowledge" $
            (pure 1 :: UnionM Integer) @?= single 1,
          testCase ">>= should work and keeps mergeable knowledge" $ do
            let v :: UnionM Integer = mrgIf "a" (mrgSingle 0) (mrgSingle 1)
            let f :: Integer -> UnionM Integer = \i ->
                  mrgIf "b" (mrgSingle $ i + 1) (mrgSingle $ i + 3)
            (v >>= f)
              @?= mrgIf
                "a"
                (mrgIf "b" (mrgSingle 1) (mrgSingle 3))
                (mrgIf "b" (mrgSingle 2) (mrgSingle 4))
        ],
      testGroup
        "UnionOp"
        [ testCase "single" $ do
            let r1 :: UnionM SymBool = single "a"
            isMerged r1 @?= False
            underlyingUnion r1 @?= UnionSingle "a",
          testGroup
            "unionIf"
            [ testCase "unionIf should work when no merged" $ do
                let r1 :: UnionM SymBool = unionIf "a" (single "b") (single "c")
                isMerged r1 @?= False
                underlyingUnion r1
                  @?= UnionIf "b" False "a" (UnionSingle "b") (UnionSingle "c"),
              testCase
                "unionIf should propagate and merge the results when some branch merged"
                $ do
                  let r1 :: UnionM SymBool =
                        unionIf "a" (mrgSingle "b") (single "c")
                  isMerged r1 @?= True
                  underlyingUnion r1 @?= UnionSingle (ites "a" "b" "c")
                  let r2 :: UnionM SymBool =
                        unionIf "a" (single "b") (mrgSingle "c")
                  isMerged r2 @?= True
                  underlyingUnion r2 @?= UnionSingle (ites "a" "b" "c")
                  let r3 :: UnionM SymBool =
                        unionIf "a" (mrgSingle "b") (mrgSingle "c")
                  isMerged r3 @?= True
                  underlyingUnion r3 @?= UnionSingle (ites "a" "b" "c")
            ],
          testCase "singleView should work" $ do
            singleView (single "a" :: UnionM SymBool) @?= Just "a"
            singleView (mrgSingle "a" :: UnionM SymBool) @?= Just "a"
            singleView
              ( unionIf "a" (single $ Left "b") (single $ Right "c") ::
                  UnionM (Either SymBool SymBool)
              )
              @?= Nothing
            case (single "a" :: UnionM SymBool) of
              Single r -> r @?= "a"
              _ -> assertFailure "Single match failed"
            case (mrgSingle "a" :: UnionM SymBool) of
              Single r -> r @?= "a"
              _ -> assertFailure "Single match failed"
            case ( unionIf "a" (single $ Left "b") (single $ Right "c") ::
                     UnionM (Either SymBool SymBool)
                 ) of
              Single _ -> assertFailure "Single match failed"
              _ -> return (),
          testCase "ifView should work" $ do
            let r1 :: UnionM (Either SymBool SymBool) =
                  unionIf "a" (single $ Left "b") (single $ Right "c")
            let r2 :: UnionM (Either SymBool SymBool) =
                  mrgIf "a" (mrgSingle $ Left "b") (mrgSingle $ Right "c")
            ifView r1 @?= Just ("a", single $ Left "b", single $ Right "c")
            ifView r2
              @?= Just ("a", mrgSingle $ Left "b", mrgSingle $ Right "c")
            ifView (single "a" :: UnionM SymBool) @?= Nothing
            case r1 of
              If c l r -> do
                c @?= "a"
                l @?= single (Left "b")
                r @?= single (Right "c")
              _ -> assertFailure "Single match failed"
            case r2 of
              If c l r -> do
                c @?= "a"
                l @?= mrgSingle (Left "b")
                r @?= mrgSingle (Right "c")
              _ -> assertFailure "Single match failed"
            case single "a" :: UnionM SymBool of
              If {} -> assertFailure "Single match failed"
              _ -> return (),
          testCase "leftMost should work" $ do
            leftMost (single "a" :: UnionM SymBool) @?= "a"
            leftMost (mrgSingle "a" :: UnionM SymBool) @?= "a"
            let r1 :: UnionM (Either SymBool SymBool) =
                  unionIf "a" (single $ Left "b") (single $ Right "c")
            let r2 :: UnionM (Either SymBool SymBool) =
                  mrgIf "a" (mrgSingle $ Left "b") (mrgSingle $ Right "c")
            leftMost r1 @?= Left "b"
            leftMost r2 @?= Left "b",
          testCase "toGuardedList should work" $ do
            let actual =
                  toGuardedList
                    ( mrgIf "a" (single 1) (mrgIf "b" (single 2) (single 3)) ::
                        UnionM Integer
                    )
            let expected =
                  [ ("a", 1),
                    (nots "a" &&~ "b", 2),
                    (nots "a" &&~ nots "b", 3)
                  ]
            actual @?=~ expected
        ],
      testGroup
        "MonadUnion"
        [ testCase "merge should work" $ do
            let r1 :: UnionM SymBool =
                  merge (unionIf "a" (single "b") (single "c"))
            isMerged r1 @?= True
            underlyingUnion r1 @?= UnionSingle (ites "a" "b" "c"),
          testCase "mrgSingle should work" $ do
            let r1 :: UnionM SymBool = mrgSingle "a"
            isMerged r1 @?= True
            underlyingUnion r1 @?= UnionSingle "a",
          testGroup
            "mrgIf should work"
            [ testCase "mrgIf should perform lazy evaluation" $ do
                (mrgIf (con True) (mrgSingle "a") undefined :: UnionM SymBool)
                  @?= mrgSingle "a"
                (mrgIf (con False) undefined (mrgSingle "a") :: UnionM SymBool)
                  @?= mrgSingle "a",
              testCase "mrgIf should work" $
                (mrgIf "a" (single "b") (single "c") :: UnionM SymBool)
                  @?= merge (unionIf "a" (single "b") (single "c"))
            ]
        ],
      let a :: SymBool = "a"
          b :: SymBool = "b"
          c :: SymBool = "c"
          d :: SymBool = "d"
          e :: SymBool = "e"
          f :: SymBool = "f"
          g1 :: UnionM (Either SymBool SymBool) =
            mrgIf a (mrgSingle $ Left b) (mrgSingle $ Right c)
          g2 :: UnionM (Either SymBool SymBool) =
            mrgIf d (mrgSingle $ Left e) (mrgSingle $ Right f)
       in testGroup
            "SEq"
            [ testCase "Single/Single" $
                (mrgSingle a :: UnionM SymBool)
                  ==~ mrgSingle b
                  @?= (a ==~ b),
              testCase "If/Single" $ do
                g1
                  ==~ mrgSingle (Left d)
                  @?= ites a (b ==~ d) (con False)
                g1
                  ==~ mrgSingle (Right d)
                  @?= ites a (con False) (c ==~ d),
              testCase "Single/If" $ do
                mrgSingle (Left d)
                  ==~ g1
                  @?= ites a (d ==~ b) (con False)
                mrgSingle (Right d)
                  ==~ g1
                  @?= ites a (con False) (d ==~ c),
              testCase "If/If" $
                g1
                  ==~ g2
                  @?= ites
                    a
                    (ites d (b ==~ e) (con False))
                    (ites d (con False) (c ==~ f))
            ],
      let a :: SymBool = "a"
          b :: SymBool = "b"
          c :: SymBool = "c"
          d :: SymBool = "d"
          e :: SymBool = "e"
          f :: SymBool = "f"

          g1 :: UnionM (Either SymBool SymBool) =
            mrgIf a (mrgSingle $ Left b) (mrgSingle $ Right c)
          g2 :: UnionM (Either SymBool SymBool) =
            mrgIf d (mrgSingle $ Left e) (mrgSingle $ Right f)
       in testGroup
            "SOrd"
            [ testCase "Single/Single" $ do
                (mrgSingle a :: UnionM SymBool)
                  <=~ mrgSingle b
                  @?= (a <=~ b :: SymBool)
                (mrgSingle a :: UnionM SymBool)
                  <~ mrgSingle b
                  @?= (a <~ b :: SymBool)
                (mrgSingle a :: UnionM SymBool)
                  >=~ mrgSingle b
                  @?= (a >=~ b :: SymBool)
                (mrgSingle a :: UnionM SymBool)
                  >~ mrgSingle b
                  @?= (a >~ b :: SymBool)
                (mrgSingle a :: UnionM SymBool)
                  `symCompare` mrgSingle b
                  @?= (a `symCompare` b :: UnionM Ordering),
              testCase "If/Single" $ do
                g1
                  <=~ mrgSingle (Left d)
                  @?= ites a (b <=~ d) (con False)
                g1
                  <~ mrgSingle (Left d)
                  @?= ites a (b <~ d) (con False)
                g1
                  >=~ mrgSingle (Left d)
                  @?= ites a (b >=~ d) (con True)
                g1
                  >~ mrgSingle (Left d)
                  @?= ites a (b >~ d) (con True)

                g1
                  `symCompare` mrgSingle (Left d)
                  @?= ( mrgIf a (b `symCompare` d) (mrgSingle GT) ::
                          UnionM Ordering
                      )

                g1
                  <=~ mrgSingle (Right d)
                  @?= ites a (con True) (c <=~ d)
                g1
                  <~ mrgSingle (Right d)
                  @?= ites a (con True) (c <~ d)
                g1
                  >=~ mrgSingle (Right d)
                  @?= ites a (con False) (c >=~ d)
                g1
                  >~ mrgSingle (Right d)
                  @?= ites a (con False) (c >~ d)

                g1
                  `symCompare` mrgSingle (Right d)
                  @?= ( mrgIf a (mrgSingle LT) (c `symCompare` d) ::
                          UnionM Ordering
                      ),
              testCase "Single/If" $ do
                mrgSingle (Left d)
                  <=~ g1
                  @?= ites a (d <=~ b) (con True)
                mrgSingle (Left d)
                  <~ g1
                  @?= ites a (d <~ b) (con True)
                mrgSingle (Left d)
                  >=~ g1
                  @?= ites a (d >=~ b) (con False)
                mrgSingle (Left d)
                  >~ g1
                  @?= ites a (d >~ b) (con False)

                mrgSingle (Left d)
                  `symCompare` g1
                  @?= ( mrgIf a (d `symCompare` b) (mrgSingle LT) ::
                          UnionM Ordering
                      )

                mrgSingle (Right d)
                  <=~ g1
                  @?= ites a (con False) (d <=~ c)
                mrgSingle (Right d)
                  <~ g1
                  @?= ites a (con False) (d <~ c)
                mrgSingle (Right d)
                  >=~ g1
                  @?= ites a (con True) (d >=~ c)
                mrgSingle (Right d)
                  >~ g1
                  @?= ites a (con True) (d >~ c)

                mrgSingle (Right d)
                  `symCompare` g1
                  @?= ( mrgIf a (mrgSingle GT) (d `symCompare` c) ::
                          UnionM Ordering
                      ),
              testCase "If/If" $ do
                g1
                  <=~ g2
                  @?= ites
                    a
                    (ites d (b <=~ e) (con True))
                    (ites d (con False) (c <=~ f))
                g1
                  <~ g2
                  @?= ites
                    a
                    (ites d (b <~ e) (con True))
                    (ites d (con False) (c <~ f))
                g1
                  >=~ g2
                  @?= ites
                    a
                    (ites d (b >=~ e) (con False))
                    (ites d (con True) (c >=~ f))
                g1
                  >~ g2
                  @?= ites
                    a
                    (ites d (b >~ e) (con False))
                    (ites d (con True) (c >~ f))
                g1
                  `symCompare` g2
                  @?= ( mrgIf
                          a
                          (mrgIf d (b `symCompare` e) (mrgSingle LT))
                          (mrgIf d (mrgSingle GT) (c `symCompare` f)) ::
                          UnionM Ordering
                      )
            ],
      testGroup
        "ToSym"
        [ testCase "From single" $
            (toSym True :: UnionM SymBool) @?= mrgSingle (con True),
          testCase "From UnionMBase" $
            (toSym (mrgSingle True :: UnionM Bool) :: UnionM SymBool)
              @?= mrgSingle (con True)
        ],
      testGroup
        "ToCon"
        [ testCase "To single" $ do
            (toCon (mrgSingle (con True) :: UnionM SymBool) :: Maybe Bool)
              @?= Just True
            (toCon (mrgSingle "a" :: UnionM SymBool) :: Maybe Bool) @?= Nothing
            ( toCon
                ( mrgIf "a" (mrgLeft $ con False) (mrgRight $ con True) ::
                    UnionM (Either SymBool SymBool)
                ) ::
                Maybe (Either Bool Bool)
              )
              @?= Nothing,
          testCase "To UnionMBase" $ do
            ( toCon (mrgSingle (con True) :: UnionM SymBool) ::
                Maybe (UnionM Bool)
              )
              @?= Just (mrgSingle True)
            (toCon (mrgSingle "a" :: UnionM SymBool) :: Maybe (UnionM Bool))
              @?= Nothing
            ( toCon
                ( mrgIf "a" (mrgLeft $ con False) (mrgRight $ con True) ::
                    UnionM (Either SymBool SymBool)
                ) ::
                Maybe (UnionM (Either Bool Bool))
              )
              @?= Just (mrgIf "a" (mrgLeft False) (mrgRight True))
            ( toCon
                ( mrgIf "a" (mrgLeft "b") (mrgRight $ con True) ::
                    UnionM (Either SymBool SymBool)
                ) ::
                Maybe (UnionM (Either Bool Bool))
              )
              @?= Nothing
        ],
      testCase "Evaluate" $ do
        let model = emptyModel
        let model1 = buildModel ("a" ::= True, "b" ::= False, "c" ::= True)
        evaluateSym False model (mrgSingle "a")
          @?= (mrgSingle "a" :: UnionM SymBool)
        evaluateSym True model (mrgSingle "a")
          @?= (mrgSingle $ con False :: UnionM SymBool)
        evaluateSym False model1 (mrgSingle "a")
          @?= (mrgSingle $ con True :: UnionM SymBool)
        evaluateSym True model1 (mrgSingle "a")
          @?= (mrgSingle $ con True :: UnionM SymBool)
        evaluateSym
          False
          model1
          ( mrgIf
              "a"
              (mrgSingle $ Left "d")
              (mrgSingle $ Right "e")
          )
          @?= (mrgSingle $ Left "d" :: UnionM (Either SymBool SymBool))
        evaluateSym
          True
          model1
          ( mrgIf
              "a"
              (mrgSingle $ Left "d")
              (mrgSingle $ Right "e")
          )
          @?= (mrgSingle $ Left $ con False :: UnionM (Either SymBool SymBool))
        evaluateSym
          False
          model1
          ( mrgIf
              "d"
              (mrgSingle $ Left "a")
              (mrgSingle $ Right "b")
          )
          @?= ( mrgIf
                  "d"
                  (mrgSingle $ Left $ con True)
                  (mrgSingle $ Right $ con False) ::
                  UnionM (Either SymBool SymBool)
              )
        evaluateSym
          True
          model1
          ( mrgIf
              "d"
              (mrgSingle $ Left "a")
              (mrgSingle $ Right "b")
          )
          @?= (mrgSingle $ Right $ con False :: UnionM (Either SymBool SymBool))
        evaluateSym
          False
          model1
          ( mrgIf
              "a"
              (mrgSingle $ Left "b")
              (mrgSingle $ Right "c")
          )
          @?= ( mrgSingle $ Left $ con False ::
                  UnionM
                    (Either SymBool SymBool)
              ),
      testCase "SubstituteSym" $ do
        let asym = "a" :: TypedSymbol Bool
        let a = "a"
        let b = "b"
        let c = "c"
        substituteSym
          asym
          b
          (mrgSingle $ Left a :: UnionM (Either SymBool SymBool))
          @?= mrgSingle (Left b)
        substituteSym
          asym
          b
          (mrgSingle $ Left c :: UnionM (Either SymBool SymBool))
          @?= mrgSingle (Left c)
        substituteSym
          asym
          b
          (mrgSingle $ Right a :: UnionM (Either SymBool SymBool))
          @?= mrgSingle (Right b)
        substituteSym
          asym
          b
          (mrgSingle $ Right c :: UnionM (Either SymBool SymBool))
          @?= mrgSingle (Right c)
        substituteSym
          asym
          b
          ( mrgIf a (mrgSingle $ Left a) (mrgSingle $ Right c) ::
              UnionM (Either SymBool SymBool)
          )
          @?= mrgIf b (mrgSingle $ Left b) (mrgSingle $ Right c)
        substituteSym
          asym
          b
          ( mrgIf c (mrgSingle $ Left c) (mrgSingle $ Right a) ::
              UnionM (Either SymBool SymBool)
          )
          @?= mrgIf c (mrgSingle $ Left c) (mrgSingle $ Right b),
      testCase "ExtractSymbolic" $ do
        extractSymbolics (mrgSingle "a" :: UnionM SymBool)
          @?= buildSymbolSet ("a" :: TypedSymbol Bool)
        extractSymbolics
          ( mrgIf "a" (mrgSingle $ Left "b") (mrgSingle $ Right "c") ::
              UnionM (Either SymBool SymBool)
          )
          @?= buildSymbolSet
            ( "a" :: TypedSymbol Bool,
              "b" :: TypedSymbol Bool,
              "c" :: TypedSymbol Bool
            ),
      testGroup
        "Num"
        [ testCase "fromInteger" $ (1 :: UnionM Integer) @?= mrgSingle 1,
          testCase "negate" $
            negate (mrgIf "a" (mrgSingle 1) (mrgSingle 2) :: UnionM Integer)
              @?= mrgIf "a" (mrgSingle $ -1) (mrgSingle $ -2),
          testCase "plus" $
            (mrgIf "a" (mrgSingle 0) (mrgSingle 1) :: UnionM Integer)
              + mrgIf "b" (mrgSingle 1) (mrgSingle 3)
              @?= mrgIf
                "a"
                (mrgIf "b" (mrgSingle 1) (mrgSingle 3))
                (mrgIf "b" (mrgSingle 2) (mrgSingle 4)),
          testCase "minus" $
            (mrgIf "a" (mrgSingle 0) (mrgSingle 1) :: UnionM Integer)
              - mrgIf "b" (mrgSingle $ -3) (mrgSingle $ -1)
              @?= mrgIf
                "a"
                (mrgIf (nots "b") (mrgSingle 1) (mrgSingle 3))
                (mrgIf (nots "b") (mrgSingle 2) (mrgSingle 4)),
          testCase "times" $
            (mrgIf "a" (mrgSingle 1) (mrgSingle 2) :: UnionM Integer)
              * mrgIf "b" (mrgSingle 3) (mrgSingle 4)
              @?= mrgIf
                "a"
                (mrgIf "b" (mrgSingle 3) (mrgSingle 4))
                (mrgIf "b" (mrgSingle 6) (mrgSingle 8)),
          testCase "abs" $
            abs (mrgIf "a" (mrgSingle $ -1) (mrgSingle 2) :: UnionM Integer)
              @?= mrgIf "a" (mrgSingle 1) (mrgSingle 2),
          testCase "signum" $
            signum (mrgIf "a" (mrgSingle $ -1) (mrgSingle 2) :: UnionM Integer)
              @?= mrgIf "a" (mrgSingle $ -1) (mrgSingle 1)
        ],
      testGroup
        "itesOp"
        [ testCase "ites" $
            ites "a" (mrgSingle "b") (mrgSingle "c")
              @?= (mrgSingle (ites "a" "b" "c") :: UnionM SymBool)
        ],
      let l = mrgIf "a" (mrgSingle False) (mrgSingle True)
          r = mrgIf "b" (mrgSingle False) (mrgSingle True)
       in testGroup
            "LogicalOp"
            [ testCase "||~" $
                l
                  ||~ r
                  @?= ( mrgIf
                          ("a" &&~ "b")
                          (mrgSingle False)
                          (mrgSingle True) ::
                          UnionM Bool
                      ),
              testCase "&&~" $
                l
                  &&~ r
                  @?= ( mrgIf
                          ("a" ||~ "b")
                          (mrgSingle False)
                          (mrgSingle True) ::
                          UnionM Bool
                      ),
              testCase "nots" $
                nots l
                  @?= mrgIf (nots "a") (mrgSingle False) (mrgSingle True),
              testCase "xors" $
                l
                  `xors` r
                  @?= ( mrgIf
                          (ites "a" "b" (nots "b"))
                          (mrgSingle False)
                          (mrgSingle True) ::
                          UnionM Bool
                      ),
              testCase "implies" $
                l
                  `implies` r
                  @?= ( mrgIf
                          (nots "a" &&~ "b")
                          (mrgSingle False)
                          (mrgSingle True) ::
                          UnionM Bool
                      )
            ],
      testCase "PrimWrapper" $ do
        con True @?= (mrgSingle $ con True :: UnionM SymBool)
        ssym "a" @?= (mrgSingle "a" :: UnionM SymBool)
        isym "a" 0 @?= (mrgSingle $ isym "a" 0 :: UnionM SymBool)
        conView (mrgSingle $ con True :: UnionM SymBool) @?= Just True
        conView (mrgSingle "a" :: UnionM SymBool) @?= Nothing
        conView
          ( mrgIf
              "a"
              (mrgSingle $ con False)
              (mrgSingle $ con True) ::
              UnionM SymBool
          )
          @?= Nothing,
      testGroup
        "Function class"
        [ testCase "Applying function in UnionMBase" $ do
            let func =
                  mrgIf "a" (mrgSingle (+ 1)) (mrgSingle (+ 2)) ::
                    UnionM (Integer -> Integer)
            func # (1 :: Integer) @?= mrgIf "a" (mrgSingle 2) (mrgSingle 3),
          testCase "Helper for applying on UnionMBase" $ do
            let func (x :: Integer) =
                  mrgIf "a" (mrgSingle $ x + 1) (mrgSingle $ x + 3)
            (func #~ (mrgIf "b" (mrgSingle 0) (mrgSingle 1) :: UnionM Integer))
              @?= ( mrgIf
                      "b"
                      (mrgIf "a" (mrgSingle 1) (mrgSingle 3))
                      (mrgIf "a" (mrgSingle 2) (mrgSingle 4)) ::
                      UnionM Integer
                  )
        ],
      testCase "IsString" $ ("x" :: UnionM B.ByteString) @?= mrgSingle "x",
      testGroup
        "GenSym"
        [ testCase "GenSym with spec" $ do
            (genSym (ListSpec 1 3 ()) "a" :: UnionM (UnionM [SymBool]))
              @?= mrgSingle
                ( mrgIf
                    (isym "a" 3)
                    (mrgSingle [isym "a" 2])
                    ( mrgIf
                        (isym "a" 4)
                        (mrgSingle [isym "a" 1, isym "a" 2])
                        (mrgSingle [isym "a" 0, isym "a" 1, isym "a" 2])
                    )
                )
            (genSymSimple (ListSpec 1 3 ()) "a" :: UnionM [SymBool])
              @?= mrgIf
                (isym "a" 3)
                (mrgSingle [isym "a" 2])
                ( mrgIf
                    (isym "a" 4)
                    (mrgSingle [isym "a" 1, isym "a" 2])
                    (mrgSingle [isym "a" 0, isym "a" 1, isym "a" 2])
                ),
          testCase "GenSym with same shape" $
            ( genSym
                ( mrgIf
                    "a"
                    (mrgSingle ["x"])
                    (mrgSingle ["y", "z"]) ::
                    UnionM [SymBool]
                )
                "a" ::
                UnionM [SymBool]
            )
              @?= mrgIf
                (isym "a" 0)
                (mrgSingle [isym "a" 1])
                (mrgSingle [isym "a" 2, isym "a" 3])
        ],
      testGroup
        "Concrete Key HashMaps"
        [ testCase "Concrete Key HashMap should work" $ do
            mrgIte
              "a"
              ( ML.fromList
                  [ (1, mrgSingle $ Just 1),
                    (2, mrgSingle $ Just 2)
                  ] ::
                  ML.HashMap Integer (UnionM (Maybe Integer))
              )
              (ML.fromList [(1, mrgSingle $ Just 2), (3, mrgSingle $ Just 3)])
              @?= ML.fromList
                [ (1, mrgIf "a" (mrgSingle $ Just 1) (mrgSingle $ Just 2)),
                  ( 2,
                    mrgIf
                      (nots "a")
                      (mrgSingle Nothing)
                      (mrgSingle $ Just 2)
                  ),
                  (3, mrgIf "a" (mrgSingle Nothing) (mrgSingle $ Just 3))
                ]
            mrgIf
              "a"
              ( mrgSingle $
                  ML.fromList
                    [ (1, mrgSingle $ Just 1),
                      (2, mrgSingle $ Just 2)
                    ] ::
                  UnionM (ML.HashMap Integer (UnionM (Maybe Integer)))
              )
              ( mrgSingle
                  ( ML.fromList
                      [ (1, mrgSingle $ Just 2),
                        (3, mrgSingle $ Just 3)
                      ]
                  )
              )
              @?= mrgSingle
                ( ML.fromList
                    [ (1, mrgIf "a" (mrgSingle $ Just 1) (mrgSingle $ Just 2)),
                      ( 2,
                        mrgIf
                          (nots "a")
                          (mrgSingle Nothing)
                          (mrgSingle $ Just 2)
                      ),
                      (3, mrgIf "a" (mrgSingle Nothing) (mrgSingle $ Just 3))
                    ]
                )
        ],
      testCase
        "unionSize"
        $ do
          unionSize (single 1 :: UnionM Integer) @?= 1
          unionSize (mrgIf (ssym "a") (single 1) (single 2) :: UnionM Integer)
            @?= 2
          unionSize (choose [1, 2, 3, 4, 5, 6, 7] "a" :: UnionM Integer) @?= 7
    ]
