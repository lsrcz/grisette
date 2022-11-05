{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Grisette.Core.Control.Monad.UnionMBaseTests where

import Control.Monad.Identity
import qualified Data.ByteString as B
import qualified Data.HashMap.Lazy as ML
import qualified Data.HashMap.Strict as M
import qualified Data.HashSet as S
import Grisette.Core.Control.Monad.UnionMBase
import Grisette.Core.Data.Class.Bool
import Grisette.Core.Data.Class.Evaluate
import Grisette.Core.Data.Class.ExtractSymbolics
import Grisette.Core.Data.Class.Function
import Grisette.Core.Data.Class.GenSym
import Grisette.Core.Data.Class.PrimWrapper
import Grisette.Core.Data.Class.SOrd
import Grisette.Core.Data.Class.SimpleMergeable
import Grisette.Core.Data.Class.ToCon
import Grisette.Core.Data.Class.ToSym
import Grisette.Core.Data.UnionBase
import Grisette.TestUtils.SBool
import Test.Tasty
import Test.Tasty.HUnit

unionMBaseTests :: TestTree
unionMBaseTests =
  testGroup
    "UnionMBaseTests"
    [ testCase "Mergeable" $ do
        let r =
              ( mrgIf
                  (SSBool "a")
                  (mrgSingle (mrgIf (SSBool "b") (mrgSingle $ Left $ SSBool "c") (mrgSingle $ Right $ SSBool "d")))
                  (mrgSingle (mrgIf (SSBool "e") (mrgSingle $ Left $ SSBool "f") (mrgSingle $ Right $ SSBool "g"))) ::
                  UnionMBase SBool (UnionMBase SBool (Either SBool SBool))
              )
        isMerged r @=? True
        underlyingUnion (underlyingUnion <$> r)
          @=? Single
            ( If
                (Left $ ITE (SSBool "a") (SSBool "c") (SSBool "f"))
                True
                (ITE (SSBool "a") (SSBool "b") (SSBool "e"))
                (Single $ Left $ ITE (SSBool "a") (SSBool "c") (SSBool "f"))
                (Single $ Right $ ITE (SSBool "a") (SSBool "d") (SSBool "g"))
            ),
      testCase "SimpleMergeable" $ do
        let l :: UnionMBase SBool (Either SBool SBool) = mrgIf (SSBool "b") (mrgSingle $ Left $ SSBool "c") (mrgSingle $ Right $ SSBool "d")
        let r = mrgIf (SSBool "e") (mrgSingle $ Left $ SSBool "f") (mrgSingle $ Right $ SSBool "g")
        let res = mrgIte (SSBool "a") l r
        let ref =
              If
                (Left $ ITE (SSBool "a") (SSBool "c") (SSBool "f"))
                True
                (ITE (SSBool "a") (SSBool "b") (SSBool "e"))
                (Single $ Left $ ITE (SSBool "a") (SSBool "c") (SSBool "f"))
                (Single $ Right $ ITE (SSBool "a") (SSBool "d") (SSBool "g"))
        isMerged res @=? True
        underlyingUnion res @=? ref,
      testCase "SimpleMergeable1" $ do
        let l :: UnionMBase SBool SBool = mrgIf (SSBool "b") (mrgSingle $ SSBool "c") (mrgSingle $ SSBool "d")
        let r :: UnionMBase SBool SBool = mrgIf (SSBool "e") (mrgSingle $ SSBool "f") (mrgSingle $ SSBool "g")
        let res = mrgIte1 (SSBool "a") l r
        isMerged res @=? True
        underlyingUnion res
          @=? Single
            ( ITE
                (SSBool "a")
                (ITE (SSBool "b") (SSBool "c") (SSBool "d"))
                (ITE (SSBool "e") (SSBool "f") (SSBool "g"))
            ),
      testGroup
        "Functor"
        [ testCase "fmap should work but would strip mergeable knowledge" $ do
            let x :: UnionMBase SBool Integer = (+ 1) <$> mrgIf (SSBool "a") (mrgSingle 1) (mrgSingle 2)
            x @=? unionIf (SSBool "a") (return 2) (return 3)
        ],
      testGroup
        "Applicative"
        [ testCase "pure should work but won't give us mergeable knowledge" $ do
            (pure 1 :: UnionMBase SBool Integer) @=? single 1,
          testCase "<*> should work but won't give us mergeable knowledge" $ do
            let f :: UnionMBase SBool (Integer -> Integer) = mrgIf (SSBool "a") (mrgSingle id) (mrgSingle (+ 1))
            let v :: UnionMBase SBool Integer = mrgIf (SSBool "b") (mrgSingle 1) (mrgSingle 3)
            f <*> v
              @=? unionIf
                (SSBool "a")
                (unionIf (SSBool "b") (single 1) (single 3))
                (unionIf (SSBool "b") (single 2) (single 4))
        ],
      testGroup
        "Monad"
        [ testCase "return should work but won't give us mergeable knowledge" $ do
            (pure 1 :: UnionMBase SBool Integer) @=? single 1,
          testCase ">>= should work and keeps mergeable knowledge" $ do
            let v :: UnionMBase SBool Integer = mrgIf (SSBool "a") (mrgSingle 0) (mrgSingle 1)
            let f :: Integer -> UnionMBase SBool Integer = \i -> mrgIf (SSBool "b") (mrgSingle $ i + 1) (mrgSingle $ i + 3)
            (v >>= f)
              @=? mrgIf
                (SSBool "a")
                (mrgIf (SSBool "b") (mrgSingle 1) (mrgSingle 3))
                (mrgIf (SSBool "b") (mrgSingle 2) (mrgSingle 4))
        ],
      testGroup
        "UnionOp"
        [ testCase "single" $ do
            let r1 :: UnionMBase SBool SBool = single (SSBool "a")
            isMerged r1 @=? False
            underlyingUnion r1 @=? Single (SSBool "a"),
          testGroup
            "unionIf"
            [ testCase "unionIf should work when no merged" $ do
                let r1 :: UnionMBase SBool SBool = unionIf (SSBool "a") (single $ SSBool "b") (single $ SSBool "c")
                isMerged r1 @=? False
                underlyingUnion r1 @=? If (SSBool "b") False (SSBool "a") (Single $ SSBool "b") (Single $ SSBool "c"),
              testCase "unionIf should propagate and merge the results when some branch merged" $ do
                let r1 :: UnionMBase SBool SBool = unionIf (SSBool "a") (mrgSingle $ SSBool "b") (single $ SSBool "c")
                isMerged r1 @=? True
                underlyingUnion r1 @=? Single (ITE (SSBool "a") (SSBool "b") (SSBool "c"))
                let r2 :: UnionMBase SBool SBool = unionIf (SSBool "a") (single $ SSBool "b") (mrgSingle $ SSBool "c")
                isMerged r2 @=? True
                underlyingUnion r2 @=? Single (ITE (SSBool "a") (SSBool "b") (SSBool "c"))
                let r3 :: UnionMBase SBool SBool = unionIf (SSBool "a") (mrgSingle $ SSBool "b") (mrgSingle $ SSBool "c")
                isMerged r3 @=? True
                underlyingUnion r3 @=? Single (ITE (SSBool "a") (SSBool "b") (SSBool "c"))
            ],
          testCase "singleView should work" $ do
            singleView (single $ SSBool "a" :: UnionMBase SBool SBool) @=? Just (SSBool "a")
            singleView (mrgSingle $ SSBool "a" :: UnionMBase SBool SBool) @=? Just (SSBool "a")
            singleView
              ( unionIf (SSBool "a") (single $ Left $ SSBool "b") (single $ Right $ SSBool "c") ::
                  UnionMBase SBool (Either SBool SBool)
              )
              @=? Nothing
            case (single $ SSBool "a" :: UnionMBase SBool SBool) of
              SingleU r -> r @=? SSBool "a"
              _ -> assertFailure "SingleU match failed"
            case (mrgSingle $ SSBool "a" :: UnionMBase SBool SBool) of
              SingleU r -> r @=? SSBool "a"
              _ -> assertFailure "SingleU match failed"
            case ( unionIf (SSBool "a") (single $ Left $ SSBool "b") (single $ Right $ SSBool "c") ::
                     UnionMBase SBool (Either SBool SBool)
                 ) of
              SingleU _ -> assertFailure "SingleU match failed"
              _ -> return (),
          testCase "ifView should work" $ do
            let r1 :: UnionMBase SBool (Either SBool SBool) =
                  unionIf (SSBool "a") (single $ Left $ SSBool "b") (single $ Right $ SSBool "c")
            let r2 :: UnionMBase SBool (Either SBool SBool) =
                  mrgIf (SSBool "a") (mrgSingle $ Left $ SSBool "b") (mrgSingle $ Right $ SSBool "c")
            ifView r1 @=? Just (SSBool "a", single $ Left $ SSBool "b", single $ Right $ SSBool "c")
            ifView r2 @=? Just (SSBool "a", mrgSingle $ Left $ SSBool "b", mrgSingle $ Right $ SSBool "c")
            ifView (single $ SSBool "a" :: UnionMBase SBool SBool) @=? Nothing
            case r1 of
              IfU c l r -> do
                c @=? SSBool "a"
                l @=? single (Left $ SSBool "b")
                r @=? single (Right $ SSBool "c")
              _ -> assertFailure "SingleU match failed"
            case r2 of
              IfU c l r -> do
                c @=? SSBool "a"
                l @=? mrgSingle (Left $ SSBool "b")
                r @=? mrgSingle (Right $ SSBool "c")
              _ -> assertFailure "SingleU match failed"
            case single $ SSBool "a" :: UnionMBase SBool SBool of
              IfU {} -> assertFailure "SingleU match failed"
              _ -> return (),
          testCase "leftMost should work" $ do
            leftMost (single $ SSBool "a" :: UnionMBase SBool SBool) @=? SSBool "a"
            leftMost (mrgSingle $ SSBool "a" :: UnionMBase SBool SBool) @=? SSBool "a"
            let r1 :: UnionMBase SBool (Either SBool SBool) =
                  unionIf (SSBool "a") (single $ Left $ SSBool "b") (single $ Right $ SSBool "c")
            let r2 :: UnionMBase SBool (Either SBool SBool) =
                  mrgIf (SSBool "a") (mrgSingle $ Left $ SSBool "b") (mrgSingle $ Right $ SSBool "c")
            leftMost r1 @=? Left (SSBool "b")
            leftMost r2 @=? Left (SSBool "b")
        ],
      testGroup
        "MonadUnion"
        [ testCase "merge should work" $ do
            let r1 :: UnionMBase SBool SBool = merge (unionIf (SSBool "a") (single $ SSBool "b") (single $ SSBool "c"))
            isMerged r1 @=? True
            underlyingUnion r1 @=? Single (ITE (SSBool "a") (SSBool "b") (SSBool "c")),
          testCase "mrgSingle should work" $ do
            let r1 :: UnionMBase SBool SBool = mrgSingle (SSBool "a")
            isMerged r1 @=? True
            underlyingUnion r1 @=? Single (SSBool "a"),
          testGroup
            "mrgIf should work"
            [ testCase "mrgIf should perform lazy evaluation" $ do
                (mrgIf (CBool True) (mrgSingle $ SSBool "a") undefined :: UnionMBase SBool SBool)
                  @=? mrgSingle (SSBool "a")
                (mrgIf (CBool False) undefined (mrgSingle $ SSBool "a") :: UnionMBase SBool SBool)
                  @=? mrgSingle (SSBool "a"),
              testCase "mrgIf should work" $ do
                (mrgIf (SSBool "a") (single $ SSBool "b") (single $ SSBool "c") :: UnionMBase SBool SBool)
                  @=? merge (unionIf (SSBool "a") (single $ SSBool "b") (single $ SSBool "c"))
            ]
        ],
      let g1 :: UnionMBase SBool (Either SBool SBool) =
            mrgIf (SSBool "a") (mrgSingle $ Left $ SSBool "b") (mrgSingle $ Right $ SSBool "c")
          g2 :: UnionMBase SBool (Either SBool SBool) =
            mrgIf (SSBool "d") (mrgSingle $ Left $ SSBool "e") (mrgSingle $ Right $ SSBool "f")
       in testGroup
            "SEq"
            [ testCase "Single/Single" $ do
                (mrgSingle $ SSBool "a" :: UnionMBase SBool SBool)
                  `gsymeq` mrgSingle (SSBool "b")
                  @=? Equal (SSBool "a") (SSBool "b"),
              testCase "If/Single" $ do
                g1
                  `gsymeq` mrgSingle (Left $ SSBool "d")
                  @=? ITE (SSBool "a") (Equal (SSBool "b") (SSBool "d")) (CBool False)
                g1
                  `gsymeq` mrgSingle (Right $ SSBool "d")
                  @=? ITE (SSBool "a") (CBool False) (Equal (SSBool "c") (SSBool "d")),
              testCase "Single/If" $ do
                mrgSingle (Left $ SSBool "d")
                  `gsymeq` g1
                  @=? ITE (SSBool "a") (Equal (SSBool "d") (SSBool "b")) (CBool False)
                mrgSingle (Right $ SSBool "d")
                  `gsymeq` g1
                  @=? ITE (SSBool "a") (CBool False) (Equal (SSBool "d") (SSBool "c")),
              testCase "If/If" $ do
                g1
                  `gsymeq` g2
                  @=? ITE
                    (SSBool "a")
                    (ITE (SSBool "d") (Equal (SSBool "b") (SSBool "e")) (CBool False))
                    (ITE (SSBool "d") (CBool False) (Equal (SSBool "c") (SSBool "f")))
            ],
      let g1 :: UnionMBase SBool (Either SBool SBool) =
            mrgIf (SSBool "a") (mrgSingle $ Left $ SSBool "b") (mrgSingle $ Right $ SSBool "c")
          g2 :: UnionMBase SBool (Either SBool SBool) =
            mrgIf (SSBool "d") (mrgSingle $ Left $ SSBool "e") (mrgSingle $ Right $ SSBool "f")
       in testGroup
            "SOrd"
            [ testCase "Single/Single" $ do
                (mrgSingle $ SSBool "a" :: UnionMBase SBool SBool)
                  `gsymle` mrgSingle (SSBool "b")
                  @=? (SSBool "a" `gsymle` SSBool "b" :: SBool)
                (mrgSingle $ SSBool "a" :: UnionMBase SBool SBool)
                  `gsymlt` mrgSingle (SSBool "b")
                  @=? (SSBool "a" `gsymlt` SSBool "b" :: SBool)
                (mrgSingle $ SSBool "a" :: UnionMBase SBool SBool)
                  `gsymge` mrgSingle (SSBool "b")
                  @=? (SSBool "a" `gsymge` SSBool "b" :: SBool)
                (mrgSingle $ SSBool "a" :: UnionMBase SBool SBool)
                  `gsymgt` mrgSingle (SSBool "b")
                  @=? (SSBool "a" `gsymgt` SSBool "b" :: SBool)
                (mrgSingle $ SSBool "a" :: UnionMBase SBool SBool)
                  `gsymCompare` mrgSingle (SSBool "b")
                  @=? (SSBool "a" `gsymCompare` SSBool "b" :: UnionMBase SBool Ordering),
              testCase "If/Single" $ do
                g1
                  `gsymle` mrgSingle (Left $ SSBool "d")
                  @=? ITE (SSBool "a") (SSBool "b" `gsymle` SSBool "d") (CBool False)
                g1
                  `gsymlt` mrgSingle (Left $ SSBool "d")
                  @=? ITE (SSBool "a") (SSBool "b" `gsymlt` SSBool "d") (CBool False)
                g1
                  `gsymge` mrgSingle (Left $ SSBool "d")
                  @=? ITE (SSBool "a") (SSBool "b" `gsymge` SSBool "d") (CBool True)
                g1
                  `gsymgt` mrgSingle (Left $ SSBool "d")
                  @=? ITE (SSBool "a") (SSBool "b" `gsymgt` SSBool "d") (CBool True)

                g1
                  `gsymCompare` mrgSingle (Left $ SSBool "d")
                  @=? ( mrgIf (SSBool "a") (SSBool "b" `gsymCompare` SSBool "d") (mrgSingle GT) ::
                          UnionMBase SBool Ordering
                      )

                g1
                  `gsymle` mrgSingle (Right $ SSBool "d")
                  @=? ITE (SSBool "a") (CBool True) (SSBool "c" `gsymle` SSBool "d")
                g1
                  `gsymlt` mrgSingle (Right $ SSBool "d")
                  @=? ITE (SSBool "a") (CBool True) (SSBool "c" `gsymlt` SSBool "d")
                g1
                  `gsymge` mrgSingle (Right $ SSBool "d")
                  @=? ITE (SSBool "a") (CBool False) (SSBool "c" `gsymge` SSBool "d")
                g1
                  `gsymgt` mrgSingle (Right $ SSBool "d")
                  @=? ITE (SSBool "a") (CBool False) (SSBool "c" `gsymgt` SSBool "d")

                g1
                  `gsymCompare` mrgSingle (Right $ SSBool "d")
                  @=? ( mrgIf (SSBool "a") (mrgSingle LT) (SSBool "c" `gsymCompare` SSBool "d") ::
                          UnionMBase SBool Ordering
                      ),
              testCase "Single/If" $ do
                mrgSingle (Left $ SSBool "d")
                  `gsymle` g1
                  @=? ITE (SSBool "a") (SSBool "d" `gsymle` SSBool "b") (CBool True)
                mrgSingle (Left $ SSBool "d")
                  `gsymlt` g1
                  @=? ITE (SSBool "a") (SSBool "d" `gsymlt` SSBool "b") (CBool True)
                mrgSingle (Left $ SSBool "d")
                  `gsymge` g1
                  @=? ITE (SSBool "a") (SSBool "d" `gsymge` SSBool "b") (CBool False)
                mrgSingle (Left $ SSBool "d")
                  `gsymgt` g1
                  @=? ITE (SSBool "a") (SSBool "d" `gsymgt` SSBool "b") (CBool False)

                mrgSingle (Left $ SSBool "d")
                  `gsymCompare` g1
                  @=? ( mrgIf (SSBool "a") (SSBool "d" `gsymCompare` SSBool "b") (mrgSingle LT) ::
                          UnionMBase SBool Ordering
                      )

                mrgSingle (Right $ SSBool "d")
                  `gsymle` g1
                  @=? ITE (SSBool "a") (CBool False) (SSBool "d" `gsymle` SSBool "c")
                mrgSingle (Right $ SSBool "d")
                  `gsymlt` g1
                  @=? ITE (SSBool "a") (CBool False) (SSBool "d" `gsymlt` SSBool "c")
                mrgSingle (Right $ SSBool "d")
                  `gsymge` g1
                  @=? ITE (SSBool "a") (CBool True) (SSBool "d" `gsymge` SSBool "c")
                mrgSingle (Right $ SSBool "d")
                  `gsymgt` g1
                  @=? ITE (SSBool "a") (CBool True) (SSBool "d" `gsymgt` SSBool "c")

                mrgSingle (Right $ SSBool "d")
                  `gsymCompare` g1
                  @=? ( mrgIf (SSBool "a") (mrgSingle GT) (SSBool "d" `gsymCompare` SSBool "c") ::
                          UnionMBase SBool Ordering
                      ),
              testCase "If/If" $ do
                g1
                  `gsymle` g2
                  @=? ITE
                    (SSBool "a")
                    (ITE (SSBool "d") (SSBool "b" `gsymle` SSBool "e") (CBool True))
                    (ITE (SSBool "d") (CBool False) (SSBool "c" `gsymle` SSBool "f"))
                g1
                  `gsymlt` g2
                  @=? ITE
                    (SSBool "a")
                    (ITE (SSBool "d") (SSBool "b" `gsymlt` SSBool "e") (CBool True))
                    (ITE (SSBool "d") (CBool False) (SSBool "c" `gsymlt` SSBool "f"))
                g1
                  `gsymge` g2
                  @=? ITE
                    (SSBool "a")
                    (ITE (SSBool "d") (SSBool "b" `gsymge` SSBool "e") (CBool False))
                    (ITE (SSBool "d") (CBool True) (SSBool "c" `gsymge` SSBool "f"))
                g1
                  `gsymgt` g2
                  @=? ITE
                    (SSBool "a")
                    (ITE (SSBool "d") (SSBool "b" `gsymgt` SSBool "e") (CBool False))
                    (ITE (SSBool "d") (CBool True) (SSBool "c" `gsymgt` SSBool "f"))
                g1
                  `gsymCompare` g2
                  @=? ( mrgIf
                          (SSBool "a")
                          (mrgIf (SSBool "d") (SSBool "b" `gsymCompare` SSBool "e") (mrgSingle LT))
                          (mrgIf (SSBool "d") (mrgSingle GT) (SSBool "c" `gsymCompare` SSBool "f")) ::
                          UnionMBase SBool Ordering
                      )
            ],
      testGroup
        "ToSym"
        [ testCase "From single" $ do
            (toSym True :: UnionMBase SBool SBool) @=? mrgSingle (CBool True),
          testCase "From UnionMBase" $ do
            (toSym (mrgSingle True :: UnionMBase SBool Bool) :: UnionMBase SBool SBool) @=? mrgSingle (CBool True),
          testCase "From Identity" $ do
            (toSym (Identity True :: Identity Bool) :: UnionMBase SBool SBool) @=? mrgSingle (CBool True)
        ],
      testCase "ToCon" $ do
        (toCon (mrgSingle (CBool True) :: UnionMBase SBool SBool) :: Maybe Bool) @=? Just True
        (toCon (mrgSingle (SSBool "a") :: UnionMBase SBool SBool) :: Maybe Bool) @=? Nothing
        ( toCon (mrgIf (SSBool "a") (mrgSingle (1 :: Integer)) (mrgSingle (2 :: Integer)) :: UnionMBase SBool Integer) ::
            Maybe Integer
          )
          @=? Nothing,
      testCase "Evaluate" $ do
        let model = M.empty :: M.HashMap Symbol Bool
        let model1 =
              M.fromList
                [ (SSymbol "a", True),
                  (SSymbol "b", False),
                  (SSymbol "c", True)
                ] ::
                M.HashMap Symbol Bool
        gevaluateSym False model (mrgSingle $ SSBool "a") @=? (mrgSingle $ SSBool "a" :: UnionMBase SBool SBool)
        gevaluateSym True model (mrgSingle $ SSBool "a") @=? (mrgSingle $ CBool False :: UnionMBase SBool SBool)
        gevaluateSym False model1 (mrgSingle $ SSBool "a") @=? (mrgSingle $ CBool True :: UnionMBase SBool SBool)
        gevaluateSym True model1 (mrgSingle $ SSBool "a") @=? (mrgSingle $ CBool True :: UnionMBase SBool SBool)
        gevaluateSym False model1 (mrgIf (SSBool "a") (mrgSingle $ Left (SSBool "d")) (mrgSingle $ Right (SSBool "e")))
          @=? (mrgSingle $ Left $ SSBool "d" :: UnionMBase SBool (Either SBool SBool))
        gevaluateSym True model1 (mrgIf (SSBool "a") (mrgSingle $ Left (SSBool "d")) (mrgSingle $ Right (SSBool "e")))
          @=? (mrgSingle $ Left $ CBool False :: UnionMBase SBool (Either SBool SBool))
        gevaluateSym False model1 (mrgIf (SSBool "d") (mrgSingle $ Left (SSBool "a")) (mrgSingle $ Right (SSBool "b")))
          @=? ( mrgIf (SSBool "d") (mrgSingle $ Left $ CBool True) (mrgSingle $ Right $ CBool False) ::
                  UnionMBase SBool (Either SBool SBool)
              )
        gevaluateSym True model1 (mrgIf (SSBool "d") (mrgSingle $ Left (SSBool "a")) (mrgSingle $ Right (SSBool "b")))
          @=? (mrgSingle $ Right $ CBool False :: UnionMBase SBool (Either SBool SBool))
        gevaluateSym False model1 (mrgIf (SSBool "a") (mrgSingle $ Left (SSBool "b")) (mrgSingle $ Right (SSBool "c")))
          @=? (mrgSingle $ Left $ CBool False :: UnionMBase SBool (Either SBool SBool)),
      testCase "ExtractSymbolic" $ do
        extractSymbolics (mrgSingle $ SSBool "a" :: UnionMBase SBool SBool)
          @=? S.singleton (SSymbol "a")
        extractSymbolics
          ( mrgIf (SSBool "a") (mrgSingle $ Left $ SSBool "b") (mrgSingle $ Right $ SSBool "c") ::
              UnionMBase SBool (Either SBool SBool)
          )
          @=? S.fromList [SSymbol "a", SSymbol "b", SSymbol "c"],
      testGroup
        "Num"
        [ testCase "fromInteger" $ do
            (1 :: UnionMBase SBool Integer) @=? mrgSingle 1,
          testCase "negate" $ do
            negate (mrgIf (SSBool "a") (mrgSingle 1) (mrgSingle 2) :: UnionMBase SBool Integer)
              @=? mrgIf (SSBool "a") (mrgSingle $ -1) (mrgSingle $ -2),
          testCase "plus" $ do
            (mrgIf (SSBool "a") (mrgSingle 0) (mrgSingle 1) :: UnionMBase SBool Integer)
              + mrgIf (SSBool "b") (mrgSingle 1) (mrgSingle 3)
              @=? mrgIf
                (SSBool "a")
                (mrgIf (SSBool "b") (mrgSingle 1) (mrgSingle 3))
                (mrgIf (SSBool "b") (mrgSingle 2) (mrgSingle 4)),
          testCase "minus" $ do
            (mrgIf (SSBool "a") (mrgSingle 0) (mrgSingle 1) :: UnionMBase SBool Integer)
              - mrgIf (SSBool "b") (mrgSingle $ -3) (mrgSingle $ -1)
              @=? mrgIf
                (SSBool "a")
                (mrgIf (Not $ SSBool "b") (mrgSingle 1) (mrgSingle 3))
                (mrgIf (Not $ SSBool "b") (mrgSingle 2) (mrgSingle 4)),
          testCase "times" $ do
            (mrgIf (SSBool "a") (mrgSingle 1) (mrgSingle 2) :: UnionMBase SBool Integer)
              * mrgIf (SSBool "b") (mrgSingle 3) (mrgSingle 4)
              @=? mrgIf
                (SSBool "a")
                (mrgIf (SSBool "b") (mrgSingle 3) (mrgSingle 4))
                (mrgIf (SSBool "b") (mrgSingle 6) (mrgSingle 8)),
          testCase "abs" $ do
            abs (mrgIf (SSBool "a") (mrgSingle $ -1) (mrgSingle 2) :: UnionMBase SBool Integer)
              @=? mrgIf (SSBool "a") (mrgSingle 1) (mrgSingle 2),
          testCase "signum" $ do
            signum (mrgIf (SSBool "a") (mrgSingle $ -1) (mrgSingle 2) :: UnionMBase SBool Integer)
              @=? mrgIf (SSBool "a") (mrgSingle $ -1) (mrgSingle 1)
        ],
      testGroup
        "ITEOp"
        [ testCase "ites" $ do
            ites (SSBool "a") (mrgSingle $ SSBool "b") (mrgSingle $ SSBool "c")
              @=? (mrgSingle (ITE (SSBool "a") (SSBool "b") (SSBool "c")) :: UnionMBase SBool SBool)
        ],
      let l = mrgIf (SSBool "a") (mrgSingle False) (mrgSingle True)
          r = mrgIf (SSBool "b") (mrgSingle False) (mrgSingle True)
       in testGroup
            "LogicalOp"
            [ testCase "||~" $ do
                l ||~ r @=? (mrgIf (And (SSBool "a") (SSBool "b")) (mrgSingle False) (mrgSingle True) :: UnionMBase SBool Bool),
              testCase "&&~" $ do
                l &&~ r @=? (mrgIf (Or (SSBool "a") (SSBool "b")) (mrgSingle False) (mrgSingle True) :: UnionMBase SBool Bool),
              testCase "nots" $ do
                nots l @=? mrgIf (Not $ SSBool "a") (mrgSingle False) (mrgSingle True),
              testCase "xors" $ do
                l
                  `xors` r
                  @=? ( mrgIf
                          (ITE (SSBool "a") (SSBool "b") (Not $ SSBool "b"))
                          (mrgSingle False)
                          (mrgSingle True) ::
                          UnionMBase SBool Bool
                      ),
              testCase "implies" $ do
                l
                  `implies` r
                  @=? ( mrgIf
                          (And (Not $ SSBool "a") (SSBool "b"))
                          (mrgSingle False)
                          (mrgSingle True) ::
                          UnionMBase SBool Bool
                      )
            ],
      testCase "PrimWrapper" $ do
        conc True @=? (mrgSingle $ CBool True :: UnionMBase SBool SBool)
        ssymb "a" @=? (mrgSingle $ SSBool "a" :: UnionMBase SBool SBool)
        isymb "a" 0 @=? (mrgSingle $ ISBool "a" 0 :: UnionMBase SBool SBool)
        concView (mrgSingle $ CBool True :: UnionMBase SBool SBool) @=? Just True
        concView (mrgSingle $ SSBool "a" :: UnionMBase SBool SBool) @=? Nothing
        concView
          ( mrgIf
              (SSBool "a")
              (mrgSingle $ CBool False)
              (mrgSingle $ CBool True) ::
              UnionMBase SBool SBool
          )
          @=? Nothing,
      testGroup
        "Function class"
        [ testCase "Applying function in UnionMBase" $ do
            let func = mrgIf (SSBool "a") (mrgSingle (+ 1)) (mrgSingle (+ 2)) :: UnionMBase SBool (Integer -> Integer)
            func # (1 :: Integer) @=? mrgIf (SSBool "a") (mrgSingle 2) (mrgSingle 3),
          testCase "Helper for applying on UnionMBase" $ do
            let func (x :: Integer) = mrgIf (SSBool "a") (mrgSingle $ x + 1) (mrgSingle $ x + 3)
            (func #~ (mrgIf (SSBool "b") (mrgSingle 0) (mrgSingle 1) :: UnionMBase SBool Integer))
              @=? ( mrgIf
                      (SSBool "b")
                      (mrgIf (SSBool "a") (mrgSingle 1) (mrgSingle 3))
                      (mrgIf (SSBool "a") (mrgSingle 2) (mrgSingle 4)) ::
                      UnionMBase SBool Integer
                  )
        ],
      testCase "IsString" $ do
        ("x" :: UnionMBase SBool B.ByteString) @=? mrgSingle "x",
      testGroup
        "GenSym"
        [ testCase "GenSym with spec" $ do
            (genSym (ListSpec 1 3 ()) "a" :: UnionMBase SBool (UnionMBase SBool [SBool]))
              @=? mrgSingle
                ( mrgIf
                    (ISBool "a" 3)
                    (mrgSingle [ISBool "a" 2])
                    ( mrgIf
                        (ISBool "a" 4)
                        (mrgSingle [ISBool "a" 1, ISBool "a" 2])
                        (mrgSingle [ISBool "a" 0, ISBool "a" 1, ISBool "a" 2])
                    )
                )
            (genSymSimple (ListSpec 1 3 ()) "a" :: UnionMBase SBool [SBool])
              @=? mrgIf
                (ISBool "a" 3)
                (mrgSingle [ISBool "a" 2])
                ( mrgIf
                    (ISBool "a" 4)
                    (mrgSingle [ISBool "a" 1, ISBool "a" 2])
                    (mrgSingle [ISBool "a" 0, ISBool "a" 1, ISBool "a" 2])
                ),
          testCase "GenSym with same shape" $ do
            ( genSym
                ( mrgIf
                    (SSBool "a")
                    (mrgSingle [SSBool "x"])
                    (mrgSingle [SSBool "y", SSBool "z"]) ::
                    UnionMBase SBool [SBool]
                )
                "a" ::
                UnionMBase SBool [SBool]
              )
              @=? mrgIf (ISBool "a" 0) (mrgSingle [ISBool "a" 1]) (mrgSingle [ISBool "a" 2, ISBool "a" 3])
        ],
      testGroup
        "Concrete Key HashMaps"
        [ testCase "Concrete Key HashMap should work" $ do
            mrgIte
              (SSBool "a")
              ( ML.fromList [(1, mrgSingle $ Just 1), (2, mrgSingle $ Just 2)] ::
                  ML.HashMap Integer (UnionMBase SBool (Maybe Integer))
              )
              (ML.fromList [(1, mrgSingle $ Just 2), (3, mrgSingle $ Just 3)])
              @=? ML.fromList
                [ (1, mrgIf (SSBool "a") (mrgSingle $ Just 1) (mrgSingle $ Just 2)),
                  (2, mrgIf (Not $ SSBool "a") (mrgSingle Nothing) (mrgSingle $ Just 2)),
                  (3, mrgIf (SSBool "a") (mrgSingle Nothing) (mrgSingle $ Just 3))
                ]
            mrgIf
              (SSBool "a")
              ( mrgSingle $ ML.fromList [(1, mrgSingle $ Just 1), (2, mrgSingle $ Just 2)] ::
                  UnionMBase SBool (ML.HashMap Integer (UnionMBase SBool (Maybe Integer)))
              )
              (mrgSingle (ML.fromList [(1, mrgSingle $ Just 2), (3, mrgSingle $ Just 3)]))
              @=? mrgSingle
                ( ML.fromList
                    [ (1, mrgIf (SSBool "a") (mrgSingle $ Just 1) (mrgSingle $ Just 2)),
                      (2, mrgIf (Not $ SSBool "a") (mrgSingle Nothing) (mrgSingle $ Just 2)),
                      (3, mrgIf (SSBool "a") (mrgSingle Nothing) (mrgSingle $ Just 3))
                    ]
                )
        ]
    ]
