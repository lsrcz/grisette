{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Grisette.IR.SymPrim.Data.SymPrimTests (symPrimTests) where

import Control.DeepSeq (NFData (rnf), force)
import Control.Exception
  ( ArithException (DivideByZero, Overflow, Underflow),
    catch,
    evaluate,
  )
import Control.Monad.Except (ExceptT, MonadError (throwError))
import Data.Bits
  ( Bits
      ( bit,
        bitSizeMaybe,
        complement,
        isSigned,
        popCount,
        rotate,
        shift,
        testBit,
        xor,
        (.&.),
        (.|.)
      ),
  )
import qualified Data.HashMap.Strict as M
import qualified Data.HashSet as S
import Data.Int (Int8)
import Data.Proxy (Proxy (Proxy))
import Data.Word (Word8)
import Grisette.Core.Control.Monad.UnionM (UnionM)
import Grisette.Core.Data.BV (IntN (IntN), WordN (WordN))
import Grisette.Core.Data.Class.BitVector
  ( BV (bv),
    SizedBV
      ( sizedBVConcat,
        sizedBVExt,
        sizedBVSelect,
        sizedBVSext,
        sizedBVZext
      ),
  )
import Grisette.Core.Data.Class.EvaluateSym
  ( EvaluateSym (evaluateSym),
  )
import Grisette.Core.Data.Class.ExtractSymbolics
  ( ExtractSymbolics (extractSymbolics),
  )
import Grisette.Core.Data.Class.Function (Apply (apply), Function ((#)))
import Grisette.Core.Data.Class.GenSym
  ( genSym,
    genSymSimple,
    nameWithInfo,
  )
import Grisette.Core.Data.Class.ITEOp (ITEOp (symIte))
import Grisette.Core.Data.Class.LogicalOp
  ( LogicalOp (symImplies, symNot, symXor, (.&&), (.||)),
  )
import Grisette.Core.Data.Class.Mergeable
  ( Mergeable (rootStrategy),
    MergingStrategy (SimpleStrategy),
  )
import Grisette.Core.Data.Class.ModelOps
  ( ModelOps (emptyModel, insertValue),
    ModelRep (buildModel),
  )
import Grisette.Core.Data.Class.SEq (SEq ((./=), (.==)))
import Grisette.Core.Data.Class.SOrd
  ( SOrd (symCompare, (.<), (.<=), (.>), (.>=)),
  )
import Grisette.Core.Data.Class.SafeDivision
  ( SafeDivision
      ( safeDiv,
        safeDiv',
        safeDivMod,
        safeDivMod',
        safeMod,
        safeMod',
        safeQuot,
        safeQuot',
        safeQuotRem,
        safeQuotRem',
        safeRem,
        safeRem'
      ),
  )
import Grisette.Core.Data.Class.SafeLinearArith
  ( SafeLinearArith
      ( safeAdd,
        safeAdd',
        safeMinus,
        safeMinus',
        safeNeg,
        safeNeg'
      ),
  )
import Grisette.Core.Data.Class.SimpleMergeable
  ( SimpleMergeable (mrgIte),
    merge,
    mrgIf,
    mrgSingle,
  )
import Grisette.Core.Data.Class.Solvable
  ( Solvable (con, conView, iinfosym, isym, ssym),
    pattern Con,
  )
import Grisette.Core.Data.Class.ToCon (ToCon (toCon))
import Grisette.Core.Data.Class.ToSym (ToSym (toSym))
import Grisette.IR.SymPrim.Data.Prim.InternedTerm.InternedCtors
  ( conTerm,
    isymTerm,
    ssymTerm,
  )
import Grisette.IR.SymPrim.Data.Prim.InternedTerm.Term
  ( LinkedRep (wrapTerm),
    Term,
    TypedSymbol (SimpleSymbol),
    someTypedSymbol,
    type (-->),
  )
import Grisette.IR.SymPrim.Data.Prim.Model
  ( Model (Model),
    SymbolSet (SymbolSet),
  )
import Grisette.IR.SymPrim.Data.Prim.ModelValue (toModelValue)
import Grisette.IR.SymPrim.Data.Prim.PartialEval.BV
  ( pevalBVConcatTerm,
    pevalBVExtendTerm,
    pevalBVSelectTerm,
  )
import Grisette.IR.SymPrim.Data.Prim.PartialEval.Bits
  ( pevalAndBitsTerm,
    pevalComplementBitsTerm,
    pevalOrBitsTerm,
    pevalRotateLeftTerm,
    pevalRotateRightTerm,
    pevalShiftLeftTerm,
    pevalShiftRightTerm,
    pevalXorBitsTerm,
  )
import Grisette.IR.SymPrim.Data.Prim.PartialEval.Bool
  ( pevalAndTerm,
    pevalEqvTerm,
    pevalITETerm,
    pevalImplyTerm,
    pevalNotTerm,
    pevalOrTerm,
    pevalXorTerm,
  )
import Grisette.IR.SymPrim.Data.Prim.PartialEval.Integral
  ( pevalDivBoundedIntegralTerm,
    pevalDivIntegralTerm,
    pevalModBoundedIntegralTerm,
    pevalModIntegralTerm,
    pevalQuotBoundedIntegralTerm,
    pevalQuotIntegralTerm,
    pevalRemBoundedIntegralTerm,
    pevalRemIntegralTerm,
  )
import Grisette.IR.SymPrim.Data.Prim.PartialEval.Num
  ( pevalAbsNumTerm,
    pevalAddNumTerm,
    pevalGeNumTerm,
    pevalGtNumTerm,
    pevalLeNumTerm,
    pevalLtNumTerm,
    pevalMinusNumTerm,
    pevalSignumNumTerm,
    pevalTimesNumTerm,
    pevalUMinusNumTerm,
  )
import Grisette.IR.SymPrim.Data.Prim.PartialEval.TabularFun
  ( pevalTabularFunApplyTerm,
  )
import Grisette.IR.SymPrim.Data.SymPrim
  ( ModelSymPair ((:=)),
    SomeSymIntN (SomeSymIntN),
    SomeSymWordN (SomeSymWordN),
    SymBool (SymBool),
    SymIntN (SymIntN),
    SymInteger (SymInteger),
    SymWordN (SymWordN),
    symSize,
    symsSize,
    (-->),
    type (-~>),
    type (=~>),
  )
import Grisette.IR.SymPrim.Data.TabularFun (type (=->))
import Test.Framework (Test, TestName, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.HUnit (Assertion, assertFailure, (@=?), (@?=))
import Test.QuickCheck (Arbitrary, ioProperty)

newtype AEWrapper = AEWrapper ArithException deriving (Eq)

instance Show AEWrapper where
  show (AEWrapper x) = show x

instance NFData AEWrapper where
  rnf (AEWrapper x) = x `seq` ()

sameSafeDiv ::
  forall c s.
  ( Show s,
    Eq s,
    Eq c,
    Num c,
    Mergeable s,
    NFData c,
    Solvable c s
  ) =>
  c ->
  c ->
  (s -> s -> ExceptT ArithException UnionM s) ->
  (c -> c -> c) ->
  Assertion
sameSafeDiv i j f cf = do
  xc <- evaluate (force $ Right $ cf i j) `catch` \(e :: ArithException) -> return $ Left $ AEWrapper e
  case xc of
    Left (AEWrapper e) -> f (con i :: s) (con j) @=? merge (throwError e)
    Right c -> f (con i :: s) (con j) @=? mrgSingle (con c)

sameSafeDiv' ::
  forall c s.
  ( Show s,
    Eq s,
    Eq c,
    Num c,
    Mergeable s,
    NFData c,
    Solvable c s
  ) =>
  c ->
  c ->
  ((ArithException -> ()) -> s -> s -> ExceptT () UnionM s) ->
  (c -> c -> c) ->
  Assertion
sameSafeDiv' i j f cf = do
  xc <- evaluate (force $ Right $ cf i j) `catch` \(_ :: ArithException) -> return $ Left ()
  case xc of
    Left () -> f (const ()) (con i :: s) (con j) @=? merge (throwError ())
    Right c -> f (const ()) (con i :: s) (con j) @=? mrgSingle (con c)

sameSafeDivMod ::
  forall c s.
  ( Show s,
    Eq s,
    Eq c,
    Num c,
    Mergeable s,
    NFData c,
    Solvable c s
  ) =>
  c ->
  c ->
  (s -> s -> ExceptT ArithException UnionM (s, s)) ->
  (c -> c -> (c, c)) ->
  Assertion
sameSafeDivMod i j f cf = do
  xc <- evaluate (force $ Right $ cf i j) `catch` \(e :: ArithException) -> return $ Left $ AEWrapper e
  case xc of
    Left (AEWrapper e) -> f (con i :: s) (con j) @=? merge (throwError e)
    Right (c1, c2) -> f (con i :: s) (con j) @=? mrgSingle (con c1, con c2)

sameSafeDivMod' ::
  forall c s.
  ( Show s,
    Eq s,
    Eq c,
    Num c,
    Mergeable s,
    NFData c,
    Solvable c s
  ) =>
  c ->
  c ->
  ((ArithException -> ()) -> s -> s -> ExceptT () UnionM (s, s)) ->
  (c -> c -> (c, c)) ->
  Assertion
sameSafeDivMod' i j f cf = do
  xc <- evaluate (force $ Right $ cf i j) `catch` \(_ :: ArithException) -> return $ Left ()
  case xc of
    Left () -> f (const ()) (con i :: s) (con j) @=? merge (throwError ())
    Right (c1, c2) -> f (const ()) (con i :: s) (con j) @=? mrgSingle (con c1, con c2)

safeDivisionBoundedOnlyTests ::
  forall c s.
  (LinkedRep c s, Bounded c, Solvable c s, Eq s, Num c, Show s, Mergeable s, SEq s) =>
  (s -> s -> ExceptT ArithException UnionM s) ->
  ((ArithException -> ()) -> s -> s -> ExceptT () UnionM s) ->
  (c -> c -> c) ->
  (Term c -> Term c -> Term c) ->
  [Test]
safeDivisionBoundedOnlyTests f f' cf pf =
  [ testCase "on concrete min divided by minus one" $ do
      sameSafeDiv minBound (-1) f cf
      sameSafeDiv' minBound (-1) f' cf,
    testCase "on symbolic" $ do
      f (ssym "a" :: s) (ssym "b")
        @=? ( mrgIf
                ((ssym "b" :: s) .== con (0 :: c) :: SymBool)
                (throwError DivideByZero)
                ( mrgIf
                    ((ssym "b" :: s) .== con (-1) .&& (ssym "a" :: s) .== con (minBound :: c) :: SymBool)
                    (throwError Overflow)
                    (mrgSingle $ wrapTerm $ pf (ssymTerm "a") (ssymTerm "b"))
                ) ::
                ExceptT ArithException UnionM s
            )
      f' (const ()) (ssym "a" :: s) (ssym "b")
        @=? ( mrgIf
                ((ssym "b" :: s) .== con (0 :: c) :: SymBool)
                (throwError ())
                ( mrgIf
                    ((ssym "b" :: s) .== con (-1) .&& (ssym "a" :: s) .== con (minBound :: c) :: SymBool)
                    (throwError ())
                    (mrgSingle $ wrapTerm $ pf (ssymTerm "a") (ssymTerm "b"))
                ) ::
                ExceptT () UnionM s
            )
  ]

safeDivisionUnboundedOnlyTests ::
  forall c s.
  (LinkedRep c s, Solvable c s, Eq s, Num c, Show s, Mergeable s, SEq s) =>
  (s -> s -> ExceptT ArithException UnionM s) ->
  ((ArithException -> ()) -> s -> s -> ExceptT () UnionM s) ->
  (Term c -> Term c -> Term c) ->
  [Test]
safeDivisionUnboundedOnlyTests f f' pf =
  [ testCase "on symbolic" $ do
      f (ssym "a" :: s) (ssym "b")
        @=? ( mrgIf
                ((ssym "b" :: s) .== con (0 :: c) :: SymBool)
                (throwError DivideByZero)
                (mrgSingle $ wrapTerm $ pf (ssymTerm "a") (ssymTerm "b")) ::
                ExceptT ArithException UnionM s
            )
      f' (const ()) (ssym "a" :: s) (ssym "b")
        @=? ( mrgIf
                ((ssym "b" :: s) .== con (0 :: c) :: SymBool)
                (throwError ())
                (mrgSingle $ wrapTerm $ pf (ssymTerm "a") (ssymTerm "b")) ::
                ExceptT () UnionM s
            )
  ]

safeDivisionGeneralTests ::
  forall c c0 s.
  (LinkedRep c s, Arbitrary c0, Show c0, Solvable c s, Eq s, Num c, Show s, Mergeable s, SEq s) =>
  (c0 -> c) ->
  (s -> s -> ExceptT ArithException UnionM s) ->
  ((ArithException -> ()) -> s -> s -> ExceptT () UnionM s) ->
  (c -> c -> c) ->
  [Test]
safeDivisionGeneralTests transform f f' cf =
  [ testProperty "on concrete prop" $ \(i0 :: c0, j0 :: c0) ->
      ioProperty $ do
        let i = transform i0
        let j = transform j0
        sameSafeDiv i j f cf
        sameSafeDiv' i j f' cf,
    testProperty "on concrete divided by zero" $ \(i0 :: c0) ->
      ioProperty $ do
        let i = transform i0
        sameSafeDiv i 0 f cf
        sameSafeDiv' i 0 f' cf,
    testCase "when divided by zero" $ do
      f (ssym "a" :: s) (con 0)
        @=? (merge $ throwError DivideByZero :: ExceptT ArithException UnionM s)
      f' (const ()) (ssym "a" :: s) (con 0)
        @=? (merge $ throwError () :: ExceptT () UnionM s)
  ]

safeDivisionBoundedTests ::
  forall c c0 s.
  (LinkedRep c s, Arbitrary c0, Show c0, Bounded c, Solvable c s, Eq s, Num c, Show s, Mergeable s, SEq s) =>
  TestName ->
  (c0 -> c) ->
  (s -> s -> ExceptT ArithException UnionM s) ->
  ((ArithException -> ()) -> s -> s -> ExceptT () UnionM s) ->
  (c -> c -> c) ->
  (Term c -> Term c -> Term c) ->
  Test
safeDivisionBoundedTests name transform f f' cf pf =
  testGroup name $
    safeDivisionGeneralTests transform f f' cf
      ++ safeDivisionBoundedOnlyTests f f' cf pf

safeDivisionUnboundedTests ::
  forall c c0 s.
  (LinkedRep c s, Arbitrary c0, Show c0, Solvable c s, Eq s, Num c, Show s, Mergeable s, SEq s) =>
  TestName ->
  (c0 -> c) ->
  (s -> s -> ExceptT ArithException UnionM s) ->
  ((ArithException -> ()) -> s -> s -> ExceptT () UnionM s) ->
  (c -> c -> c) ->
  (Term c -> Term c -> Term c) ->
  Test
safeDivisionUnboundedTests name transform f f' cf pf =
  testGroup name $
    safeDivisionGeneralTests transform f f' cf
      ++ safeDivisionUnboundedOnlyTests f f' pf

safeDivModBoundedOnlyTests ::
  forall c s.
  (LinkedRep c s, Bounded c, Solvable c s, Eq s, Num c, Show s, Mergeable s, SEq s) =>
  ( s ->
    s ->
    ExceptT ArithException UnionM (s, s)
  ) ->
  ( (ArithException -> ()) ->
    s ->
    s ->
    ExceptT () UnionM (s, s)
  ) ->
  (c -> c -> (c, c)) ->
  (Term c -> Term c -> Term c) ->
  (Term c -> Term c -> Term c) ->
  [Test]
safeDivModBoundedOnlyTests f f' cf pf1 pf2 =
  [ testCase "on concrete min divided by minus one" $ do
      sameSafeDivMod minBound (-1) f cf
      sameSafeDivMod' minBound (-1) f' cf,
    testCase "on symbolic" $ do
      f (ssym "a" :: s) (ssym "b")
        @=? ( mrgIf
                ((ssym "b" :: s) .== con (0 :: c) :: SymBool)
                (throwError DivideByZero)
                ( mrgIf
                    ((ssym "b" :: s) .== con (-1) .&& (ssym "a" :: s) .== con (minBound :: c) :: SymBool)
                    (throwError Overflow)
                    ( mrgSingle
                        ( wrapTerm $ pf1 (ssymTerm "a") (ssymTerm "b"),
                          wrapTerm $ pf2 (ssymTerm "a") (ssymTerm "b")
                        )
                    )
                ) ::
                ExceptT ArithException UnionM (s, s)
            )
      f' (const ()) (ssym "a" :: s) (ssym "b")
        @=? ( mrgIf
                ((ssym "b" :: s) .== con (0 :: c) :: SymBool)
                (throwError ())
                ( mrgIf
                    ((ssym "b" :: s) .== con (-1) .&& (ssym "a" :: s) .== con (minBound :: c) :: SymBool)
                    (throwError ())
                    ( mrgSingle
                        ( wrapTerm $ pf1 (ssymTerm "a") (ssymTerm "b"),
                          wrapTerm $ pf2 (ssymTerm "a") (ssymTerm "b")
                        )
                    )
                ) ::
                ExceptT () UnionM (s, s)
            )
  ]

safeDivModUnboundedOnlyTests ::
  forall c s.
  (LinkedRep c s, Solvable c s, Eq s, Num c, Show s, Mergeable s, SEq s) =>
  ( s ->
    s ->
    ExceptT ArithException UnionM (s, s)
  ) ->
  ( (ArithException -> ()) ->
    s ->
    s ->
    ExceptT () UnionM (s, s)
  ) ->
  (Term c -> Term c -> Term c) ->
  (Term c -> Term c -> Term c) ->
  [Test]
safeDivModUnboundedOnlyTests f f' pf1 pf2 =
  [ testCase "on symbolic" $ do
      f (ssym "a" :: s) (ssym "b")
        @=? ( mrgIf
                ((ssym "b" :: s) .== con (0 :: c) :: SymBool)
                (throwError DivideByZero)
                ( mrgSingle
                    ( wrapTerm $ pf1 (ssymTerm "a") (ssymTerm "b"),
                      wrapTerm $ pf2 (ssymTerm "a") (ssymTerm "b")
                    )
                ) ::
                ExceptT ArithException UnionM (s, s)
            )
      f' (const ()) (ssym "a" :: s) (ssym "b")
        @=? ( mrgIf
                ((ssym "b" :: s) .== con (0 :: c) :: SymBool)
                (throwError ())
                ( mrgSingle
                    ( wrapTerm $ pf1 (ssymTerm "a") (ssymTerm "b"),
                      wrapTerm $ pf2 (ssymTerm "a") (ssymTerm "b")
                    )
                ) ::
                ExceptT () UnionM (s, s)
            )
  ]

safeDivModGeneralTests ::
  forall c c0 s.
  (LinkedRep c s, Arbitrary c0, Show c0, Solvable c s, Eq s, Num c, Show s, Mergeable s, SEq s) =>
  (c0 -> c) ->
  ( s ->
    s ->
    ExceptT ArithException UnionM (s, s)
  ) ->
  ( (ArithException -> ()) ->
    s ->
    s ->
    ExceptT () UnionM (s, s)
  ) ->
  (c -> c -> (c, c)) ->
  [Test]
safeDivModGeneralTests transform f f' cf =
  [ testProperty "on concrete" $ \(i0 :: c0, j0 :: c0) ->
      ioProperty $ do
        let i = transform i0
        let j = transform j0
        sameSafeDivMod i j f cf
        sameSafeDivMod' i j f' cf,
    testProperty "on concrete divided by zero" $ \(i0 :: c0) ->
      ioProperty $ do
        let i = transform i0
        sameSafeDivMod i 0 f cf
        sameSafeDivMod' i 0 f' cf,
    testCase "when divided by zero" $ do
      f (ssym "a" :: s) (con 0)
        @=? (merge $ throwError DivideByZero :: ExceptT ArithException UnionM (s, s))
      f' (const ()) (ssym "a" :: s) (con 0)
        @=? (merge $ throwError () :: ExceptT () UnionM (s, s))
  ]

safeDivModBoundedTests ::
  forall c c0 s.
  (LinkedRep c s, Arbitrary c0, Show c0, Bounded c, Solvable c s, Eq s, Num c, Show s, Mergeable s, SEq s) =>
  TestName ->
  (c0 -> c) ->
  ( s ->
    s ->
    ExceptT ArithException UnionM (s, s)
  ) ->
  ( (ArithException -> ()) ->
    s ->
    s ->
    ExceptT () UnionM (s, s)
  ) ->
  (c -> c -> (c, c)) ->
  (Term c -> Term c -> Term c) ->
  (Term c -> Term c -> Term c) ->
  Test
safeDivModBoundedTests name transform f f' cf pf1 pf2 =
  testGroup name $
    safeDivModGeneralTests transform f f' cf
      ++ safeDivModBoundedOnlyTests f f' cf pf1 pf2

safeDivModUnboundedTests ::
  forall c c0 s.
  (LinkedRep c s, Arbitrary c0, Show c0, Solvable c s, Eq s, Num c, Show s, Mergeable s, SEq s) =>
  TestName ->
  (c0 -> c) ->
  ( s ->
    s ->
    ExceptT ArithException UnionM (s, s)
  ) ->
  ( (ArithException -> ()) ->
    s ->
    s ->
    ExceptT () UnionM (s, s)
  ) ->
  (c -> c -> (c, c)) ->
  (Term c -> Term c -> Term c) ->
  (Term c -> Term c -> Term c) ->
  Test
safeDivModUnboundedTests name transform f f' cf pf1 pf2 =
  testGroup name $
    safeDivModGeneralTests transform f f' cf
      ++ safeDivModUnboundedOnlyTests f f' pf1 pf2

symPrimTests :: Test
symPrimTests =
  testGroup
    "SymPrim"
    [ testGroup
        "General SymPrim"
        [ testGroup
            "Solvable"
            [ testCase "con" $ (con 1 :: SymInteger) @=? SymInteger (conTerm 1),
              testCase "ssym" $ (ssym "a" :: SymInteger) @=? SymInteger (ssymTerm "a"),
              testCase "isym" $ (isym "a" 1 :: SymInteger) @=? SymInteger (isymTerm "a" 1),
              testCase "conView" $ do
                conView (con 1 :: SymInteger) @=? Just 1
                conView (ssym "a" :: SymInteger) @=? Nothing
                case con 1 :: SymInteger of
                  Con 1 -> return ()
                  _ -> assertFailure "Bad match"
                case ssym "a" :: SymInteger of
                  Con _ -> assertFailure "Bad match"
                  _ -> return ()
            ],
          testGroup
            "ITEOp"
            [ testCase "symIte" $
                symIte (ssym "a" :: SymBool) (ssym "b" :: SymInteger) (ssym "c")
                  @=? SymInteger (pevalITETerm (ssymTerm "a") (ssymTerm "b") (ssymTerm "c"))
            ],
          testCase "Mergeable" $ do
            let SimpleStrategy s = rootStrategy :: MergingStrategy SymInteger
            s (ssym "a") (ssym "b") (ssym "c")
              @=? symIte (ssym "a" :: SymBool) (ssym "b" :: SymInteger) (ssym "c"),
          testCase "SimpleMergeable" $
            mrgIte (ssym "a" :: SymBool) (ssym "b") (ssym "c")
              @=? symIte (ssym "a" :: SymBool) (ssym "b" :: SymInteger) (ssym "c"),
          testCase "IsString" $ ("a" :: SymBool) @=? SymBool (ssymTerm "a"),
          testGroup
            "ToSym"
            [ testCase "From self" $ toSym (ssym "a" :: SymBool) @=? (ssym "a" :: SymBool),
              testCase "From concrete" $ toSym True @=? (con True :: SymBool)
            ],
          testGroup
            "ToCon"
            [ testCase "To self" $ toCon (ssym "a" :: SymBool) @=? (Nothing :: Maybe Bool),
              testCase "To concrete" $ toCon True @=? Just True
            ],
          testCase "EvaluateSym" $ do
            let m1 = emptyModel :: Model
            let m2 = insertValue (SimpleSymbol "a") (1 :: Integer) m1
            let m3 = insertValue (SimpleSymbol "b") True m2
            evaluateSym False m3 (symIte ("c" :: SymBool) "a" ("a" + "a" :: SymInteger))
              @=? symIte ("c" :: SymBool) 1 2
            evaluateSym True m3 (symIte ("c" :: SymBool) "a" ("a" + "a" :: SymInteger)) @=? 2,
          testCase "ExtractSymbolics" $
            extractSymbolics (symIte ("c" :: SymBool) ("a" :: SymInteger) ("b" :: SymInteger))
              @=? SymbolSet
                ( S.fromList
                    [ someTypedSymbol (SimpleSymbol "c" :: TypedSymbol Bool),
                      someTypedSymbol (SimpleSymbol "a" :: TypedSymbol Integer),
                      someTypedSymbol (SimpleSymbol "b" :: TypedSymbol Integer)
                    ]
                ),
          testCase "GenSym" $ do
            (genSym () "a" :: UnionM SymBool) @=? mrgSingle (isym "a" 0)
            (genSymSimple () "a" :: SymBool) @=? isym "a" 0
            (genSym (ssym "a" :: SymBool) "a" :: UnionM SymBool) @=? mrgSingle (isym "a" 0)
            (genSymSimple (ssym "a" :: SymBool) "a" :: SymBool) @=? isym "a" 0
            (genSym () (nameWithInfo "a" True) :: UnionM SymBool) @=? mrgSingle (iinfosym "a" 0 True)
            (genSymSimple () (nameWithInfo "a" True) :: SymBool) @=? iinfosym "a" 0 True,
          testCase "SEq" $ do
            (ssym "a" :: SymBool) .== ssym "b" @=? SymBool (pevalEqvTerm (ssymTerm "a" :: Term Bool) (ssymTerm "b"))
            (ssym "a" :: SymBool) ./= ssym "b" @=? SymBool (pevalNotTerm $ pevalEqvTerm (ssymTerm "a" :: Term Bool) (ssymTerm "b"))
        ],
      testGroup
        "SymBool"
        [ testGroup
            "LogicalOp"
            [ testCase ".||" $ ssym "a" .|| ssym "b" @=? SymBool (pevalOrTerm (ssymTerm "a") (ssymTerm "b")),
              testCase ".&&" $ ssym "a" .&& ssym "b" @=? SymBool (pevalAndTerm (ssymTerm "a") (ssymTerm "b")),
              testCase "symNot" $ symNot (ssym "a") @=? SymBool (pevalNotTerm (ssymTerm "a")),
              testCase "symXor" $ symXor (ssym "a") (ssym "b") @=? SymBool (pevalXorTerm (ssymTerm "a") (ssymTerm "b")),
              testCase "symImplies" $ symImplies (ssym "a") (ssym "b") @=? SymBool (pevalImplyTerm (ssymTerm "a") (ssymTerm "b"))
            ]
        ],
      testGroup
        "SymInteger"
        [ testGroup
            "Num"
            [ testCase "fromInteger" $ (1 :: SymInteger) @=? SymInteger (conTerm 1),
              testCase "(+)" $ (ssym "a" :: SymInteger) + ssym "b" @=? SymInteger (pevalAddNumTerm (ssymTerm "a") (ssymTerm "b")),
              testCase "(-)" $ (ssym "a" :: SymInteger) - ssym "b" @=? SymInteger (pevalMinusNumTerm (ssymTerm "a") (ssymTerm "b")),
              testCase "(*)" $ (ssym "a" :: SymInteger) * ssym "b" @=? SymInteger (pevalTimesNumTerm (ssymTerm "a") (ssymTerm "b")),
              testCase "negate" $ negate (ssym "a" :: SymInteger) @=? SymInteger (pevalUMinusNumTerm (ssymTerm "a")),
              testCase "abs" $ abs (ssym "a" :: SymInteger) @=? SymInteger (pevalAbsNumTerm (ssymTerm "a")),
              testCase "signum" $ signum (ssym "a" :: SymInteger) @=? SymInteger (pevalSignumNumTerm (ssymTerm "a"))
            ],
          testGroup
            "SafeDivision"
            [ safeDivisionUnboundedTests @Integer "safeDiv" id safeDiv safeDiv' div pevalDivIntegralTerm,
              safeDivisionUnboundedTests @Integer "safeMod" id safeMod safeMod' mod pevalModIntegralTerm,
              safeDivModUnboundedTests @Integer "safeDivMod" id safeDivMod safeDivMod' divMod pevalDivIntegralTerm pevalModIntegralTerm,
              safeDivisionUnboundedTests @Integer "safeQuot" id safeQuot safeQuot' quot pevalQuotIntegralTerm,
              safeDivisionUnboundedTests @Integer "safeRem" id safeRem safeRem' rem pevalRemIntegralTerm,
              safeDivModUnboundedTests @Integer "safeQuotRem" id safeQuotRem safeQuotRem' quotRem pevalQuotIntegralTerm pevalRemIntegralTerm
            ],
          testGroup
            "SafeLinearArith"
            [ testProperty "safeAdd on concrete" $ \(i :: Integer, j :: Integer) ->
                ioProperty $ do
                  safeAdd (con i :: SymInteger) (con j)
                    @=? (mrgSingle $ con $ i + j :: ExceptT ArithException UnionM SymInteger)
                  safeAdd' (const ()) (con i :: SymInteger) (con j)
                    @=? (mrgSingle $ con $ i + j :: ExceptT () UnionM SymInteger),
              testCase "safeAdd on symbolic" $ do
                safeAdd (ssym "a" :: SymInteger) (ssym "b")
                  @=? (mrgSingle $ SymInteger $ pevalAddNumTerm (ssymTerm "a") (ssymTerm "b") :: ExceptT ArithException UnionM SymInteger)
                safeAdd' (const ()) (ssym "a" :: SymInteger) (ssym "b")
                  @=? (mrgSingle $ SymInteger $ pevalAddNumTerm (ssymTerm "a") (ssymTerm "b") :: ExceptT () UnionM SymInteger),
              testProperty "safeNeg on concrete" $ \(i :: Integer) ->
                ioProperty $ do
                  safeNeg (con i :: SymInteger)
                    @=? (mrgSingle $ con $ -i :: ExceptT ArithException UnionM SymInteger)
                  safeNeg' (const ()) (con i :: SymInteger)
                    @=? (mrgSingle $ con $ -i :: ExceptT () UnionM SymInteger),
              testCase "safeNeg on symbolic" $ do
                safeNeg (ssym "a" :: SymInteger)
                  @=? (mrgSingle $ SymInteger $ pevalUMinusNumTerm (ssymTerm "a") :: ExceptT ArithException UnionM SymInteger)
                safeNeg' (const ()) (ssym "a" :: SymInteger)
                  @=? (mrgSingle $ SymInteger $ pevalUMinusNumTerm (ssymTerm "a") :: ExceptT () UnionM SymInteger),
              testProperty "safeMinus on concrete" $ \(i :: Integer, j :: Integer) ->
                ioProperty $ do
                  safeMinus (con i :: SymInteger) (con j)
                    @=? (mrgSingle $ con $ i - j :: ExceptT ArithException UnionM SymInteger)
                  safeMinus' (const ()) (con i :: SymInteger) (con j)
                    @=? (mrgSingle $ con $ i - j :: ExceptT () UnionM SymInteger),
              testCase "safeMinus on symbolic" $ do
                safeMinus (ssym "a" :: SymInteger) (ssym "b")
                  @=? (mrgSingle $ SymInteger $ pevalMinusNumTerm (ssymTerm "a") (ssymTerm "b") :: ExceptT ArithException UnionM SymInteger)
                safeMinus' (const ()) (ssym "a" :: SymInteger) (ssym "b")
                  @=? (mrgSingle $ SymInteger $ pevalMinusNumTerm (ssymTerm "a") (ssymTerm "b") :: ExceptT () UnionM SymInteger)
            ],
          testGroup
            "SOrd"
            [ testProperty "SOrd on concrete" $ \(i :: Integer, j :: Integer) -> ioProperty $ do
                (con i :: SymInteger) .<= con j @=? (con (i <= j) :: SymBool)
                (con i :: SymInteger) .< con j @=? (con (i < j) :: SymBool)
                (con i :: SymInteger) .>= con j @=? (con (i >= j) :: SymBool)
                (con i :: SymInteger) .> con j @=? (con (i > j) :: SymBool)
                (con i :: SymInteger)
                  `symCompare` con j
                  @=? (i `symCompare` j :: UnionM Ordering),
              testCase "SOrd on symbolic" $ do
                let a :: SymInteger = ssym "a"
                let b :: SymInteger = ssym "b"
                let at :: Term Integer = ssymTerm "a"
                let bt :: Term Integer = ssymTerm "b"
                a .<= b @=? SymBool (pevalLeNumTerm at bt)
                a .< b @=? SymBool (pevalLtNumTerm at bt)
                a .>= b @=? SymBool (pevalGeNumTerm at bt)
                a .> b @=? SymBool (pevalGtNumTerm at bt)
                (a `symCompare` ssym "b" :: UnionM Ordering)
                  @=? mrgIf (a .< b) (mrgSingle LT) (mrgIf (a .== b) (mrgSingle EQ) (mrgSingle GT))
            ]
        ],
      let au :: SymWordN 4 = ssym "a"
          bu :: SymWordN 4 = ssym "b"
          as :: SymIntN 4 = ssym "a"
          bs :: SymIntN 4 = ssym "b"
          aut :: Term (WordN 4) = ssymTerm "a"
          but :: Term (WordN 4) = ssymTerm "b"
          ast :: Term (IntN 4) = ssymTerm "a"
          bst :: Term (IntN 4) = ssymTerm "b"
       in testGroup
            "Sym BV"
            [ testGroup
                "Num"
                [ testCase "fromInteger" $ do
                    (1 :: SymWordN 4) @=? SymWordN (conTerm 1)
                    (1 :: SymIntN 4) @=? SymIntN (conTerm 1),
                  testCase "(+)" $ do
                    au + bu @=? SymWordN (pevalAddNumTerm aut but)
                    as + bs @=? SymIntN (pevalAddNumTerm ast bst),
                  testCase "(-)" $ do
                    au - bu @=? SymWordN (pevalMinusNumTerm aut but)
                    as - bs @=? SymIntN (pevalMinusNumTerm ast bst),
                  testCase "(*)" $ do
                    au * bu @=? SymWordN (pevalTimesNumTerm aut but)
                    as * bs @=? SymIntN (pevalTimesNumTerm ast bst),
                  testCase "negate" $ do
                    negate au @=? SymWordN (pevalUMinusNumTerm aut)
                    negate as @=? SymIntN (pevalUMinusNumTerm ast),
                  testCase "abs" $ do
                    abs au @=? SymWordN (pevalAbsNumTerm aut)
                    abs as @=? SymIntN (pevalAbsNumTerm ast),
                  testCase "signum" $ do
                    signum au @=? SymWordN (pevalSignumNumTerm aut)
                    signum as @=? SymIntN (pevalSignumNumTerm ast)
                ],
              testGroup
                "SafeDivision"
                [ testGroup
                    "WordN"
                    [ safeDivisionUnboundedTests @(WordN 4) "safeDiv" WordN safeDiv safeDiv' div pevalDivIntegralTerm,
                      safeDivisionUnboundedTests @(WordN 4) "safeMod" WordN safeMod safeMod' mod pevalModIntegralTerm,
                      safeDivModUnboundedTests @(WordN 4) "safeDivMod" WordN safeDivMod safeDivMod' divMod pevalDivIntegralTerm pevalModIntegralTerm,
                      safeDivisionUnboundedTests @(WordN 4) "safeQuot" WordN safeQuot safeQuot' quot pevalQuotIntegralTerm,
                      safeDivisionUnboundedTests @(WordN 4) "safeRem" WordN safeRem safeRem' rem pevalRemIntegralTerm,
                      safeDivModUnboundedTests @(WordN 4) "safeQuotRem" WordN safeQuotRem safeQuotRem' divMod pevalQuotIntegralTerm pevalRemIntegralTerm
                    ],
                  testGroup
                    "IntN"
                    [ safeDivisionBoundedTests @(IntN 4) "safeDiv" IntN safeDiv safeDiv' div pevalDivBoundedIntegralTerm,
                      safeDivisionUnboundedTests @(IntN 4) "safeMod" IntN safeMod safeMod' mod pevalModBoundedIntegralTerm,
                      safeDivModBoundedTests @(IntN 4) "safeDivMod" IntN safeDivMod safeDivMod' divMod pevalDivBoundedIntegralTerm pevalModBoundedIntegralTerm,
                      safeDivisionBoundedTests @(IntN 4) "safeQuot" IntN safeQuot safeQuot' quot pevalQuotBoundedIntegralTerm,
                      safeDivisionUnboundedTests @(IntN 4) "safeRem" IntN safeRem safeRem' rem pevalRemBoundedIntegralTerm,
                      safeDivModBoundedTests @(IntN 4) "safeQuotRem" IntN safeQuotRem safeQuotRem' quotRem pevalQuotBoundedIntegralTerm pevalRemBoundedIntegralTerm
                    ]
                ],
              testGroup
                "SafeLinearArith"
                [ testGroup
                    "IntN"
                    [ testProperty "safeAdd on concrete" $ \(i :: Int8, j :: Int8) ->
                        ioProperty $
                          let iint = fromIntegral i :: Integer
                              jint = fromIntegral j
                           in safeAdd (toSym i :: SymIntN 8) (toSym j)
                                @=? mrgIf
                                  (iint + jint .< fromIntegral (i + j))
                                  (throwError Underflow)
                                  ( mrgIf
                                      (iint + jint .> fromIntegral (i + j))
                                      (throwError Overflow)
                                      (mrgSingle $ toSym $ i + j :: ExceptT ArithException UnionM (SymIntN 8))
                                  ),
                      testProperty "safeMinus on concrete" $ \(i :: Int8, j :: Int8) ->
                        ioProperty $
                          let iint = fromIntegral i :: Integer
                              jint = fromIntegral j
                           in safeMinus (toSym i :: SymIntN 8) (toSym j)
                                @=? mrgIf
                                  (iint - jint .< fromIntegral (i - j))
                                  (throwError Underflow)
                                  ( mrgIf
                                      (iint - jint .> fromIntegral (i - j))
                                      (throwError Overflow)
                                      (mrgSingle $ toSym $ i - j :: ExceptT ArithException UnionM (SymIntN 8))
                                  ),
                      testProperty "safeNeg on concrete" $ \(i :: Int8) ->
                        ioProperty $
                          let iint = fromIntegral i :: Integer
                           in safeNeg (toSym i :: SymIntN 8)
                                @=? mrgIf
                                  (-iint .< fromIntegral (-i))
                                  (throwError Underflow)
                                  ( mrgIf
                                      (-iint .> fromIntegral (-i))
                                      (throwError Overflow)
                                      (mrgSingle $ toSym $ -i :: ExceptT ArithException UnionM (SymIntN 8))
                                  )
                    ],
                  testGroup
                    "WordN"
                    [ testProperty "safeAdd on concrete" $ \(i :: Word8, j :: Word8) ->
                        ioProperty $
                          let iint = fromIntegral i :: Integer
                              jint = fromIntegral j
                           in safeAdd (toSym i :: SymWordN 8) (toSym j)
                                @=? mrgIf
                                  (iint + jint .< fromIntegral (i + j))
                                  (throwError Underflow)
                                  ( mrgIf
                                      (iint + jint .> fromIntegral (i + j))
                                      (throwError Overflow)
                                      (mrgSingle $ toSym $ i + j :: ExceptT ArithException UnionM (SymWordN 8))
                                  ),
                      testProperty "safeMinus on concrete" $ \(i :: Word8, j :: Word8) ->
                        ioProperty $
                          let iint = fromIntegral i :: Integer
                              jint = fromIntegral j
                           in safeMinus (toSym i :: SymWordN 8) (toSym j)
                                @=? mrgIf
                                  (iint - jint .< fromIntegral (i - j))
                                  (throwError Underflow)
                                  ( mrgIf
                                      (iint - jint .> fromIntegral (i - j))
                                      (throwError Overflow)
                                      (mrgSingle $ toSym $ i - j :: ExceptT ArithException UnionM (SymWordN 8))
                                  ),
                      testProperty "safeNeg on concrete" $ \(i :: Word8) ->
                        ioProperty $
                          let iint = fromIntegral i :: Integer
                           in safeNeg (toSym i :: SymWordN 8)
                                @=? mrgIf
                                  (-iint .< fromIntegral (-i))
                                  (throwError Underflow)
                                  ( mrgIf
                                      (-iint .> fromIntegral (-i))
                                      (throwError Overflow)
                                      (mrgSingle $ toSym $ -i :: ExceptT ArithException UnionM (SymWordN 8))
                                  )
                    ]
                ],
              testGroup
                "SOrd"
                [ testProperty "SOrd on concrete" $ \(i :: Integer, j :: Integer) -> ioProperty $ do
                    let iu :: WordN 4 = fromInteger i
                    let ju :: WordN 4 = fromInteger j
                    let is :: IntN 4 = fromInteger i
                    let js :: IntN 4 = fromInteger j
                    let normalizeu k = k - k `div` 16 * 16
                    let normalizes k = if normalizeu k >= 8 then normalizeu k - 16 else normalizeu k
                    (con iu :: SymWordN 4) .<= con ju @=? (con (normalizeu i <= normalizeu j) :: SymBool)
                    (con iu :: SymWordN 4) .< con ju @=? (con (normalizeu i < normalizeu j) :: SymBool)
                    (con iu :: SymWordN 4) .>= con ju @=? (con (normalizeu i >= normalizeu j) :: SymBool)
                    (con iu :: SymWordN 4) .> con ju @=? (con (normalizeu i > normalizeu j) :: SymBool)
                    (con iu :: SymWordN 4)
                      `symCompare` con ju
                      @=? (normalizeu i `symCompare` normalizeu j :: UnionM Ordering)
                    (con is :: SymIntN 4) .<= con js @=? (con (normalizes i <= normalizes j) :: SymBool)
                    (con is :: SymIntN 4) .< con js @=? (con (normalizes i < normalizes j) :: SymBool)
                    (con is :: SymIntN 4) .>= con js @=? (con (normalizes i >= normalizes j) :: SymBool)
                    (con is :: SymIntN 4) .> con js @=? (con (normalizes i > normalizes j) :: SymBool)
                    (con is :: SymIntN 4)
                      `symCompare` con js
                      @=? (normalizes i `symCompare` normalizes j :: UnionM Ordering),
                  testCase "SOrd on symbolic" $ do
                    au .<= bu @=? SymBool (pevalLeNumTerm aut but)
                    au .< bu @=? SymBool (pevalLtNumTerm aut but)
                    au .>= bu @=? SymBool (pevalGeNumTerm aut but)
                    au .> bu @=? SymBool (pevalGtNumTerm aut but)
                    (au `symCompare` bu :: UnionM Ordering)
                      @=? mrgIf (au .< bu) (mrgSingle LT) (mrgIf (au .== bu) (mrgSingle EQ) (mrgSingle GT))

                    as .<= bs @=? SymBool (pevalLeNumTerm ast bst)
                    as .< bs @=? SymBool (pevalLtNumTerm ast bst)
                    as .>= bs @=? SymBool (pevalGeNumTerm ast bst)
                    as .> bs @=? SymBool (pevalGtNumTerm ast bst)
                    (as `symCompare` bs :: UnionM Ordering)
                      @=? mrgIf (as .< bs) (mrgSingle LT) (mrgIf (as .== bs) (mrgSingle EQ) (mrgSingle GT))
                ],
              testGroup
                "Bits"
                [ testCase ".&." $ do
                    au .&. bu @=? SymWordN (pevalAndBitsTerm aut but)
                    as .&. bs @=? SymIntN (pevalAndBitsTerm ast bst),
                  testCase ".|." $ do
                    au .|. bu @=? SymWordN (pevalOrBitsTerm aut but)
                    as .|. bs @=? SymIntN (pevalOrBitsTerm ast bst),
                  testCase "xor" $ do
                    au `xor` bu @=? SymWordN (pevalXorBitsTerm aut but)
                    as `xor` bs @=? SymIntN (pevalXorBitsTerm ast bst),
                  testCase "complement" $ do
                    complement au @=? SymWordN (pevalComplementBitsTerm aut)
                    complement as @=? SymIntN (pevalComplementBitsTerm ast),
                  testCase "shift" $ do
                    shift au 1 @=? SymWordN (pevalShiftLeftTerm aut $ conTerm 1)
                    shift as 1 @=? SymIntN (pevalShiftLeftTerm ast $ conTerm 1)
                    shift au (-1) @=? SymWordN (pevalShiftRightTerm aut $ conTerm 1)
                    shift as (-1) @=? SymIntN (pevalShiftRightTerm ast $ conTerm 1),
                  testCase "rotate" $ do
                    rotate au 1 @=? SymWordN (pevalRotateLeftTerm aut $ conTerm 1)
                    rotate as 1 @=? SymIntN (pevalRotateLeftTerm ast $ conTerm 1)
                    rotate au (-1) @=? SymWordN (pevalRotateRightTerm aut $ conTerm 1)
                    rotate as (-1) @=? SymIntN (pevalRotateRightTerm ast $ conTerm 1),
                  testCase "bitSize" $ do
                    bitSizeMaybe au @=? Just 4
                    bitSizeMaybe as @=? Just 4,
                  testCase "isSigned" $ do
                    isSigned au @=? False
                    isSigned as @=? True,
                  testCase "testBit would only work on concrete ones" $ do
                    testBit (con 3 :: SymWordN 4) 1 @=? True
                    testBit (con 3 :: SymWordN 4) 2 @=? False
                    testBit (con 3 :: SymIntN 4) 1 @=? True
                    testBit (con 3 :: SymIntN 4) 2 @=? False,
                  testCase "bit would work" $ do
                    bit 1 @=? (con 2 :: SymWordN 4)
                    bit 1 @=? (con 2 :: SymIntN 4),
                  testCase "popCount would only work on concrete ones" $ do
                    popCount (con 3 :: SymWordN 4) @=? 2
                    popCount (con 3 :: SymWordN 4) @=? 2
                    popCount (con 3 :: SymIntN 4) @=? 2
                    popCount (con 3 :: SymIntN 4) @=? 2
                ],
              testGroup
                "sizedBVConcat"
                [ testCase "sizedBVConcat" $
                    sizedBVConcat
                      (ssym "a" :: SymWordN 4)
                      (ssym "b" :: SymWordN 3)
                      @=? SymWordN
                        ( pevalBVConcatTerm
                            (ssymTerm "a" :: Term (WordN 4))
                            (ssymTerm "b" :: Term (WordN 3))
                        )
                ],
              testGroup
                "sizedBVExt for Sym BV"
                [ testCase "sizedBVZext" $ do
                    sizedBVZext (Proxy @6) au @=? SymWordN (pevalBVExtendTerm False (Proxy @6) aut)
                    sizedBVZext (Proxy @6) as @=? SymIntN (pevalBVExtendTerm False (Proxy @6) ast),
                  testCase "sizedBVSext" $ do
                    sizedBVSext (Proxy @6) au @=? SymWordN (pevalBVExtendTerm True (Proxy @6) aut)
                    sizedBVSext (Proxy @6) as @=? SymIntN (pevalBVExtendTerm True (Proxy @6) ast),
                  testCase "sizedBVExt" $ do
                    sizedBVExt (Proxy @6) au @=? SymWordN (pevalBVExtendTerm False (Proxy @6) aut)
                    sizedBVExt (Proxy @6) as @=? SymIntN (pevalBVExtendTerm True (Proxy @6) ast)
                ],
              testGroup
                "sizedBVSelect for Sym BV"
                [ testCase "sizedBVSelect" $ do
                    sizedBVSelect (Proxy @2) (Proxy @1) au
                      @=? SymWordN (pevalBVSelectTerm (Proxy @2) (Proxy @1) aut)
                    sizedBVSelect (Proxy @2) (Proxy @1) as
                      @=? SymIntN (pevalBVSelectTerm (Proxy @2) (Proxy @1) ast)
                ],
              testGroup
                "conversion between Int8 and Sym BV"
                [ testCase "toSym" $ do
                    toSym (0 :: Int8) @=? (con 0 :: SymIntN 8)
                    toSym (-127 :: Int8) @=? (con $ -127 :: SymIntN 8)
                    toSym (-128 :: Int8) @=? (con $ -128 :: SymIntN 8)
                    toSym (127 :: Int8) @=? (con 127 :: SymIntN 8),
                  testCase "toCon" $ do
                    toCon (con 0 :: SymIntN 8) @=? Just (0 :: Int8)
                    toCon (con $ -127 :: SymIntN 8) @=? Just (-127 :: Int8)
                    toCon (con $ -128 :: SymIntN 8) @=? Just (-128 :: Int8)
                    toCon (con 127 :: SymIntN 8) @=? Just (127 :: Int8)
                ],
              testGroup
                "conversion between Word8 and Sym BV"
                [ testCase "toSym" $ do
                    toSym (0 :: Word8) @=? (con 0 :: SymWordN 8)
                    toSym (1 :: Word8) @=? (con 1 :: SymWordN 8)
                    toSym (255 :: Word8) @=? (con 255 :: SymWordN 8),
                  testCase "toCon" $ do
                    toCon (con 0 :: SymWordN 8) @=? Just (0 :: Word8)
                    toCon (con 1 :: SymWordN 8) @=? Just (1 :: Word8)
                    toCon (con 255 :: SymWordN 8) @=? Just (255 :: Word8)
                ]
            ],
      testGroup
        "SomeSym"
        [ testGroup
            "BV"
            [ testCase "bv" $ do
                (bv 12 21 :: SomeSymWordN) @?= SomeSymWordN (21 :: SymWordN 12)
                (bv 12 21 :: SomeSymIntN) @?= SomeSymIntN (21 :: SymIntN 12)
            ]
        ],
      testGroup
        "TabularFun"
        [ testCase "#" $
            (ssym "a" :: SymInteger =~> SymInteger)
              # ssym "b"
              @=? SymInteger (pevalTabularFunApplyTerm (ssymTerm "a" :: Term (Integer =-> Integer)) (ssymTerm "b")),
          testCase "apply" $
            apply
              (ssym "f" :: SymInteger =~> SymInteger =~> SymInteger)
              (ssym "a")
              (ssym "b")
              @=? SymInteger
                ( pevalTabularFunApplyTerm
                    ( pevalTabularFunApplyTerm
                        (ssymTerm "f" :: Term (Integer =-> Integer =-> Integer))
                        (ssymTerm "a")
                    )
                    (ssymTerm "b")
                )
        ],
      testGroup
        "GeneralFun"
        [ testCase "evaluate" $ do
            evaluateSym
              False
              (buildModel ("a" := (1 :: Integer), "b" := (2 :: Integer)))
              (con ("a" --> "a" + "b") :: SymInteger -~> SymInteger)
              @=? (con ("a" --> "a" + 2) :: SymInteger -~> SymInteger)
            evaluateSym
              False
              (buildModel ("a" := (1 :: Integer), "b" := (2 :: Integer), "c" := (3 :: Integer)))
              (con ("a" --> con ("b" --> "a" + "b" + "c")) :: SymInteger -~> SymInteger -~> SymInteger)
              @=? con ("a" --> con ("b" --> "a" + "b" + 3) :: Integer --> Integer --> Integer),
          testCase "#" $ do
            let f :: SymInteger -~> SymInteger -~> SymInteger =
                  con ("a" --> con ("b" --> "a" + "b"))
            f # ssym "x" @=? con ("b" --> "x" + "b"),
          testCase "apply" $ do
            let f :: SymInteger -~> SymInteger -~> SymInteger =
                  con ("a" --> con ("b" --> "a" + "b"))
            apply f "x" "y" @=? "x" + "y"
        ],
      testGroup
        "Symbolic size"
        [ testCase "symSize" $ do
            symSize (ssym "a" :: SymInteger) @=? 1
            symSize (con 1 :: SymInteger) @=? 1
            symSize (con 1 + ssym "a" :: SymInteger) @=? 3
            symSize (ssym "a" + ssym "a" :: SymInteger) @=? 2
            symSize (-(ssym "a") :: SymInteger) @=? 2
            symSize (symIte (ssym "a" :: SymBool) (ssym "b") (ssym "c") :: SymInteger) @=? 4,
          testCase "symsSize" $ symsSize [ssym "a" :: SymInteger, ssym "a" + ssym "a"] @=? 2
        ],
      let asymbol :: TypedSymbol Integer = "a"
          bsymbol :: TypedSymbol Bool = "b"
          csymbol :: TypedSymbol Integer = "c"
          dsymbol :: TypedSymbol Bool = "d"
          esymbol :: TypedSymbol (WordN 4) = "e"
          fsymbol :: TypedSymbol (IntN 4) = "f"
          gsymbol :: TypedSymbol (WordN 16) = "g"
          hsymbol :: TypedSymbol (IntN 16) = "h"
          va :: Integer = 1
          vc :: Integer = 2
          ve :: WordN 4 = 3
          vf :: IntN 4 = 4
          vg :: WordN 16 = 5
          vh :: IntN 16 = 6
       in testCase
            "construting Model from ModelSymPair"
            $ do
              buildModel ("a" := va) @=? Model (M.singleton (someTypedSymbol asymbol) (toModelValue va))
              buildModel ("a" := va, "b" := True)
                @=? Model
                  ( M.fromList
                      [ (someTypedSymbol asymbol, toModelValue va),
                        (someTypedSymbol bsymbol, toModelValue True)
                      ]
                  )
              buildModel
                ( "a" := va,
                  "b" := True,
                  "c" := vc
                )
                @=? Model
                  ( M.fromList
                      [ (someTypedSymbol asymbol, toModelValue va),
                        (someTypedSymbol bsymbol, toModelValue True),
                        (someTypedSymbol csymbol, toModelValue vc)
                      ]
                  )
              buildModel
                ( "a" := va,
                  "b" := True,
                  "c" := vc,
                  "d" := False
                )
                @=? Model
                  ( M.fromList
                      [ (someTypedSymbol asymbol, toModelValue va),
                        (someTypedSymbol bsymbol, toModelValue True),
                        (someTypedSymbol csymbol, toModelValue vc),
                        (someTypedSymbol dsymbol, toModelValue False)
                      ]
                  )
              buildModel
                ( "a" := va,
                  "b" := True,
                  "c" := vc,
                  "d" := False,
                  "e" := ve
                )
                @=? Model
                  ( M.fromList
                      [ (someTypedSymbol asymbol, toModelValue va),
                        (someTypedSymbol bsymbol, toModelValue True),
                        (someTypedSymbol csymbol, toModelValue vc),
                        (someTypedSymbol dsymbol, toModelValue False),
                        (someTypedSymbol esymbol, toModelValue ve)
                      ]
                  )
              buildModel
                ( "a" := va,
                  "b" := True,
                  "c" := vc,
                  "d" := False,
                  "e" := ve,
                  "f" := vf
                )
                @=? Model
                  ( M.fromList
                      [ (someTypedSymbol asymbol, toModelValue va),
                        (someTypedSymbol bsymbol, toModelValue True),
                        (someTypedSymbol csymbol, toModelValue vc),
                        (someTypedSymbol dsymbol, toModelValue False),
                        (someTypedSymbol esymbol, toModelValue ve),
                        (someTypedSymbol fsymbol, toModelValue vf)
                      ]
                  )
              buildModel
                ( "a" := va,
                  "b" := True,
                  "c" := vc,
                  "d" := False,
                  "e" := ve,
                  "f" := vf,
                  "g" := vg
                )
                @=? Model
                  ( M.fromList
                      [ (someTypedSymbol asymbol, toModelValue va),
                        (someTypedSymbol bsymbol, toModelValue True),
                        (someTypedSymbol csymbol, toModelValue vc),
                        (someTypedSymbol dsymbol, toModelValue False),
                        (someTypedSymbol esymbol, toModelValue ve),
                        (someTypedSymbol fsymbol, toModelValue vf),
                        (someTypedSymbol gsymbol, toModelValue vg)
                      ]
                  )
              buildModel
                ( "a" := va,
                  "b" := True,
                  "c" := vc,
                  "d" := False,
                  "e" := ve,
                  "f" := vf,
                  "g" := vg,
                  "h" := vh
                )
                @=? Model
                  ( M.fromList
                      [ (someTypedSymbol asymbol, toModelValue va),
                        (someTypedSymbol bsymbol, toModelValue True),
                        (someTypedSymbol csymbol, toModelValue vc),
                        (someTypedSymbol dsymbol, toModelValue False),
                        (someTypedSymbol esymbol, toModelValue ve),
                        (someTypedSymbol fsymbol, toModelValue vf),
                        (someTypedSymbol gsymbol, toModelValue vg),
                        (someTypedSymbol hsymbol, toModelValue vh)
                      ]
                  )
    ]
