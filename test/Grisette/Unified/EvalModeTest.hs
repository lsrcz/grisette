{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# HLINT ignore "Unused LANGUAGE pragma" #-}
{-# HLINT ignore "Use fewer imports" #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Grisette.Unified.EvalModeTest (evalModeTest) where

#if !MIN_VERSION_base(4,16,0)
import Grisette.Unified
  ( SafeUnifiedBV,
    SafeUnifiedBVFPConversion,
    SafeUnifiedSomeBV,
    UnifiedBV,
    UnifiedData,
    UnifiedBVBVConversion,
    UnifiedBVFPConversion,
    UnifiedFPFPConversion
  )
#endif

import Control.Exception (ArithException (DivideByZero))
import Control.Monad.Error.Class (MonadError)
import Control.Monad.Except (ExceptT (ExceptT))
import Control.Monad.Identity (Identity (Identity))
import GHC.Generics (Generic)
import GHC.TypeLits (KnownNat, type (<=))
import Grisette
  ( BV (bv),
    BitCast (bitCast),
    Default (Default),
    Function ((#)),
    IEEEFPConstants (fpNaN),
    IEEEFPConvertible (toFP),
    IEEEFPRoundingMode (rne),
    IntN,
    Mergeable,
    MonadTryMerge,
    SomeBVException,
    SymBool,
    SymFP,
    SymIntN,
    SymInteger,
    ToSym (toSym),
    Union,
    WordN,
    bitCastOrCanonical,
    mrgReturn,
    type (=->) (TabularFun),
  )
import qualified Grisette
import Grisette.Internal.Core.Data.Class.LogicalOp (LogicalOp ((.&&)))
import Grisette.Internal.SymPrim.FP (NotRepresentableFPError (NaNError))
import Grisette.Internal.SymPrim.SomeBV (SomeIntN, SomeSymIntN, ssymBV)
import Grisette.Unified
  ( EvalModeBV,
    EvalModeBase,
    EvalModeFP,
    EvalModeInteger,
    EvalModeTag (C, S),
    GetBool,
    GetData,
    GetFP,
    GetFPRoundingMode,
    GetIntN,
    GetInteger,
    GetSomeIntN,
    GetWordN,
    TheoryToUnify (UFun, UIntN, UWordN),
    UnifiedBranching,
    extractData,
    genEvalMode,
    mrgIte,
    safeDiv,
    symFromIntegral,
    symIte,
    (.<),
    (.==),
  )
import Grisette.Unified.Internal.Class.UnifiedSafeBitCast (safeBitCast)
import Grisette.Unified.Internal.UnifiedFun (UnifiedFun (GetFun))
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit ((@?=))

fbool ::
  forall mode. (EvalModeBase mode) => GetBool mode -> GetBool mode -> GetBool mode
fbool l r =
  mrgIte
    (l .== r :: GetBool mode)
    (l .< r)
    (symIte (l .&& r) l r)

finteger ::
  forall mode.
  (EvalModeBase mode, EvalModeInteger mode) =>
  GetInteger mode ->
  GetInteger mode ->
  GetInteger mode
finteger l r =
  mrgIte
    (l .== r :: GetBool mode)
    (l + r)
    (symIte (l .< r :: GetBool mode) l r)

#if MIN_VERSION_base(4,16,0)
type BVConstraint mode m n =
  ( EvalModeBase mode,
    EvalModeBV mode,
    MonadError ArithException m,
    UnifiedBranching mode m,
    MonadTryMerge m,
    KnownNat n,
    1 <= n
  )
#else
type BVConstraint mode m n =
  ( EvalModeBase mode,
    EvalModeBV mode,
    MonadError ArithException m,
    UnifiedBranching mode m,
    MonadTryMerge m,
    SafeUnifiedBV mode n m
  )
#endif

fbv ::
  forall mode n m.
  (BVConstraint mode m n) =>
  GetIntN mode n ->
  GetIntN mode n ->
  m (GetIntN mode n)
fbv l r = do
  v <- safeDiv @mode l r
  mrgReturn $
    mrgIte @mode
      (l .== r)
      (v + r)
      (symIte @mode (l .< r) l r)

#if MIN_VERSION_base(4,16,0)
type BVConstraint' mode m n =
  ( EvalModeBase mode,
    EvalModeBV mode,
    MonadTryMerge m,
    UnifiedBranching mode m,
    KnownNat n,
    1 <= n
  )
#else
type BVConstraint' mode m n =
  ( EvalModeBase mode,
    EvalModeBV mode,
    MonadTryMerge m,
    UnifiedBranching mode m,
    SafeUnifiedBV mode n (ExceptT ArithException m)
  )
#endif

fbv' ::
  forall mode n m.
  (BVConstraint' mode m n) =>
  GetIntN mode n ->
  GetIntN mode n ->
  ExceptT ArithException m (GetIntN mode n)
fbv' l r = do
  v <- safeDiv @mode l r
  mrgReturn $
    mrgIte @mode
      (l .== r)
      (v + r)
      (symIte @mode (l .< r) l r)

#if MIN_VERSION_base(4,16,0)
type SomeBVConstraint mode m =
  ( EvalModeBase mode,
    EvalModeBV mode,
    UnifiedBranching mode m,
    MonadTryMerge m,
    MonadError (Either SomeBVException ArithException) m
  )
#else
type SomeBVConstraint mode m =
  ( EvalModeBase mode,
    EvalModeBV mode,
    UnifiedBranching mode m,
    MonadTryMerge m,
    MonadError (Either SomeBVException ArithException) m,
    SafeUnifiedSomeBV mode m
  )
#endif

fsomebv ::
  forall mode m.
  (SomeBVConstraint mode m) =>
  GetSomeIntN mode ->
  GetSomeIntN mode ->
  m (GetSomeIntN mode)
fsomebv l r = do
  v <- safeDiv @mode l r
  mrgReturn $
    symIte @mode
      (l .== r)
      (v + r)
      (symIte @mode (l .< r) l r)

#if MIN_VERSION_base(4,16,0)
type SomeBVConstraint' mode m =
  (EvalModeBase mode, EvalModeBV mode, MonadTryMerge m, UnifiedBranching mode m)
#else
type SomeBVConstraint' mode m =
  ( EvalModeBase mode,
    EvalModeBV mode,
    MonadTryMerge m,
    UnifiedBranching mode m,
    SafeUnifiedSomeBV mode (ExceptT (Either SomeBVException ArithException) m)
  )
#endif

fsomebv' ::
  forall mode m.
  (SomeBVConstraint' mode m) =>
  GetSomeIntN mode ->
  GetSomeIntN mode ->
  ExceptT (Either SomeBVException ArithException) m (GetSomeIntN mode)
fsomebv' l r = do
  v <- safeDiv @mode l r
  mrgReturn $
    symIte @mode
      (l .== r)
      (v + r)
      (symIte @mode (l .< r) l r)

data A mode = A (GetIntN mode 8) | AT (GetData mode (A mode))
  deriving (Generic)

#if MIN_VERSION_base(4,16,0)
type DataConstraint mode = (EvalModeBase mode, EvalModeBV mode)
#else
type DataConstraint mode =
  ( EvalModeBase mode,
    EvalModeBV mode,
    UnifiedData mode (A mode),
    UnifiedBV mode 8
  )
#endif

deriving via
  (Default (A mode))
  instance
    (DataConstraint mode) =>
    (Mergeable (A mode))

#if MIN_VERSION_base(4,16,0)
type FDataConstraint mode m =
  ( EvalModeBase mode,
    EvalModeBV mode,
    UnifiedBranching mode m,
    MonadError ArithException m
  )
#else
type FDataConstraint mode m =
  ( EvalModeBase mode,
    EvalModeBV mode,
    MonadError ArithException m,
    UnifiedBranching mode m,
    UnifiedData mode (A mode),
    SafeUnifiedBV mode 8 m
  )
#endif

fdata ::
  forall mode m.
  (FDataConstraint mode m) =>
  GetData mode (A mode) ->
  m (GetIntN mode 8)
fdata d = do
  a <- extractData d
  case a of
    A v -> safeDiv @mode v (v - 1)
    AT v -> fdata v

#if MIN_VERSION_base(4,16,0)
type BVToFPConstraint mode = (EvalModeFP mode)
#else
type BVToFPConstraint mode =
  (EvalModeFP mode, BitCast (GetIntN mode 8) (GetFP mode 4 4))
#endif

bvToFPBitCast ::
  forall mode.
  (BVToFPConstraint mode) =>
  GetIntN mode 8 ->
  GetFP mode 4 4
bvToFPBitCast = bitCast

#if MIN_VERSION_base(4,16,0)
type FPToBVConstraint mode = (EvalModeFP mode)
#else
type FPToBVConstraint mode =
  (EvalModeFP mode, UnifiedBVFPConversion mode 8 4 4)
#endif

fpToBVBitCast ::
  forall mode.
  (FPToBVConstraint mode) =>
  GetFP mode 4 4 ->
  GetIntN mode 8
fpToBVBitCast = bitCastOrCanonical

#if MIN_VERSION_base(4,16,0)
type SafeFPToBVConstraint mode m =
  ( EvalModeFP mode,
    UnifiedBranching mode m,
    MonadError NotRepresentableFPError m
  )
#else
type SafeFPToBVConstraint mode m =
  ( EvalModeFP mode,
    UnifiedBranching mode m,
    MonadError NotRepresentableFPError m,
    SafeUnifiedBVFPConversion mode 8 4 4 m
  )
#endif

safeFPToBVBitCast ::
  forall mode m.
  (SafeFPToBVConstraint mode m) =>
  GetFP mode 4 4 ->
  m (GetIntN mode 8)
safeFPToBVBitCast = safeBitCast @mode

#if MIN_VERSION_base(4,16,0)
type FPToFPConstraint mode = (EvalModeFP mode)
#else
type FPToFPConstraint mode =
  ( EvalModeFP mode,
    UnifiedFPFPConversion mode 4 4 3 5
  )
#endif

fpToFPConvert ::
  forall mode.
  (FPToFPConstraint mode) =>
  GetFPRoundingMode mode ->
  GetFP mode 4 4 ->
  GetFP mode 3 5
fpToFPConvert = toFP

#if MIN_VERSION_base(4,16,0)
type BVToBVConstraint mode = (EvalModeBase mode, EvalModeBV mode)
#else
type BVToBVConstraint mode =
  ( EvalModeBase mode,
    EvalModeBV mode,
    UnifiedBVBVConversion mode 4 4
  )
#endif

bvToBVFromIntegral ::
  forall mode.
  (BVToBVConstraint mode) =>
  GetIntN mode 4 ->
  GetWordN mode 4
bvToBVFromIntegral = symFromIntegral @mode

genEvalMode "EvalMode" [UFun [UIntN, UWordN]]

#if MIN_VERSION_base(4,16,0)
type EvalModeUFunConstraint mode n m =
  ( EvalMode mode,
    KnownNat n,
    1 <= n,
    KnownNat m,
    1 <= m
  )
#else
type EvalModeUFunConstraint mode n m =
  ( EvalMode mode,
    KnownNat n,
    1 <= n,
    KnownNat m,
    1 <= m,
    EvalModeFunUIntNUWordN mode n m
  )
#endif

ufuncTest0 ::
  forall mode n m.
  (EvalModeUFunConstraint mode n m) =>
  GetFun mode (GetIntN mode n) (GetWordN mode m) ->
  GetIntN mode n ->
  GetWordN mode m
ufuncTest0 f = (f #)

ufunc0 ::
  forall mode n m.
  (EvalModeUFunConstraint mode n m) =>
  GetFun mode (GetIntN mode n) (GetWordN mode m)
ufunc0 = toSym (TabularFun [(1, 0)] 2 :: IntN n =-> WordN m)

ufuncTest :: forall mode. (EvalMode mode) => GetIntN mode 8 -> GetWordN mode 8
ufuncTest = ufuncTest0 ufunc0

#if MIN_VERSION_base(4,16,0)
type EvalModeBVConstraint mode m n =
  ( MonadEvalMode mode m,
    MonadError ArithException m,
    KnownNat n,
    1 <= n
  )
#else
type EvalModeBVConstraint mode m n =
  ( MonadEvalMode mode m,
    MonadError ArithException m,
    MonadTryMerge m,
    SafeUnifiedBV mode n m
  )
#endif

fbvEvalMode ::
  forall mode n m.
  (EvalModeBVConstraint mode m n) =>
  GetIntN mode n ->
  GetIntN mode n ->
  m (GetIntN mode n)
fbvEvalMode l r = do
  v <- safeDiv @mode l r
  mrgReturn $
    mrgIte @mode
      (l .== r)
      (v + r)
      (symIte @mode (l .< r) l r)

evalModeTest :: Test
evalModeTest =
  testGroup
    "EvalMode"
    [ testGroup
        "GetBool"
        [ testCase "C" $ fbool True False @?= False,
          testCase "S" $ do
            let l = "l" :: SymBool
            let r = "r" :: SymBool
            fbool l r
              @?= Grisette.mrgIte
                (l Grisette..== r)
                (l Grisette..< r)
                (Grisette.symIte (l Grisette..&& r) l r)
        ],
      testGroup
        "GetInteger"
        [ testCase "C" $ finteger (1 :: Integer) 2 @?= 1,
          testCase "S" $ do
            let l = "l" :: SymInteger
            let r = "r" :: SymInteger
            finteger l r
              @?= Grisette.mrgIte
                (l Grisette..== r)
                (l + r)
                (Grisette.symIte (l Grisette..< r) l r)
        ],
      testGroup
        "GetIntN"
        [ testCase "C" $ do
            fbv (1 :: IntN 8) 2 @?= Right 1
            fbv' (1 :: IntN 8) 2 @?= ExceptT (Identity (Right 1))
            fbvEvalMode (1 :: IntN 8) 2 @?= ExceptT (Identity (Right 1)),
          testCase "S" $ do
            let l = "l" :: SymIntN 8
            let r = "r" :: SymIntN 8
            let expected = do
                  v <- Grisette.safeDiv l r
                  mrgReturn $
                    Grisette.symIte
                      (l Grisette..== r)
                      (v + r)
                      (Grisette.symIte (l Grisette..< r) l r) ::
                    ExceptT
                      ArithException
                      Union
                      (SymIntN 8)
            fbv l r @?= expected
            fbv' l r @?= expected
            fbvEvalMode l r @?= expected
        ],
      testGroup
        "GetSomeIntN"
        [ testCase "C" $ do
            fsomebv (bv 8 1 :: SomeIntN) (bv 8 2) @?= Right (bv 8 1)
            fsomebv' (bv 8 1 :: SomeIntN) (bv 8 2)
              @?= ExceptT (Identity (Right (bv 8 1))),
          testCase "S" $ do
            let l = ssymBV 8 "l" :: SomeSymIntN
            let r = ssymBV 8 "r" :: SomeSymIntN
            let expected = do
                  v <- Grisette.safeDiv l r
                  mrgReturn $
                    Grisette.symIte
                      (l Grisette..== r)
                      (v + r)
                      (Grisette.symIte (l Grisette..< r) l r) ::
                    ExceptT
                      (Either SomeBVException ArithException)
                      Union
                      SomeSymIntN
            fsomebv l r @?= expected
            fsomebv' l r @?= expected
        ],
      testGroup
        "GetData"
        [ testCase "C" $ do
            fdata @'C (Identity $ A 2) @?= Right 2
            fdata @'C (Identity $ A 1) @?= Left DivideByZero,
          testCase "S" $ do
            let a = "a" :: SymIntN 8
            fdata (mrgReturn $ A a)
              @?= ( Grisette.safeDiv a (a - 1) ::
                      ExceptT ArithException Union (SymIntN 8)
                  )
        ],
      testGroup
        "Conversion"
        [ testGroup
            "FP/BV"
            [ testCase "C" $ do
                bvToFPBitCast @'C 0x22 @?= 0.15625
                fpToBVBitCast @'C 0.15625 @?= 0x22
                fpToBVBitCast @'C fpNaN @?= 0x7c
                safeFPToBVBitCast @'C 0.15625 @?= Right 0x22
                safeFPToBVBitCast @'C fpNaN @?= Left NaNError,
              testCase "S" $ do
                bvToFPBitCast @'S 0x22 @?= 0.15625
                let a = "a" :: SymIntN 8
                bvToFPBitCast @'S a @?= bitCast a
                fpToBVBitCast @'S 0.15625 @?= 0x22
                fpToBVBitCast @'S fpNaN @?= 0x7c
                let b = "b" :: SymFP 4 4
                fpToBVBitCast @'S b @?= bitCastOrCanonical b
                safeFPToBVBitCast @'S b
                  @?= ( Grisette.safeBitCast b ::
                          ExceptT NotRepresentableFPError Union (SymIntN 8)
                      )
            ],
          testGroup
            "FP/FP"
            [ testCase "C" $ do
                fpToFPConvert @'C rne 1 @?= 1,
              testCase "S" $ do
                fpToFPConvert @'S rne 1 @?= 1
            ],
          testGroup
            "BV/BV"
            [ testCase "C" $ do
                bvToBVFromIntegral @'C 0xa @?= 0xa,
              testCase "S" $ do
                bvToBVFromIntegral @'S 0xa @?= 0xa
            ]
        ],
      testGroup
        "GetFun"
        [ testCase "C" $ do
            ufuncTest @'C 1 @?= 0
            ufuncTest @'C 2 @?= 2,
          testCase "S" $ do
            let a = "a"
            ufuncTest @'S a @?= symIte (a Grisette..== 1) 0 2
        ]
    ]
