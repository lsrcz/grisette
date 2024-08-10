{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Unused LANGUAGE pragma" #-}
{-# HLINT ignore "Use fewer imports" #-}

module Grisette.Unified.EvalModeTest (evalModeTest) where

#if MIN_VERSION_base(4,16,0)
import GHC.TypeLits (KnownNat, type (<=))
#else
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
import Grisette
  ( BV (bv),
    BitCast (bitCast),
    Default (Default),
    IEEEFPConstants (fpNaN),
    IEEEFPConvertible (toFP),
    IEEEFPRoundingMode (rne),
    IntN,
    Mergeable,
    SomeBVException,
    SymBool,
    SymFP,
    SymIntN,
    SymInteger,
    Union,
    bitCastOrCanonical,
    mrgReturn,
  )
import qualified Grisette
import Grisette.Internal.Core.Data.Class.LogicalOp (LogicalOp ((.&&)))
import Grisette.Internal.SymPrim.FP (NotRepresentableFPError (NaNError))
import Grisette.Internal.SymPrim.SomeBV (SomeIntN, SomeSymIntN, ssymBV)
import Grisette.Unified
  ( EvalMode,
    EvalModeTag (Con, Sym),
    GetBool,
    GetData,
    GetFP,
    GetFPRoundingMode,
    GetIntN,
    GetInteger,
    GetSomeIntN,
    GetWordN,
    MonadWithMode,
    extractData,
    mrgIte,
    safeDiv,
    symFromIntegral,
    symIte,
    (.<),
    (.==),
  )
import Grisette.Unified.Internal.Class.UnifiedSafeBitCast (safeBitCast)
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit ((@?=))

fbool ::
  forall mode. (EvalMode mode) => GetBool mode -> GetBool mode -> GetBool mode
fbool l r =
  mrgIte
    (l .== r :: GetBool mode)
    (l .< r)
    (symIte (l .&& r) l r)

finteger ::
  forall mode.
  (EvalMode mode) =>
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
  (MonadWithMode mode m, MonadError ArithException m, KnownNat n, 1 <= n)
#else
type BVConstraint mode m n =
  (MonadWithMode mode m, MonadError ArithException m, SafeUnifiedBV mode n m)
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
  (MonadWithMode mode m, KnownNat n, 1 <= n)
#else
type BVConstraint' mode m n =
  ( MonadWithMode mode m,
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
  (MonadWithMode mode m, MonadError (Either SomeBVException ArithException) m)
#else
type SomeBVConstraint mode m =
  ( MonadWithMode mode m,
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
  (MonadWithMode mode m)
#else
type SomeBVConstraint' mode m =
  ( MonadWithMode mode m,
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
type DataConstraint mode = (EvalMode mode)
#else
type DataConstraint mode =
  (EvalMode mode, UnifiedData mode (A mode), UnifiedBV mode 8)
#endif

deriving via
  (Default (A mode))
  instance
    (DataConstraint mode) =>
    (Mergeable (A mode))

#if MIN_VERSION_base(4,16,0)
type FDataConstraint mode m =
  (MonadWithMode mode m, MonadError ArithException m)
#else
type FDataConstraint mode m =
  ( MonadWithMode mode m,
    MonadError ArithException m,
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
type BVToFPConstraint mode = (EvalMode mode)
#else
type BVToFPConstraint mode =
  (EvalMode mode, BitCast (GetIntN mode 8) (GetFP mode 4 4))
#endif

bvToFPBitCast ::
  forall mode.
  (BVToFPConstraint mode) =>
  GetIntN mode 8 ->
  GetFP mode 4 4
bvToFPBitCast = bitCast

#if MIN_VERSION_base(4,16,0)
type FPToBVConstraint mode = (EvalMode mode)
#else
type FPToBVConstraint mode =
  (EvalMode mode, UnifiedBVFPConversion mode 8 4 4)
#endif

fpToBVBitCast ::
  forall mode.
  (FPToBVConstraint mode) =>
  GetFP mode 4 4 ->
  GetIntN mode 8
fpToBVBitCast = bitCastOrCanonical

#if MIN_VERSION_base(4,16,0)
type SafeFPToBVConstraint mode m =
  (MonadWithMode mode m, MonadError NotRepresentableFPError m)
#else
type SafeFPToBVConstraint mode m =
  ( MonadWithMode mode m,
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
type FPToFPConstraint mode = (EvalMode mode)
#else
type FPToFPConstraint mode =
  ( EvalMode mode,
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
type BVToBVConstraint mode = (EvalMode mode)
#else
type BVToBVConstraint mode =
  ( EvalMode mode,
    UnifiedBVBVConversion mode 4 4
  )
#endif

bvToBVFromIntegral ::
  forall mode.
  (BVToBVConstraint mode) =>
  GetIntN mode 4 ->
  GetWordN mode 4
bvToBVFromIntegral = symFromIntegral @mode

evalModeTest :: Test
evalModeTest =
  testGroup
    "EvalMode"
    [ testGroup
        "GetBool"
        [ testCase "Con" $ fbool True False @?= False,
          testCase "Sym" $ do
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
        [ testCase "Con" $ finteger (1 :: Integer) 2 @?= 1,
          testCase "Sym" $ do
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
        [ testCase "Con" $ do
            fbv (1 :: IntN 8) 2 @?= Right 1
            fbv' (1 :: IntN 8) 2 @?= ExceptT (Identity (Right 1)),
          testCase "Sym" $ do
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
        ],
      testGroup
        "GetSomeIntN"
        [ testCase "Con" $ do
            fsomebv (bv 8 1 :: SomeIntN) (bv 8 2) @?= Right (bv 8 1)
            fsomebv' (bv 8 1 :: SomeIntN) (bv 8 2)
              @?= ExceptT (Identity (Right (bv 8 1))),
          testCase "Sym" $ do
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
        [ testCase "Con" $ do
            fdata @'Con (Identity $ A 2) @?= Right 2
            fdata @'Con (Identity $ A 1) @?= Left DivideByZero,
          testCase "Sym" $ do
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
            [ testCase "Con" $ do
                bvToFPBitCast @'Con 0x22 @?= 0.15625
                fpToBVBitCast @'Con 0.15625 @?= 0x22
                fpToBVBitCast @'Con fpNaN @?= 0x7c
                safeFPToBVBitCast @'Con 0.15625 @?= Right 0x22
                safeFPToBVBitCast @'Con fpNaN @?= Left NaNError,
              testCase "Sym" $ do
                bvToFPBitCast @'Sym 0x22 @?= 0.15625
                let a = "a" :: SymIntN 8
                bvToFPBitCast @'Sym a @?= bitCast a
                fpToBVBitCast @'Sym 0.15625 @?= 0x22
                fpToBVBitCast @'Sym fpNaN @?= 0x7c
                let b = "b" :: SymFP 4 4
                fpToBVBitCast @'Sym b @?= bitCastOrCanonical b
                safeFPToBVBitCast @'Sym b
                  @?= ( Grisette.safeBitCast b ::
                          ExceptT NotRepresentableFPError Union (SymIntN 8)
                      )
            ],
          testGroup
            "FP/FP"
            [ testCase "Con" $ do
                fpToFPConvert @'Con rne 1 @?= 1,
              testCase "Sym" $ do
                fpToFPConvert @'Sym rne 1 @?= 1
            ],
          testGroup
            "BV/BV"
            [ testCase "Con" $ do
                bvToBVFromIntegral @'Con 0xa @?= 0xa,
              testCase "Sym" $ do
                bvToBVFromIntegral @'Sym 0xa @?= 0xa
            ]
        ]
    ]
