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

module Grisette.Unified.IsModeTest (isModeTest) where

import Control.Exception (ArithException (DivideByZero))
import Control.Monad.Error.Class (MonadError)
import Control.Monad.Except (ExceptT)
import GHC.Generics (Generic)
import Grisette
  ( Default (Default),
    IntN,
    Mergeable,
    SafeDivision (safeDiv),
    SymBool,
    SymIntN,
    SymInteger,
    UnionM,
    mrgReturn,
  )
import qualified Grisette
import Grisette.Internal.Core.Data.Class.LogicalOp (LogicalOp ((.&&)))
import Grisette.Internal.SymPrim.SomeBV (SomeSymIntN, ssymBV)
import Grisette.Lib.Control.Monad.Except (mrgModifyError)
import Grisette.Unified
  ( EvaluationMode (Con),
    GetBool,
    GetData,
    GetIntN,
    GetInteger,
    GetSomeIntN,
    IsMode,
    MonadWithMode,
    UnifiedITEOp (symIte),
    UnifiedSimpleMergeable (mrgIte),
    extractData,
    (.<),
    (.==),
  )
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit ((@?=))

#if MIN_VERSION_base(4,16,0)
import GHC.TypeLits (KnownNat, type (<=))
#else
import Grisette.Unified (UnifiedBV, UnifiedData, SafeUnifiedBV)
#endif

fbool ::
  forall mode. (IsMode mode) => GetBool mode -> GetBool mode -> GetBool mode
fbool l r =
  mrgIte
    (l .== r :: GetBool mode)
    (l .< r)
    (symIte (l .&& r) l r)

finteger ::
  forall mode.
  (IsMode mode) =>
  GetInteger mode ->
  GetInteger mode ->
  GetInteger mode
finteger l r =
  mrgIte
    (l .== r :: GetBool mode)
    (l + r)
    (symIte (l .< r :: GetBool mode) l r)

#if MIN_VERSION_base(4,16,0)
type BVConstraint mode n = (IsMode mode, KnownNat n, 1 <= n)
#else
type BVConstraint mode n = (IsMode mode, UnifiedBV mode n)
#endif

fbv ::
  forall mode n.
  (BVConstraint mode n) =>
  GetIntN mode n ->
  GetIntN mode n ->
  GetIntN mode n
fbv l r =
  mrgIte @mode
    (l .== r)
    (l + r)
    (symIte @mode (l .< r) l r)

fsomebv ::
  forall mode.
  (IsMode mode) =>
  GetSomeIntN mode ->
  GetSomeIntN mode ->
  GetSomeIntN mode
fsomebv l r =
  symIte @mode
    (l .== r)
    (l + r)
    (symIte @mode (l .< r) l r)

data A mode = A (GetIntN mode 8) | AT (GetData mode (A mode))
  deriving (Generic)

#if MIN_VERSION_base(4,16,0)
type DataConstraint mode = (IsMode mode)
#else
type DataConstraint mode =
  (IsMode mode, UnifiedData mode (A mode), UnifiedBV mode 8)
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
  a :: A mode <- extractData @mode d
  case a of
    A v -> mrgModifyError id $ safeDiv v (v - 1)
    AT v -> fdata v

isModeTest :: Test
isModeTest =
  testGroup
    "IsMode"
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
        [ testCase "Con" $ fbv (1 :: IntN 8) 2 @?= 1,
          testCase "Sym" $ do
            let l = "l" :: SymIntN 8
            let r = "r" :: SymIntN 8
            fbv l r
              @?= Grisette.mrgIte
                (l Grisette..== r)
                (l + r)
                (Grisette.symIte (l Grisette..< r) l r)
        ],
      testGroup
        "GetSomeIntN"
        [ testCase "Con" $ fbv (1 :: IntN 8) 2 @?= 1,
          testCase "Sym" $ do
            let l = ssymBV 8 "l" :: SomeSymIntN
            let r = ssymBV 8 "r" :: SomeSymIntN
            fsomebv l r
              @?= Grisette.symIte
                (l Grisette..== r)
                (l + r)
                (Grisette.symIte (l Grisette..< r) l r)
        ],
      testGroup
        "GetData"
        [ testCase "Con" $ do
            fdata @'Con (A 2) @?= Right 2
            fdata @'Con (A 1) @?= Left DivideByZero,
          testCase "Sym" $ do
            let a = "a" :: SymIntN 8
            fdata (mrgReturn $ A a)
              @?= ( safeDiv a (a - 1) ::
                      ExceptT ArithException UnionM (SymIntN 8)
                  )
        ]
    ]
