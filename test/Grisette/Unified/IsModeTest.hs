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
import Control.Monad.Except (ExceptT (ExceptT))
import Control.Monad.Identity (Identity (Identity))
import GHC.Generics (Generic)
import Grisette
  ( BV (bv),
    BitwidthMismatch,
    Default (Default),
    IntN,
    Mergeable,
    SymBool,
    SymIntN,
    SymInteger,
    UnionM,
    mrgReturn,
  )
import qualified Grisette
import Grisette.Internal.Core.Data.Class.LogicalOp (LogicalOp ((.&&)))
import Grisette.Internal.SymPrim.SomeBV (SomeIntN, SomeSymIntN, ssymBV)
import Grisette.Unified
  ( EvaluationMode (Con),
    GetBool,
    GetData,
    GetIntN,
    GetInteger,
    GetSomeIntN,
    IsMode,
    MonadWithMode,
    extractData,
    mrgIte,
    safeDiv,
    symIte,
    (.<),
    (.==),
  )
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit ((@?=))

#if MIN_VERSION_base(4,16,0)
import GHC.TypeLits (KnownNat, type (<=))
#else
import Grisette.Unified
  ( SafeUnifiedBV,
    SafeUnifiedSomeBV,
    UnifiedBV,
    UnifiedData,
  )
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
  v <- safeDiv @mode @ArithException l r
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
  (MonadWithMode mode m, MonadError (Either BitwidthMismatch ArithException) m)
#else
type SomeBVConstraint mode m =
  ( MonadWithMode mode m,
    MonadError (Either BitwidthMismatch ArithException) m,
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
    SafeUnifiedSomeBV mode (ExceptT (Either BitwidthMismatch ArithException) m)
  )
#endif

fsomebv' ::
  forall mode m.
  (SomeBVConstraint' mode m) =>
  GetSomeIntN mode ->
  GetSomeIntN mode ->
  ExceptT (Either BitwidthMismatch ArithException) m (GetSomeIntN mode)
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
    A v -> safeDiv @mode v (v - 1)
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
                      UnionM
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
                      (Either BitwidthMismatch ArithException)
                      UnionM
                      SomeSymIntN
            fsomebv l r @?= expected
            fsomebv' l r @?= expected
        ],
      testGroup
        "GetData"
        [ testCase "Con" $ do
            fdata @'Con (A 2) @?= Right 2
            fdata @'Con (A 1) @?= Left DivideByZero,
          testCase "Sym" $ do
            let a = "a" :: SymIntN 8
            fdata (mrgReturn $ A a)
              @?= ( Grisette.safeDiv a (a - 1) ::
                      ExceptT ArithException UnionM (SymIntN 8)
                  )
        ]
    ]
