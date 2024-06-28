{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Grisette.Unified.UnifiedClassesTest (unifiedClassesTest) where

import Control.Monad.Except (ExceptT, MonadError (throwError))
import qualified Data.Text as T
import GHC.TypeNats (KnownNat, type (<=))
import Grisette
  ( Default (Default),
    SymBool,
    SymInteger,
    SymWordN,
    UnionM,
    WordN,
    mrgReturn,
  )
import qualified Grisette
import Grisette.TH (deriveAll)
import Grisette.Unified
  ( BaseMonad,
    GetBool,
    GetData,
    GetInteger,
    GetWordN,
    IsMode,
    MonadWithMode,
    mrgIf,
    (.==),
  )
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit ((@?=))

type M mode = ExceptT T.Text (BaseMonad mode)

testBranching ::
  forall mode m.
  (MonadWithMode mode m, MonadError T.Text m) =>
  GetInteger mode ->
  m (GetInteger mode)
testBranching x =
  mrgIf (x .== 1 :: GetBool mode) (return x) (throwError "err")

testBranchingBase ::
  forall mode. (IsMode mode) => GetInteger mode -> M mode (GetInteger mode)
testBranchingBase x =
  mrgIf (x .== 1 :: GetBool mode) (return x) (throwError "err")

data X mode n
  = X
      (GetBool mode)
      [GetWordN mode n]
      (GetData mode (X mode n))
      [GetData mode (X mode n)]
  | XNil

deriveAll ''X

testSEq ::
  forall mode n.
  (IsMode mode, 1 <= n, KnownNat n) =>
  X mode n ->
  X mode n ->
  GetBool mode
testSEq = (.==)

unifiedClassesTest :: Test
unifiedClassesTest =
  testGroup
    "UnifiedClasses"
    [ testGroup
        "UnifiedBranching"
        [ testCase "branchingBase 'Con" $
            testBranchingBase (1 :: Integer) @?= return 1,
          testCase "branchingBase 'Sym" $ do
            let a = "a" :: SymInteger
            let expected =
                  Grisette.mrgIf
                    (a Grisette..== 1)
                    (return a)
                    (throwError "err")
            testBranchingBase a @?= expected,
          testCase "branching 'Con" $
            testBranching 1 @?= (return 1 :: Either T.Text Integer),
          testCase "branching 'Sym" $
            testBranching 1 @?= (return 1 :: ExceptT T.Text UnionM SymInteger)
        ],
      testGroup
        "UnifiedSEq"
        [ testCase "testSEq 'Con" $ do
            let x1 = X True [1 :: WordN 8] XNil [XNil]
            let x2 = X False [1 :: WordN 8] XNil [XNil]
            testSEq x1 x1 @?= True
            testSEq x1 x2 @?= False,
          testCase "testSEq 'Sym" $ do
            let x1 = X "a" [1 :: SymWordN 8] (mrgReturn XNil) [mrgReturn XNil]
            let x2 = X "b" [1 :: SymWordN 8] (mrgReturn XNil) [mrgReturn XNil]
            testSEq x1 x2 @?= ("a" :: SymBool) .== "b"
        ]
    ]
