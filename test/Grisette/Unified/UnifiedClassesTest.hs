{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
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
import Control.Monad.Identity (Identity (Identity))
import qualified Data.Text as T
import GHC.TypeNats (KnownNat, type (<=))
import Grisette
  ( SymBool,
    SymInteger,
    SymWordN,
    Union,
    WordN,
    deriveGADTAllWith,
    mrgReturn,
  )
import qualified Grisette
import Grisette.Internal.TH.GADT.Common
  ( DeriveConfig
      ( bitSizePositions,
        evalModeConfig
      ),
    EvalModeConfig (EvalModeConstraints),
  )
import Grisette.Unified
  ( BaseMonad,
    EvalModeBV,
    EvalModeBase,
    EvalModeInteger,
    GetBool,
    GetData,
    GetInteger,
    GetWordN,
    UnifiedBranching,
    UnifiedSymEq (withBaseSymEq),
    mrgIf,
    (.==),
  )
import Grisette.Unified.Internal.Util (withMode)
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit ((@?=))

type M mode = ExceptT T.Text (BaseMonad mode)

testBranching ::
  forall mode m.
  ( EvalModeBase mode,
    EvalModeInteger mode,
    UnifiedBranching mode m,
    MonadError T.Text m
  ) =>
  GetInteger mode ->
  m (GetInteger mode)
testBranching x =
  mrgIf (x .== 1 :: GetBool mode) (return x) (throwError "err")

testBranchingBase ::
  forall mode.
  (EvalModeBase mode, EvalModeInteger mode) =>
  GetInteger mode ->
  M mode (GetInteger mode)
testBranchingBase x =
  mrgIf (x .== 1 :: GetBool mode) (return x) (throwError "err")

data X mode n
  = X
      (GetBool mode)
      [GetWordN mode n]
      (GetData mode (X mode n))
      [GetData mode (X mode n)]
  | XNil

deriveGADTAllWith
  ( mempty
      { evalModeConfig =
          [(0, EvalModeConstraints [''EvalModeBV, ''EvalModeBase])],
        bitSizePositions = [1]
      }
  )
  ''X

instance
  (EvalModeBase mode, EvalModeBV mode, 1 <= n, KnownNat n) =>
  UnifiedSymEq mode (X mode n)
  where
  withBaseSymEq r = withMode @mode r r

testSEq ::
  forall mode n.
  (EvalModeBase mode, EvalModeBV mode, 1 <= n, KnownNat n) =>
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
            testBranching 1 @?= (return 1 :: ExceptT T.Text Union SymInteger)
        ],
      testGroup
        "UnifiedSEq"
        [ testCase "testSEq 'Con" $ do
            let x1 = X True [1 :: WordN 8] (Identity XNil) [Identity XNil]
            let x2 = X False [1 :: WordN 8] (Identity XNil) [Identity XNil]
            testSEq x1 x1 @?= True
            testSEq x1 x2 @?= False,
          testCase "testSEq 'Sym" $ do
            let x1 = X "a" [1 :: SymWordN 8] (mrgReturn XNil) [mrgReturn XNil]
            let x2 = X "b" [1 :: SymWordN 8] (mrgReturn XNil) [mrgReturn XNil]
            testSEq x1 x2 @?= ("a" :: SymBool) .== "b"
        ]
    ]
