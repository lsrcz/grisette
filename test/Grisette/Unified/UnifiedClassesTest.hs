{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Grisette.Unified.UnifiedClassesTest (unifiedClassesTest) where

import Control.Monad.Except (ExceptT, MonadError (throwError))
import qualified Data.Text as T
import Grisette (SymInteger, UnionM)
import qualified Grisette
import Grisette.Unified
  ( BaseMonad,
    GetBool,
    GetInteger,
    IsMode,
    MonadWithMode,
    UnifiedSEq ((.==)),
    mrgIf,
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
        ]
    ]
