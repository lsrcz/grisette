{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Grisette.Unified.GetDataTest (getDataTest) where

import Data.Functor.Identity (Identity (Identity))
import Grisette
  ( DeriveConfig (evalModeConfig),
    EvalModeConfig (EvalModeConstraints),
    Union,
    basicClasses0,
    deriveWith,
  )
import Grisette.TestUtil.SymbolicAssertion ((.@?=))
import Grisette.Unified (EvalModeBase, EvalModeInteger, EvalModeTag (S), GetBool, GetData, GetInteger, mrgIf, mrgIte)
import Grisette.Unified.Lib.Control.Monad (mrgReturn)
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit ((@?=))

data A mode = A (GetData mode Integer) (GetInteger mode)

deriveWith
  mempty
    { evalModeConfig =
        [(0, EvalModeConstraints [''EvalModeBase, ''EvalModeInteger])]
    }
  [''A]
  basicClasses0

mrgIfFunc ::
  (EvalModeBase mode, EvalModeInteger mode) =>
  GetBool mode ->
  GetData mode (A mode) ->
  GetData mode (A mode) ->
  GetData mode (A mode)
mrgIfFunc = mrgIf

getDataTest :: Test
getDataTest =
  testGroup
    "GetData"
    [ testCase "mrgIf C" $ do
        let a = Identity (A (Identity 1) 2)
        let b = Identity (A (Identity 2) 3)
        mrgIfFunc True a b @?= a,
      testCase "mrgIf S" $ do
        let a = mrgReturn (A (mrgReturn 1) "a") :: Union (A 'S)
        let b = mrgReturn (A (mrgReturn 2) "b") :: Union (A 'S)
        mrgIfFunc @'S "x" a b
          .@?= mrgReturn
            ( A
                (mrgIf @'S "x" 1 2)
                (mrgIte @'S "x" "a" "b")
            )
    ]
