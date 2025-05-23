{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
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
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Grisette.Unified.UnifiedConstructorTest (unifiedConstructorTest) where

#if MIN_VERSION_base(4,16,0)
import Grisette.Unified (EvalModeBase, EvalModeTag (S), GetBool, GetData)
#else
import Grisette.Unified
  ( EvalModeBase,
    EvalModeTag (S),
    GetBool,
    GetData,
    UnifiedData,
  )
#endif

import Control.Monad.Identity (Identity (Identity))
import Grisette
  ( Solvable (con),
    SymInteger,
    ToSym (toSym),
    Union,
    allClasses0,
    allClasses01,
    derive,
    deriveWith,
    mrgReturn,
  )
import Grisette.Internal.TH.Derivation.Common
  ( DeriveConfig (evalModeConfig, needExtraMergeableUnderEvalMode),
    EvalModeConfig (EvalModeConstraints),
  )
import Grisette.TH (makeNamedUnifiedCtor, makePrefixedUnifiedCtor)
import Grisette.TestUtil.SymbolicAssertion ((.@?=))
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)

data T mode a
  = T (GetBool mode) a (GetData mode (T mode a))
  | T1

#if MIN_VERSION_base(4,16,0)
deriveWith
  ( mempty
      { evalModeConfig = [(0, EvalModeConstraints [''EvalModeBase])],
        needExtraMergeableUnderEvalMode = True
      }
  )
  [''T]
  allClasses0

makePrefixedUnifiedCtor [''EvalModeBase] "mk" ''T
#endif

#if MIN_VERSION_base(4,16,0)
type FConstraint mode = (EvalModeBase mode)
#else
type FConstraint mode =
  (EvalModeBase mode, UnifiedData mode (T mode SymInteger))
#endif

#if MIN_VERSION_base(4,16,0)
f :: forall mode. (FConstraint mode) => GetData mode (T mode SymInteger)
f = mkT (toSym True) 10 mkT1
#endif

data TNoMode a = TNoMode0 Bool a (TNoMode a) | TNoMode1

derive [''TNoMode] allClasses01
makeNamedUnifiedCtor [] ["tNoMode0", "tNoMode1"] ''TNoMode

data TNoArg = TNoArg

derive [''TNoArg] allClasses0
makePrefixedUnifiedCtor [] "mk" ''TNoArg

#if MIN_VERSION_base(4,16,0)
unifiedConstructorExtraTest :: [Test]
unifiedConstructorExtraTest =
  [ testCase "mkUnifiedConstructor" $ do
      f .@?= Identity (T True 10 (Identity T1))
      f
        .@?= ( mrgReturn (T (con True) 10 (mrgReturn T1)) ::
                Union (T 'S SymInteger)
            )
  ]
#else
unifiedConstructorExtraTest :: [Test]
unifiedConstructorExtraTest = []
#endif

unifiedConstructorTest :: Test
unifiedConstructorTest =
  testGroup "UnifiedConstructor" $
    [ testCase "NoMode" $ do
        tNoMode0 True (10 :: Int) TNoMode1
          .@?= Identity (TNoMode0 True 10 TNoMode1)
        tNoMode1 .@?= (mrgReturn TNoMode1 :: Union (TNoMode Int)),
      testCase "NoArg" $ do
        mkTNoArg .@?= Identity TNoArg
        mkTNoArg .@?= (mrgReturn TNoArg :: Union TNoArg)
    ]
      ++ unifiedConstructorExtraTest
