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
{-# LANGUAGE UndecidableInstances #-}

module Grisette.Unified.UnifiedConstructorTest (unifiedConstructorTest) where

#if MIN_VERSION_base(4,16,0)
import Grisette.Unified.Internal.UnifiedData (GetData)
#else
import Grisette.Unified.Internal.UnifiedData (GetData, UnifiedData)
#endif

import Control.Monad.Identity (Identity (Identity))
import Grisette
  ( Solvable (con),
    SymInteger,
    ToSym (toSym),
    Union,
    deriveGADTAll,
    deriveGADTAllWith,
    mrgReturn,
  )
import Grisette.Internal.TH.GADT.Common
  ( DeriveConfig (evalModeConfig, needExtraMergeable),
    EvalModeConfig (EvalModeConstraints),
  )
import Grisette.TH (makeNamedUnifiedCtor, makePrefixedUnifiedCtor)
import Grisette.Unified.Internal.EvalMode (EvalModeBase)
import Grisette.Unified.Internal.EvalModeTag (EvalModeTag (S))
import Grisette.Unified.Internal.UnifiedBool (UnifiedBool (GetBool))
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit ((@?=))

data T mode a
  = T (GetBool mode) a (GetData mode (T mode a))
  | T1

deriveGADTAllWith
  ( mempty
      { evalModeConfig = [(0, EvalModeConstraints [''EvalModeBase])],
        needExtraMergeable = True
      }
  )
  ''T
makePrefixedUnifiedCtor "mk" ''T

#if MIN_VERSION_base(4,16,0)
type FConstraint mode = (EvalModeBase mode)
#else
type FConstraint mode =
  (EvalModeBase mode, UnifiedData mode (T mode SymInteger))
#endif

f :: forall mode. (FConstraint mode) => GetData mode (T mode SymInteger)
f = mkT (toSym True) 10 mkT1

data TNoMode a = TNoMode0 Bool a (TNoMode a) | TNoMode1

deriveGADTAll ''TNoMode
makeNamedUnifiedCtor ["tNoMode0", "tNoMode1"] ''TNoMode

data TNoArg = TNoArg

deriveGADTAll ''TNoArg
makePrefixedUnifiedCtor "mk" ''TNoArg

unifiedConstructorTest :: Test
unifiedConstructorTest =
  testGroup
    "UnifiedConstructor"
    [ testCase "mkUnifiedConstructor" $ do
        f @?= Identity (T True 10 (Identity T1))
        f
          @?= ( mrgReturn (T (con True) 10 (mrgReturn T1)) ::
                  Union (T 'S SymInteger)
              ),
      testCase "NoMode" $ do
        tNoMode0 True (10 :: Int) TNoMode1
          @?= Identity (TNoMode0 True 10 TNoMode1)
        tNoMode1 @?= (mrgReturn TNoMode1 :: Union (TNoMode Int)),
      testCase "NoArg" $ do
        mkTNoArg @?= Identity TNoArg
        mkTNoArg @?= (mrgReturn TNoArg :: Union TNoArg)
    ]
