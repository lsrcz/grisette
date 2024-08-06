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
import Generics.Deriving (Default (Default))
import Grisette (Solvable (con), SymInteger, ToSym (toSym), mrgReturn)
import Grisette.TH (deriveAll, mkUnifiedConstructor)
import Grisette.Unified.Internal.EvalMode (EvalMode)
import Grisette.Unified.Internal.EvalModeTag (EvalModeTag (Con, Sym))
import Grisette.Unified.Internal.UnifiedBool (UnifiedBool (GetBool))
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit ((@?=))

data T mode a
  = T (GetBool mode) a (GetData mode (T mode a))
  | T1

deriveAll ''T
mkUnifiedConstructor "mk" ''T

#if MIN_VERSION_base(4,16,0)
type FConstraint mode = (EvalMode mode)
#else
type FConstraint mode =
  (EvalMode mode, UnifiedData mode (T mode SymInteger))
#endif

f :: forall mode. (FConstraint mode) => GetData mode (T mode SymInteger)
f = mkT (toSym True) 10 mkT1

unifiedConstructorTest :: Test
unifiedConstructorTest =
  testGroup
    "UnifiedConstructor"
    [ testCase "mkUnifiedConstructor" $ do
        f @'Con @?= Identity (T True 10 (Identity T1))
        f @'Sym @?= (mrgReturn (T (con True) 10 (mrgReturn T1)))
    ]
