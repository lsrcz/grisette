{-# LANGUAGE AllowAmbiguousTypes #-}
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

import Generics.Deriving (Default (Default))
import Grisette (Solvable (con), SymInteger, ToSym (toSym), mrgReturn)
import Grisette.Internal.Core.TH.DeriveGrisette (deriveAllGrisette)
import Grisette.Unified.Internal.EvaluationMode (EvaluationMode (Con, Sym))
import Grisette.Unified.Internal.IsMode (IsMode)
import Grisette.Unified.Internal.TH.UnifiedConstructor (mkUnifiedConstructor)
import Grisette.Unified.Internal.UnifiedBool (UnifiedBool (GetBool))
import Grisette.Unified.Internal.UnifiedData (GetData, UnifiedData)
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit ((@?=))

data T mode a
  = T (GetBool mode) a (GetData mode (T mode a))
  | T1

deriveAllGrisette ''T
mkUnifiedConstructor "mk" ''T

unifiedConstructorTest :: Test
unifiedConstructorTest =
  testGroup
    "UnifiedConstructor"
    [ testCase "mkUnifiedConstructor" $ do
        let f ::
              forall mode.
              (IsMode mode, UnifiedData mode (T mode SymInteger)) =>
              GetData mode (T mode SymInteger)
            f =
              mkT @mode
                (toSym True)
                (10 :: SymInteger)
                (mkT1 @mode @SymInteger)
        f @'Con @?= T True 10 T1
        f @'Sym @?= (mrgReturn (T (con True) 10 (mrgReturn T1)))
    ]
