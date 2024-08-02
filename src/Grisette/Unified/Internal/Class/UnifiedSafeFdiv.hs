{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Eta reduce" #-}

module Grisette.Unified.Internal.Class.UnifiedSafeFdiv
  ( UnifiedSafeFdiv (..),
  )
where

import Control.Exception (ArithException)
import Control.Monad.Error.Class (MonadError)
import Data.Typeable (Typeable)
import Grisette.Internal.Core.Data.Class.SafeFdiv (SafeFdiv)
import Grisette.Internal.SymPrim.AlgReal (AlgReal)
import Grisette.Internal.SymPrim.SymAlgReal (SymAlgReal)
import Grisette.Unified.Internal.Class.UnifiedSimpleMergeable
  ( UnifiedBranching (withBaseBranching),
  )
import Grisette.Unified.Internal.EvalModeTag (EvalModeTag (Sym))
import Grisette.Unified.Internal.Util (withMode)

class UnifiedSafeFdiv (mode :: EvalModeTag) e a m where
  withBaseUnifiedSafeFdiv :: ((SafeFdiv e a m) => r) -> r

instance
  {-# INCOHERENT #-}
  (UnifiedBranching mode m, SafeFdiv e a m) =>
  UnifiedSafeFdiv mode e a m
  where
  withBaseUnifiedSafeFdiv r = r

instance
  (Typeable mode, MonadError ArithException m, UnifiedBranching mode m) =>
  UnifiedSafeFdiv mode ArithException AlgReal m
  where
  withBaseUnifiedSafeFdiv r =
    withMode @mode (withBaseBranching @mode @m r) (withBaseBranching @mode @m r)

instance
  (MonadError ArithException m, UnifiedBranching 'Sym m) =>
  UnifiedSafeFdiv 'Sym ArithException SymAlgReal m
  where
  withBaseUnifiedSafeFdiv r = withBaseBranching @'Sym @m r