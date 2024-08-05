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
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Eta reduce" #-}

-- |
-- Module      :   Grisette.Unified.Internal.Class.UnifiedSafeFromFP
-- Copyright   :   (c) Sirui Lu 2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Unified.Internal.Class.UnifiedSafeFromFP
  ( UnifiedSafeFromFP (..),
    safeFromFP,
  )
where

import Control.Monad.Error.Class (MonadError)
import GHC.TypeNats (KnownNat, type (<=))
import Grisette.Internal.Core.Data.Class.SafeFromFP (SafeFromFP)
import qualified Grisette.Internal.Core.Data.Class.SafeFromFP as SafeFromFP
import Grisette.Internal.SymPrim.AlgReal (AlgReal)
import Grisette.Internal.SymPrim.BV (IntN, WordN)
import Grisette.Internal.SymPrim.FP
  ( FP,
    FPRoundingMode,
    NotRepresentableFPError,
    ValidFP,
  )
import Grisette.Internal.SymPrim.SymAlgReal (SymAlgReal)
import Grisette.Internal.SymPrim.SymBV (SymIntN, SymWordN)
import Grisette.Internal.SymPrim.SymFP (SymFP, SymFPRoundingMode)
import Grisette.Internal.SymPrim.SymInteger (SymInteger)
import Grisette.Unified.Internal.Class.UnifiedSimpleMergeable
  ( UnifiedBranching (withBaseBranching),
  )
import Grisette.Unified.Internal.EvalModeTag (EvalModeTag (Sym))
import Grisette.Unified.Internal.Util (withMode)

-- | Unified `Grisette.Internal.Core.Data.Class.SafeFromFP.safeFromFP`
-- operation.
--
-- This function isn't able to infer the mode, so you need to provide the mode
-- explicitly. For example:
--
-- > safeFromFP @mode mode fp
safeFromFP ::
  forall mode e a fp fprd m.
  (UnifiedSafeFromFP mode e a fp fprd m, MonadError e m) =>
  fprd ->
  fp ->
  m a
safeFromFP rd fp =
  withBaseSafeFromFP @mode @e @a @fp @fprd @m $
    SafeFromFP.safeFromFP rd fp

-- | A class that provides unified safe conversion from floating points.
--
-- We use this type class to help resolve the constraints for `SafeFromFP`.
class UnifiedSafeFromFP (mode :: EvalModeTag) e a fp fprd m where
  withBaseSafeFromFP :: ((SafeFromFP e a fp fprd m) => r) -> r

instance
  {-# INCOHERENT #-}
  (UnifiedBranching mode m, SafeFromFP e a fp fprd m) =>
  UnifiedSafeFromFP mode e a fp fprd m
  where
  withBaseSafeFromFP r = r

instance
  ( MonadError NotRepresentableFPError m,
    UnifiedBranching mode m,
    ValidFP eb sb
  ) =>
  UnifiedSafeFromFP
    mode
    NotRepresentableFPError
    Integer
    (FP eb sb)
    FPRoundingMode
    m
  where
  withBaseSafeFromFP r =
    withMode @mode (withBaseBranching @mode @m r) (withBaseBranching @mode @m r)

instance
  ( MonadError NotRepresentableFPError m,
    UnifiedBranching mode m,
    ValidFP eb sb
  ) =>
  UnifiedSafeFromFP
    mode
    NotRepresentableFPError
    AlgReal
    (FP eb sb)
    FPRoundingMode
    m
  where
  withBaseSafeFromFP r =
    withMode @mode (withBaseBranching @mode @m r) (withBaseBranching @mode @m r)

instance
  ( MonadError NotRepresentableFPError m,
    UnifiedBranching mode m,
    ValidFP eb sb,
    KnownNat n,
    1 <= n
  ) =>
  UnifiedSafeFromFP
    mode
    NotRepresentableFPError
    (IntN n)
    (FP eb sb)
    FPRoundingMode
    m
  where
  withBaseSafeFromFP r =
    withMode @mode (withBaseBranching @mode @m r) (withBaseBranching @mode @m r)

instance
  ( MonadError NotRepresentableFPError m,
    UnifiedBranching mode m,
    ValidFP eb sb,
    KnownNat n,
    1 <= n
  ) =>
  UnifiedSafeFromFP
    mode
    NotRepresentableFPError
    (WordN n)
    (FP eb sb)
    FPRoundingMode
    m
  where
  withBaseSafeFromFP r =
    withMode @mode (withBaseBranching @mode @m r) (withBaseBranching @mode @m r)

instance
  ( MonadError NotRepresentableFPError m,
    UnifiedBranching 'Sym m,
    ValidFP eb sb
  ) =>
  UnifiedSafeFromFP
    'Sym
    NotRepresentableFPError
    SymInteger
    (SymFP eb sb)
    SymFPRoundingMode
    m
  where
  withBaseSafeFromFP r = withBaseBranching @'Sym @m r

instance
  ( MonadError NotRepresentableFPError m,
    UnifiedBranching 'Sym m,
    ValidFP eb sb
  ) =>
  UnifiedSafeFromFP
    'Sym
    NotRepresentableFPError
    SymAlgReal
    (SymFP eb sb)
    SymFPRoundingMode
    m
  where
  withBaseSafeFromFP r = withBaseBranching @'Sym @m r

instance
  ( MonadError NotRepresentableFPError m,
    UnifiedBranching 'Sym m,
    ValidFP eb sb,
    KnownNat n,
    1 <= n
  ) =>
  UnifiedSafeFromFP
    'Sym
    NotRepresentableFPError
    (SymIntN n)
    (SymFP eb sb)
    SymFPRoundingMode
    m
  where
  withBaseSafeFromFP r = withBaseBranching @'Sym @m r

instance
  ( MonadError NotRepresentableFPError m,
    UnifiedBranching 'Sym m,
    ValidFP eb sb,
    KnownNat n,
    1 <= n
  ) =>
  UnifiedSafeFromFP
    'Sym
    NotRepresentableFPError
    (SymWordN n)
    (SymFP eb sb)
    SymFPRoundingMode
    m
  where
  withBaseSafeFromFP r = withBaseBranching @'Sym @m r
