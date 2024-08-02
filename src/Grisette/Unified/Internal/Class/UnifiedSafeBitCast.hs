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

module Grisette.Unified.Internal.Class.UnifiedSafeBitCast
  ( safeBitCast,
    UnifiedSafeBitCast (..),
  )
where

import Control.Monad.Error.Class (MonadError)
import Data.Typeable (Typeable)
import GHC.TypeLits (KnownNat, type (+), type (<=))
import Grisette.Internal.Core.Data.Class.SafeBitCast (SafeBitCast)
import qualified Grisette.Internal.Core.Data.Class.SafeBitCast
import Grisette.Internal.SymPrim.BV (IntN, WordN)
import Grisette.Internal.SymPrim.FP (BitCastNaNError, FP, ValidFP)
import Grisette.Internal.SymPrim.SymBV (SymIntN, SymWordN)
import Grisette.Internal.SymPrim.SymFP (SymFP)
import Grisette.Unified.Internal.Class.UnifiedSimpleMergeable
  ( UnifiedBranching (withBaseBranching),
  )
import Grisette.Unified.Internal.EvalModeTag (EvalModeTag (Sym))
import Grisette.Unified.Internal.Util (withMode)

-- | Unified `Grisette.Internal.Core.Data.Class.SafeLinearArith.safeSub`
-- operation.
--
-- This function isn't able to infer the mode, so you need to provide the mode
-- explicitly. For example:
--
-- > safeSub @mode a b
safeBitCast ::
  forall mode e a b m.
  ( MonadError e m,
    UnifiedSafeBitCast mode e a b m
  ) =>
  a ->
  m b
safeBitCast a =
  withBaseSafeBitCast @mode @e @a @b @m $
    Grisette.Internal.Core.Data.Class.SafeBitCast.safeBitCast a
{-# INLINE safeBitCast #-}

class UnifiedSafeBitCast (mode :: EvalModeTag) e a b m where
  withBaseSafeBitCast :: ((SafeBitCast e a b m) => r) -> r

instance
  {-# INCOHERENT #-}
  (UnifiedBranching mode m, SafeBitCast e a b m) =>
  UnifiedSafeBitCast mode e a b m
  where
  withBaseSafeBitCast r = r

instance
  ( Typeable mode,
    MonadError BitCastNaNError m,
    UnifiedBranching mode m,
    ValidFP eb sb,
    KnownNat n,
    1 <= n,
    n ~ eb + sb
  ) =>
  UnifiedSafeBitCast mode BitCastNaNError (FP eb sb) (WordN n) m
  where
  withBaseSafeBitCast r =
    withMode @mode (withBaseBranching @mode @m r) (withBaseBranching @mode @m r)

instance
  ( Typeable mode,
    MonadError BitCastNaNError m,
    UnifiedBranching mode m,
    ValidFP eb sb,
    KnownNat n,
    1 <= n,
    n ~ eb + sb
  ) =>
  UnifiedSafeBitCast mode BitCastNaNError (FP eb sb) (IntN n) m
  where
  withBaseSafeBitCast r =
    withMode @mode (withBaseBranching @mode @m r) (withBaseBranching @mode @m r)

instance
  ( MonadError BitCastNaNError m,
    UnifiedBranching 'Sym m,
    ValidFP eb sb,
    KnownNat n,
    1 <= n,
    n ~ eb + sb
  ) =>
  UnifiedSafeBitCast 'Sym BitCastNaNError (SymFP eb sb) (SymWordN n) m
  where
  withBaseSafeBitCast r = withBaseBranching @'Sym @m r

instance
  ( MonadError BitCastNaNError m,
    UnifiedBranching 'Sym m,
    ValidFP eb sb,
    KnownNat n,
    1 <= n,
    n ~ eb + sb
  ) =>
  UnifiedSafeBitCast 'Sym BitCastNaNError (SymFP eb sb) (SymIntN n) m
  where
  withBaseSafeBitCast r = withBaseBranching @'Sym @m r
