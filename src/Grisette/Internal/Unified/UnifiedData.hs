{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      :   Grisette.Internal.Unified.UnifiedData
-- Copyright   :   (c) Sirui Lu 2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Internal.Unified.UnifiedData
  ( GetData,
    wrapData,
    extractData,
    UnifiedData,
    AllUnifiedData,
  )
where

import Control.DeepSeq (NFData)
import Control.Monad.Identity (Identity (Identity, runIdentity))
import Data.Bytes.Serial (Serial)
import Data.Hashable (Hashable)
import Grisette.Internal.Core.Control.Monad.Union (Union)
import Grisette.Internal.Core.Data.Class.EvalSym (EvalSym)
import Grisette.Internal.Core.Data.Class.ExtractSym (ExtractSym)
import Grisette.Internal.Core.Data.Class.ITEOp (ITEOp)
import Grisette.Internal.Core.Data.Class.LogicalOp (LogicalOp)
import Grisette.Internal.Core.Data.Class.Mergeable (Mergeable)
import Grisette.Internal.Core.Data.Class.PPrint (PPrint)
import Grisette.Internal.Core.Data.Class.SubstSym (SubstSym)
import Grisette.Internal.Core.Data.Class.SymEq (SymEq)
import Grisette.Internal.Core.Data.Class.SymOrd (SymOrd)
import Grisette.Internal.Core.Data.Class.ToCon (ToCon)
import Grisette.Internal.Core.Data.Class.ToSym (ToSym)
import Grisette.Internal.Core.Data.Class.TryMerge (mrgSingle)
import Grisette.Internal.SymPrim.AllSyms (AllSyms)
import Grisette.Internal.Unified.Class.UnifiedITEOp (UnifiedITEOp)
import Grisette.Internal.Unified.Class.UnifiedSimpleMergeable
  ( UnifiedBranching (withBaseBranching),
    UnifiedSimpleMergeable,
    liftBaseMonad,
  )
import Grisette.Internal.Unified.Class.UnifiedSymEq (UnifiedSymEq)
import Grisette.Internal.Unified.Class.UnifiedSymOrd (UnifiedSymOrd)
import Grisette.Internal.Unified.EvalModeTag (EvalModeTag (C, S))
import Instances.TH.Lift ()
import Language.Haskell.TH.Syntax (Lift)

class
  ( u ~ GetData mode v,
    (Mergeable v) => Mergeable u,
    (AllSyms v) => AllSyms u,
    (Eq v) => Eq u,
    (EvalSym v) => EvalSym u,
    (ExtractSym v) => ExtractSym u,
    (ITEOp v, Mergeable v) => ITEOp u,
    (PPrint v) => PPrint u,
    (Hashable v) => Hashable u,
    (Lift v) => Lift u,
    (LogicalOp v, Mergeable v) => LogicalOp u,
    (NFData v) => NFData u,
    (Num v, Mergeable v) => Num u,
    (SymEq v) => SymEq u,
    (Show v) => Show u,
    (SymOrd v) => SymOrd u,
    (SubstSym v) => SubstSym u,
    (Serial v, Mergeable v) => Serial u,
    (UnifiedITEOp mode v, Mergeable v) => UnifiedITEOp mode u,
    (UnifiedSimpleMergeable mode v, Mergeable v) =>
    UnifiedSimpleMergeable mode u,
    (UnifiedSymEq mode v) => UnifiedSymEq mode u,
    (UnifiedSymOrd mode v) => UnifiedSymOrd mode u,
    forall a. (ToSym a v) => ToSym (Identity a) u,
    forall a. (ToSym v a) => ToSym u (Union a),
    forall a. (ToCon v a) => ToCon u (Identity a),
    forall a. (ToCon a v) => ToCon (Union a) u
  ) =>
  UnifiedDataImpl (mode :: EvalModeTag) v u
    | u -> mode v
  where
  -- | Get a unified data type. Resolves to @v@ in 'C' mode, and @'Union' v@
  -- in 'S' mode.
  type GetData mode v = r | r -> mode v

  -- | Wraps a value into the unified data type.
  wrapData :: (Mergeable v) => v -> u

  -- | Extracts a value from the unified data type.
  extractData :: (Mergeable v, Monad m, UnifiedBranching mode m) => u -> m v

instance UnifiedDataImpl 'C v (Identity v) where
  type GetData 'C v = Identity v
  wrapData = Identity
  extractData ::
    forall m. (Mergeable v, Monad m, UnifiedBranching C m) => Identity v -> m v
  extractData = withBaseBranching @'C @m $ return . runIdentity

instance UnifiedDataImpl 'S v (Union v) where
  type GetData 'S v = Union v
  wrapData = mrgSingle
  extractData ::
    forall m. (Mergeable v, Monad m, UnifiedBranching S m) => Union v -> m v
  extractData = liftBaseMonad

-- | This class is needed as constraint in user code prior to GHC 9.2.1.
-- See the notes in 'Grisette.Internal.Unified.IsMode.IsMode'.
class (UnifiedDataImpl mode v (GetData mode v)) => UnifiedData mode v

instance (UnifiedDataImpl bool v (GetData bool v)) => UnifiedData bool v

class
  (UnifiedSimpleMergeable 'S (GetData 'S v)) =>
  UnifiedDataSimpleMergeable v

instance (Mergeable v) => UnifiedDataSimpleMergeable v

-- | Evaluation mode with unified data types.
class
  ( forall v. UnifiedData bool v,
    forall v. (Mergeable v) => UnifiedDataSimpleMergeable v
  ) =>
  AllUnifiedData bool

instance
  ( forall v. UnifiedData bool v,
    forall v. (Mergeable v) => UnifiedDataSimpleMergeable v
  ) =>
  AllUnifiedData bool
