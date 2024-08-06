{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      :   Grisette.Unified.Internal.UnifiedData
-- Copyright   :   (c) Sirui Lu 2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Unified.Internal.UnifiedData
  ( GetData,
    wrapData,
    extractData,
    UnifiedData,
    AllUnifiedData,
  )
where

import Control.DeepSeq (NFData)
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
import Grisette.Unified.Internal.Class.UnifiedITEOp (UnifiedITEOp)
import Grisette.Unified.Internal.Class.UnifiedSimpleMergeable
  ( UnifiedBranching (withBaseBranching),
    UnifiedSimpleMergeable,
    liftBaseMonad,
  )
import Grisette.Unified.Internal.Class.UnifiedSymEq (UnifiedSymEq)
import Grisette.Unified.Internal.Class.UnifiedSymOrd (UnifiedSymOrd)
import Grisette.Unified.Internal.EvalModeTag (EvalModeTag (Con, Sym))
import Language.Haskell.TH.Syntax (Lift)

class
  ( u ~ GetData mode v,
    Mergeable u,
    (AllSyms v) => AllSyms u,
    (Eq v) => Eq u,
    (EvalSym v) => EvalSym u,
    (ExtractSym v) => ExtractSym u,
    (ITEOp v) => ITEOp u,
    (PPrint v) => PPrint u,
    (Hashable v) => Hashable u,
    (Lift v) => Lift u,
    (LogicalOp v) => LogicalOp u,
    (NFData v) => NFData u,
    (Num v) => Num u,
    (SymEq v) => SymEq u,
    (Show v) => Show u,
    (SymOrd v) => SymOrd u,
    (SubstSym v) => SubstSym u,
    (UnifiedITEOp mode v) => UnifiedITEOp mode u,
    (UnifiedSimpleMergeable mode v) => UnifiedSimpleMergeable mode u,
    (mode ~ 'Sym) => UnifiedSimpleMergeable mode u,
    (UnifiedSymEq mode v) => UnifiedSymEq mode u,
    (UnifiedSymOrd mode v) => UnifiedSymOrd mode u,
    forall b. (ToCon v b) => ToCon u b,
    forall a. (ToSym a v) => ToSym a u
  ) =>
  UnifiedDataImpl (mode :: EvalModeTag) v u
    | u mode -> v,
      u v -> mode
  where
  -- | Get a unified data type. Resolves to @v@ in 'Con' mode, and @'Union' v@
  -- in 'Sym' mode.
  type GetData mode v

  -- | Wraps a value into the unified data type.
  wrapData :: v -> u

  -- | Extracts a value from the unified data type.
  extractData :: (Monad m, UnifiedBranching mode m) => u -> m v

instance (Mergeable v) => UnifiedDataImpl 'Con v v where
  type GetData 'Con v = v
  wrapData = id
  extractData ::
    forall m. (Mergeable v, Monad m, UnifiedBranching Con m) => v -> m v
  extractData = withBaseBranching @'Con @m mrgSingle

instance (Mergeable v) => UnifiedDataImpl 'Sym v (Union v) where
  type GetData 'Sym v = Union v
  wrapData = mrgSingle
  extractData ::
    forall m. (Mergeable v, Monad m, UnifiedBranching Sym m) => Union v -> m v
  extractData = liftBaseMonad

-- | This class is needed as constraint in user code prior to GHC 9.2.1.
-- See the notes in 'Grisette.Unified.Internal.IsMode.IsMode'.
class (UnifiedDataImpl mode v (GetData mode v)) => UnifiedData mode v

instance (UnifiedDataImpl bool v (GetData bool v)) => UnifiedData bool v

-- | Evaluation mode with unified data types.
class (forall v. (Mergeable v) => UnifiedData bool v) => AllUnifiedData bool

instance (forall v. (Mergeable v) => UnifiedData bool v) => AllUnifiedData bool
