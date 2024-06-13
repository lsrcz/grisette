{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Grisette.Unified.Internal.UnifiedData
  ( UnifiedDataImpl (..),
    UnifiedData,
    AllUnifiedData,
  )
where

import Control.DeepSeq (NFData)
import Data.Hashable (Hashable)
import Grisette.Internal.Core.Control.Monad.UnionM (UnionM, liftUnionM)
import Grisette.Internal.Core.Data.Class.EvaluateSym (EvaluateSym)
import Grisette.Internal.Core.Data.Class.ExtractSymbolics (ExtractSymbolics)
import Grisette.Internal.Core.Data.Class.GPretty (GPretty)
import Grisette.Internal.Core.Data.Class.ITEOp (ITEOp)
import Grisette.Internal.Core.Data.Class.LogicalOp (LogicalOp)
import Grisette.Internal.Core.Data.Class.Mergeable (Mergeable)
import Grisette.Internal.Core.Data.Class.SEq (SEq)
import Grisette.Internal.Core.Data.Class.SOrd (SOrd)
import Grisette.Internal.Core.Data.Class.SubstituteSym (SubstituteSym)
import Grisette.Internal.Core.Data.Class.ToCon (ToCon)
import Grisette.Internal.Core.Data.Class.ToSym (ToSym)
import Grisette.Internal.Core.Data.Class.TryMerge (mrgSingle)
import Grisette.Internal.SymPrim.AllSyms (AllSyms)
import Grisette.Unified.Internal.Class.UnifiedBranching (UnifiedBranching)
import Grisette.Unified.Internal.EvaluationMode (EvaluationMode (Con, Sym))
import Language.Haskell.TH.Syntax (Lift)

class
  ( u ~ GetData mode v,
    Mergeable u,
    (AllSyms v) => AllSyms u,
    (Eq v) => Eq u,
    (EvaluateSym v) => EvaluateSym u,
    (ExtractSymbolics v) => ExtractSymbolics u,
    (ITEOp v) => ITEOp u,
    (GPretty v) => GPretty u,
    (Hashable v) => Hashable u,
    (Lift v) => Lift u,
    (LogicalOp v) => LogicalOp u,
    (NFData v) => NFData u,
    (Num v) => Num u,
    (SEq v) => SEq u,
    (Show v) => Show u,
    (SOrd v) => SOrd u,
    (SubstituteSym v) => SubstituteSym u,
    forall b. (ToCon v b) => ToCon u b,
    forall a. (ToSym a v) => ToSym a u
  ) =>
  UnifiedDataImpl (mode :: EvaluationMode) v u
    | u mode -> v,
      u v -> mode
  where
  -- | Get a unified data type. Resolves to @v@ in 'Con' mode, and @'UnionM' v@
  -- in 'Sym' mode.
  type GetData mode v

  -- | Wraps a value into the unified data type.
  wrapData :: v -> u

  -- | Extracts a value from the unified data type.
  extractData :: (Monad m, UnifiedBranching mode m) => u -> m v

instance (Mergeable v) => UnifiedDataImpl 'Con v v where
  type GetData 'Con v = v
  wrapData = id
  extractData = mrgSingle

instance (Mergeable v) => UnifiedDataImpl 'Sym v (UnionM v) where
  type GetData 'Sym v = UnionM v
  wrapData = mrgSingle
  extractData = liftUnionM

-- | This class is needed as constraint in user code prior to GHC 9.2.1.
-- See the notes in 'Grisette.Unified.Internal.IsMode.IsMode'.
class (UnifiedDataImpl mode v (GetData mode v)) => UnifiedData mode v

instance (UnifiedDataImpl bool v (GetData bool v)) => UnifiedData bool v

class (forall v. (Mergeable v) => UnifiedData bool v) => AllUnifiedData bool

instance (forall v. (Mergeable v) => UnifiedData bool v) => AllUnifiedData bool