{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

-- |
-- Module      :   Grisette.Unified.Internal.Class.UnifiedBranching
-- Copyright   :   (c) Sirui Lu 2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Unified.Internal.Class.UnifiedBranching
  ( UnifiedBranching (..),
  )
where

import Control.Monad.Identity (Identity (runIdentity))
import Data.Kind (Constraint)
import Data.Type.Bool (If)
import Grisette.Internal.Core.Control.Monad.UnionM (liftUnionM)
import Grisette.Internal.Core.Data.Class.Mergeable (Mergeable)
import Grisette.Internal.Core.Data.Class.SimpleMergeable (UnionMergeable1)
import qualified Grisette.Internal.Core.Data.Class.SimpleMergeable
import Grisette.Internal.Core.Data.Class.TryMerge (TryMerge, mrgSingle, tryMerge)
import Grisette.Unified.Internal.EvaluationMode
  ( BaseMonad,
    EvaluationMode (Con, Sym),
    IsConMode,
  )
import Grisette.Unified.Internal.UnifiedBool (UnifiedBool (GetBool))

-- | A class that provides a unified branching operation for unified types.
--
-- On all monads with 'TryMerge' instance, the 'mrgIf' function could use
-- concrete Boolean variable for branching.
--
-- On all monads with 'UnionMergeable1' instance, the 'mrgIf' function could use
-- symbolic Boolean variable for branching.
--
-- Note that you may sometimes need to write visible type application for the
-- mode parameter when the mode for the boolean variable isn't clear.
--
-- > mrgIf @mode (a .== b) ...
--
-- or
--
-- > mrgIf (a .== b :: GetBool mode) ...
class
  ( TryMerge m,
    If (IsConMode mode) (() :: Constraint) (UnionMergeable1 m)
  ) =>
  UnifiedBranching (mode :: EvaluationMode) m
  where
  mrgIf :: (Mergeable a) => GetBool mode -> m a -> m a -> m a
  liftBaseMonad ::
    (Applicative m, UnifiedBranching mode m, Mergeable a) =>
    BaseMonad mode a ->
    m a

instance (TryMerge m) => UnifiedBranching 'Con m where
  mrgIf True t _ = tryMerge t
  mrgIf False _ e = tryMerge e
  liftBaseMonad = mrgSingle . runIdentity

instance (UnionMergeable1 m) => UnifiedBranching 'Sym m where
  mrgIf = Grisette.Internal.Core.Data.Class.SimpleMergeable.mrgIf
  liftBaseMonad = liftUnionM
