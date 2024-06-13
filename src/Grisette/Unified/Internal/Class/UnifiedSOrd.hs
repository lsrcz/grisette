{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Grisette.Unified.Internal.Class.UnifiedSOrd (UnifiedSOrd (..)) where

import Data.Type.Bool (If)
import Grisette.Internal.Core.Control.Monad.UnionM (liftUnionM)
import Grisette.Internal.Core.Data.Class.SOrd (SOrd)
import qualified Grisette.Internal.Core.Data.Class.SOrd
import Grisette.Unified.Internal.Class.UnifiedBranching (UnifiedBranching)
import Grisette.Unified.Internal.EvaluationMode
  ( EvaluationMode (Con, Sym),
    IsConMode,
  )
import Grisette.Unified.Internal.UnifiedBool (UnifiedBool (GetBool))

-- | A class that provides a unified symbolic comparison for unified types.
--
-- On all values with 'Ord' instance, the comparisons could return concrete
-- results.
--
-- On all values with 'Grisette.SOrd' instance, the comparisons could return
-- symbolic results.
--
-- Note that you may sometimes need to write type annotations for the result
-- when the mode isn't clear:
--
-- > a .< b :: GetBool mode
class
  (If (IsConMode mode) (Ord a) (SOrd a)) =>
  UnifiedSOrd mode a
  where
  (.<=) :: a -> a -> GetBool mode
  (.>=) :: a -> a -> GetBool mode
  (.>) :: a -> a -> GetBool mode
  (.<) :: a -> a -> GetBool mode
  symCompare ::
    (Monad ctx, UnifiedBranching mode ctx) => a -> a -> ctx Ordering

instance (Ord a) => UnifiedSOrd 'Con a where
  (.<=) = (<=)
  (.>=) = (>=)
  (.>) = (>)
  (.<) = (<)
  symCompare x y = pure (compare x y)

instance (SOrd a) => UnifiedSOrd 'Sym a where
  (.<=) = (Grisette.Internal.Core.Data.Class.SOrd..<=)
  (.>=) = (Grisette.Internal.Core.Data.Class.SOrd..>=)
  (.>) = (Grisette.Internal.Core.Data.Class.SOrd..>)
  (.<) = (Grisette.Internal.Core.Data.Class.SOrd..<)
  symCompare x y =
    liftUnionM $ Grisette.Internal.Core.Data.Class.SOrd.symCompare x y
