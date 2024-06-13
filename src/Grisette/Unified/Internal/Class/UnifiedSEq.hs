{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Grisette.Unified.Internal.Class.UnifiedSEq (UnifiedSEq (..)) where

import Data.Type.Bool (If)
import Grisette.Internal.Core.Data.Class.SEq (SEq)
import qualified Grisette.Internal.Core.Data.Class.SEq
import Grisette.Unified.Internal.EvaluationMode
  ( EvaluationMode (Con, Sym),
    IsConMode,
  )
import Grisette.Unified.Internal.UnifiedBool (UnifiedBool (GetBool))

-- | A class that provides a unified symbolic equality comparison for unified
-- types.
--
-- On all values with 'Eq' instance, the comparisons could return concrete
-- results.
--
-- On all values with 'Grisette.SEq' instance, the comparisons could return
-- symbolic results.
--
-- Note that you may sometimes need to write type annotations for the result
-- when the mode isn't clear:
--
-- > a .== b :: GetBool mode
class
  (If (IsConMode mode) (Eq a) (SEq a)) =>
  UnifiedSEq mode a
  where
  (.==) :: a -> a -> GetBool mode
  (./=) :: a -> a -> GetBool mode

instance (Eq a) => UnifiedSEq 'Con a where
  (.==) = (==)
  (./=) = (/=)

instance (SEq a) => UnifiedSEq 'Sym a where
  (.==) = (Grisette.Internal.Core.Data.Class.SEq..==)
  (./=) = (Grisette.Internal.Core.Data.Class.SEq../=)
