{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Grisette.Unified.Internal.Class.UnifiedSimpleMergeable
  ( UnifiedSimpleMergeable (..),
  )
where

import Data.Kind (Constraint)
import Data.Type.Bool (If)
import Grisette.Internal.Core.Data.Class.SimpleMergeable (SimpleMergeable)
import qualified Grisette.Internal.Core.Data.Class.SimpleMergeable
import Grisette.Unified.Internal.EvaluationMode
  ( EvaluationMode (Con, Sym),
    IsConMode,
  )
import Grisette.Unified.Internal.UnifiedBool (UnifiedBool (GetBool))

-- | A class that provides a unified simple merging function for unified types.
--
-- On all values, the 'mrgIte' function could work with concrete Boolean
-- conditions.
--
-- On all values with 'Grisette.SimpleMergeable' instance, the 'mrgIte' function
-- could work with symbolic Boolean conditions.
--
-- Note that you may sometimes need to write visible type application for the
-- mode parameter when the mode for the boolean variable isn't clear.
--
-- > mrgIte @mode (a .== b) ...
--
-- or
--
-- > mrgIte (a .== b :: GetBool mode) ...
class
  (If (IsConMode mode) (() :: Constraint) (SimpleMergeable a)) =>
  UnifiedSimpleMergeable mode a
  where
  mrgIte :: GetBool mode -> a -> a -> a

instance UnifiedSimpleMergeable 'Con a where
  mrgIte True a _ = a
  mrgIte False _ a = a

instance
  (SimpleMergeable a) =>
  UnifiedSimpleMergeable 'Sym a
  where
  mrgIte = Grisette.Internal.Core.Data.Class.SimpleMergeable.mrgIte
