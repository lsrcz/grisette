{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Grisette.Unified.Internal.Class.UnifiedITEOp (UnifiedITEOp (..)) where

import Control.Monad.Identity (Identity (runIdentity))
import Data.Kind (Constraint)
import Data.Type.Bool (If)
import Grisette.Internal.Core.Data.Class.ITEOp (ITEOp)
import qualified Grisette.Internal.Core.Data.Class.ITEOp
import Grisette.Internal.Core.Data.Class.Mergeable (Mergeable)
import qualified Grisette.Internal.Core.Data.Class.PlainUnion
import Grisette.Unified.Internal.BaseMonad (BaseMonad)
import Grisette.Unified.Internal.EvaluationMode
  ( EvaluationMode (Con, Sym),
    IsConMode,
  )
import Grisette.Unified.Internal.UnifiedBool (UnifiedBool (GetBool))

-- | A class that provides a unified symbolic ite operation for unified types.
--
-- On all values, the 'symIte' function could work with concrete Boolean
-- conditions.
--
-- On all values with 'Grisette.ITEOp' instance, the 'mrgIf' function could work
-- with symbolic Boolean conditions.
--
-- Note that you may sometimes need to write visible type application for the
-- mode parameter when the mode for the boolean variable isn't clear.
--
-- > symIte @mode (a .== b) ...
--
-- or
--
-- > symIte (a .== b :: GetBool mode) ...
class
  (If (IsConMode mode) (() :: Constraint) (ITEOp v)) =>
  UnifiedITEOp mode v
  where
  symIte :: GetBool mode -> v -> v -> v

  -- | Merges GetData into a single value, using 'symIte'.
  symIteMerge :: (UnifiedITEOp mode v, Mergeable v) => BaseMonad mode v -> v

instance UnifiedITEOp 'Con a where
  symIte True t _ = t
  symIte False _ e = e
  symIteMerge = runIdentity

instance (ITEOp a) => UnifiedITEOp 'Sym a where
  symIte = Grisette.Internal.Core.Data.Class.ITEOp.symIte
  symIteMerge = Grisette.Internal.Core.Data.Class.PlainUnion.symIteMerge
