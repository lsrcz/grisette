{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}

module Grisette.Internal.Unified.Class.UnionViewMode (UnionViewMode) where

import Control.Monad.Identity (Identity)
import Data.Kind (Type)
import Grisette.Internal.Internal.Decl.Core.Control.Monad.Union (Union)
import Grisette.Internal.Unified.EvalModeTag (EvalModeTag (C, S))

-- | This class is used to determine the type of the Boolean used in the
-- 'Grisette.Core.UnionView' class.
--
-- For 'Identity', we use 'Bool' as the Boolean type, and this ensures that
-- the 'Grisette.Core.ifView' function will return 'Nothing'.
--
-- For 'Union', we use 'SymBool' as the Boolean type, and 'Grisette.Core.ifView'
-- function can return 'Nothing' or 'Just'.
class UnionViewMode (mode :: EvalModeTag) (u :: Type -> Type) | u -> mode

instance UnionViewMode 'C Identity

instance UnionViewMode 'S Union
