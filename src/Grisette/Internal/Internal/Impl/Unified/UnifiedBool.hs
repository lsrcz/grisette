{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Grisette.Internal.Internal.Impl.Unified.UnifiedBool () where

import Grisette.Internal.Core.Data.Class.Internal.Instances.SymOrd ()
import Grisette.Internal.Internal.Decl.Unified.UnifiedBool
  ( UnifiedBool (GetBool),
  )
import Grisette.Internal.SymPrim.SymBool (SymBool)
import Grisette.Internal.Unified.EvalModeTag (EvalModeTag (C, S))

instance UnifiedBool 'C where
  type GetBool 'C = Bool

instance UnifiedBool 'S where
  type GetBool 'S = SymBool
