{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Trustworthy #-}

module Grisette.Core.Data.Class.SimpleMergeable (SimpleMergeable (..)) where

import {-# SOURCE #-} Grisette.Core.Data.Class.Mergeable
import {-# SOURCE #-} Grisette.IR.SymPrim.Data.SymPrim

class Mergeable a => SimpleMergeable a where
  mrgIte :: SymBool -> a -> a -> a
