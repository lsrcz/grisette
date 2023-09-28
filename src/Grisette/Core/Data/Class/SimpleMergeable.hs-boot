{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Trustworthy #-}

module Grisette.Core.Data.Class.SimpleMergeable (SimpleMergeable (..)) where

import {-# SOURCE #-} Grisette.Core.Data.Class.Mergeable
  ( Mergeable,
  )
import {-# SOURCE #-} Grisette.IR.SymPrim.Data.SymPrim (SymBool)

class (Mergeable a) => SimpleMergeable a where
  mrgIte :: SymBool -> a -> a -> a
