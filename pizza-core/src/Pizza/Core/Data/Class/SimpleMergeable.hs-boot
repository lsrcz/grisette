{-# LANGUAGE MultiParamTypeClasses #-}

module Pizza.Core.Data.Class.SimpleMergeable (SimpleMergeable) where

import {-# SOURCE #-} Pizza.Core.Data.Class.Mergeable

class Mergeable bool a => SimpleMergeable bool a where
  mrgIte :: bool -> a -> a -> a
