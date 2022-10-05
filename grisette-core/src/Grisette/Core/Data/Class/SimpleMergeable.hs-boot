{-# LANGUAGE MultiParamTypeClasses #-}

module Grisette.Core.Data.Class.SimpleMergeable (SimpleMergeable) where

import {-# SOURCE #-} Grisette.Core.Data.Class.Mergeable

class Mergeable bool a => SimpleMergeable bool a where
  mrgIte :: bool -> a -> a -> a
