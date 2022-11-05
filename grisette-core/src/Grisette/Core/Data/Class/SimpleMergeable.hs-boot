{-# LANGUAGE MultiParamTypeClasses #-}

module Grisette.Core.Data.Class.SimpleMergeable (GSimpleMergeable) where

import {-# SOURCE #-} Grisette.Core.Data.Class.Mergeable

class GMergeable bool a => GSimpleMergeable bool a where
  gmrgIte :: bool -> a -> a -> a
