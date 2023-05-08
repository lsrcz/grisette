module Grisette.Core.Data.Class.SBits where

import {-# SOURCE #-} Grisette.IR.SymPrim.Data.SymPrim

class SBits a where
  symTestBit :: a -> Int -> SymBool
