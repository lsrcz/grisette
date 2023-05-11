module Grisette.Core.Data.Class.SBits (
  SBits (..),
  symLsb,
  symMsb)
  where

import {-# SOURCE #-} Grisette.IR.SymPrim.Data.SymPrim
import Data.Bits

class SBits a where
  symTestBit :: a -> Int -> SymBool

symLsb :: (SBits a, FiniteBits a) => a -> SymBool
symLsb a = symTestBit a 0

symMsb :: (SBits a, FiniteBits a) => a -> SymBool
symMsb a = symTestBit a (finiteBitSize a - 1)
