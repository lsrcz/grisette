{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      :   Grisette.Internal.Core.Data.Class.SafeBitCast
-- Copyright   :   (c) Sirui Lu 2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Internal.Core.Data.Class.SafeBitCast
  ( BitCastCanonical (..),
    BitCastOr (..),
    bitCastOrCanonical,
    BitCastOrCanonical,
  )
where

-- | The canonical value when the bitcast cannot be precisely performed.
--
-- For example, with SMT-LIB2, there is only one NaN for floating point numbers,
-- with multiple bit representations. Our underlying 'FP' type also follows this
-- convention. This means that we cannot precisely bitcast a 'FP' to other
-- types. So instead, we bitcast the NaN value to a canonical representation,
-- defined with this type class.
class BitCastCanonical from to where
  bitCastCanonicalValue :: proxy from -> to

-- | Bitcasting a value. If the value cannot be precisely bitcast, use the
-- default value.
class BitCastOr from to where
  bitCastOr :: to -> from -> to

-- | Constraint for bitcasting a value and when the value cannot be precisely
-- bitcast, use the canonical value.
type BitCastOrCanonical a b = (BitCastCanonical a b, BitCastOr a b)

-- | Bitcasting a value and when the value cannot be precisely bitcast, use the
-- canonical value.
bitCastOrCanonical :: (BitCastOrCanonical from to) => from -> to
bitCastOrCanonical x = bitCastOr (bitCastCanonicalValue [x]) x
