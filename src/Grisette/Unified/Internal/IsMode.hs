{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      :   Grisette.Unified.Internal.IsMode
-- Copyright   :   (c) Sirui Lu 2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Unified.Internal.IsMode (IsMode) where

import Grisette.Unified.Internal.UnifiedBV (AllUnifiedBV)
import Grisette.Unified.Internal.UnifiedBool (UnifiedBool (GetBool))
import Grisette.Unified.Internal.UnifiedConstraint (UnifiedPrimitive)
import Grisette.Unified.Internal.UnifiedData (AllUnifiedData)
import Grisette.Unified.Internal.UnifiedInteger (UnifiedInteger)

-- | A constraint that specifies that the mode is valid, and provide all the
-- corresponding constraints for the operaions for the types.
--
-- Note for users with GHC prior to 9.2.1: the GHC compiler isn't able to
-- resolve the operations for sized bitvectors and data types. In this case,
-- you may need to provide `Grisette.Unified.Internal.UnifiedBV.UnifiedBV`,
-- `Grisette.Unified.Internal.UnifiedBV.SafeUnifiedBV`, and
-- `Grisette.Unified.Internal.UnifiedData.UnifiedData` constraints manually.
--
-- For example, the following code is valid for GHC 9.2.1 and later:
--
-- > fbv ::
-- >   forall mode n.
-- >   (IsMode mode, KnownNat n, 1 <= n) =>
-- >   GetIntN mode n ->
-- >   GetIntN mode n ->
-- >   GetIntN mode n
-- > fbv l r =
-- >   mrgIte @mode
-- >     (l .== r)
-- >     (l + r)
-- >     (symIte @mode (l .< r) l r)
--
-- But with older GHCs, you need to write:
--
-- > fbv ::
-- >   forall mode n.
-- >   (IsMode mode, KnownNat n, 1 <= n, UnifiedBV mode n) =>
-- >   GetIntN mode n ->
-- >   GetIntN mode n ->
-- >   GetIntN mode n
-- > fbv l r =
-- >   mrgIte @mode
-- >     (l .== r)
-- >     (l + r)
-- >     (symIte @mode (l .< r) l r)
type IsMode mode =
  ( UnifiedBool mode,
    UnifiedPrimitive mode (GetBool mode),
    UnifiedInteger mode,
    AllUnifiedBV mode,
    AllUnifiedData mode
  )
