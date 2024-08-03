{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
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
  ( SafeBitCast (..),
  )
where

import Control.Monad.Error.Class (MonadError (throwError))
import Data.Int (Int16, Int32, Int64)
import Data.SBV (Word32)
import Data.Word (Word16, Word64)
import GHC.TypeLits (KnownNat, type (+), type (<=))
import Grisette.Internal.Core.Control.Monad.Class.Union (MonadUnion)
import Grisette.Internal.Core.Data.Class.BitCast
  ( BitCast (bitCast),
    BitCastOr,
    bitCastOrCanonical,
  )
import Grisette.Internal.Core.Data.Class.IEEEFP (fpIsNaN)
import Grisette.Internal.Core.Data.Class.Mergeable (Mergeable)
import Grisette.Internal.Core.Data.Class.SimpleMergeable (mrgIf)
import Grisette.Internal.Core.Data.Class.SymIEEEFP
  ( SymIEEEFPTraits (symFpIsNaN),
  )
import Grisette.Internal.Core.Data.Class.TryMerge (TryMerge, tryMerge)
import Grisette.Internal.SymPrim.BV (IntN, WordN, WordN16, WordN32, WordN64)
import Grisette.Internal.SymPrim.FP
  ( NotRepresentableFPError (NaNError),
    FP,
    FP16,
    FP32,
    FP64,
    ValidFP,
  )
import Grisette.Internal.SymPrim.SymBV (SymIntN, SymWordN)
import Grisette.Internal.SymPrim.SymFP (SymFP)

-- | Bitcasting a value. If the value cannot be precisely bitcast, throw an
-- error.
class
  (MonadError e m, TryMerge m, Mergeable b, BitCastOr a b) =>
  SafeBitCast e a b m
  where
  safeBitCast :: a -> m b

instance
  ( ValidFP eb sb,
    r ~ (eb + sb),
    KnownNat r,
    1 <= r,
    TryMerge m,
    MonadError NotRepresentableFPError m
  ) =>
  SafeBitCast NotRepresentableFPError (FP eb sb) (WordN r) m
  where
  safeBitCast a
    | fpIsNaN a = tryMerge $ throwError NaNError
    | otherwise = tryMerge $ return $ bitCastOrCanonical a

instance
  ( ValidFP eb sb,
    r ~ (eb + sb),
    KnownNat r,
    1 <= r,
    TryMerge m,
    MonadError NotRepresentableFPError m
  ) =>
  SafeBitCast NotRepresentableFPError (FP eb sb) (IntN r) m
  where
  safeBitCast a
    | fpIsNaN a = tryMerge $ throwError NaNError
    | otherwise = tryMerge $ return $ bitCastOrCanonical a

#define SAFE_BIT_CAST_VIA_INTERMEDIATE(from, to, intermediate) \
instance \
  (MonadError NotRepresentableFPError m, TryMerge m) => \
  SafeBitCast NotRepresentableFPError from to m \
  where \
  safeBitCast a = do \
    r :: intermediate <- safeBitCast a; \
    tryMerge $ return $ bitCast r

#if 1
SAFE_BIT_CAST_VIA_INTERMEDIATE(FP64, Word64, WordN64)
SAFE_BIT_CAST_VIA_INTERMEDIATE(FP64, Int64, WordN64)
SAFE_BIT_CAST_VIA_INTERMEDIATE(FP64, Double, WordN64)
SAFE_BIT_CAST_VIA_INTERMEDIATE(FP32, Word32, WordN32)
SAFE_BIT_CAST_VIA_INTERMEDIATE(FP32, Int32, WordN32)
SAFE_BIT_CAST_VIA_INTERMEDIATE(FP32, Float, WordN32)
SAFE_BIT_CAST_VIA_INTERMEDIATE(FP16, Word16, WordN16)
SAFE_BIT_CAST_VIA_INTERMEDIATE(FP16, Int16, WordN16)
#endif

instance
  ( ValidFP eb sb,
    r ~ (eb + sb),
    KnownNat r,
    1 <= r,
    MonadUnion m,
    MonadError NotRepresentableFPError m
  ) =>
  SafeBitCast NotRepresentableFPError (SymFP eb sb) (SymWordN r) m
  where
  safeBitCast a =
    mrgIf
      (symFpIsNaN a)
      (throwError NaNError)
      (return $ bitCastOrCanonical a)

instance
  ( ValidFP eb sb,
    r ~ (eb + sb),
    KnownNat r,
    1 <= r,
    MonadUnion m,
    MonadError NotRepresentableFPError m
  ) =>
  SafeBitCast NotRepresentableFPError (SymFP eb sb) (SymIntN r) m
  where
  safeBitCast a =
    mrgIf
      (symFpIsNaN a)
      (throwError NaNError)
      (return $ bitCastOrCanonical a)
