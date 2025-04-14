{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      :   Grisette.Internal.Core.Data.Class.SafeFdiv
-- Copyright   :   (c) Sirui Lu 2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Internal.Core.Data.Class.SafeFdiv
  ( SafeFdiv (..),
    FdivOr (..),
    fdivOrZero,
    recipOrZero,
  )
where

import Control.Exception (ArithException (RatioZeroDenominator), throw)
import Control.Monad.Error.Class (MonadError (throwError))
import Grisette.Internal.Core.Control.Monad.Class.Union (MonadUnion)
import Grisette.Internal.Core.Data.Class.ITEOp (ITEOp (symIte))
import Grisette.Internal.Core.Data.Class.Mergeable (Mergeable)
import Grisette.Internal.Core.Data.Class.SimpleMergeable (mrgIf)
import Grisette.Internal.Core.Data.Class.Solvable (Solvable (con))
import Grisette.Internal.Core.Data.Class.SymEq (SymEq ((.==)))
import Grisette.Internal.Core.Data.Class.TryMerge (TryMerge, tryMerge)
import Grisette.Internal.SymPrim.AlgReal
  ( AlgReal (AlgExactRational),
    UnsupportedAlgRealOperation (UnsupportedAlgRealOperation),
  )
import Grisette.Internal.SymPrim.SymAlgReal (SymAlgReal)

-- $setup
-- >>> import Grisette.Core
-- >>> import Grisette.SymPrim
-- >>> import Control.Monad.Except
-- >>> import Control.Exception

-- | Safe fractional with default values returned on exception.
class FdivOr a where
  -- | Safe '/' with default values returned on exception.
  --
  -- >>> fdivOr "d" "a" "b" :: SymAlgReal
  -- (ite (= b 0.0) d (fdiv a b))
  fdivOr :: a -> a -> a -> a

  -- | Safe 'recip' with default values returned on exception.
  --
  -- >>> recipOr "d" "a" :: SymAlgReal
  -- (ite (= a 0.0) d (recip a))
  recipOr :: a -> a -> a

-- | Safe '/' with 0 returned on exception.
fdivOrZero :: (FdivOr a, Num a) => a -> a -> a
fdivOrZero l = fdivOr (l - l) l

-- | Safe 'recip' with 0 returned on exception.
recipOrZero :: (FdivOr a, Num a) => a -> a
recipOrZero v = recipOr (v - v) v

-- | Safe fractional division with monadic error handling in multi-path
-- execution. These procedures throw an exception when the denominator is zero.
-- The result should be able to handle errors with `MonadError`.
class (MonadError e m, TryMerge m, Mergeable a) => SafeFdiv e a m where
  -- | Safe fractional division with monadic error handling in multi-path
  -- execution.
  --
  -- >>> safeFdiv "a" "b" :: ExceptT ArithException Union SymAlgReal
  -- ExceptT {If (= b 0.0) (Left Ratio has zero denominator) (Right (fdiv a b))}
  safeFdiv :: a -> a -> m a

  -- | Safe fractional reciprocal with monadic error handling in multi-path
  -- execution.
  --
  -- >>> safeRecip "a" :: ExceptT ArithException Union SymAlgReal
  -- ExceptT {If (= a 0.0) (Left Ratio has zero denominator) (Right (recip a))}
  safeRecip :: a -> m a
  default safeRecip :: (Fractional a) => a -> m a
  safeRecip = safeFdiv (fromRational 1)
  {-# INLINE safeRecip #-}

  {-# MINIMAL safeFdiv #-}

instance FdivOr AlgReal where
  fdivOr d (AlgExactRational l) (AlgExactRational r)
    | r /= 0 = AlgExactRational (l / r)
    | otherwise = d
  fdivOr d l r =
    -- Throw the error because the user should never construct an AlgReal
    -- other than AlgExactRational.
    throw $
      UnsupportedAlgRealOperation "fdivOr" $
        show d <> " and " <> show l <> " and " <> show r
  {-# INLINE fdivOr #-}
  recipOr d (AlgExactRational l)
    | l /= 0 = AlgExactRational (recip l)
    | otherwise = d
  recipOr d l =
    throw $ UnsupportedAlgRealOperation "recipOr" $ show d <> " and " <> show l
  {-# INLINE recipOr #-}

instance
  ( MonadError ArithException m,
    TryMerge m
  ) =>
  SafeFdiv ArithException AlgReal m
  where
  safeFdiv (AlgExactRational l) (AlgExactRational r)
    | r /= 0 =
        pure $ AlgExactRational (l / r)
    | otherwise = tryMerge $ throwError RatioZeroDenominator
  safeFdiv l r =
    -- Throw the error because the user should never construct an AlgReal
    -- other than AlgExactRational.
    throw $
      UnsupportedAlgRealOperation "safeFdiv" $
        show l <> " and " <> show r
  {-# INLINE safeFdiv #-}
  safeRecip (AlgExactRational l)
    | l /= 0 =
        pure $ AlgExactRational (recip l)
    | otherwise = tryMerge $ throwError RatioZeroDenominator
  safeRecip l =
    throw $ UnsupportedAlgRealOperation "safeRecip" $ show l

instance FdivOr SymAlgReal where
  fdivOr d l r = symIte (r .== con 0) d (l / r)
  recipOr d l = symIte (l .== con 0) d (recip l)

instance
  (MonadError ArithException m, MonadUnion m) =>
  SafeFdiv ArithException SymAlgReal m
  where
  safeFdiv l r =
    mrgIf (r .== con 0) (throwError RatioZeroDenominator) (pure $ l / r)
  safeRecip l =
    mrgIf (l .== con 0) (throwError RatioZeroDenominator) (pure $ recip l)
