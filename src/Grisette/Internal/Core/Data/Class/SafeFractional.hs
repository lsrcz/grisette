{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      :   Grisette.Internal.Core.Data.Class.SafeFractional
-- Copyright   :   (c) Sirui Lu 2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Internal.Core.Data.Class.SafeFractional
  ( SafeFractional (..),
  )
where

import Control.Exception (ArithException (RatioZeroDenominator), throw)
import Control.Monad.Error.Class (MonadError (throwError))
import Grisette.Internal.Core.Control.Monad.Class.Union (MonadUnion)
import Grisette.Internal.Core.Data.Class.Mergeable (Mergeable)
import Grisette.Internal.Core.Data.Class.SimpleMergeable (mrgIf)
import Grisette.Internal.Core.Data.Class.Solvable (Solvable (con))
import Grisette.Internal.Core.Data.Class.SymEq (SymEq ((.==)))
import Grisette.Internal.Core.Data.Class.TryMerge (TryMerge, tryMerge)
import Grisette.Internal.SymPrim.AlgReal
  ( AlgReal (AlgExactRational),
    UnsupportedAlgRealOperation (UnsupportedAlgRealOperation),
  )
import Grisette.Internal.SymPrim.Prim.Internal.Term
  ( PEvalFractionalTerm (pevalFdivTerm, pevalRecipTerm),
  )
import Grisette.Internal.SymPrim.SymAlgReal (SymAlgReal (SymAlgReal))

-- $setup
-- >>> import Grisette.Core
-- >>> import Grisette.SymPrim
-- >>> import Control.Monad.Except
-- >>> import Control.Exception

-- | Safe fractional division with monadic error handling in multi-path
-- execution. These procedures throw an exception when the denominator is zero.
-- The result should be able to handle errors with `MonadError`.
class (MonadError e m, TryMerge m, Mergeable a) => SafeFractional e a m where
  -- | Safe fractional division with monadic error handling in multi-path
  -- execution.
  --
  -- >>> safeFdiv (ssym "a") (ssym "b") :: ExceptT ArithException Union SymAlgReal
  -- ExceptT {If (= b 0.0) (Left Ratio has zero denominator) (Right (fdiv a b))}
  safeFdiv :: a -> a -> m a

  -- | Safe fractional reciprocal with monadic error handling in multi-path
  -- execution.
  --
  -- >>> safeRecip (ssym "a") :: ExceptT ArithException Union SymAlgReal
  -- ExceptT {If (= a 0.0) (Left Ratio has zero denominator) (Right (recip a))}
  safeRecip :: a -> m a
  default safeRecip :: (Fractional a) => a -> m a
  safeRecip = safeFdiv (fromRational 1)
  {-# INLINE safeRecip #-}

  {-# MINIMAL safeFdiv #-}

instance
  ( MonadError ArithException m,
    TryMerge m
  ) =>
  SafeFractional ArithException AlgReal m
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
    throw $
      UnsupportedAlgRealOperation "safeRecip" $
        show l

instance
  (MonadError ArithException m, MonadUnion m) =>
  SafeFractional ArithException SymAlgReal m
  where
  safeFdiv (SymAlgReal lt) r@(SymAlgReal rt) =
    mrgIf
      (r .== con 0)
      (throwError RatioZeroDenominator)
      (pure $ SymAlgReal $ pevalFdivTerm lt rt)
  safeRecip l@(SymAlgReal lt) =
    mrgIf
      (l .== con 0)
      (throwError RatioZeroDenominator)
      (pure $ SymAlgReal $ pevalRecipTerm lt)
