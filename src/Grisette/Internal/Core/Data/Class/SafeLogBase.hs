{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Grisette.Internal.Core.Data.Class.SafeLogBase (SafeLogBase (..)) where

import Control.Exception (ArithException (RatioZeroDenominator))
import Control.Monad.Error.Class (MonadError (throwError))
import Grisette.Internal.Core.Control.Monad.Class.Union (MonadUnion)
import Grisette.Internal.Core.Data.Class.Mergeable (Mergeable)
import Grisette.Internal.Core.Data.Class.SimpleMergeable (mrgIf)
import Grisette.Internal.Core.Data.Class.SymEq (SymEq ((.==)))
import Grisette.Internal.Core.Data.Class.TryMerge (TryMerge)
import Grisette.Internal.SymPrim.Prim.Internal.Term
  ( FloatingUnaryOp (FloatingLog),
    PEvalFloatingTerm (pevalFloatingUnaryTerm),
    PEvalFractionalTerm (pevalFdivTerm),
  )
import Grisette.Internal.SymPrim.SymAlgReal (SymAlgReal (SymAlgReal))

-- $setup
-- >>> import Grisette.Core
-- >>> import Grisette.SymPrim
-- >>> import Control.Monad.Except
-- >>> import Control.Exception

-- | Safe 'logBase' with monadic error handling in multi-path execution.
-- These procedures throw an exception when the base is 1.
-- The result should be able to handle errors with `MonadError`.
class (MonadError e m, TryMerge m, Mergeable a) => SafeLogBase e a m where
  -- | Safe 'logBase' with monadic error handling in multi-path execution.
  --
  -- >>> safeLogBase (ssym "base") (ssym "val") :: ExceptT ArithException Union SymAlgReal
  -- ExceptT {If (= base 1.0) (Left Ratio has zero denominator) (Right (fdiv (log val) (log base)))}
  safeLogBase :: a -> a -> m a
  safeLogBase = undefined
  {-# INLINE safeLogBase #-}

  {-# MINIMAL safeLogBase #-}

instance
  (MonadError ArithException m, MonadUnion m) =>
  SafeLogBase ArithException SymAlgReal m
  where
  safeLogBase base@(SymAlgReal baset) (SymAlgReal at) =
    mrgIf (base .== 1) (throwError RatioZeroDenominator) $
      pure $
        SymAlgReal $
          pevalFdivTerm
            (pevalFloatingUnaryTerm FloatingLog at)
            (pevalFloatingUnaryTerm FloatingLog baset)
  {-# INLINE safeLogBase #-}
