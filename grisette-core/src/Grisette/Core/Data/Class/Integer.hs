{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Grisette.Core.Data.Class.Integer
  ( ArithException (..),
    SignedDivMod (..),
    UnsignedDivMod (..),
    SignedQuotRem (..),
    SymIntegerOp,
  )
where

import Control.Exception
import Control.Monad.Except
import Grisette.Core.Control.Monad.Union
import Grisette.Core.Data.Class.Bool
import Grisette.Core.Data.Class.Error
import Grisette.Core.Data.Class.PrimWrapper
import Grisette.Core.Data.Class.SOrd

-- $setup
-- >>> import Grisette.Core
-- >>> import Grisette.IR.SymPrim

-- | Safe signed 'div' and 'mod' with monadic error handling in multi-path execution.
class SignedDivMod bool a where
  -- | Safe signed 'div' with monadic error handling in multi-path execution.
  --
  -- >>> divs (ssymb "a") (ssymb "b") :: ExceptT AssertionError UnionM SymInteger
  -- ExceptT (UMrg (If (= b 0I) (Single (Left AssertionError)) (Single (Right (div a b)))))
  divs :: (MonadError e uf, MonadUnion bool uf, TransformError ArithException e) => a -> a -> uf a

  mods :: (MonadError e uf, MonadUnion bool uf, TransformError ArithException e) => a -> a -> uf a

-- | Safe unsigned 'div' and 'mod' with monadic error handling in multi-path execution.
class UnsignedDivMod bool a where
  udivs :: (MonadError e uf, MonadUnion bool uf, TransformError ArithException e) => a -> a -> uf a
  umods :: (MonadError e uf, MonadUnion bool uf, TransformError ArithException e) => a -> a -> uf a

-- | Safe signed 'quot' and 'rem' with monadic error handling in multi-path execution.
class SignedQuotRem bool a where
  quots :: (MonadError e uf, MonadUnion bool uf, TransformError ArithException e) => a -> a -> uf a
  rems :: (MonadError e uf, MonadUnion bool uf, TransformError ArithException e) => a -> a -> uf a

-- | Aggregation for the operations on symbolic integer types
class (Num a, SEq bool a, SOrd bool a, PrimWrapper a Integer) => SymIntegerOp bool a
