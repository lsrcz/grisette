{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Trustworthy #-}

-- |
-- Module      :   Grisette.Core.Data.Class.Error
-- Copyright   :   (c) Sirui Lu 2021-2022
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only

module Grisette.Core.Data.Class.Error
  ( -- * Note for the examples

    --

    -- | This module does not contain actual implementation for symbolic primitive types, and
    -- the examples in this module cannot be executed solely with @grisette-core@ package.
    -- They rely on the implementation in @grisette-symir@ package.

    -- * Error transformation
    TransformError (..),

    -- * Throwing error
    symFailIfNot,
    symThrowTransformableError,
  )
where

import Control.Monad.Except
import Grisette.Core.Control.Monad.Union
import Grisette.Core.Data.Class.Bool
import Grisette.Core.Data.Class.Mergeable
import Grisette.Core.Data.Class.SimpleMergeable

-- $setup
-- >>> import Control.Exception
-- >>> import Grisette.Core
-- >>> import Grisette.IR.SymPrim
-- >>> :set -XOverloadedStrings
-- >>> :set -XFlexibleContexts

-- | This class indicates error type @to@ can always represent the error type
-- @from@.
--
-- This is useful in implementing generic procedures that may throw errors.
-- For example, we support symbolic division and modulo operations. These
-- operations should throw an error when the divisor is zero, and we use the
-- standard error type 'Control.Exception.ArithException' for this purpose.
--
-- However, the user may use other type to represent errors, so we need this
-- type class to transform the 'Control.Exception.ArithException' to the
-- user-defined types.
class TransformError from to where
  -- | Transforms an error with type @from@ to an error with type @to@.
  transformError :: from -> to

instance {-# OVERLAPPABLE #-} TransformError a a where
  transformError = id
  {-# INLINE transformError #-}

instance {-# OVERLAPS #-} TransformError a () where
  transformError _ = ()
  {-# INLINE transformError #-}

instance {-# OVERLAPPING #-} TransformError () () where
  transformError _ = ()
  {-# INLINE transformError #-}

-- | Used within a monadic multi path computation to begin exception processing.
--
-- Terminate the current execution path with the specified error. Compatible
-- errors can be transformed.
--
-- >>> symThrowTransformableError Overflow :: ExceptT AssertionError UnionM ()
-- ExceptT (UMrg (Single (Left AssertionError)))
symThrowTransformableError ::
  ( SymBoolOp bool,
    GMergeable bool to,
    GMergeable bool a,
    TransformError from to,
    MonadError to erm,
    GMonadUnion bool erm
  ) =>
  from ->
  erm a
symThrowTransformableError = merge . throwError . transformError
{-# INLINE symThrowTransformableError #-}

-- | Used within a monadic multi path computation for exception processing.
--
-- Terminate the current execution path with the specified error if the condition does not hold.
--
-- >>> let assert = symFailIfNot AssertionError
-- >>> assert "a" :: ExceptT AssertionError UnionM ()
-- ExceptT (UMrg (If (! a) (Single (Left AssertionError)) (Single (Right ()))))
symFailIfNot ::
  ( SymBoolOp bool,
    GMergeable bool to,
    TransformError from to,
    MonadError to erm,
    GMonadUnion bool erm
  ) =>
  from ->
  bool ->
  erm ()
symFailIfNot err cond = mrgIf cond (return ()) (symThrowTransformableError err)
{-# INLINE symFailIfNot #-}
