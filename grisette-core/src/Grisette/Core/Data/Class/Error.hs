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

    -- | This module does not contain the implementation for solvable (see "Grisette.Core#solvable")
    -- types, and the examples in this module rely on the implementations in
    -- the [grisette-symir](https://hackage.haskell.org/package/grisette-symir) package.

    -- * Error transformation
    TransformError (..),

    -- * Throwing error
    symAssertTransformableError,
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

-- | This class indicates that the error type @to@ can always represent the
-- error type @from@.
--
-- This is useful in implementing generic procedures that may throw errors.
-- For example, we support symbolic division and modulo operations. These
-- operations should throw an error when the divisor is zero, and we use the
-- standard error type 'Control.Exception.ArithException' for this purpose.
-- However, the user may use other type to represent errors, so we need this
-- type class to transform the 'Control.Exception.ArithException' to the
-- user-defined types.
--
-- Another example of these generic procedures is the
-- 'Grisette.Core.symAssert' and 'Grisette.Core.symAssume' functions.
-- They can be used with any error types that are
-- compatible with the 'Grisette.Core.AssertionError' and
-- 'Grisette.Core.VerificationConditions' types, respectively.
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
-- ExceptT {Left AssertionError}
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
-- Compatible error can be transformed.
--
-- >>> let assert = symAssertTransformableError AssertionError
-- >>> assert "a" :: ExceptT AssertionError UnionM ()
-- ExceptT {If (! a) (Left AssertionError) (Right ())}
symAssertTransformableError ::
  ( SymBoolOp bool,
    GMergeable bool to,
    TransformError from to,
    MonadError to erm,
    GMonadUnion bool erm
  ) =>
  from ->
  bool ->
  erm ()
symAssertTransformableError err cond = mrgIf cond (return ()) (symThrowTransformableError err)
{-# INLINE symAssertTransformableError #-}
