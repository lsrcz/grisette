{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Trustworthy #-}

-- |
-- Module      :   Grisette.Core.Data.Class.Error
-- Copyright   :   (c) Sirui Lu 2021-2023
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Core.Data.Class.Error
  ( -- * Error transformation
    TransformError (..),

    -- * Throwing error
    symAssertWith,
    symAssertTransformableError,
    symThrowTransformableError,
    symAssert,
    symAssume,
  )
where

import Control.Exception (ArithException, ArrayException)
import Control.Monad.Except (MonadError (throwError))
import Grisette.Core.Control.Exception
  ( AssertionError (AssertionError),
    VerificationConditions (AssertionViolation, AssumptionViolation),
  )
import Grisette.Core.Control.Monad.Union (MonadUnion)
import Grisette.Core.Data.Class.Mergeable (Mergeable)
import Grisette.Core.Data.Class.SimpleMergeable (mrgIf)
import Grisette.Core.Data.Class.TryMerge (tryMerge)
import Grisette.IR.SymPrim.Data.SymBool (SymBool)

-- $setup
-- >>> import Control.Exception
-- >>> import Grisette.Core
-- >>> import Grisette.IR.SymPrim
-- >>> import Grisette.Lib.Control.Monad
-- >>> import Control.Monad.Except
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
  ( Mergeable to,
    Mergeable a,
    TransformError from to,
    MonadError to erm,
    MonadUnion erm
  ) =>
  from ->
  erm a
symThrowTransformableError = tryMerge . throwError . transformError
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
  ( Mergeable to,
    TransformError from to,
    MonadError to erm,
    MonadUnion erm
  ) =>
  from ->
  SymBool ->
  erm ()
symAssertTransformableError err cond = mrgIf cond (return ()) (symThrowTransformableError err)
{-# INLINE symAssertTransformableError #-}

symAssertWith ::
  ( Mergeable e,
    MonadError e erm,
    MonadUnion erm
  ) =>
  e ->
  SymBool ->
  erm ()
symAssertWith err cond = mrgIf cond (return ()) (throwError err)
{-# INLINE symAssertWith #-}

instance TransformError VerificationConditions VerificationConditions where
  transformError = id

instance TransformError AssertionError VerificationConditions where
  transformError _ = AssertionViolation

instance TransformError ArithException AssertionError where
  transformError _ = AssertionError

instance TransformError ArrayException AssertionError where
  transformError _ = AssertionError

instance TransformError AssertionError AssertionError where
  transformError = id

-- | Used within a monadic multi path computation to begin exception processing.
--
-- Checks the condition passed to the function.
-- The current execution path will be terminated with assertion error if the condition is false.
--
-- If the condition is symbolic, Grisette will split the execution into two paths based on the condition.
-- The symbolic execution will continue on the then-branch, where the condition is true.
-- For the else branch, where the condition is false, the execution will be terminated.
--
-- The resulting monadic environment should be compatible with the 'AssertionError'
-- error type. See 'TransformError' type class for details.
--
-- __/Examples/__:
--
-- Terminates the execution if the condition is false.
-- Note that we may lose the 'Mergeable' knowledge here if no possible execution
-- path is viable. This may affect the efficiency in theory, but in practice this
-- should not be a problem as all paths are terminated and no further evaluation
-- would be performed.
--
-- >>> symAssert (con False) :: ExceptT AssertionError UnionM ()
-- ExceptT {Left AssertionError}
-- >>> do; symAssert (con False); mrgReturn 1 :: ExceptT AssertionError UnionM Integer
-- ExceptT <Left AssertionError>
--
-- No effect if the condition is true:
--
-- >>> symAssert (con True) :: ExceptT AssertionError UnionM ()
-- ExceptT {Right ()}
-- >>> do; symAssert (con True); mrgReturn 1 :: ExceptT AssertionError UnionM Integer
-- ExceptT {Right 1}
--
-- Splitting the path and terminate one of them when the condition is symbolic.
--
-- >>> symAssert (ssym "a") :: ExceptT AssertionError UnionM ()
-- ExceptT {If (! a) (Left AssertionError) (Right ())}
-- >>> do; symAssert (ssym "a"); mrgReturn 1 :: ExceptT AssertionError UnionM Integer
-- ExceptT {If (! a) (Left AssertionError) (Right 1)}
--
-- 'AssertionError' is compatible with 'VerificationConditions':
--
-- >>> symAssert (ssym "a") :: ExceptT VerificationConditions UnionM ()
-- ExceptT {If (! a) (Left AssertionViolation) (Right ())}
symAssert ::
  (TransformError AssertionError to, Mergeable to, MonadError to erm, MonadUnion erm) =>
  SymBool ->
  erm ()
symAssert = symAssertTransformableError AssertionError

-- | Used within a monadic multi path computation to begin exception processing.
--
-- Similar to 'symAssert', but terminates the execution path with 'AssumptionViolation' error.
--
-- /Examples/:
--
-- >>> symAssume (ssym "a") :: ExceptT VerificationConditions UnionM ()
-- ExceptT {If (! a) (Left AssumptionViolation) (Right ())}
symAssume ::
  (TransformError VerificationConditions to, Mergeable to, MonadError to erm, MonadUnion erm) =>
  SymBool ->
  erm ()
symAssume = symAssertTransformableError AssumptionViolation
