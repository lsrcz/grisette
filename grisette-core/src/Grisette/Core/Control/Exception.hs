{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE Trustworthy #-}

-- |
-- Module      :   Grisette.Core.Control.Exception
-- Copyright   :   (c) Sirui Lu 2021-2022
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Core.Control.Exception
  ( -- * Note for the examples

    --

    -- | This module does not contain the implementation for solvable (see "Grisette.Core#solvable")
    -- types, and the examples in this module rely on the implementations in
    -- the [grisette-symir](https://hackage.haskell.org/package/grisette-symir) package.

    -- * Predefined exceptions
    AssertionError (..),
    VerificationConditions (..),
    symAssert,
    symAssume,
  )
where

import Control.DeepSeq
import Control.Exception
import Control.Monad.Except
import GHC.Generics
import Generics.Deriving
import Grisette.Core.Control.Monad.Union
import Grisette.Core.Data.Class.Bool
import Grisette.Core.Data.Class.Error
import Grisette.Core.Data.Class.Evaluate
import Grisette.Core.Data.Class.ExtractSymbolics
import Grisette.Core.Data.Class.Mergeable
import Grisette.Core.Data.Class.SOrd
import Grisette.Core.Data.Class.SimpleMergeable
import Grisette.Core.Data.Class.Solvable
import Grisette.Core.Data.Class.ToCon
import Grisette.Core.Data.Class.ToSym

-- $setup
-- >>> import Grisette.Core
-- >>> import Grisette.Lib.Base
-- >>> import Grisette.IR.SymPrim
-- >>> import Control.Monad.Trans.Except

-- | Assertion error.
data AssertionError = AssertionError
  deriving (Show, Eq, Ord, Generic, NFData)
  deriving (ToCon AssertionError, ToSym AssertionError) via (Default AssertionError)

deriving via (Default AssertionError) instance (SymBoolOp bool) => GMergeable bool AssertionError

deriving via (Default AssertionError) instance (SymBoolOp bool) => GSimpleMergeable bool AssertionError

deriving via (Default AssertionError) instance (SymBoolOp bool) => GSEq bool AssertionError

instance (SymBoolOp bool) => GSOrd bool AssertionError where
  _ `gsymle` _ = con True
  _ `gsymlt` _ = con False
  _ `gsymge` _ = con True
  _ `gsymgt` _ = con False
  _ `gsymCompare` _ = mrgSingle EQ

deriving via (Default AssertionError) instance GEvaluateSym a AssertionError

deriving via (Default AssertionError) instance (Monoid a) => GExtractSymbolics a AssertionError

-- | Verification conditions.
-- A crashed program path can terminate with either assertion violation errors or assumption violation errors.
data VerificationConditions
  = AssertionViolation
  | AssumptionViolation
  deriving (Show, Eq, Ord, Generic, NFData)
  deriving (ToCon VerificationConditions, ToSym VerificationConditions) via (Default VerificationConditions)

deriving via (Default VerificationConditions) instance (SymBoolOp bool) => GMergeable bool VerificationConditions

deriving via (Default VerificationConditions) instance (SymBoolOp bool) => GSEq bool VerificationConditions

instance (SymBoolOp bool) => GSOrd bool VerificationConditions where
  l `gsymle` r = con $ l <= r
  l `gsymlt` r = con $ l < r
  l `gsymge` r = con $ l >= r
  l `gsymgt` r = con $ l > r
  l `gsymCompare` r = mrgSingle $ l `compare` r

deriving via (Default VerificationConditions) instance GEvaluateSym a VerificationConditions

deriving via (Default VerificationConditions) instance (Monoid a) => GExtractSymbolics a VerificationConditions

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
-- Note that we may lose the 'GMergeable' knowledge here if no possible execution
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
  (TransformError AssertionError to, GMergeable bool to, MonadError to erm, SymBoolOp bool, GMonadUnion bool erm) =>
  bool ->
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
  (TransformError VerificationConditions to, GMergeable bool to, MonadError to erm, SymBoolOp bool, GMonadUnion bool erm) =>
  bool ->
  erm ()
symAssume = symAssertTransformableError AssumptionViolation
