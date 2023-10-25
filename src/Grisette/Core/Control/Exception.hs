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
-- Copyright   :   (c) Sirui Lu 2021-2023
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Core.Control.Exception
  ( -- * Predefined exceptions
    AssertionError (..),
    VerificationConditions (..),
    symAssert,
    symAssume,
  )
where

import Control.DeepSeq (NFData)
import Control.Exception (ArithException, ArrayException)
import Control.Monad.Except (MonadError)
import GHC.Generics (Generic)
import Generics.Deriving (Default (Default))
import Grisette.Core.Control.Monad.Union (MonadUnion)
import Grisette.Core.Data.Class.Error
  ( TransformError (transformError),
    symAssertTransformableError,
  )
import Grisette.Core.Data.Class.EvaluateSym (EvaluateSym)
import Grisette.Core.Data.Class.ExtractSymbolics
  ( ExtractSymbolics,
  )
import Grisette.Core.Data.Class.Mergeable (Mergeable)
import Grisette.Core.Data.Class.SEq (SEq)
import Grisette.Core.Data.Class.SOrd
  ( SOrd (symCompare, (<=~), (<~), (>=~), (>~)),
  )
import Grisette.Core.Data.Class.SimpleMergeable
  ( SimpleMergeable,
    mrgSingle,
  )
import Grisette.Core.Data.Class.Solvable (Solvable (con))
import Grisette.Core.Data.Class.ToCon (ToCon)
import Grisette.Core.Data.Class.ToSym (ToSym)
import {-# SOURCE #-} Grisette.IR.SymPrim.Data.SymPrim (SymBool)

-- $setup
-- >>> import Grisette.Core
-- >>> import Grisette.Lib.Base
-- >>> import Grisette.IR.SymPrim
-- >>> import Control.Monad.Trans.Except

-- | Assertion error.
data AssertionError = AssertionError
  deriving (Show, Eq, Ord, Generic, NFData)
  deriving (ToCon AssertionError, ToSym AssertionError) via (Default AssertionError)

deriving via (Default AssertionError) instance Mergeable AssertionError

deriving via (Default AssertionError) instance SimpleMergeable AssertionError

deriving via (Default AssertionError) instance SEq AssertionError

instance SOrd AssertionError where
  _ <=~ _ = con True
  _ <~ _ = con False
  _ >=~ _ = con True
  _ >~ _ = con False
  _ `symCompare` _ = mrgSingle EQ

deriving via (Default AssertionError) instance EvaluateSym AssertionError

deriving via (Default AssertionError) instance ExtractSymbolics AssertionError

-- | Verification conditions.
-- A crashed program path can terminate with either assertion violation errors or assumption violation errors.
data VerificationConditions
  = AssertionViolation
  | AssumptionViolation
  deriving (Show, Eq, Ord, Generic, NFData)
  deriving (ToCon VerificationConditions, ToSym VerificationConditions) via (Default VerificationConditions)

deriving via (Default VerificationConditions) instance Mergeable VerificationConditions

deriving via (Default VerificationConditions) instance SEq VerificationConditions

instance SOrd VerificationConditions where
  l >=~ r = con $ l >= r
  l >~ r = con $ l > r
  l <=~ r = con $ l <= r
  l <~ r = con $ l < r
  l `symCompare` r = mrgSingle $ l `compare` r

deriving via (Default VerificationConditions) instance EvaluateSym VerificationConditions

deriving via (Default VerificationConditions) instance ExtractSymbolics VerificationConditions

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
