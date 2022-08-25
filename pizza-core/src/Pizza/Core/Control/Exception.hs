{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}

module Pizza.Core.Control.Exception
  ( AssertionError (..),
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
import Pizza.Core.Control.Monad.Union
import Pizza.Core.Data.Class.Bool
import Pizza.Core.Data.Class.Error
import Pizza.Core.Data.Class.Evaluate
import Pizza.Core.Data.Class.ExtractSymbolics
import Pizza.Core.Data.Class.Mergeable
import Pizza.Core.Data.Class.PrimWrapper
import Pizza.Core.Data.Class.SOrd
import Pizza.Core.Data.Class.SimpleMergeable
import Pizza.Core.Data.Class.ToCon
import Pizza.Core.Data.Class.ToSym

-- $setup
-- >>> import Pizza.Core
-- >>> import Pizza.Lib.Base
-- >>> import Pizza.IR.SymPrim
-- >>> import Control.Monad.Trans.Except

-- | Assertion error.
data AssertionError = AssertionError
  deriving (Show, Eq, Ord, Generic, NFData)
  deriving (ToCon AssertionError, ToSym AssertionError) via (Default AssertionError)

deriving via (Default AssertionError) instance (SymBoolOp bool) => Mergeable bool AssertionError

deriving via (Default AssertionError) instance (SymBoolOp bool) => SimpleMergeable bool AssertionError

deriving via (Default AssertionError) instance (SymBoolOp bool) => SEq bool AssertionError

instance (SymBoolOp bool) => SOrd bool AssertionError where
  _ <=~ _ = conc True
  _ <~ _ = conc False
  _ >=~ _ = conc True
  _ >~ _ = conc False
  _ `symCompare` _ = mrgSingle EQ

deriving via (Default AssertionError) instance EvaluateSym a AssertionError

deriving via (Default AssertionError) instance (Monoid a) => ExtractSymbolics a AssertionError

-- | Verification conditions.
-- A crashed program path can terminate with either assertion violation errors or assumption violation errors.
data VerificationConditions
  = AssertionViolation
  | AssumptionViolation
  deriving (Show, Eq, Ord, Generic, NFData)
  deriving (ToCon VerificationConditions, ToSym VerificationConditions) via (Default VerificationConditions)

deriving via (Default VerificationConditions) instance (SymBoolOp bool) => Mergeable bool VerificationConditions

deriving via (Default VerificationConditions) instance (SymBoolOp bool) => SEq bool VerificationConditions

instance (SymBoolOp bool) => SOrd bool VerificationConditions where
  l <=~ r = conc $ l <= r
  l <~ r = conc $ l < r
  l >=~ r = conc $ l >= r
  l >~ r = conc $ l > r
  l `symCompare` r = mrgSingle $ l `compare` r

deriving via (Default VerificationConditions) instance EvaluateSym a VerificationConditions

deriving via (Default VerificationConditions) instance (Monoid a) => ExtractSymbolics a VerificationConditions

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
-- If the condition is symbolic, Pizza will split the execution into two paths based on the condition.
-- The symbolic execution will continue on the then-branch, where the condition is true.
-- For the else branch, where the condition is false, the execution will be terminated.
--
-- /Examples/:
--
-- Terminates the execution if the condition is false.
-- Note that we may lose the 'Mergeable' knowledge here if no possible execution path is viable.
-- This may affect the efficiency in theory, but in practice this should not be a problem
-- because Pizza will not try to further execute the terminated paths.
--
-- >>> symAssert (conc False) :: ExceptT AssertionError UnionM ()
-- ExceptT (UMrg (Single (Left AssertionError)))
-- >>> do; symAssert (conc False); mrgReturn 1 :: ExceptT AssertionError UnionM Integer
-- ExceptT (UAny (Single (Left AssertionError)))
--
-- No effect if the condition is true:
--
-- >>> symAssert (conc True) :: ExceptT AssertionError UnionM ()
-- ExceptT (UMrg (Single (Right ())))
-- >>> do; symAssert (conc True); mrgReturn 1 :: ExceptT AssertionError UnionM Integer
-- ExceptT (UMrg (Single (Right 1)))
--
-- Splitting the path and terminate one of them.
--
-- >>> symAssert (ssymb "a") :: ExceptT AssertionError UnionM ()
-- ExceptT (UMrg (If (! a) (Single (Left AssertionError)) (Single (Right ()))))
-- >>> do; symAssert (ssymb "a"); mrgReturn 1 :: ExceptT AssertionError UnionM Integer
-- ExceptT (UMrg (If (! a) (Single (Left AssertionError)) (Single (Right 1))))
--
-- 'AssertionError' is compatible with 'VerificationConditions':
--
-- >>> symAssert (ssymb "a") :: ExceptT VerificationConditions UnionM ()
-- ExceptT (UMrg (If (! a) (Single (Left AssertionViolation)) (Single (Right ()))))
symAssert ::
  (TransformError AssertionError to, Mergeable bool to, MonadError to erm, SymBoolOp bool, MonadUnion bool erm) =>
  bool ->
  erm ()
symAssert = symFailIfNot AssertionError

-- | Used within a monadic multi path computation to begin exception processing.
--
-- Similar to 'gassert', but terminates the execution path with 'AssumptionViolation' error.
--
-- /Examples/:
--
-- >>> symAssume (ssymb "a") :: ExceptT VerificationConditions UnionM ()
-- ExceptT (UMrg (If (! a) (Single (Left AssumptionViolation)) (Single (Right ()))))
symAssume ::
  (TransformError VerificationConditions to, Mergeable bool to, MonadError to erm, SymBoolOp bool, MonadUnion bool erm) =>
  bool ->
  erm ()
symAssume = symFailIfNot AssumptionViolation
