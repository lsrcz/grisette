{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      :   Grisette.Unified.Internal.UnifiedFP
-- Copyright   :   (c) Sirui Lu 2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Unified.Internal.UnifiedFP
  ( GetFP,
    GetFPRoundingMode,
    UnifiedFP,
    SafeUnifiedFP,
    AllUnifiedFP,
    UnifiedFPImpl,
  )
where

import Control.Monad.Error.Class (MonadError)
import Data.Kind (Type)
import GHC.TypeNats (Nat)
import Grisette.Internal.Core.Data.Class.IEEEFP
  ( IEEEFPConstants,
    IEEEFPConvertible,
    IEEEFPOp,
    IEEEFPRoundingOp,
    IEEEFPToAlgReal,
  )
import Grisette.Internal.Core.Data.Class.SymIEEEFP (SymIEEEFPTraits)
import Grisette.Internal.SymPrim.FP (FP, FPRoundingMode, NotRepresentableFPError, ValidFP)
import Grisette.Internal.SymPrim.SymFP (SymFP, SymFPRoundingMode)
import Grisette.Internal.SymPrim.SymPrim (Prim)
import Grisette.Unified.Internal.BaseConstraint (ConSymConversion)
import Grisette.Unified.Internal.Class.UnifiedFromIntegral (UnifiedFromIntegral)
import Grisette.Unified.Internal.Class.UnifiedSafeFromFP (UnifiedSafeFromFP)
import Grisette.Unified.Internal.Class.UnifiedSimpleMergeable (UnifiedBranching)
import Grisette.Unified.Internal.EvalModeTag (EvalModeTag (Con, Sym))
import Grisette.Unified.Internal.UnifiedAlgReal (GetAlgReal)
import Grisette.Unified.Internal.UnifiedConstraint (UnifiedPrimitive)
import Grisette.Unified.Internal.UnifiedInteger (GetInteger)

-- | Implementation for 'UnifiedFP'.
class
  ( Prim fp,
    ConSymConversion (FP eb sb) (SymFP eb sb) fp,
    UnifiedPrimitive mode fp,
    Floating fp,
    SymIEEEFPTraits fp,
    IEEEFPConstants fp,
    IEEEFPOp fp,
    IEEEFPRoundingOp fp rd,
    UnifiedFromIntegral mode (GetInteger mode) fp,
    IEEEFPToAlgReal (GetAlgReal mode) fp rd,
    IEEEFPConvertible (GetInteger mode) fp rd,
    fpn ~ GetFP mode,
    fp ~ fpn eb sb,
    rd ~ GetFPRoundingMode mode
  ) =>
  UnifiedFPImpl (mode :: EvalModeTag) fpn eb sb fp rd
    | fpn eb sb -> fp rd,
      fp -> fpn eb sb rd,
      rd -> fpn,
      rd eb sb -> fp
  where
  -- | Get a unified floating point type. Resolves to 'FP' in 'Con' mode, and
  -- 'SymFP' in 'Sym' mode.
  type GetFP mode = (f :: Nat -> Nat -> Type) | f -> mode

  -- | Get a unified floating point rounding mode type. Resolves to
  -- 'FPRoundingMode' in 'Con' mode, and 'SymFPRoundingMode' in 'Sym' mode.
  type GetFPRoundingMode mode = r | r -> mode

instance
  (ValidFP eb sb) =>
  UnifiedFPImpl 'Con FP eb sb (FP eb sb) FPRoundingMode
  where
  type GetFP 'Con = FP
  type GetFPRoundingMode 'Con = FPRoundingMode

instance
  (ValidFP eb sb) =>
  UnifiedFPImpl 'Sym SymFP eb sb (SymFP eb sb) SymFPRoundingMode
  where
  type GetFP 'Sym = SymFP
  type GetFPRoundingMode 'Sym = SymFPRoundingMode

-- | Evaluation mode with unified 'FP' type.
class
  ( UnifiedFPImpl
      mode
      (GetFP mode)
      eb
      sb
      (GetFP mode eb sb)
      (GetFPRoundingMode mode)
  ) =>
  UnifiedFP mode eb sb

instance
  ( UnifiedFPImpl
      mode
      (GetFP mode)
      eb
      sb
      (GetFP mode eb sb)
      (GetFPRoundingMode mode)
  ) =>
  UnifiedFP mode eb sb

class
  (UnifiedFPImpl mode fpn eb sb fp rd) =>
  SafeUnifiedFPImpl mode fpn eb sb fp rd (m :: Type -> Type)

instance
  (UnifiedFPImpl mode fpn eb sb fp rd) =>
  SafeUnifiedFPImpl mode fpn eb sb fp rd m

-- | This class is needed as constraint in user code prior to GHC 9.2.1.
-- See the notes in 'Grisette.Unified.Internal.EvalMode.EvalMode'.
class
  ( SafeUnifiedFPImpl
      mode
      (GetFP mode)
      eb
      sb
      (GetFP mode eb sb)
      (GetFPRoundingMode mode)
      m,
    UnifiedSafeFromFP
      mode
      NotRepresentableFPError
      (GetInteger mode)
      (GetFP mode eb sb)
      (GetFPRoundingMode mode)
      m
  ) =>
  SafeUnifiedFP mode eb sb m

instance
  ( SafeUnifiedFPImpl
      mode
      (GetFP mode)
      eb
      sb
      (GetFP mode eb sb)
      (GetFPRoundingMode mode)
      m,
    UnifiedSafeFromFP
      mode
      NotRepresentableFPError
      (GetInteger mode)
      (GetFP mode eb sb)
      (GetFPRoundingMode mode)
      m
  ) =>
  SafeUnifiedFP mode eb sb m

-- | Evaluation mode with unified floating point type.
class
  ( forall eb sb. (ValidFP eb sb) => UnifiedFP mode eb sb,
    forall eb sb m.
    ( ValidFP eb sb,
      UnifiedBranching mode m,
      MonadError NotRepresentableFPError m
    ) =>
    SafeUnifiedFP mode eb sb m
  ) =>
  AllUnifiedFP mode

instance
  ( forall eb sb. (ValidFP eb sb) => UnifiedFP mode eb sb,
    forall eb sb m.
    ( ValidFP eb sb,
      UnifiedBranching mode m,
      MonadError NotRepresentableFPError m
    ) =>
    SafeUnifiedFP mode eb sb m
  ) =>
  AllUnifiedFP mode
