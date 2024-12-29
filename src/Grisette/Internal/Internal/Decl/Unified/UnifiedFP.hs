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
-- Module      :   Grisette.Internal.Internal.Decl.Unified.UnifiedFP
-- Copyright   :   (c) Sirui Lu 2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Internal.Internal.Decl.Unified.UnifiedFP
  ( UnifiedFP,
    SafeUnifiedFP,
    AllUnifiedFP,
    UnifiedFPImpl (GetFP, GetFPRoundingMode),
    SafeUnifiedFPImpl,
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
import Grisette.Internal.SymPrim.FP (FP, NotRepresentableFPError, ValidFP)
import Grisette.Internal.SymPrim.SymFP (SymFP)
import Grisette.Internal.Unified.BaseConstraint (ConSymConversion)
import Grisette.Internal.Unified.Class.UnifiedFromIntegral (UnifiedFromIntegral)
import Grisette.Internal.Unified.Class.UnifiedRep
  ( UnifiedConRep (ConType),
    UnifiedSymRep (SymType),
  )
import Grisette.Internal.Unified.Class.UnifiedSafeFromFP (UnifiedSafeFromFP)
import Grisette.Internal.Unified.Class.UnifiedSimpleMergeable (UnifiedBranching)
import Grisette.Internal.Unified.EvalModeTag (EvalModeTag)
import Grisette.Internal.Unified.UnifiedAlgReal (GetAlgReal)
import Grisette.Internal.Unified.UnifiedInteger (GetInteger)
import Grisette.Internal.Unified.UnifiedPrim (UnifiedBasicPrim)

-- | Implementation for 'UnifiedFP'.
class
  ( UnifiedConRep fp,
    UnifiedSymRep fp,
    ConType fp ~ FP eb sb,
    SymType fp ~ SymFP eb sb,
    UnifiedBasicPrim mode fp,
    Floating fp,
    SymIEEEFPTraits fp,
    IEEEFPConstants fp,
    IEEEFPOp fp,
    IEEEFPRoundingOp fp rd,
    UnifiedFromIntegral mode (GetInteger mode) fp,
    IEEEFPToAlgReal (GetAlgReal mode) fp rd,
    IEEEFPConvertible (GetInteger mode) fp rd,
    ConSymConversion (FP eb sb) (SymFP eb sb) fp,
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
  -- | Get a unified floating point type. Resolves to 'FP' in
  -- 'Grisette.Unified.C' mode, and 'SymFP' in 'Grisette.Unified.S' mode.
  type GetFP mode = (f :: Nat -> Nat -> Type) | f -> mode

  -- | Get a unified floating point rounding mode type. Resolves to
  -- 'Grisette.FPRoundingMode' in 'Grisette.Unified.C' mode, and
  -- 'Grisette.SymFPRoundingMode' in 'Grisette.Unified.S' mode.
  type GetFPRoundingMode mode = r | r -> mode

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

-- | Implementation for 'SafeUnifiedFP'.
class
  (UnifiedFPImpl mode fpn eb sb fp rd) =>
  SafeUnifiedFPImpl mode fpn eb sb fp rd (m :: Type -> Type)

-- | This class is needed as constraint in user code prior to GHC 9.2.1.
-- See the notes in 'Grisette.Internal.Unified.EvalMode.EvalMode'.
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
