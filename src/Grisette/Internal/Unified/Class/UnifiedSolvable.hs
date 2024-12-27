{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- Module      :   Grisette.Internal.Unified.Class.UnifiedSolvable
-- Copyright   :   (c) Sirui Lu 2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Internal.Unified.Class.UnifiedSolvable
  ( UnifiedSolvable (withBaseSolvable),
    con,
    pattern Con,
    conView,
  )
where

import Data.Type.Bool (If)
import GHC.TypeLits (KnownNat, type (<=))
import Grisette.Internal.Core.Data.Class.Solvable (Solvable)
import qualified Grisette.Internal.Core.Data.Class.Solvable as Grisette
import Grisette.Internal.SymPrim.AlgReal (AlgReal)
import Grisette.Internal.SymPrim.BV (IntN, WordN)
import Grisette.Internal.SymPrim.FP (FP, ValidFP)
import Grisette.Internal.SymPrim.SymAlgReal (SymAlgReal)
import Grisette.Internal.SymPrim.SymBV (SymIntN, SymWordN)
import Grisette.Internal.SymPrim.SymBool (SymBool)
import Grisette.Internal.SymPrim.SymFP (SymFP)
import Grisette.Internal.SymPrim.SymInteger (SymInteger)
import Grisette.Internal.Unified.EvalModeTag (EvalModeTag (C, S), IsConMode)
import Grisette.Internal.Unified.Util (DecideEvalMode, withMode)

-- $setup
-- >>> import Grisette.Core (ssym)

-- | Wrap a concrete value in a symbolic value.
--
-- >>> con True :: Bool
-- True
--
-- >>> con True :: SymBool
-- true
con ::
  forall mode a con. (DecideEvalMode mode, UnifiedSolvable mode a con) => con -> a
con v =
  withMode @mode
    (withBaseSolvable @mode @a @con v)
    (withBaseSolvable @mode @a @con $ Grisette.con v)

-- | Extract the concrete value from a symbolic value.
--
-- >>> conView (con True :: SymBool)
-- Just True
--
-- >>> conView (ssym "a" :: SymBool)
-- Nothing
--
-- >>> conView True
-- Just True
conView ::
  forall mode a con.
  (DecideEvalMode mode, UnifiedSolvable mode a con) =>
  a ->
  Maybe con
conView v =
  withMode @mode
    (withBaseSolvable @mode @a @con $ Just v)
    (withBaseSolvable @mode @a @con $ Grisette.conView v)

-- | A pattern synonym for extracting the concrete value from a symbolic value.
--
-- >>> case con True :: SymBool of Con v -> v
-- True
--
-- >>> case ssym "a" :: SymBool of Con v -> Just v; _ -> Nothing
-- Nothing
pattern Con :: (DecideEvalMode mode, UnifiedSolvable mode a con) => con -> a
pattern Con v <-
  (conView -> Just v)
  where
    Con v = con v

-- | A class that provides the ability to extract/wrap the concrete value
-- from/into a symbolic value.
class UnifiedSolvable mode a con | a -> mode con, con mode -> a where
  withBaseSolvable ::
    ((If (IsConMode mode) (a ~ con) (Solvable con a)) => r) -> r

instance UnifiedSolvable 'C Bool Bool where
  withBaseSolvable r = r

instance UnifiedSolvable 'S SymBool Bool where
  withBaseSolvable r = r

instance UnifiedSolvable 'C Integer Integer where
  withBaseSolvable r = r

instance UnifiedSolvable 'S SymInteger Integer where
  withBaseSolvable r = r

instance UnifiedSolvable 'C AlgReal AlgReal where
  withBaseSolvable r = r

instance UnifiedSolvable 'S SymAlgReal AlgReal where
  withBaseSolvable r = r

instance (KnownNat n, 1 <= n) => UnifiedSolvable 'C (WordN n) (WordN n) where
  withBaseSolvable r = r

instance (KnownNat n, 1 <= n) => UnifiedSolvable 'S (SymWordN n) (WordN n) where
  withBaseSolvable r = r

instance (KnownNat n, 1 <= n) => UnifiedSolvable 'C (IntN n) (IntN n) where
  withBaseSolvable r = r

instance (KnownNat n, 1 <= n) => UnifiedSolvable 'S (SymIntN n) (IntN n) where
  withBaseSolvable r = r

instance (ValidFP eb sb) => UnifiedSolvable 'C (FP eb sb) (FP eb sb) where
  withBaseSolvable r = r

instance (ValidFP eb sb) => UnifiedSolvable 'S (SymFP eb sb) (FP eb sb) where
  withBaseSolvable r = r
