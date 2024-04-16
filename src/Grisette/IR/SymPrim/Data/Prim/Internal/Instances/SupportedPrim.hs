{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      :   Grisette.IR.SymPrim.Data.Prim.Internal.Instances.SupportedPrim
-- Copyright   :   (c) Sirui Lu 2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.IR.SymPrim.Data.Prim.Internal.Instances.SupportedPrim
  (
  )
where

import GHC.TypeNats (KnownNat, type (<=))
import Grisette.Core.Data.BV (IntN, WordN)
import Grisette.IR.SymPrim.Data.Prim.Internal.Term
  ( SupportedPrim
      ( PrimConstraint,
        defaultValue,
        defaultValueDynamic,
        pevalITETerm,
        pformatCon
      ),
    pevalITEBasicTerm,
  )
import Grisette.IR.SymPrim.Data.Prim.ModelValue (ModelValue, toModelValue)

defaultValueForInteger :: Integer
defaultValueForInteger = 0

defaultValueForIntegerDyn :: ModelValue
defaultValueForIntegerDyn = toModelValue defaultValueForInteger

-- Basic Integer
instance SupportedPrim Integer where
  pformatCon = show
  defaultValue = defaultValueForInteger
  defaultValueDynamic _ = defaultValueForIntegerDyn
  pevalITETerm = pevalITEBasicTerm

-- Signed BV
instance (KnownNat w, 1 <= w) => SupportedPrim (IntN w) where
  type PrimConstraint (IntN w) = (KnownNat w, 1 <= w)
  pformatCon = show
  defaultValue = 0
  pevalITETerm = pevalITEBasicTerm

-- Unsigned BV
instance (KnownNat w, 1 <= w) => SupportedPrim (WordN w) where
  type PrimConstraint (WordN w) = (KnownNat w, 1 <= w)
  pformatCon = show
  defaultValue = 0
  pevalITETerm = pevalITEBasicTerm
