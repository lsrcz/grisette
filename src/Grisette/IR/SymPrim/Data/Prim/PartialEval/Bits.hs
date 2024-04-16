{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- Module      :   Grisette.IR.SymPrim.Data.Prim.PartialEval.Bits
-- Copyright   :   (c) Sirui Lu 2021-2023
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.IR.SymPrim.Data.Prim.PartialEval.Bits
  ( pattern BitsConTerm,
  )
where

import Data.Bits (Bits)
import Data.Typeable (Typeable, cast)
import Grisette.IR.SymPrim.Data.Prim.Internal.Term
  ( Term (ConTerm),
  )

bitsConTermView :: (Bits b, Typeable b) => Term a -> Maybe b
bitsConTermView (ConTerm _ b) = cast b
bitsConTermView _ = Nothing

pattern BitsConTerm :: forall b a. (Bits b, Typeable b) => b -> Term a
pattern BitsConTerm b <- (bitsConTermView -> Just b)
