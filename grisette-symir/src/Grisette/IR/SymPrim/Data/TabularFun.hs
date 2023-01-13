{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- Module      :   Grisette.IR.SymPrim.Data.TabularFun
-- Copyright   :   (c) Sirui Lu 2021-2022
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.IR.SymPrim.Data.TabularFun
  ( type (=->) (..),
  )
where

import Control.DeepSeq
import Data.Hashable
import GHC.Generics
import Grisette.Core.Data.Class.Function
import Grisette.IR.SymPrim.Data.Prim.InternedTerm.Term
import Language.Haskell.TH.Syntax

-- $setup
-- >>> import Grisette.Core
-- >>> import Grisette.IR.SymPrim

-- |
-- Functions as a table. Use the `#` operator to apply the function.
--
-- >>> :set -XTypeOperators
-- >>> let f = TabularFun [(1, 2), (3, 4)] 0 :: Int =-> Int
-- >>> f # 1
-- 2
-- >>> f # 2
-- 0
-- >>> f # 3
-- 4
data (=->) a b = TabularFun {funcTable :: [(a, b)], defaultFuncValue :: b}
  deriving (Show, Eq, Generic, Generic1, Lift, NFData, NFData1)

infixr 0 =->

instance
  (SupportedPrim a, SupportedPrim b) =>
  SupportedPrim (a =-> b)
  where
  type PrimConstraint (a =-> b) = (SupportedPrim a, SupportedPrim b)
  defaultValue = TabularFun [] (defaultValue @b)

instance (Eq a) => Function (a =-> b) where
  type Arg (a =-> b) = a
  type Ret (a =-> b) = b
  (TabularFun table d) # a = go table
    where
      go [] = d
      go ((av, bv) : s)
        | a == av = bv
        | otherwise = go s

instance (Hashable a, Hashable b) => Hashable (a =-> b)
