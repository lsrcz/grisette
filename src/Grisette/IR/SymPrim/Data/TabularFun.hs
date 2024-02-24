{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- Module      :   Grisette.IR.SymPrim.Data.TabularFun
-- Copyright   :   (c) Sirui Lu 2021-2023
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.IR.SymPrim.Data.TabularFun
  ( type (=->) (..),
  )
where

import Control.DeepSeq (NFData, NFData1)
import Data.Hashable (Hashable)
import GHC.Generics (Generic, Generic1)
import Grisette.Core.Data.Class.Function (Function ((#)))
import Language.Haskell.TH.Syntax (Lift)

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

instance (Eq a) => Function (a =-> b) a b where
  (TabularFun table d) # a = go table
    where
      go [] = d
      go ((av, bv) : s)
        | a == av = bv
        | otherwise = go s

instance (Hashable a, Hashable b) => Hashable (a =-> b)
