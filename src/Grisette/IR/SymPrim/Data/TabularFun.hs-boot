{-# LANGUAGE TypeOperators #-}

module Grisette.IR.SymPrim.Data.TabularFun
  ( type (=->) (..),
  )
where

import Control.DeepSeq (NFData)
import Data.Hashable (Hashable)
import Language.Haskell.TH.Syntax (Lift)

data (=->) a b = TabularFun {funcTable :: [(a, b)], defaultFuncValue :: b}

instance (Eq a, Eq b) => Eq (a =-> b)

instance (Show a, Show b) => Show (a =-> b)

instance (Hashable a, Hashable b) => Hashable (a =-> b)

instance (Lift a, Lift b) => Lift (a =-> b)

instance (NFData a, NFData b) => NFData (a =-> b)
