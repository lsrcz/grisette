{-# LANGUAGE TypeOperators #-}

module Pizza.IR.SymPrim.Data.TabularFunc
  ( type (=->) (..),
  )
where

data (=->) a b = TabularFunc {funcTable :: [(a, b)], defaultFuncValue :: b}
