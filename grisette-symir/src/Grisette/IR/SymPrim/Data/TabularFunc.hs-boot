{-# LANGUAGE TypeOperators #-}

module Grisette.IR.SymPrim.Data.TabularFunc
  ( type (=->) (..),
  )
where

data (=->) a b = TabularFunc {funcTable :: [(a, b)], defaultFuncValue :: b}
