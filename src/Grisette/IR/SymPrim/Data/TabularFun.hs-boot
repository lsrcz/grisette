{-# LANGUAGE TypeOperators #-}

module Grisette.IR.SymPrim.Data.TabularFun
  ( type (=->) (..),
  )
where

data (=->) a b = TabularFun {funcTable :: [(a, b)], defaultFuncValue :: b}
