{-# LANGUAGE TypeFamilies #-}

module Grisette.Core.Data.Class.Function
  ( Function (..),
  )
where

-- | Abstraction for function-like types.
class Function f where
  -- | Argument type
  type Arg f

  -- | Return type
  type Ret f

  -- | Function application operator
  (#) :: f -> Arg f -> Ret f

  infixl 9 #

instance Function (a -> b) where
  type Arg (a -> b) = a
  type Ret (a -> b) = b
  f # a = f a
