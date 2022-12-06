{-# LANGUAGE TypeFamilies #-}

module Grisette.Core.Data.Class.Function
  ( -- * Note for the examples

    --

    -- | This module does not contain actual implementation for symbolic primitive types, and
    -- the examples in this module cannot be executed solely with @grisette-core@ package.
    -- They rely on the implementation in @grisette-symir@ package.

    -- * Function operations
    Function (..),
  )
where

-- | Abstraction for function-like types.
class Function f where
  -- | Argument type
  type Arg f

  -- | Return type
  type Ret f

  -- | Function application operator.
  --
  -- The operator is not right associated (like `($)`). It is left associated,
  -- and you can provide many arguments with this operator once at a time.
  --
  -- >>> (+1) # 2
  -- 3
  --
  -- >>> (+) # 2 # 3
  -- 5
  (#) :: f -> Arg f -> Ret f

  infixl 9 #

instance Function (a -> b) where
  type Arg (a -> b) = a
  type Ret (a -> b) = b
  f # a = f a
