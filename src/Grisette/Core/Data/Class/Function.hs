{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      :   Grisette.Core.Data.Class.Function
-- Copyright   :   (c) Sirui Lu 2021-2023
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Core.Data.Class.Function
  ( -- * Function operations
    Function (..),
    Apply (..),
  )
where

-- $setup
-- >>> import Grisette.Core
-- >>> import Grisette.IR.SymPrim
-- >>> :set -XDataKinds
-- >>> :set -XBinaryLiterals
-- >>> :set -XFlexibleContexts
-- >>> :set -XFlexibleInstances
-- >>> :set -XFunctionalDependencies
-- >>> :set -XOverloadedStrings
-- >>> :set -XTypeOperators

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

-- | Applying an uninterpreted function.
--
-- >>> let f = "f" :: SymInteger =~> SymInteger =~> SymInteger
-- >>> apply f "a" "b"
-- (apply (apply f a) b)
--
-- Note that for implementation reasons, you can also use `apply` function on
-- a non-function symbolic value. In this case, the function is treated as an
-- `id` function.
class Apply uf where
  type FunType uf
  apply :: uf -> FunType uf

instance (Apply b) => Apply (a -> b) where
  type FunType (a -> b) = a -> FunType b
  apply f a = apply (f a)
