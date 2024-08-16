{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      :   Grisette.Internal.Core.Data.Class.Function
-- Copyright   :   (c) Sirui Lu 2021-2023
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Internal.Core.Data.Class.Function
  ( -- * Function operations
    Function (..),
    Apply (..),
  )
where

-- $setup
-- >>> import Grisette.Core
-- >>> import Grisette.SymPrim

-- | Abstraction for function-like types.
class Function f arg ret | f -> arg ret where
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
  (#) :: f -> arg -> ret

  infixl 9 #

instance Function (a -> b) a b where
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

instance Apply Integer where
  type FunType Integer = Integer
  apply = id

instance Apply Bool where
  type FunType Bool = Bool
  apply = id

instance (Apply b) => Apply (a -> b) where
  type FunType (a -> b) = a -> FunType b
  apply f a = apply (f a)
