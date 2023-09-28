{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Trustworthy #-}

module Grisette.Core.Data.Class.Mergeable
  ( MergingStrategy (..),
    Mergeable' (..),
    Mergeable (..),
  )
where

import Data.Typeable (Typeable)
import {-# SOURCE #-} Grisette.IR.SymPrim.Data.SymPrim (SymBool)

data MergingStrategy a where
  SimpleStrategy :: (SymBool -> a -> a -> a) -> MergingStrategy a
  SortedStrategy ::
    (Ord idx, Typeable idx, Show idx) =>
    (a -> idx) ->
    (idx -> MergingStrategy a) ->
    MergingStrategy a
  NoStrategy :: MergingStrategy a

class Mergeable' f where
  rootStrategy' :: MergingStrategy (f a)

class Mergeable a where
  rootStrategy :: MergingStrategy a
