{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Grisette.Core.Data.Class.Mergeable where

import Data.Typeable

data MergingStrategy bool a where
  SimpleStrategy :: (bool -> a -> a -> a) -> MergingStrategy bool a
  SortedStrategy ::
    (Ord idx, Typeable idx, Show idx) =>
    (a -> idx) ->
    (idx -> MergingStrategy bool a) ->
    MergingStrategy bool a
  NoStrategy :: MergingStrategy bool a

class Mergeable' bool f where
  mergingStrategy' :: MergingStrategy bool (f a)

class Mergeable bool a where
  mergingStrategy :: MergingStrategy bool a
