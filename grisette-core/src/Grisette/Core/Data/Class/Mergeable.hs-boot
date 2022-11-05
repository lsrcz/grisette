{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Grisette.Core.Data.Class.Mergeable where

import Data.Typeable

data GMergingStrategy bool a where
  SimpleStrategy :: (bool -> a -> a -> a) -> GMergingStrategy bool a
  SortedStrategy ::
    (Ord idx, Typeable idx, Show idx) =>
    (a -> idx) ->
    (idx -> GMergingStrategy bool a) ->
    GMergingStrategy bool a
  NoStrategy :: GMergingStrategy bool a

class GMergeable' bool f where
  gmergingStrategy' :: GMergingStrategy bool (f a)

class GMergeable bool a where
  gmergingStrategy :: GMergingStrategy bool a
