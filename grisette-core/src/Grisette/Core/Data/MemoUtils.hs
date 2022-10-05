{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Grisette.Core.Data.MemoUtils
  ( htmemo,
    htmemo2,
    htmemo3,
    htmup,
    htmemoFix,
  )
where

import Data.Function (fix)
import Data.HashTable.IO as H
import Data.Hashable
import System.IO.Unsafe

type HashTable k v = H.BasicHashTable k v

-- | Function memoizer with mutable hash table.
htmemo :: (Eq k, Hashable k) => (k -> a) -> k -> a
htmemo f = unsafePerformIO $ do
  cache <- H.new :: IO (HashTable k v)
  return $ \x -> unsafePerformIO $ do
    tryV <- H.lookup cache x
    case tryV of
      Nothing -> do
        -- traceM "New value"
        let v = f x
        H.insert cache x v
        return v
      Just v -> return v

-- | Lift a memoizer to work with one more argument.
htmup :: (Eq k, Hashable k) => (b -> c) -> (k -> b) -> (k -> c)
htmup mem f = htmemo (mem . f)

-- | Function memoizer with mutable hash table. Works on binary functions.
htmemo2 :: (Eq k1, Hashable k1, Eq k2, Hashable k2) => (k1 -> k2 -> a) -> (k1 -> k2 -> a)
htmemo2 = htmup htmemo

-- | Function memoizer with mutable hash table. Works on ternery functions.
htmemo3 ::
  (Eq k1, Hashable k1, Eq k2, Hashable k2, Eq k3, Hashable k3) =>
  (k1 -> k2 -> k3 -> a) ->
  (k1 -> k2 -> k3 -> a)
htmemo3 = htmup htmemo2

-- | Memoizing recursion. Use like 'fix'.
htmemoFix :: (Eq k, Hashable k) => ((k -> a) -> (k -> a)) -> k -> a
htmemoFix h = fix (htmemo . h)
