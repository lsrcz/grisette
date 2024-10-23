{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- Module      :   Grisette.Internal.Core.Data.MemoUtils
-- Copyright   :   (c) Sirui Lu 2021-2023
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Internal.Core.Data.MemoUtils
  ( -- * Memoization
    stableMemo,
    stableMemo2,
    stableMemo3,
    stableMup,
    stableMemoFix,
    weakStableMemo,
    weakStableMemo2,
    weakStableMemo3,
    weakStableMup,
    weakStableMemoFix,
    htmemo,
    htmemo2,
    htmemo3,
    htmemoFix,
    htmup,
  )
where

import Control.Applicative (Const (Const, getConst))
import Control.Monad.Fix (fix)
import Data.Atomics (atomicModifyIORefCAS_)
import qualified Data.HashMap.Strict as HM
import Data.Hashable (Hashable)
import Data.IORef (IORef, newIORef, readIORef)
import Data.Proxy (Proxy (Proxy))
import GHC.Base (Any, Type)
import System.IO.Unsafe (unsafePerformIO)
import System.Mem.StableName (StableName, makeStableName)
import System.Mem.Weak (Weak)
import qualified System.Mem.Weak as Weak
import Unsafe.Coerce (unsafeCoerce)

newtype (f <<< g) a = O {unO :: f (g a)}

-- Invariant: The type parameters for a key and its corresponding
-- value are the same.
type SNMap f g = IORef (HM.HashMap (StableName (f Any)) (g Any))

type MemoTable ref f g = SNMap f (ref <<< g)

class Ref ref where
  mkRef :: a -> b -> IO () -> IO (ref b)
  deRef :: ref a -> IO (Maybe a)
  finalize :: ref a -> IO ()
  tableFinalizer :: MemoTable ref f g -> IO ()
  tableFinalizer t = readIORef t >>= mapM_ (finalize . unO)

instance Ref Weak where
  mkRef x y = Weak.mkWeak x y . Just
  deRef = Weak.deRefWeak
  finalize = Weak.finalize

newtype Strong a = Strong a

instance Ref Strong where
  mkRef _ y _ = return $ Strong y
  deRef (Strong x) = return $ Just x
  finalize (Strong _) = return ()
  tableFinalizer _ = return ()

finalizer :: StableName (f Any) -> Weak (MemoTable ref f g) -> IO ()
finalizer sn weakTbl = do
  r <- Weak.deRefWeak weakTbl
  case r of
    Nothing -> return ()
    Just tbl -> do
      atomicModifyIORefCAS_ tbl $ HM.delete sn

unsafeToAny :: f a -> f Any
unsafeToAny = unsafeCoerce

unsafeFromAny :: f Any -> f a
unsafeFromAny = unsafeCoerce

{-# NOINLINE memo' #-}
memo' ::
  (Ref ref) =>
  Proxy ref ->
  (forall a. f a -> g a) ->
  MemoTable ref f g ->
  Weak (MemoTable ref f g) ->
  f b ->
  g b
memo' _ f tbl weakTbl !x = unsafePerformIO $ do
  sn <- makeStableName $ unsafeToAny x
  lkp <- HM.lookup sn <$> readIORef tbl
  case lkp of
    Nothing -> notFound sn
    Just (O w) -> do
      maybeVal <- deRef w
      case maybeVal of
        Nothing -> notFound sn
        Just val -> do
          return $ unsafeFromAny val
  where
    notFound sn = do
      let !y = f x
      weak <- mkRef x (unsafeToAny y) $ finalizer sn weakTbl
      atomicModifyIORefCAS_ tbl $ HM.insert sn $ O weak
      return y

{-# NOINLINE memo0 #-}
memo0 ::
  (Ref ref) =>
  Proxy (ref :: Type -> Type) ->
  (forall a. f a -> g a) ->
  f b ->
  g b
memo0 p f =
  let (tbl, weak) = unsafePerformIO $ do
        tbl' <- newIORef HM.empty
        weak' <- Weak.mkWeakPtr tbl . Just $ tableFinalizer tbl
        return (tbl', weak')
   in memo' p f tbl weak

-- | Memoize a unary function.
stableMemo :: (a -> b) -> (a -> b)
stableMemo f = getConst . memo0 (Proxy :: Proxy Strong) (Const . f . getConst) . Const

-- | Lift a memoizer to work with one more argument.
stableMup :: (b -> c) -> (a -> b) -> (a -> c)
stableMup mem f = stableMemo (mem . f)

-- | Curried memoization to share partial evaluation
stableMemo2 :: (a -> b -> c) -> (a -> b -> c)
stableMemo2 = stableMup stableMemo

-- | Curried memoization to share partial evaluation
stableMemo3 :: (a -> b -> c -> d) -> (a -> b -> c -> d)
stableMemo3 = stableMup stableMemo2

-- | Memoizing recursion. Use like 'fix'.
stableMemoFix :: ((a -> b) -> (a -> b)) -> a -> b
stableMemoFix h = fix (stableMemo . h)

-- | Memoize a unary function.
weakStableMemo :: (a -> b) -> (a -> b)
weakStableMemo f = getConst . memo0 (Proxy :: Proxy Weak) (Const . f . getConst) . Const

-- | Lift a memoizer to work with one more argument.
weakStableMup :: (b -> c) -> (a -> b) -> (a -> c)
weakStableMup mem f = weakStableMemo (mem . f)

-- | Curried memoization to share partial evaluation
weakStableMemo2 :: (a -> b -> c) -> (a -> b -> c)
weakStableMemo2 = weakStableMup weakStableMemo

-- | Curried memoization to share partial evaluation
weakStableMemo3 :: (a -> b -> c -> d) -> (a -> b -> c -> d)
weakStableMemo3 = weakStableMup weakStableMemo2

-- | Memoizing recursion. Use like 'fix'.
weakStableMemoFix :: ((a -> b) -> (a -> b)) -> a -> b
weakStableMemoFix h = fix (weakStableMemo . h)

-- | Function memoizer with mutable hash table.
{-# NOINLINE htmemo #-}
htmemo :: (Eq k, Hashable k) => (k -> a) -> k -> a
htmemo f = unsafePerformIO $ do
  cache <- newIORef HM.empty
  -- cache <- H.new :: IO (HashTable k v)
  return $ \(!x) -> unsafePerformIO $ do
    tryV <- HM.lookup x <$> readIORef cache
    case tryV of
      Nothing -> do
        let !v = f x
        atomicModifyIORefCAS_ cache $ \old -> HM.insert x v old
        return v
      Just v -> return v

-- | Lift a memoizer to work with one more argument.
htmup :: (Eq k, Hashable k) => (b -> c) -> (k -> b) -> (k -> c)
htmup mem f = htmemo (mem . f)

-- | Function memoizer with mutable hash table. Works on binary functions.
htmemo2 :: (Eq k1, Hashable k1, Eq k2, Hashable k2) => (k1 -> k2 -> a) -> (k1 -> k2 -> a)
htmemo2 = htmup htmemo

-- | Function memoizer with mutable hash table. Works on ternary functions.
htmemo3 ::
  (Eq k1, Hashable k1, Eq k2, Hashable k2, Eq k3, Hashable k3) =>
  (k1 -> k2 -> k3 -> a) ->
  (k1 -> k2 -> k3 -> a)
htmemo3 = htmup htmemo2

-- | Memoizing recursion. Use like 'fix'.
htmemoFix :: (Eq k, Hashable k) => ((k -> a) -> (k -> a)) -> k -> a
htmemoFix h = fix (htmemo . h)
