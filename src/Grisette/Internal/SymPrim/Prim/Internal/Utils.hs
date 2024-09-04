{-# LANGUAGE CPP #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UnliftedFFITypes #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- Module      :   Grisette.Internal.SymPrim.Prim.Internal.Utils
-- Copyright   :   (c) Sirui Lu 2021-2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Internal.SymPrim.Prim.Internal.Utils
  ( pattern Dyn,
    cmpHetero,
    eqHetero,
    cmpHeteroRep,
    eqHeteroRep,
    eqTypeRepBool,
    WeakThreadId,
    weakThreadId,
    WeakThreadIdRef,
    myWeakThreadId,
    weakThreadRefAlive,
  )
where

#if MIN_VERSION_base(4,19,0)
import GHC.Conc.Sync
  ( ThreadId,
    ThreadStatus (ThreadDied, ThreadFinished),
    fromThreadId,
    myThreadId,
    threadStatus,
  )
#else
import GHC.Conc
  ( ThreadId (ThreadId),
    ThreadStatus (ThreadDied, ThreadFinished),
    myThreadId,
    threadStatus,
  )
import GHC.Exts (Addr#, ThreadId#, unsafeCoerce#)
#if __GLASGOW_HASKELL__ >= 904
#elif __GLASGOW_HASKELL__ >= 900
import Foreign.C.Types (CLong (CLong))
#else
import Foreign.C.Types (CInt (CInt))
#endif
#endif
import Data.Typeable (cast)
import Data.Word (Word64)
import System.Mem.Weak (Weak, deRefWeak)
import Type.Reflection
  ( TypeRep,
    Typeable,
    eqTypeRep,
    typeRep,
    type (:~~:) (HRefl),
  )

-- | Pattern synonym for dynamic type casting.
pattern Dyn :: (Typeable a, Typeable b) => a -> b
pattern Dyn x <- (cast -> Just x)

-- | Compare two values of different types, resolve the type equality using the
-- type representation.
cmpHeteroRep :: forall a b. TypeRep a -> TypeRep b -> (a -> a -> Bool) -> a -> b -> Bool
cmpHeteroRep ta tb f a b = case eqTypeRep ta tb of
  Just HRefl -> f a b
  _ -> False
{-# INLINE cmpHeteroRep #-}

-- | Compare two values of different types.
cmpHetero :: forall a b. (Typeable a, Typeable b) => (a -> a -> Bool) -> a -> b -> Bool
cmpHetero = cmpHeteroRep (typeRep @a) (typeRep @b)
{-# INLINE cmpHetero #-}

-- | Compare two values of different types for equality.
eqHetero :: forall a b. (Typeable a, Typeable b, Eq a) => a -> b -> Bool
eqHetero = cmpHetero (==)
{-# INLINE eqHetero #-}

-- | Compare two values of different types for equality, resolve the type
-- equality using the type representation.
eqHeteroRep :: forall a b. (Eq a) => TypeRep a -> TypeRep b -> a -> b -> Bool
eqHeteroRep ta tb = cmpHeteroRep ta tb (==)
{-# INLINE eqHeteroRep #-}

-- | Compare two type representations for equality.
eqTypeRepBool :: forall ka kb (a :: ka) (b :: kb). TypeRep a -> TypeRep b -> Bool
eqTypeRepBool a b = case eqTypeRep a b of
  Just HRefl -> True
  _ -> False
{-# INLINE eqTypeRepBool #-}

-- | A weak identifier to a thread id that doesn't prevent its garbage
-- collection.
type WeakThreadId = Word64

-- | A weak reference to a thread id that doesn't prevent its garbage
-- collection.
type WeakThreadIdRef = Weak ThreadId

{-# INLINE weakThreadId #-}

-- | Get an id of a thread that doesn't prevent its garbage collection.
weakThreadId :: ThreadId -> Word64
#if MIN_VERSION_base(4,19,0)
weakThreadId = fromThreadId
#else
weakThreadId (ThreadId t#) = fromIntegral $ rts_getThreadId (threadIdToAddr# t#)

foreign import ccall unsafe "rts_getThreadId"
#if __GLASGOW_HASKELL__ >= 904
  -- https://gitlab.haskell.org/ghc/ghc/-/merge_requests/6163
  rts_getThreadId :: Addr# -> Word64
#elif __GLASGOW_HASKELL__ >= 900
  -- https://gitlab.haskell.org/ghc/ghc/-/merge_requests/1254
  rts_getThreadId :: Addr# -> CLong
#else
  rts_getThreadId :: Addr# -> CInt
#endif

-- Note: FFI imports take Addr# instead of ThreadId# because of
-- https://gitlab.haskell.org/ghc/ghc/-/issues/8281, which would prevent loading
-- effectful-core into GHCi.
--
-- Previous workaround was to use an internal library with just this module, but
-- this is not viable because of bugs in stack (sigh).
--
-- The coercion is fine because GHC represents ThreadId# as a pointer.
{-# INLINE threadIdToAddr# #-}
threadIdToAddr# :: ThreadId# -> Addr#
threadIdToAddr# = unsafeCoerce#
#endif

-- | Get a weak identifier to the current thread id.
myWeakThreadId :: IO WeakThreadId
myWeakThreadId = weakThreadId <$> myThreadId
{-# INLINE myWeakThreadId #-}

-- | Check if a weak reference to a thread id is still alive.
weakThreadRefAlive :: WeakThreadIdRef -> IO Bool
weakThreadRefAlive wtid = do
  tid <- deRefWeak wtid
  case tid of
    Nothing -> return False
    Just tid -> do
      st <- threadStatus tid
      return $ st `notElem` [ThreadFinished, ThreadDied]
{-# INLINE weakThreadRefAlive #-}
