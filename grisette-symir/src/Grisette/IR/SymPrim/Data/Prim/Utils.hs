{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Grisette.IR.SymPrim.Data.Prim.Utils
  ( pattern Dyn,
    cmpHetero,
    eqHetero,
    cmpHeteroRep,
    eqHeteroRep,
    eqTypeRepBool,
  )
where

import Data.Typeable (cast)
import Type.Reflection

pattern Dyn :: (Typeable a, Typeable b) => a -> b
pattern Dyn x <- (cast -> Just x)

cmpHeteroRep :: forall a b. TypeRep a -> TypeRep b -> (a -> a -> Bool) -> a -> b -> Bool
cmpHeteroRep ta tb f a b = case eqTypeRep ta tb of
  Just HRefl -> f a b
  _ -> False
{-# INLINE cmpHeteroRep #-}

cmpHetero :: forall a b. (Typeable a, Typeable b) => (a -> a -> Bool) -> a -> b -> Bool
cmpHetero = cmpHeteroRep (typeRep @a) (typeRep @b)
{-# INLINE cmpHetero #-}

eqHetero :: forall a b. (Typeable a, Typeable b, Eq a) => a -> b -> Bool
eqHetero = cmpHetero (==)
{-# INLINE eqHetero #-}

eqHeteroRep :: forall a b. Eq a => TypeRep a -> TypeRep b -> a -> b -> Bool
eqHeteroRep ta tb = cmpHeteroRep ta tb (==)
{-# INLINE eqHeteroRep #-}

eqTypeRepBool :: forall ka kb (a :: ka) (b :: kb). TypeRep a -> TypeRep b -> Bool
eqTypeRepBool a b = case eqTypeRep a b of
  Just HRefl -> True
  _ -> False
{-# INLINE eqTypeRepBool #-}
