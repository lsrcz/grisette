{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      :   Grisette.Core.Data.Class.Bool
-- Copyright   :   (c) Sirui Lu 2021-2023
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Core.Data.Class.SEq
  ( -- * Symbolic equality
    SEq (..),
    SEq' (..),
  )
where

import Control.Monad.Except (ExceptT (ExceptT))
import Control.Monad.Identity
  ( Identity (Identity),
    IdentityT (IdentityT),
  )
import Control.Monad.Trans.Maybe (MaybeT (MaybeT))
import qualified Control.Monad.Writer.Lazy as WriterLazy
import qualified Control.Monad.Writer.Strict as WriterStrict
import qualified Data.ByteString as B
import Data.Functor.Sum (Sum)
import Data.Int (Int16, Int32, Int64, Int8)
import qualified Data.Text as T
import Data.Typeable (Proxy (Proxy), type (:~:) (Refl))
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.TypeLits (sameNat)
import GHC.TypeNats (KnownNat, type (<=))
import Generics.Deriving
  ( Default (Default),
    Generic (Rep, from),
    K1 (K1),
    M1 (M1),
    U1,
    V1,
    type (:*:) ((:*:)),
    type (:+:) (L1, R1),
  )
import Grisette.Core.Data.BV (IntN, SomeIntN, SomeWordN, WordN)
import Grisette.Core.Data.Class.LogicalOp (LogicalOp (nots, (&&~)))
import Grisette.Core.Data.Class.Solvable (Solvable (con))
import Grisette.IR.SymPrim.Data.Prim.PartialEval.Bool (pevalEqvTerm)
import {-# SOURCE #-} Grisette.IR.SymPrim.Data.SymPrim
  ( SomeSymIntN (SomeSymIntN),
    SomeSymWordN (SomeSymWordN),
    SymBool (SymBool),
    SymIntN (SymIntN),
    SymInteger (SymInteger),
    SymWordN (SymWordN),
  )

-- $setup
-- >>> import Grisette.Core
-- >>> import Grisette.IR.SymPrim
-- >>> :set -XDataKinds
-- >>> :set -XBinaryLiterals
-- >>> :set -XFlexibleContexts
-- >>> :set -XFlexibleInstances
-- >>> :set -XFunctionalDependencies

-- | Symbolic equality. Note that we can't use Haskell's 'Eq' class since
-- symbolic comparison won't necessarily return a concrete 'Bool' value.
--
-- >>> let a = 1 :: SymInteger
-- >>> let b = 2 :: SymInteger
-- >>> a ==~ b
-- false
-- >>> a /=~ b
-- true
--
-- >>> let a = "a" :: SymInteger
-- >>> let b = "b" :: SymInteger
-- >>> a /=~ b
-- (! (= a b))
-- >>> a /=~ b
-- (! (= a b))
--
-- __Note:__ This type class can be derived for algebraic data types.
-- You may need the @DerivingVia@ and @DerivingStrategies@ extensions.
--
-- > data X = ... deriving Generic deriving SEq via (Default X)
class SEq a where
  (==~) :: a -> a -> SymBool
  a ==~ b = nots $ a /=~ b
  {-# INLINE (==~) #-}
  infix 4 ==~

  (/=~) :: a -> a -> SymBool
  a /=~ b = nots $ a ==~ b
  {-# INLINE (/=~) #-}
  infix 4 /=~
  {-# MINIMAL (==~) | (/=~) #-}

-- SEq instances
#define CONCRETE_SEQ(type) \
instance SEq type where \
  l ==~ r = con $ l == r; \
  {-# INLINE (==~) #-}

#define CONCRETE_SEQ_BV(type) \
instance (KnownNat n, 1 <= n) => SEq (type n) where \
  l ==~ r = con $ l == r; \
  {-# INLINE (==~) #-}

#if 1
CONCRETE_SEQ(Bool)
CONCRETE_SEQ(Integer)
CONCRETE_SEQ(Char)
CONCRETE_SEQ(Int)
CONCRETE_SEQ(Int8)
CONCRETE_SEQ(Int16)
CONCRETE_SEQ(Int32)
CONCRETE_SEQ(Int64)
CONCRETE_SEQ(Word)
CONCRETE_SEQ(Word8)
CONCRETE_SEQ(Word16)
CONCRETE_SEQ(Word32)
CONCRETE_SEQ(Word64)
CONCRETE_SEQ(B.ByteString)
CONCRETE_SEQ(T.Text)
CONCRETE_SEQ_BV(WordN)
CONCRETE_SEQ_BV(IntN)
CONCRETE_SEQ(SomeWordN)
CONCRETE_SEQ(SomeIntN)
#endif

-- List
deriving via (Default [a]) instance (SEq a) => SEq [a]

-- Maybe
deriving via (Default (Maybe a)) instance (SEq a) => SEq (Maybe a)

-- Either
deriving via (Default (Either e a)) instance (SEq e, SEq a) => SEq (Either e a)

-- ExceptT
instance (SEq (m (Either e a))) => SEq (ExceptT e m a) where
  (ExceptT a) ==~ (ExceptT b) = a ==~ b
  {-# INLINE (==~) #-}

-- MaybeT
instance (SEq (m (Maybe a))) => SEq (MaybeT m a) where
  (MaybeT a) ==~ (MaybeT b) = a ==~ b
  {-# INLINE (==~) #-}

-- ()
instance SEq () where
  _ ==~ _ = con True
  {-# INLINE (==~) #-}

-- (,)
deriving via (Default (a, b)) instance (SEq a, SEq b) => SEq (a, b)

-- (,,)
deriving via (Default (a, b, c)) instance (SEq a, SEq b, SEq c) => SEq (a, b, c)

-- (,,,)
deriving via
  (Default (a, b, c, d))
  instance
    (SEq a, SEq b, SEq c, SEq d) =>
    SEq (a, b, c, d)

-- (,,,,)
deriving via
  (Default (a, b, c, d, e))
  instance
    (SEq a, SEq b, SEq c, SEq d, SEq e) =>
    SEq (a, b, c, d, e)

-- (,,,,,)
deriving via
  (Default (a, b, c, d, e, f))
  instance
    (SEq a, SEq b, SEq c, SEq d, SEq e, SEq f) =>
    SEq (a, b, c, d, e, f)

-- (,,,,,,)
deriving via
  (Default (a, b, c, d, e, f, g))
  instance
    (SEq a, SEq b, SEq c, SEq d, SEq e, SEq f, SEq g) =>
    SEq (a, b, c, d, e, f, g)

-- (,,,,,,,)
deriving via
  (Default (a, b, c, d, e, f, g, h))
  instance
    (SEq a, SEq b, SEq c, SEq d, SEq e, SEq f, SEq g, SEq h) =>
    SEq (a, b, c, d, e, f, g, h)

-- Sum
deriving via
  (Default (Sum f g a))
  instance
    (SEq (f a), SEq (g a)) => SEq (Sum f g a)

-- Writer
instance (SEq (m (a, s))) => SEq (WriterLazy.WriterT s m a) where
  (WriterLazy.WriterT l) ==~ (WriterLazy.WriterT r) = l ==~ r
  {-# INLINE (==~) #-}

instance (SEq (m (a, s))) => SEq (WriterStrict.WriterT s m a) where
  (WriterStrict.WriterT l) ==~ (WriterStrict.WriterT r) = l ==~ r
  {-# INLINE (==~) #-}

-- Identity
instance (SEq a) => SEq (Identity a) where
  (Identity l) ==~ (Identity r) = l ==~ r
  {-# INLINE (==~) #-}

-- IdentityT
instance (SEq (m a)) => SEq (IdentityT m a) where
  (IdentityT l) ==~ (IdentityT r) = l ==~ r
  {-# INLINE (==~) #-}

-- Symbolic types
#define SEQ_SIMPLE(symtype) \
instance SEq symtype where \
  (symtype l) ==~ (symtype r) = SymBool $ pevalEqvTerm l r

#define SEQ_BV(symtype) \
instance (KnownNat n, 1 <= n) => SEq (symtype n) where \
  (symtype l) ==~ (symtype r) = SymBool $ pevalEqvTerm l r

#define SEQ_BV_SOME(somety, origty) \
instance SEq somety where \
  somety (l :: origty l) ==~ somety (r :: origty r) = \
    (case sameNat (Proxy @l) (Proxy @r) of \
      Just Refl -> l ==~ r; \
      Nothing -> con False); \
  {-# INLINE (==~) #-}; \
  somety (l :: origty l) /=~ somety (r :: origty r) = \
    (case sameNat (Proxy @l) (Proxy @r) of \
      Just Refl -> l /=~ r; \
      Nothing -> con True); \
  {-# INLINE (/=~) #-}

#if 1
SEQ_SIMPLE(SymBool)
SEQ_SIMPLE(SymInteger)
SEQ_BV(SymIntN)
SEQ_BV(SymWordN)
SEQ_BV_SOME(SomeSymIntN, SymIntN)
SEQ_BV_SOME(SomeSymWordN, SymWordN)
#endif

-- | Auxiliary class for 'SEq' instance derivation
class SEq' f where
  -- | Auxiliary function for '(==~~) derivation
  (==~~) :: f a -> f a -> SymBool

  infix 4 ==~~

instance SEq' U1 where
  _ ==~~ _ = con True
  {-# INLINE (==~~) #-}

instance SEq' V1 where
  _ ==~~ _ = con True
  {-# INLINE (==~~) #-}

instance (SEq c) => SEq' (K1 i c) where
  (K1 a) ==~~ (K1 b) = a ==~ b
  {-# INLINE (==~~) #-}

instance (SEq' a) => SEq' (M1 i c a) where
  (M1 a) ==~~ (M1 b) = a ==~~ b
  {-# INLINE (==~~) #-}

instance (SEq' a, SEq' b) => SEq' (a :+: b) where
  (L1 a) ==~~ (L1 b) = a ==~~ b
  (R1 a) ==~~ (R1 b) = a ==~~ b
  _ ==~~ _ = con False
  {-# INLINE (==~~) #-}

instance (SEq' a, SEq' b) => SEq' (a :*: b) where
  (a1 :*: b1) ==~~ (a2 :*: b2) = (a1 ==~~ a2) &&~ (b1 ==~~ b2)
  {-# INLINE (==~~) #-}

instance (Generic a, SEq' (Rep a)) => SEq (Default a) where
  Default l ==~ Default r = from l ==~~ from r
  {-# INLINE (==~) #-}
