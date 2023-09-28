{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE Trustworthy #-}
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
module Grisette.Core.Data.Class.Bool
  ( -- * Symbolic equality
    SEq (..),
    SEq' (..),

    -- * Symbolic Boolean operations
    LogicalOp (..),
    SymBoolOp,
    ITEOp (..),
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
import Data.Word (Word16, Word32, Word64, Word8)
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
import {-# SOURCE #-} Grisette.Core.Data.Class.SimpleMergeable
  ( SimpleMergeable,
  )
import Grisette.Core.Data.Class.Solvable (Solvable (con))
import Grisette.IR.SymPrim.Data.Prim.InternedTerm.Term
  ( LinkedRep,
    SupportedPrim,
  )
import Grisette.IR.SymPrim.Data.Prim.PartialEval.Bool
  ( pevalAndTerm,
    pevalITETerm,
    pevalImplyTerm,
    pevalNotTerm,
    pevalOrTerm,
    pevalXorTerm,
  )
import {-# SOURCE #-} Grisette.IR.SymPrim.Data.SymPrim
  ( SomeSymIntN,
    SomeSymWordN,
    SymBool (SymBool),
    SymIntN (SymIntN),
    SymInteger (SymInteger),
    SymWordN (SymWordN),
    binSomeSymIntNR1,
    binSomeSymWordNR1,
    type (-~>) (SymGeneralFun),
    type (=~>) (SymTabularFun),
  )

-- $setup
-- >>> import Grisette.Core
-- >>> import Grisette.IR.SymPrim
-- >>> :set -XDataKinds
-- >>> :set -XBinaryLiterals
-- >>> :set -XFlexibleContexts
-- >>> :set -XFlexibleInstances
-- >>> :set -XFunctionalDependencies

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

instance (Generic a, SEq' (Rep a)) => SEq (Default a) where
  Default l ==~ Default r = from l ==~~ from r
  {-# INLINE (==~) #-}

-- | Symbolic logical operators for symbolic booleans.
--
-- >>> let t = con True :: SymBool
-- >>> let f = con False :: SymBool
-- >>> let a = "a" :: SymBool
-- >>> let b = "b" :: SymBool
-- >>> t ||~ f
-- true
-- >>> a ||~ t
-- true
-- >>> a ||~ f
-- a
-- >>> a ||~ b
-- (|| a b)
-- >>> t &&~ f
-- false
-- >>> a &&~ t
-- a
-- >>> a &&~ f
-- false
-- >>> a &&~ b
-- (&& a b)
-- >>> nots t
-- false
-- >>> nots f
-- true
-- >>> nots a
-- (! a)
-- >>> t `xors` f
-- true
-- >>> t `xors` t
-- false
-- >>> a `xors` t
-- (! a)
-- >>> a `xors` f
-- a
-- >>> a `xors` b
-- (|| (&& (! a) b) (&& a (! b)))
class LogicalOp b where
  -- | Symbolic disjunction
  (||~) :: b -> b -> b
  a ||~ b = nots $ nots a &&~ nots b
  {-# INLINE (||~) #-}

  infixr 2 ||~

  -- | Symbolic conjunction
  (&&~) :: b -> b -> b
  a &&~ b = nots $ nots a ||~ nots b
  {-# INLINE (&&~) #-}

  infixr 3 &&~

  -- | Symbolic negation
  nots :: b -> b

  -- | Symbolic exclusive disjunction
  xors :: b -> b -> b
  a `xors` b = (a &&~ nots b) ||~ (nots a &&~ b)
  {-# INLINE xors #-}

  -- | Symbolic implication
  implies :: b -> b -> b
  a `implies` b = nots a ||~ b
  {-# INLINE implies #-}

  {-# MINIMAL (||~), nots | (&&~), nots #-}

instance LogicalOp Bool where
  (||~) = (||)
  {-# INLINE (||~) #-}
  (&&~) = (&&)
  {-# INLINE (&&~) #-}
  nots = not
  {-# INLINE nots #-}

-- | ITE operator for solvable (see "Grisette.Core#solvable")s, including symbolic boolean, integer, etc.
--
-- >>> let a = "a" :: SymBool
-- >>> let b = "b" :: SymBool
-- >>> let c = "c" :: SymBool
-- >>> ites a b c
-- (ite a b c)
class ITEOp v where
  ites :: SymBool -> v -> v -> v

-- | Aggregation for the operations on symbolic boolean types
class (SimpleMergeable b, SEq b, Eq b, LogicalOp b, Solvable Bool b, ITEOp b) => SymBoolOp b

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

#define ITEOP_SIMPLE(type) \
instance ITEOp type where \
  ites (SymBool c) (type t) (type f) = type $ pevalITETerm c t f; \
  {-# INLINE ites #-}

#define ITEOP_BV(type) \
instance (KnownNat n, 1 <= n) => ITEOp (type n) where \
  ites (SymBool c) (type t) (type f) = type $ pevalITETerm c t f; \
  {-# INLINE ites #-}

#define ITEOP_BV_SOME(symtype, bf) \
instance ITEOp symtype where \
  ites c = bf (ites c) "ites"; \
  {-# INLINE ites #-}

#define ITEOP_FUN(op, cons) \
instance (SupportedPrim ca, SupportedPrim cb, LinkedRep ca sa, LinkedRep cb sb) => ITEOp (sa op sb) where \
  ites (SymBool c) (cons t) (cons f) = cons $ pevalITETerm c t f; \
  {-# INLINE ites #-}

#if 1
ITEOP_SIMPLE(SymBool)
ITEOP_SIMPLE(SymInteger)
ITEOP_BV(SymIntN)
ITEOP_BV(SymWordN)
ITEOP_BV_SOME(SomeSymIntN, binSomeSymIntNR1)
ITEOP_BV_SOME(SomeSymWordN, binSomeSymWordNR1)
ITEOP_FUN(=~>, SymTabularFun)
ITEOP_FUN(-~>, SymGeneralFun)
#endif

instance LogicalOp SymBool where
  (SymBool l) ||~ (SymBool r) = SymBool $ pevalOrTerm l r
  (SymBool l) &&~ (SymBool r) = SymBool $ pevalAndTerm l r
  nots (SymBool v) = SymBool $ pevalNotTerm v
  (SymBool l) `xors` (SymBool r) = SymBool $ pevalXorTerm l r
  (SymBool l) `implies` (SymBool r) = SymBool $ pevalImplyTerm l r
