{-# LANGUAGE CPP #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      :   Grisette.Core.Data.Class.Bool
-- Copyright   :   (c) Sirui Lu 2021-2022
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Core.Data.Class.Bool
  ( -- * Note for the examples

    --

    -- | This module does not contain the implementation for solvable (see "Grisette.Core#solvable")
    -- types, and the examples in this module rely on the implementations in
    -- the [grisette-symir](https://hackage.haskell.org/package/grisette-symir) package.

    -- * Symbolic equality
    SEq (..),
    SEq' (..),

    -- * Symbolic Boolean operations
    LogicalOp (..),
    SymBoolOp,
    ITEOp (..),
  )
where

import Control.Monad.Except
import Control.Monad.Identity
  ( Identity (Identity),
    IdentityT (IdentityT),
  )
import Control.Monad.Trans.Maybe
import qualified Control.Monad.Writer.Lazy as WriterLazy
import qualified Control.Monad.Writer.Strict as WriterStrict
import qualified Data.ByteString as B
import Data.Functor.Sum
import Data.Int
import Data.Word
import Generics.Deriving
import {-# SOURCE #-} Grisette.Core.Data.Class.SimpleMergeable
import Grisette.Core.Data.Class.Solvable
import Grisette.IR.SymPrim.Data.Prim.InternedTerm.Term
import Grisette.IR.SymPrim.Data.Prim.PartialEval.Bool
import {-# SOURCE #-} Grisette.IR.SymPrim.Data.SymPrim

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

instance SEq c => SEq' (K1 i c) where
  (K1 a) ==~~ (K1 b) = a ==~ b
  {-# INLINE (==~~) #-}

instance SEq' a => SEq' (M1 i c a) where
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

-- | Symbolic Equality. Note that we can't use Haskell's 'Eq' class since
-- symbolic comparison won't necessarily return a concrete 'Bool' value.
--
-- >>> let a = 1 :: SymInteger
-- >>> let b = 2 :: SymInteger
-- >>> a ==~ b :: SymBool
-- false
-- >>> a /=~ b :: SymBool
-- true
--
-- >>> let a = "a" :: SymInteger
-- >>> let b = "b" :: SymInteger
-- >>> a /=~ b :: SymBool
-- (! (= a b))
-- >>> a /=~ b :: SymBool
-- (! (= a b))
--
-- __Note 1:__ This type class can be derived for algebraic data types.
-- You may need the @DerivingVia@ and @DerivingStrategies@ extensions.
--
-- > data X = ... deriving Generic deriving (GMergeable SymBool) via (Default X)
--
-- __Note 2:__ The @bool@ type is the symbolic Boolean type to return. It should
-- be an instance of `SymBoolOp`. If you do not need to use an alternative
-- symbolic Boolean type, and will use the 'SymBool' type provided by the
-- [grisette-symir](https://hackage.haskell.org/package/grisette-symir) package, you can use the specialized `SEq` type synonym for
-- the constraints and use specialized `(==~)`, `(/=~)` operators from
-- [grisette-symir](https://hackage.haskell.org/package/grisette-symir) to write code with fewer type annotations.
-- However, you still need @'SEq' SymBool@ for implementing or deriving the
-- type class due to GHC's limitation.
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

instance (SupportedPrim a) => ITEOp (Sym a) where
  ites (Sym c) (Sym t) (Sym f) = Sym $ pevalITETerm c t f

instance LogicalOp (Sym Bool) where
  (Sym l) ||~ (Sym r) = Sym $ pevalOrTerm l r
  (Sym l) &&~ (Sym r) = Sym $ pevalAndTerm l r
  nots (Sym v) = Sym $ pevalNotTerm v
  (Sym l) `xors` (Sym r) = Sym $ pevalXorTerm l r
  (Sym l) `implies` (Sym r) = Sym $ pevalImplyTerm l r

