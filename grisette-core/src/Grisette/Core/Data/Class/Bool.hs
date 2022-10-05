{-# LANGUAGE CPP #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Grisette.Core.Data.Class.Bool
  ( SEq (..),
    SEq' (..),
    LogicalOp (..),
    SymBoolOp,
    ITEOp (..),
  )
where

import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.Trans.Maybe
import qualified Control.Monad.Writer.Lazy as WriterLazy
import qualified Control.Monad.Writer.Strict as WriterStrict
import qualified Data.ByteString as B
import Data.Functor.Sum
import Data.Int
import Data.Word
import Generics.Deriving
import Grisette.Core.Data.Class.PrimWrapper
import {-# SOURCE #-} Grisette.Core.Data.Class.SimpleMergeable

-- | Auxiliary class for 'SEq' instance derivation
class (SymBoolOp bool) => SEq' bool f where
  -- | Auxiliary function for '(==~)' derivation
  (==~~) :: f a -> f a -> bool

  infix 4 ==~~

instance (SymBoolOp bool) => SEq' bool U1 where
  _ ==~~ _ = conc True
  {-# INLINE (==~~) #-}

instance (SymBoolOp bool) => SEq' bool V1 where
  _ ==~~ _ = conc True
  {-# INLINE (==~~) #-}

instance (SymBoolOp bool, SEq bool c) => SEq' bool (K1 i c) where
  (K1 a) ==~~ (K1 b) = a ==~ b
  {-# INLINE (==~~) #-}

instance (SymBoolOp bool, SEq' bool a) => SEq' bool (M1 i c a) where
  (M1 a) ==~~ (M1 b) = a ==~~ b
  {-# INLINE (==~~) #-}

instance (SymBoolOp bool, SEq' bool a, SEq' bool b) => SEq' bool (a :+: b) where
  (L1 a) ==~~ (L1 b) = a ==~~ b
  (R1 a) ==~~ (R1 b) = a ==~~ b
  _ ==~~ _ = conc False
  {-# INLINE (==~~) #-}

instance (SymBoolOp bool, SEq' bool a, SEq' bool b) => SEq' bool (a :*: b) where
  (a1 :*: b1) ==~~ (a2 :*: b2) = (a1 ==~~ a2) &&~ (b1 ==~~ b2)
  {-# INLINE (==~~) #-}

-- | Symbolic Equality. Note that we can't use Haskell's 'Eq' class since symbolic comparison won't necessarily return
-- a concrete 'Bool' value.
--
-- The @bool@ type is the symbolic boolean type to return.
class LogicalOp bool => SEq bool a where
  (==~) :: a -> a -> bool
  a ==~ b = nots $ a /=~ b
  {-# INLINE (==~) #-}
  infix 4 ==~

  (/=~) :: a -> a -> bool
  a /=~ b = nots $ a ==~ b
  {-# INLINE (/=~) #-}
  infix 4 /=~
  {-# MINIMAL (==~) | (/=~) #-}

instance (Generic a, SymBoolOp bool, SEq' bool (Rep a)) => SEq bool (Default a) where
  Default l ==~ Default r = from l ==~~ from r
  {-# INLINE (==~) #-}

-- | Logical operators for symbolic booleans.
class LogicalOp b where
  (||~) :: b -> b -> b
  a ||~ b = nots $ nots a &&~ nots b
  {-# INLINE (||~) #-}
  infixr 2 ||~
  (&&~) :: b -> b -> b
  a &&~ b = nots $ nots a ||~ nots b
  {-# INLINE (&&~) #-}
  infixr 3 &&~
  nots :: b -> b
  xors :: b -> b -> b
  a `xors` b = (a &&~ nots b) ||~ (nots a &&~ b)
  {-# INLINE xors #-}
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

-- | ITE operator for symbolic primitives, including symbolic boolean, integer, etc.
class ITEOp b v where
  ites :: b -> v -> v -> v

-- | Aggregation for the operations on symbolic boolean types
class (SimpleMergeable b b, SEq b b, Eq b, LogicalOp b, PrimWrapper b Bool, ITEOp b b) => SymBoolOp b

#define CONCRETE_SEQ(type) \
instance (SymBoolOp bool) => SEq bool type where \
  l ==~ r = conc $ l == r; \
  {-# INLINE (==~) #-}

CONCRETE_SEQ (Bool)
CONCRETE_SEQ (Integer)
CONCRETE_SEQ (Char)
CONCRETE_SEQ (Int)
CONCRETE_SEQ (Int8)
CONCRETE_SEQ (Int16)
CONCRETE_SEQ (Int32)
CONCRETE_SEQ (Int64)
CONCRETE_SEQ (Word)
CONCRETE_SEQ (Word8)
CONCRETE_SEQ (Word16)
CONCRETE_SEQ (Word32)
CONCRETE_SEQ (Word64)
CONCRETE_SEQ (B.ByteString)

-- List
deriving via (Default [a]) instance (SymBoolOp bool, SEq bool a) => SEq bool [a]

-- Maybe
deriving via (Default (Maybe a)) instance (SymBoolOp bool, SEq bool a) => SEq bool (Maybe a)

-- Either
deriving via (Default (Either e a)) instance (SymBoolOp bool, SEq bool e, SEq bool a) => SEq bool (Either e a)

-- ExceptT
instance (SymBoolOp bool, SEq bool (m (Either e a))) => SEq bool (ExceptT e m a) where
  (ExceptT a) ==~ (ExceptT b) = a ==~ b
  {-# INLINE (==~) #-}

-- MaybeT
instance (SymBoolOp bool, SEq bool (m (Maybe a))) => SEq bool (MaybeT m a) where
  (MaybeT a) ==~ (MaybeT b) = a ==~ b
  {-# INLINE (==~) #-}

-- ()
instance (SymBoolOp bool) => SEq bool () where
  _ ==~ _ = conc True
  {-# INLINE (==~) #-}

-- (,)
deriving via (Default (a, b)) instance (SymBoolOp bool, SEq bool a, SEq bool b) => SEq bool (a, b)

-- (,,)
deriving via (Default (a, b, c)) instance (SymBoolOp bool, SEq bool a, SEq bool b, SEq bool c) => SEq bool (a, b, c)

-- (,,,)
deriving via
  (Default (a, b, c, d))
  instance
    (SymBoolOp bool, SEq bool a, SEq bool b, SEq bool c, SEq bool d) =>
    SEq bool (a, b, c, d)

-- (,,,,)
deriving via
  (Default (a, b, c, d, e))
  instance
    (SymBoolOp bool, SEq bool a, SEq bool b, SEq bool c, SEq bool d, SEq bool e) =>
    SEq bool (a, b, c, d, e)

-- (,,,,,)
deriving via
  (Default (a, b, c, d, e, f))
  instance
    (SymBoolOp bool, SEq bool a, SEq bool b, SEq bool c, SEq bool d, SEq bool e, SEq bool f) =>
    SEq bool (a, b, c, d, e, f)

-- (,,,,,,)
deriving via
  (Default (a, b, c, d, e, f, g))
  instance
    (SymBoolOp bool, SEq bool a, SEq bool b, SEq bool c, SEq bool d, SEq bool e, SEq bool f, SEq bool g) =>
    SEq bool (a, b, c, d, e, f, g)

-- (,,,,,,,)
deriving via
  (Default (a, b, c, d, e, f, g, h))
  instance
    (SymBoolOp bool, SEq bool a, SEq bool b, SEq bool c, SEq bool d, SEq bool e, SEq bool f, SEq bool g, SEq bool h) =>
    SEq bool (a, b, c, d, e, f, g, h)

-- Sum
deriving via
  (Default (Sum f g a))
  instance
    (SymBoolOp bool, SEq bool (f a), SEq bool (g a)) => SEq bool (Sum f g a)

-- Writer
instance (SymBoolOp bool, SEq bool (m (a, s))) => SEq bool (WriterLazy.WriterT s m a) where
  (WriterLazy.WriterT l) ==~ (WriterLazy.WriterT r) = l ==~ r
  {-# INLINE (==~) #-}

instance (SymBoolOp bool, SEq bool (m (a, s))) => SEq bool (WriterStrict.WriterT s m a) where
  (WriterStrict.WriterT l) ==~ (WriterStrict.WriterT r) = l ==~ r
  {-# INLINE (==~) #-}

-- Identity
instance (SymBoolOp bool, SEq bool a) => SEq bool (Identity a) where
  (Identity l) ==~ (Identity r) = l ==~ r
  {-# INLINE (==~) #-}

-- IdentityT
instance (SymBoolOp bool, SEq bool (m a)) => SEq bool (IdentityT m a) where
  (IdentityT l) ==~ (IdentityT r) = l ==~ r
  {-# INLINE (==~) #-}
