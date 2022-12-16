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

    -- | This module does not contain the implementation for symbolic primitive
    -- types, and the examples in this module rely on the implementations in
    -- the [grisette-symir](https://hackage.haskell.org/package/grisette-symir) package.

    -- * Symbolic equality
    GSEq (..),
    GSEq' (..),

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

-- $setup
-- >>> import Grisette.Core
-- >>> import Grisette.IR.SymPrim
-- >>> :set -XDataKinds
-- >>> :set -XBinaryLiterals
-- >>> :set -XFlexibleContexts
-- >>> :set -XFlexibleInstances
-- >>> :set -XFunctionalDependencies

-- | Auxiliary class for 'GSEq' instance derivation
class (SymBoolOp bool) => GSEq' bool f where
  -- | Auxiliary function for 'gsymeq' derivation
  gsymeq' :: f a -> f a -> bool

instance (SymBoolOp bool) => GSEq' bool U1 where
  _ `gsymeq'` _ = conc True
  {-# INLINE gsymeq' #-}

instance (SymBoolOp bool) => GSEq' bool V1 where
  _ `gsymeq'` _ = conc True
  {-# INLINE gsymeq' #-}

instance (SymBoolOp bool, GSEq bool c) => GSEq' bool (K1 i c) where
  (K1 a) `gsymeq'` (K1 b) = a `gsymeq` b
  {-# INLINE gsymeq' #-}

instance (SymBoolOp bool, GSEq' bool a) => GSEq' bool (M1 i c a) where
  (M1 a) `gsymeq'` (M1 b) = a `gsymeq'` b
  {-# INLINE gsymeq' #-}

instance (SymBoolOp bool, GSEq' bool a, GSEq' bool b) => GSEq' bool (a :+: b) where
  (L1 a) `gsymeq'` (L1 b) = a `gsymeq'` b
  (R1 a) `gsymeq'` (R1 b) = a `gsymeq'` b
  _ `gsymeq'` _ = conc False
  {-# INLINE gsymeq' #-}

instance (SymBoolOp bool, GSEq' bool a, GSEq' bool b) => GSEq' bool (a :*: b) where
  (a1 :*: b1) `gsymeq'` (a2 :*: b2) = (a1 `gsymeq'` a2) &&~ (b1 `gsymeq'` b2)
  {-# INLINE gsymeq' #-}

-- | Symbolic Equality. Note that we can't use Haskell's 'Eq' class since
-- symbolic comparison won't necessarily return a concrete 'Bool' value.
--
-- >>> let a = 1 :: SymInteger
-- >>> let b = 2 :: SymInteger
-- >>> a `gsymeq` b :: SymBool
-- false
-- >>> a `gsymne` b :: SymBool
-- true
--
-- >>> let a = "a" :: SymInteger
-- >>> let b = "b" :: SymInteger
-- >>> a `gsymne` b :: SymBool
-- (! (= a b))
-- >>> a `gsymne` b :: SymBool
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
-- However, you still need @'GSEq' SymBool@ for implementing or deriving the
-- type class due to GHC's limitation.
class LogicalOp bool => GSEq bool a where
  gsymeq :: a -> a -> bool
  a `gsymeq` b = nots $ a `gsymne` b
  {-# INLINE gsymeq #-}

  gsymne :: a -> a -> bool
  a `gsymne` b = nots $ a `gsymeq` b
  {-# INLINE gsymne #-}
  {-# MINIMAL gsymeq | gsymne #-}

instance (Generic a, SymBoolOp bool, GSEq' bool (Rep a)) => GSEq bool (Default a) where
  Default l `gsymeq` Default r = from l `gsymeq'` from r
  {-# INLINE gsymeq #-}

-- | Symbolic logical operators for symbolic booleans.
--
-- >>> let t = conc True :: SymBool
-- >>> let f = conc False :: SymBool
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

-- | ITE operator for symbolic primitives, including symbolic boolean, integer, etc.
--
-- >>> let a = "a" :: SymBool
-- >>> let b = "b" :: SymBool
-- >>> let c = "c" :: SymBool
-- >>> ites a b c
-- (ite a b c)
class ITEOp b v where
  ites :: b -> v -> v -> v

-- | Aggregation for the operations on symbolic boolean types
class (GSimpleMergeable b b, GSEq b b, Eq b, LogicalOp b, Solvable Bool b, ITEOp b b) => SymBoolOp b

#define CONCRETE_SEQ(type) \
instance (SymBoolOp bool) => GSEq bool type where \
  l `gsymeq` r = conc $ l == r; \
  {-# INLINE gsymeq #-}

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
deriving via (Default [a]) instance (SymBoolOp bool, GSEq bool a) => GSEq bool [a]

-- Maybe
deriving via (Default (Maybe a)) instance (SymBoolOp bool, GSEq bool a) => GSEq bool (Maybe a)

-- Either
deriving via (Default (Either e a)) instance (SymBoolOp bool, GSEq bool e, GSEq bool a) => GSEq bool (Either e a)

-- ExceptT
instance (SymBoolOp bool, GSEq bool (m (Either e a))) => GSEq bool (ExceptT e m a) where
  (ExceptT a) `gsymeq` (ExceptT b) = a `gsymeq` b
  {-# INLINE gsymeq #-}

-- MaybeT
instance (SymBoolOp bool, GSEq bool (m (Maybe a))) => GSEq bool (MaybeT m a) where
  (MaybeT a) `gsymeq` (MaybeT b) = a `gsymeq` b
  {-# INLINE gsymeq #-}

-- ()
instance (SymBoolOp bool) => GSEq bool () where
  _ `gsymeq` _ = conc True
  {-# INLINE gsymeq #-}

-- (,)
deriving via (Default (a, b)) instance (SymBoolOp bool, GSEq bool a, GSEq bool b) => GSEq bool (a, b)

-- (,,)
deriving via (Default (a, b, c)) instance (SymBoolOp bool, GSEq bool a, GSEq bool b, GSEq bool c) => GSEq bool (a, b, c)

-- (,,,)
deriving via
  (Default (a, b, c, d))
  instance
    (SymBoolOp bool, GSEq bool a, GSEq bool b, GSEq bool c, GSEq bool d) =>
    GSEq bool (a, b, c, d)

-- (,,,,)
deriving via
  (Default (a, b, c, d, e))
  instance
    (SymBoolOp bool, GSEq bool a, GSEq bool b, GSEq bool c, GSEq bool d, GSEq bool e) =>
    GSEq bool (a, b, c, d, e)

-- (,,,,,)
deriving via
  (Default (a, b, c, d, e, f))
  instance
    (SymBoolOp bool, GSEq bool a, GSEq bool b, GSEq bool c, GSEq bool d, GSEq bool e, GSEq bool f) =>
    GSEq bool (a, b, c, d, e, f)

-- (,,,,,,)
deriving via
  (Default (a, b, c, d, e, f, g))
  instance
    (SymBoolOp bool, GSEq bool a, GSEq bool b, GSEq bool c, GSEq bool d, GSEq bool e, GSEq bool f, GSEq bool g) =>
    GSEq bool (a, b, c, d, e, f, g)

-- (,,,,,,,)
deriving via
  (Default (a, b, c, d, e, f, g, h))
  instance
    (SymBoolOp bool, GSEq bool a, GSEq bool b, GSEq bool c, GSEq bool d, GSEq bool e, GSEq bool f, GSEq bool g, GSEq bool h) =>
    GSEq bool (a, b, c, d, e, f, g, h)

-- Sum
deriving via
  (Default (Sum f g a))
  instance
    (SymBoolOp bool, GSEq bool (f a), GSEq bool (g a)) => GSEq bool (Sum f g a)

-- Writer
instance (SymBoolOp bool, GSEq bool (m (a, s))) => GSEq bool (WriterLazy.WriterT s m a) where
  (WriterLazy.WriterT l) `gsymeq` (WriterLazy.WriterT r) = l `gsymeq` r
  {-# INLINE gsymeq #-}

instance (SymBoolOp bool, GSEq bool (m (a, s))) => GSEq bool (WriterStrict.WriterT s m a) where
  (WriterStrict.WriterT l) `gsymeq` (WriterStrict.WriterT r) = l `gsymeq` r
  {-# INLINE gsymeq #-}

-- Identity
instance (SymBoolOp bool, GSEq bool a) => GSEq bool (Identity a) where
  (Identity l) `gsymeq` (Identity r) = l `gsymeq` r
  {-# INLINE gsymeq #-}

-- IdentityT
instance (SymBoolOp bool, GSEq bool (m a)) => GSEq bool (IdentityT m a) where
  (IdentityT l) `gsymeq` (IdentityT r) = l `gsymeq` r
  {-# INLINE gsymeq #-}
