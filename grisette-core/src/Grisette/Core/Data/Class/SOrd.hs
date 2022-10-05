{-# LANGUAGE CPP #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Grisette.Core.Data.Class.SOrd
  ( SOrd (..),
    SOrd' (..),
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
import Grisette.Core.Data.Class.Bool
import Grisette.Core.Data.Class.PrimWrapper
import Grisette.Core.Data.Class.SimpleMergeable

-- | Auxiliary class for 'SOrd' instance derivation
class (SEq' bool f) => SOrd' bool f where
  (<~~) :: f a -> f a -> bool
  (<=~~) :: f a -> f a -> bool
  (>~~) :: f a -> f a -> bool
  (>=~~) :: f a -> f a -> bool
  infix 4 <~~
  infix 4 <=~~
  infix 4 >~~
  infix 4 >=~~
  symCompare' :: (UnionLike bool u, Monad u) => f a -> f a -> u Ordering

instance (SymBoolOp bool) => SOrd' bool U1 where
  _ <~~ _ = conc False
  _ <=~~ _ = conc True
  _ >~~ _ = conc False
  _ >=~~ _ = conc True
  symCompare' _ _ = mrgSingle EQ

instance (SymBoolOp bool) => SOrd' bool V1 where
  _ <~~ _ = conc False
  _ <=~~ _ = conc True
  _ >~~ _ = conc False
  _ >=~~ _ = conc True
  symCompare' _ _ = mrgSingle EQ

instance (SymBoolOp bool, SOrd bool c) => SOrd' bool (K1 i c) where
  (K1 a) <~~ (K1 b) = a <~ b
  (K1 a) <=~~ (K1 b) = a <=~ b
  (K1 a) >~~ (K1 b) = a >~ b
  (K1 a) >=~~ (K1 b) = a >=~ b
  symCompare' (K1 a) (K1 b) = symCompare a b

instance (SymBoolOp bool, SOrd' bool a) => SOrd' bool (M1 i c a) where
  (M1 a) <~~ (M1 b) = a <~~ b
  (M1 a) <=~~ (M1 b) = a <=~~ b
  (M1 a) >~~ (M1 b) = a >~~ b
  (M1 a) >=~~ (M1 b) = a >=~~ b
  symCompare' (M1 a) (M1 b) = symCompare' a b

instance (SymBoolOp bool, SOrd' bool a, SOrd' bool b) => SOrd' bool (a :+: b) where
  (L1 _) <~~ (R1 _) = conc True
  (L1 a) <~~ (L1 b) = a <~~ b
  (R1 _) <~~ (L1 _) = conc False
  (R1 a) <~~ (R1 b) = a <~~ b
  (L1 _) <=~~ (R1 _) = conc True
  (L1 a) <=~~ (L1 b) = a <=~~ b
  (R1 _) <=~~ (L1 _) = conc False
  (R1 a) <=~~ (R1 b) = a <=~~ b

  (L1 _) >~~ (R1 _) = conc False
  (L1 a) >~~ (L1 b) = a >~~ b
  (R1 _) >~~ (L1 _) = conc True
  (R1 a) >~~ (R1 b) = a >~~ b
  (L1 _) >=~~ (R1 _) = conc False
  (L1 a) >=~~ (L1 b) = a >=~~ b
  (R1 _) >=~~ (L1 _) = conc True
  (R1 a) >=~~ (R1 b) = a >=~~ b

  symCompare' (L1 a) (L1 b) = symCompare' a b
  symCompare' (L1 _) (R1 _) = mrgSingle LT
  symCompare' (R1 a) (R1 b) = symCompare' a b
  symCompare' (R1 _) (L1 _) = mrgSingle GT

instance (SymBoolOp bool, SOrd' bool a, SOrd' bool b) => SOrd' bool (a :*: b) where
  (a1 :*: b1) <~~ (a2 :*: b2) = (a1 <~~ a2) ||~ ((a1 ==~~ a2) &&~ (b1 <~~ b2))
  (a1 :*: b1) <=~~ (a2 :*: b2) = (a1 <~~ a2) ||~ ((a1 ==~~ a2) &&~ (b1 <=~~ b2))
  (a1 :*: b1) >~~ (a2 :*: b2) = (a1 >~~ a2) ||~ ((a1 ==~~ a2) &&~ (b1 >~~ b2))
  (a1 :*: b1) >=~~ (a2 :*: b2) = (a1 >~~ a2) ||~ ((a1 ==~~ a2) &&~ (b1 >=~~ b2))
  symCompare' (a1 :*: b1) (a2 :*: b2) = do
    l <- symCompare' a1 a2
    case l of
      EQ -> symCompare' b1 b2
      _ -> mrgSingle l

derivedSymLt :: (Generic a, SOrd' bool (Rep a)) => a -> a -> bool
derivedSymLt x y = from x <~~ from y

derivedSymLe :: (Generic a, SOrd' bool (Rep a)) => a -> a -> bool
derivedSymLe x y = from x <=~~ from y

derivedSymGt :: (Generic a, SOrd' bool (Rep a)) => a -> a -> bool
derivedSymGt x y = from x >~~ from y

derivedSymGe :: (Generic a, SOrd' bool (Rep a)) => a -> a -> bool
derivedSymGe x y = from x >=~~ from y

derivedSymCompare :: (Generic a, SOrd' bool (Rep a), UnionLike bool u, Monad u) => a -> a -> u Ordering
derivedSymCompare x y = symCompare' (from x) (from y)

-- | Symbolic total order. Note that we can't use Haskell's 'Ord' class since symbolic comparison won't necessarily return
-- a concrete 'Bool' or 'Ordering' value.
--
-- The @bool@ type is the symbolic boolean type to return.
class (SEq bool a) => SOrd bool a where
  (<~) :: a -> a -> bool
  (<=~) :: a -> a -> bool
  (>~) :: a -> a -> bool
  (>=~) :: a -> a -> bool
  infix 4 <~
  infix 4 <=~
  infix 4 >~
  infix 4 >=~
  x <~ y = x <=~ y &&~ x /=~ y
  x >~ y = y <~ x
  x >=~ y = y <=~ x
  symCompare :: (UnionLike bool u, Monad u) => a -> a -> u Ordering
  symCompare l r =
    mrgIf
      (l <~ r :: bool)
      (mrgSingle LT)
      (mrgIf (l ==~ r :: bool) (mrgSingle EQ) (mrgSingle GT))
  {-# MINIMAL (<=~) #-}

instance (SEq bool a, Generic a, SOrd' bool (Rep a)) => SOrd bool (Default a) where
  (Default l) <=~ (Default r) = l `derivedSymLe` r
  (Default l) <~ (Default r) = l `derivedSymLt` r
  (Default l) >=~ (Default r) = l `derivedSymGe` r
  (Default l) >~ (Default r) = l `derivedSymGt` r
  symCompare (Default l) (Default r) = derivedSymCompare l r

#define CONCRETE_SORD(type) \
instance (SymBoolOp bool) => SOrd bool type where \
  l <=~ r = conc $ l <= r; \
  l <~ r = conc $ l < r; \
  l >=~ r = conc $ l >= r; \
  l >~ r = conc $ l > r; \
  symCompare l r = mrgSingle $ compare l r

CONCRETE_SORD (Bool)
CONCRETE_SORD (Integer)
CONCRETE_SORD (Char)
CONCRETE_SORD (Int)
CONCRETE_SORD (Int8)
CONCRETE_SORD (Int16)
CONCRETE_SORD (Int32)
CONCRETE_SORD (Int64)
CONCRETE_SORD (Word)
CONCRETE_SORD (Word8)
CONCRETE_SORD (Word16)
CONCRETE_SORD (Word32)
CONCRETE_SORD (Word64)
CONCRETE_SORD (B.ByteString)

symCompareSingleList :: (SymBoolOp bool, SOrd bool a) => Bool -> Bool -> [a] -> [a] -> bool
symCompareSingleList isLess isStrict = go
  where
    go [] [] = conc (not isStrict)
    go (x : xs) (y : ys) = (if isLess then x <~ y else x >~ y) ||~ (x ==~ y &&~ go xs ys)
    go [] _ = if isLess then conc True else conc False
    go _ [] = if isLess then conc False else conc True

symCompareList :: (SymBoolOp bool, SOrd bool a, UnionLike bool u, Monad u) => [a] -> [a] -> u Ordering
symCompareList [] [] = mrgSingle EQ
symCompareList (x : xs) (y : ys) = do
  oxy <- symCompare x y
  case oxy of
    LT -> mrgSingle LT
    EQ -> symCompareList xs ys
    GT -> mrgSingle GT
symCompareList [] _ = mrgSingle LT
symCompareList _ [] = mrgSingle GT

instance (SymBoolOp bool, SOrd bool a) => SOrd bool [a] where
  (<=~) = symCompareSingleList True False
  (<~) = symCompareSingleList True True
  (>=~) = symCompareSingleList False False
  (>~) = symCompareSingleList False True
  symCompare = symCompareList

deriving via (Default (Maybe a)) instance (SymBoolOp bool, SOrd bool a) => SOrd bool (Maybe a)

deriving via (Default (Either a b)) instance (SymBoolOp bool, SOrd bool a, SOrd bool b) => SOrd bool (Either a b)

deriving via (Default ()) instance (SymBoolOp bool) => SOrd bool ()

deriving via (Default (a, b)) instance (SymBoolOp bool, SOrd bool a, SOrd bool b) => SOrd bool (a, b)

deriving via (Default (a, b, c)) instance (SymBoolOp bool, SOrd bool a, SOrd bool b, SOrd bool c) => SOrd bool (a, b, c)

deriving via
  (Default (a, b, c, d))
  instance
    (SymBoolOp bool, SOrd bool a, SOrd bool b, SOrd bool c, SOrd bool d) =>
    SOrd bool (a, b, c, d)

deriving via
  (Default (a, b, c, d, e))
  instance
    (SymBoolOp bool, SOrd bool a, SOrd bool b, SOrd bool c, SOrd bool d, SOrd bool e) =>
    SOrd bool (a, b, c, d, e)

deriving via
  (Default (a, b, c, d, e, f))
  instance
    (SymBoolOp bool, SOrd bool a, SOrd bool b, SOrd bool c, SOrd bool d, SOrd bool e, SOrd bool f) =>
    SOrd bool (a, b, c, d, e, f)

deriving via
  (Default (a, b, c, d, e, f, g))
  instance
    (SymBoolOp bool, SOrd bool a, SOrd bool b, SOrd bool c, SOrd bool d, SOrd bool e, SOrd bool f, SOrd bool g) =>
    SOrd bool (a, b, c, d, e, f, g)

deriving via
  (Default (a, b, c, d, e, f, g, h))
  instance
    ( SymBoolOp bool,
      SOrd bool a,
      SOrd bool b,
      SOrd bool c,
      SOrd bool d,
      SOrd bool e,
      SOrd bool f,
      SOrd bool g,
      SOrd bool h
    ) =>
    SOrd bool (a, b, c, d, e, f, g, h)

deriving via
  (Default (Sum f g a))
  instance
    (SymBoolOp bool, SOrd bool (f a), SOrd bool (g a)) => SOrd bool (Sum f g a)

instance (SymBoolOp bool, SOrd bool (m (Maybe a))) => SOrd bool (MaybeT m a) where
  (MaybeT l) <=~ (MaybeT r) = l <=~ r
  (MaybeT l) <~ (MaybeT r) = l <~ r
  (MaybeT l) >=~ (MaybeT r) = l >=~ r
  (MaybeT l) >~ (MaybeT r) = l >~ r
  symCompare (MaybeT l) (MaybeT r) = symCompare l r

instance (SymBoolOp bool, SOrd bool (m (Either e a))) => SOrd bool (ExceptT e m a) where
  (ExceptT l) <=~ (ExceptT r) = l <=~ r
  (ExceptT l) <~ (ExceptT r) = l <~ r
  (ExceptT l) >=~ (ExceptT r) = l >=~ r
  (ExceptT l) >~ (ExceptT r) = l >~ r
  symCompare (ExceptT l) (ExceptT r) = symCompare l r

instance (SymBoolOp bool, SOrd bool (m (a, s))) => SOrd bool (WriterLazy.WriterT s m a) where
  (WriterLazy.WriterT l) <=~ (WriterLazy.WriterT r) = l <=~ r
  (WriterLazy.WriterT l) <~ (WriterLazy.WriterT r) = l <~ r
  (WriterLazy.WriterT l) >=~ (WriterLazy.WriterT r) = l >=~ r
  (WriterLazy.WriterT l) >~ (WriterLazy.WriterT r) = l >~ r
  symCompare (WriterLazy.WriterT l) (WriterLazy.WriterT r) = symCompare l r

instance (SymBoolOp bool, SOrd bool (m (a, s))) => SOrd bool (WriterStrict.WriterT s m a) where
  (WriterStrict.WriterT l) <=~ (WriterStrict.WriterT r) = l <=~ r
  (WriterStrict.WriterT l) <~ (WriterStrict.WriterT r) = l <~ r
  (WriterStrict.WriterT l) >=~ (WriterStrict.WriterT r) = l >=~ r
  (WriterStrict.WriterT l) >~ (WriterStrict.WriterT r) = l >~ r
  symCompare (WriterStrict.WriterT l) (WriterStrict.WriterT r) = symCompare l r

instance (SymBoolOp bool, SOrd bool a) => SOrd bool (Identity a) where
  (Identity l) <=~ (Identity r) = l <=~ r
  (Identity l) <~ (Identity r) = l <~ r
  (Identity l) >=~ (Identity r) = l >=~ r
  (Identity l) >~ (Identity r) = l >~ r
  (Identity l) `symCompare` (Identity r) = l `symCompare` r

instance (SymBoolOp bool, SOrd bool (m a)) => SOrd bool (IdentityT m a) where
  (IdentityT l) <=~ (IdentityT r) = l <=~ r
  (IdentityT l) <~ (IdentityT r) = l <~ r
  (IdentityT l) >=~ (IdentityT r) = l >=~ r
  (IdentityT l) >~ (IdentityT r) = l >~ r
  (IdentityT l) `symCompare` (IdentityT r) = l `symCompare` r
