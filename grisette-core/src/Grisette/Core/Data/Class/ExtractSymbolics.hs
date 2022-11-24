{-# LANGUAGE CPP #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Grisette.Core.Data.Class.ExtractSymbolics
  ( GExtractSymbolics (..),
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

-- $setup
-- >>> import Grisette.Core
-- >>> import Grisette.IR.SymPrim
-- >>> import Grisette.Lib.Base
-- >>> import Data.HashSet as HashSet
-- >>> import Data.List (sort)

-- | Extracts all the symbolic variables that are transitively contained in the given value.
--
-- >>> gextractSymbolics ("a" :: SymBool) :: SymbolSet
-- SymbolSet {a :: Bool}
--
-- >>> :{
--   sort $ HashSet.toList $ unSymbolSet $
--     gextractSymbolics (mrgIf "a" (mrgReturn ["b"]) (mrgReturn ["c", "d"]) :: UnionM [SymBool]) :: [SomeTypedSymbol]
-- :}
-- [a :: Bool,b :: Bool,c :: Bool,d :: Bool]
class (Monoid symbolSet) => GExtractSymbolics symbolSet a where
  gextractSymbolics :: a -> symbolSet

instance (Generic a, Monoid symbolSet, GExtractSymbolics' symbolSet (Rep a)) => GExtractSymbolics symbolSet (Default a) where
  gextractSymbolics = gextractSymbolics' . from . unDefault

class (Monoid symbolSet) => GExtractSymbolics' symbolSet a where
  gextractSymbolics' :: a c -> symbolSet

instance (Monoid symbolSet) => GExtractSymbolics' symbolSet U1 where
  gextractSymbolics' _ = mempty

instance (Monoid symbolSet, GExtractSymbolics symbolSet c) => GExtractSymbolics' symbolSet (K1 i c) where
  gextractSymbolics' = gextractSymbolics . unK1

instance (Monoid symbolSet, GExtractSymbolics' symbolSet a) => GExtractSymbolics' symbolSet (M1 i c a) where
  gextractSymbolics' = gextractSymbolics' . unM1

instance
  (Monoid symbolSet, GExtractSymbolics' symbolSet a, GExtractSymbolics' symbolSet b) =>
  GExtractSymbolics' symbolSet (a :+: b)
  where
  gextractSymbolics' (L1 l) = gextractSymbolics' l
  gextractSymbolics' (R1 r) = gextractSymbolics' r

instance
  (Monoid symbolSet, GExtractSymbolics' symbolSet a, GExtractSymbolics' symbolSet b) =>
  GExtractSymbolics' symbolSet (a :*: b)
  where
  gextractSymbolics' (l :*: r) = gextractSymbolics' l <> gextractSymbolics' r

-- instances

#define CONCRETE_EXTRACT_SYMBOLICS(type) \
instance (Monoid symbolSet) => GExtractSymbolics symbolSet type where \
  gextractSymbolics _ = mempty

CONCRETE_EXTRACT_SYMBOLICS (Bool)
CONCRETE_EXTRACT_SYMBOLICS (Integer)
CONCRETE_EXTRACT_SYMBOLICS (Char)
CONCRETE_EXTRACT_SYMBOLICS (Int)
CONCRETE_EXTRACT_SYMBOLICS (Int8)
CONCRETE_EXTRACT_SYMBOLICS (Int16)
CONCRETE_EXTRACT_SYMBOLICS (Int32)
CONCRETE_EXTRACT_SYMBOLICS (Int64)
CONCRETE_EXTRACT_SYMBOLICS (Word)
CONCRETE_EXTRACT_SYMBOLICS (Word8)
CONCRETE_EXTRACT_SYMBOLICS (Word16)
CONCRETE_EXTRACT_SYMBOLICS (Word32)
CONCRETE_EXTRACT_SYMBOLICS (Word64)
CONCRETE_EXTRACT_SYMBOLICS (B.ByteString)

-- ()
instance (Monoid symbolSet) => GExtractSymbolics symbolSet () where
  gextractSymbolics _ = mempty

-- Either
deriving via
  (Default (Either a b))
  instance
    (Monoid symbolSet, GExtractSymbolics symbolSet a, GExtractSymbolics symbolSet b) =>
    GExtractSymbolics symbolSet (Either a b)

-- Maybe
deriving via
  (Default (Maybe a))
  instance
    (Monoid symbolSet, GExtractSymbolics symbolSet a) => GExtractSymbolics symbolSet (Maybe a)

-- List
deriving via
  (Default [a])
  instance
    (Monoid symbolSet, GExtractSymbolics symbolSet a) => GExtractSymbolics symbolSet [a]

-- (,)
deriving via
  (Default (a, b))
  instance
    (Monoid symbolSet, GExtractSymbolics symbolSet a, GExtractSymbolics symbolSet b) =>
    GExtractSymbolics symbolSet (a, b)

-- (,,)
deriving via
  (Default (a, b, c))
  instance
    (Monoid symbolSet, GExtractSymbolics symbolSet a, GExtractSymbolics symbolSet b, GExtractSymbolics symbolSet c) =>
    GExtractSymbolics symbolSet (a, b, c)

-- MaybeT
instance (Monoid symbolSet, GExtractSymbolics symbolSet (m (Maybe a))) => GExtractSymbolics symbolSet (MaybeT m a) where
  gextractSymbolics (MaybeT v) = gextractSymbolics v

-- ExceptT
instance
  (Monoid symbolSet, GExtractSymbolics symbolSet (m (Either e a))) =>
  GExtractSymbolics symbolSet (ExceptT e m a)
  where
  gextractSymbolics (ExceptT v) = gextractSymbolics v

-- Sum
deriving via
  (Default (Sum f g a))
  instance
    (Monoid symbolSet, GExtractSymbolics symbolSet (f a), GExtractSymbolics symbolSet (g a)) =>
    GExtractSymbolics symbolSet (Sum f g a)

-- WriterT
instance
  (Monoid symbolSet, GExtractSymbolics symbolSet (m (a, s))) =>
  GExtractSymbolics symbolSet (WriterLazy.WriterT s m a)
  where
  gextractSymbolics (WriterLazy.WriterT f) = gextractSymbolics f

instance
  (Monoid symbolSet, GExtractSymbolics symbolSet (m (a, s))) =>
  GExtractSymbolics symbolSet (WriterStrict.WriterT s m a)
  where
  gextractSymbolics (WriterStrict.WriterT f) = gextractSymbolics f

-- Identity
instance (Monoid symbolSet, GExtractSymbolics symbolSet a) => GExtractSymbolics symbolSet (Identity a) where
  gextractSymbolics (Identity a) = gextractSymbolics a

-- IdentityT
instance (Monoid symbolSet, GExtractSymbolics symbolSet (m a)) => GExtractSymbolics symbolSet (IdentityT m a) where
  gextractSymbolics (IdentityT a) = gextractSymbolics a
