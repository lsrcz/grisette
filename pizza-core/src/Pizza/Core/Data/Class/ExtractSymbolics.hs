{-# LANGUAGE CPP #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Pizza.Core.Data.Class.ExtractSymbolics
  ( ExtractSymbolics (..),
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
-- >>> import Pizza.Core
-- >>> import Pizza.IR.SymPrim
-- >>> import Pizza.Lib.Base
-- >>> import Data.HashSet as HashSet
-- >>> import Data.List (sort)

-- | Extracts all the symbolic variables that are transitively contained in the given value.
--
-- >>> extractSymbolics ("a" :: SymBool) :: HashSet TermSymbol
-- fromList [a :: Bool]
--
-- >>> :{
--   sort $
--     HashSet.toList $
--       extractSymbolics (mrgIf "a" (mrgReturn ["b"]) (mrgReturn ["c", "d"]) :: UnionM [SymBool]) :: [TermSymbol]
-- :}
-- [a :: Bool,b :: Bool,c :: Bool,d :: Bool]
class (Monoid symbolSet) => ExtractSymbolics symbolSet a where
  extractSymbolics :: a -> symbolSet

instance (Generic a, Monoid symbolSet, ExtractSymbolics' symbolSet (Rep a)) => ExtractSymbolics symbolSet (Default a) where
  extractSymbolics = extractSymbolics' . from . unDefault

class (Monoid symbolSet) => ExtractSymbolics' symbolSet a where
  extractSymbolics' :: a c -> symbolSet

instance (Monoid symbolSet) => ExtractSymbolics' symbolSet U1 where
  extractSymbolics' _ = mempty

instance (Monoid symbolSet, ExtractSymbolics symbolSet c) => ExtractSymbolics' symbolSet (K1 i c) where
  extractSymbolics' = extractSymbolics . unK1

instance (Monoid symbolSet, ExtractSymbolics' symbolSet a) => ExtractSymbolics' symbolSet (M1 i c a) where
  extractSymbolics' = extractSymbolics' . unM1

instance
  (Monoid symbolSet, ExtractSymbolics' symbolSet a, ExtractSymbolics' symbolSet b) =>
  ExtractSymbolics' symbolSet (a :+: b)
  where
  extractSymbolics' (L1 l) = extractSymbolics' l
  extractSymbolics' (R1 r) = extractSymbolics' r

instance
  (Monoid symbolSet, ExtractSymbolics' symbolSet a, ExtractSymbolics' symbolSet b) =>
  ExtractSymbolics' symbolSet (a :*: b)
  where
  extractSymbolics' (l :*: r) = extractSymbolics' l <> extractSymbolics' r

-- instances

#define CONCRETE_EXTRACT_SYMBOLICS(type) \
instance (Monoid symbolSet) => ExtractSymbolics symbolSet type where \
  extractSymbolics _ = mempty

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
instance (Monoid symbolSet) => ExtractSymbolics symbolSet () where
  extractSymbolics _ = mempty

-- Either
deriving via
  (Default (Either a b))
  instance
    (Monoid symbolSet, ExtractSymbolics symbolSet a, ExtractSymbolics symbolSet b) =>
    ExtractSymbolics symbolSet (Either a b)

-- Maybe
deriving via
  (Default (Maybe a))
  instance
    (Monoid symbolSet, ExtractSymbolics symbolSet a) => ExtractSymbolics symbolSet (Maybe a)

-- List
deriving via
  (Default [a])
  instance
    (Monoid symbolSet, ExtractSymbolics symbolSet a) => ExtractSymbolics symbolSet [a]

-- (,)
deriving via
  (Default (a, b))
  instance
    (Monoid symbolSet, ExtractSymbolics symbolSet a, ExtractSymbolics symbolSet b) =>
    ExtractSymbolics symbolSet (a, b)

-- (,,)
deriving via
  (Default (a, b, c))
  instance
    (Monoid symbolSet, ExtractSymbolics symbolSet a, ExtractSymbolics symbolSet b, ExtractSymbolics symbolSet c) =>
    ExtractSymbolics symbolSet (a, b, c)

-- MaybeT
instance (Monoid symbolSet, ExtractSymbolics symbolSet (m (Maybe a))) => ExtractSymbolics symbolSet (MaybeT m a) where
  extractSymbolics (MaybeT v) = extractSymbolics v

-- ExceptT
instance
  (Monoid symbolSet, ExtractSymbolics symbolSet (m (Either e a))) =>
  ExtractSymbolics symbolSet (ExceptT e m a)
  where
  extractSymbolics (ExceptT v) = extractSymbolics v

-- Sum
deriving via
  (Default (Sum f g a))
  instance
    (Monoid symbolSet, ExtractSymbolics symbolSet (f a), ExtractSymbolics symbolSet (g a)) =>
    ExtractSymbolics symbolSet (Sum f g a)

-- WriterT
instance
  (Monoid symbolSet, ExtractSymbolics symbolSet (m (a, s))) =>
  ExtractSymbolics symbolSet (WriterLazy.WriterT s m a)
  where
  extractSymbolics (WriterLazy.WriterT f) = extractSymbolics f

instance
  (Monoid symbolSet, ExtractSymbolics symbolSet (m (a, s))) =>
  ExtractSymbolics symbolSet (WriterStrict.WriterT s m a)
  where
  extractSymbolics (WriterStrict.WriterT f) = extractSymbolics f

-- Identity
instance (Monoid symbolSet, ExtractSymbolics symbolSet a) => ExtractSymbolics symbolSet (Identity a) where
  extractSymbolics (Identity a) = extractSymbolics a

-- IdentityT
instance (Monoid symbolSet, ExtractSymbolics symbolSet (m a)) => ExtractSymbolics symbolSet (IdentityT m a) where
  extractSymbolics (IdentityT a) = extractSymbolics a
