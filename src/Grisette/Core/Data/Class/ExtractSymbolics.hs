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
-- Module      :   Grisette.Core.Data.Class.ExtractSymbolics
-- Copyright   :   (c) Sirui Lu 2021-2023
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Core.Data.Class.ExtractSymbolics
  ( -- * Extracting symbolic constant set from a value
    ExtractSymbolics (..),
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
  ( Default (Default, unDefault),
    Generic (Rep, from),
    K1 (unK1),
    M1 (unM1),
    U1,
    type (:*:) ((:*:)),
    type (:+:) (L1, R1),
  )
import Grisette.Core.Data.BV (IntN, SomeIntN, SomeWordN, WordN)
import {-# SOURCE #-} Grisette.IR.SymPrim.Data.Prim.Model
  ( SymbolSet,
  )

-- $setup
-- >>> import Grisette.Core
-- >>> import Grisette.IR.SymPrim
-- >>> import Grisette.Lib.Base
-- >>> import Data.HashSet as HashSet
-- >>> import Data.List (sort)

-- | Extracts all the symbolic variables that are transitively contained in the given value.
--
-- >>> extractSymbolics ("a" :: SymBool) :: SymbolSet
-- SymbolSet {a :: Bool}
--
-- >>> extractSymbolics (mrgIf "a" (mrgReturn ["b"]) (mrgReturn ["c", "d"]) :: UnionM [SymBool]) :: SymbolSet
-- SymbolSet {a :: Bool, b :: Bool, c :: Bool, d :: Bool}
--
-- __Note 1:__ This type class can be derived for algebraic data types.
-- You may need the @DerivingVia@ and @DerivingStrategies@ extensions.
--
-- > data X = ... deriving Generic deriving ExtractSymbolics via (Default X)
class ExtractSymbolics a where
  extractSymbolics :: a -> SymbolSet

instance (Generic a, ExtractSymbolics' (Rep a)) => ExtractSymbolics (Default a) where
  extractSymbolics = extractSymbolics' . from . unDefault

class ExtractSymbolics' a where
  extractSymbolics' :: a c -> SymbolSet

instance ExtractSymbolics' U1 where
  extractSymbolics' _ = mempty

instance (ExtractSymbolics c) => ExtractSymbolics' (K1 i c) where
  extractSymbolics' = extractSymbolics . unK1

instance (ExtractSymbolics' a) => ExtractSymbolics' (M1 i c a) where
  extractSymbolics' = extractSymbolics' . unM1

instance
  (ExtractSymbolics' a, ExtractSymbolics' b) =>
  ExtractSymbolics' (a :+: b)
  where
  extractSymbolics' (L1 l) = extractSymbolics' l
  extractSymbolics' (R1 r) = extractSymbolics' r

instance
  (ExtractSymbolics' a, ExtractSymbolics' b) =>
  ExtractSymbolics' (a :*: b)
  where
  extractSymbolics' (l :*: r) = extractSymbolics' l <> extractSymbolics' r

-- instances

#define CONCRETE_EXTRACT_SYMBOLICS(type) \
instance ExtractSymbolics type where \
  extractSymbolics _ = mempty

#define CONCRETE_EXTRACT_SYMBOLICS_BV(type) \
instance (KnownNat n, 1 <= n) => ExtractSymbolics (type n) where \
  extractSymbolics _ = mempty

#if 1
CONCRETE_EXTRACT_SYMBOLICS(Bool)
CONCRETE_EXTRACT_SYMBOLICS(Integer)
CONCRETE_EXTRACT_SYMBOLICS(Char)
CONCRETE_EXTRACT_SYMBOLICS(Int)
CONCRETE_EXTRACT_SYMBOLICS(Int8)
CONCRETE_EXTRACT_SYMBOLICS(Int16)
CONCRETE_EXTRACT_SYMBOLICS(Int32)
CONCRETE_EXTRACT_SYMBOLICS(Int64)
CONCRETE_EXTRACT_SYMBOLICS(Word)
CONCRETE_EXTRACT_SYMBOLICS(Word8)
CONCRETE_EXTRACT_SYMBOLICS(Word16)
CONCRETE_EXTRACT_SYMBOLICS(Word32)
CONCRETE_EXTRACT_SYMBOLICS(Word64)
CONCRETE_EXTRACT_SYMBOLICS(SomeWordN)
CONCRETE_EXTRACT_SYMBOLICS(SomeIntN)
CONCRETE_EXTRACT_SYMBOLICS(B.ByteString)
CONCRETE_EXTRACT_SYMBOLICS(T.Text)
CONCRETE_EXTRACT_SYMBOLICS_BV(WordN)
CONCRETE_EXTRACT_SYMBOLICS_BV(IntN)
#endif

-- ()
instance ExtractSymbolics () where
  extractSymbolics _ = mempty

-- Either
deriving via
  (Default (Either a b))
  instance
    (ExtractSymbolics a, ExtractSymbolics b) =>
    ExtractSymbolics (Either a b)

-- Maybe
deriving via
  (Default (Maybe a))
  instance
    (ExtractSymbolics a) => ExtractSymbolics (Maybe a)

-- List
deriving via
  (Default [a])
  instance
    (ExtractSymbolics a) => ExtractSymbolics [a]

-- (,)
deriving via
  (Default (a, b))
  instance
    (ExtractSymbolics a, ExtractSymbolics b) =>
    ExtractSymbolics (a, b)

-- (,,)
deriving via
  (Default (a, b, c))
  instance
    (ExtractSymbolics a, ExtractSymbolics b, ExtractSymbolics c) =>
    ExtractSymbolics (a, b, c)

-- (,,,)
deriving via
  (Default (a, b, c, d))
  instance
    ( ExtractSymbolics a,
      ExtractSymbolics b,
      ExtractSymbolics c,
      ExtractSymbolics d
    ) =>
    ExtractSymbolics (a, b, c, d)

-- (,,,,)
deriving via
  (Default (a, b, c, d, e))
  instance
    ( ExtractSymbolics a,
      ExtractSymbolics b,
      ExtractSymbolics c,
      ExtractSymbolics d,
      ExtractSymbolics e
    ) =>
    ExtractSymbolics (a, b, c, d, e)

-- (,,,,,)
deriving via
  (Default (a, b, c, d, e, f))
  instance
    ( ExtractSymbolics a,
      ExtractSymbolics b,
      ExtractSymbolics c,
      ExtractSymbolics d,
      ExtractSymbolics e,
      ExtractSymbolics f
    ) =>
    ExtractSymbolics (a, b, c, d, e, f)

-- (,,,,,,)
deriving via
  (Default (a, b, c, d, e, f, g))
  instance
    ( ExtractSymbolics a,
      ExtractSymbolics b,
      ExtractSymbolics c,
      ExtractSymbolics d,
      ExtractSymbolics e,
      ExtractSymbolics f,
      ExtractSymbolics g
    ) =>
    ExtractSymbolics (a, b, c, d, e, f, g)

-- (,,,,,,,)
deriving via
  (Default (a, b, c, d, e, f, g, h))
  instance
    ( ExtractSymbolics a,
      ExtractSymbolics b,
      ExtractSymbolics c,
      ExtractSymbolics d,
      ExtractSymbolics e,
      ExtractSymbolics f,
      ExtractSymbolics g,
      ExtractSymbolics h
    ) =>
    ExtractSymbolics (a, b, c, d, e, f, g, h)

-- MaybeT
instance (ExtractSymbolics (m (Maybe a))) => ExtractSymbolics (MaybeT m a) where
  extractSymbolics (MaybeT v) = extractSymbolics v

-- ExceptT
instance
  (ExtractSymbolics (m (Either e a))) =>
  ExtractSymbolics (ExceptT e m a)
  where
  extractSymbolics (ExceptT v) = extractSymbolics v

-- Sum
deriving via
  (Default (Sum f g a))
  instance
    (ExtractSymbolics (f a), ExtractSymbolics (g a)) =>
    ExtractSymbolics (Sum f g a)

-- WriterT
instance
  (ExtractSymbolics (m (a, s))) =>
  ExtractSymbolics (WriterLazy.WriterT s m a)
  where
  extractSymbolics (WriterLazy.WriterT f) = extractSymbolics f

instance
  (ExtractSymbolics (m (a, s))) =>
  ExtractSymbolics (WriterStrict.WriterT s m a)
  where
  extractSymbolics (WriterStrict.WriterT f) = extractSymbolics f

-- Identity
instance (ExtractSymbolics a) => ExtractSymbolics (Identity a) where
  extractSymbolics (Identity a) = extractSymbolics a

-- IdentityT
instance (ExtractSymbolics (m a)) => ExtractSymbolics (IdentityT m a) where
  extractSymbolics (IdentityT a) = extractSymbolics a
