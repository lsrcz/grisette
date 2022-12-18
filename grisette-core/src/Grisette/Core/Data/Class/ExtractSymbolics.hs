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
-- Module      :   Grisette.Core.Data.Class.ExtractSymbolics
-- Copyright   :   (c) Sirui Lu 2021-2022
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Core.Data.Class.ExtractSymbolics
  ( -- * Note for the examples

    --

    -- | This module does not contain the implementation for solvable (see "Grisette.Core#solvable")
    -- types, and the examples in this module rely on the implementations in
    -- the [grisette-symir](https://hackage.haskell.org/package/grisette-symir) package.

    -- * Extracting symbolic constant set from a value
    GExtractSymbolics (..),
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
-- >>> gextractSymbolics (mrgIf "a" (mrgReturn ["b"]) (mrgReturn ["c", "d"]) :: UnionM [SymBool]) :: SymbolSet
-- SymbolSet {a :: Bool, b :: Bool, c :: Bool, d :: Bool}
--
-- __Note 1:__ This type class can be derived for algebraic data types.
-- You may need the @DerivingVia@ and @DerivingStrategies@ extensions.
--
-- > data X = ... deriving Generic deriving (GExtractSymbolics SymBool) via (Default X)
--
-- __Note 2:__ The @symbolSet@ type is the symbolic constant set type for the
-- solver backend. It should be an instance of `Grisette.Core.Data.Class.ModelOps.SymbolSetOps`. If you do not need
-- to use an alternative solver backend, and will use the 'SymbolSet' type
-- provided by the [grisette-symir](https://hackage.haskell.org/package/grisette-symir) package, you can use the specialized
-- `ExtractSymbolics` type synonym for the constraints and use specialized
-- `extractSymbolics` combinator from [grisette-symir](https://hackage.haskell.org/package/grisette-symir) to write code with fewer
-- type annotations.
-- However, You still need @'GMergeable' SymBool@ for implementing or deriving the
-- type class due to GHC's limitation.
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
