{-# LANGUAGE CPP #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      :   Grisette.Core.Data.Class.Substitute
-- Copyright   :   (c) Sirui Lu 2021-2022
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Core.Data.Class.Substitute
  ( -- * Note for the examples

    --

    -- | This module does not contain the implementation for symbolic primitive
    -- types, and the examples in this module rely on the implementations in
    -- the [grisette-symir](https://hackage.haskell.org/package/grisette-symir) package.

    -- * Substituting symbolic constants
    GSubstituteSymSymbol (..),
    GSubstituteSym (..),
    GSubstituteSym' (..),
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
import Generics.Deriving.Instances ()

-- $setup
-- >>> import Grisette.Core
-- >>> import Grisette.IR.SymPrim

-- | An empty type class to link the typed symbol and the solvable type.
-- This is used to help Haskell compiler with functional dependency.
class GSubstituteSymSymbol (typedSymbol :: * -> *) (sym :: * -> *) | sym -> typedSymbol

-- | Substitution of symbolic constants.
--
-- >>> a = "a" :: TypedSymbol Bool
-- >>> v = "x" &&~ "y" :: SymBool
-- >>> substituteSym a v (["a" &&~ "b", "a"] :: [SymBool])
-- [(&& (&& x y) b),(&& x y)]
--
-- __Note 1:__ This type class can be derived for algebraic data types.
-- You may need the @DerivingVia@ and @DerivingStrategies@ extensions.
--
-- > data X = ... deriving Generic deriving (GSubstituteSym TypedSymbol Sym) via (Default X)
--
-- __Note 2:__ The @typedSymbol@ type is the typed symbol type for the symbolic
-- constants, and the @sym@ type is the solvable type. If you do not need to use
-- an alternative solvable type implementation, and will use the 'TypedSymbol'
-- and 'Sym' types provided by the [grisette-symir](https://hackage.haskell.org/package/grisette-symir) package, you can use the
-- specialized 'SubstituteSym' type synonym for the constraints and use the
-- specialized 'substituteSym' function to write code with fewer type annotations.
-- However, You still need @'GSubstituteSym' TypedSymbol Symbol@ for implementing
-- or deriving the type class due to GHC's limitation.
class GSubstituteSymSymbol typedSymbol sym => GSubstituteSym typedSymbol sym a | sym -> typedSymbol where
  -- Substitute a symbolic constant to some symbolic value
  --
  -- >>> gsubstituteSym "a" ("c" &&~ "d" :: Sym Bool) ["a" &&~ "b" :: Sym Bool, "a"]
  -- [(&& (&& c d) b),(&& c d)]
  gsubstituteSym :: typedSymbol b -> sym b -> a -> a

-- | Auxiliary class for 'GSubstituteSym' instance derivation
class GSubstituteSym' typedSymbol sym a | sym -> typedSymbol where
  -- | Auxiliary function for 'gsubstituteSym' derivation
  gsubstituteSym' :: typedSymbol b -> sym b -> a c -> a c

instance
  ( Generic a,
    GSubstituteSymSymbol typedSymbol sym,
    GSubstituteSym' typedSymbol sym (Rep a)
  ) =>
  GSubstituteSym typedSymbol sym (Default a)
  where
  gsubstituteSym sym val = Default . to . gsubstituteSym' sym val . from . unDefault

instance GSubstituteSymSymbol typedSymbol sym => GSubstituteSym' typedSymbol sym U1 where
  gsubstituteSym' _ _ = id

instance GSubstituteSym typedSymbol sym c => GSubstituteSym' typedSymbol sym (K1 i c) where
  gsubstituteSym' sym val (K1 v) = K1 $ gsubstituteSym sym val v

instance GSubstituteSym' typedSymbol sym a => GSubstituteSym' typedSymbol sym (M1 i c a) where
  gsubstituteSym' sym val (M1 v) = M1 $ gsubstituteSym' sym val v

instance (GSubstituteSym' typedSymbol sym a, GSubstituteSym' typedSymbol sym b) => GSubstituteSym' typedSymbol sym (a :+: b) where
  gsubstituteSym' sym val (L1 l) = L1 $ gsubstituteSym' sym val l
  gsubstituteSym' sym val (R1 r) = R1 $ gsubstituteSym' sym val r

instance (GSubstituteSym' typedSymbol sym a, GSubstituteSym' typedSymbol sym b) => GSubstituteSym' typedSymbol sym (a :*: b) where
  gsubstituteSym' sym val (a :*: b) = gsubstituteSym' sym val a :*: gsubstituteSym' sym val b

#define CONCRETE_SUBSTITUTESYM(type) \
instance GSubstituteSymSymbol typedSymbol sym => GSubstituteSym typedSymbol sym type where \
  gsubstituteSym _ _ = id

CONCRETE_SUBSTITUTESYM (Bool)
CONCRETE_SUBSTITUTESYM (Integer)
CONCRETE_SUBSTITUTESYM (Char)
CONCRETE_SUBSTITUTESYM (Int)
CONCRETE_SUBSTITUTESYM (Int8)
CONCRETE_SUBSTITUTESYM (Int16)
CONCRETE_SUBSTITUTESYM (Int32)
CONCRETE_SUBSTITUTESYM (Int64)
CONCRETE_SUBSTITUTESYM (Word)
CONCRETE_SUBSTITUTESYM (Word8)
CONCRETE_SUBSTITUTESYM (Word16)
CONCRETE_SUBSTITUTESYM (Word32)
CONCRETE_SUBSTITUTESYM (Word64)
CONCRETE_SUBSTITUTESYM (B.ByteString)

instance GSubstituteSymSymbol typedSymbol sym => GSubstituteSym typedSymbol sym () where
  gsubstituteSym _ _ = id

-- Either
deriving via
  (Default (Either a b))
  instance
    ( GSubstituteSym typedSymbol sym a,
      GSubstituteSym typedSymbol sym b
    ) =>
    GSubstituteSym typedSymbol sym (Either a b)

-- Maybe
deriving via (Default (Maybe a)) instance (GSubstituteSym typedSymbol sym a) => GSubstituteSym typedSymbol sym (Maybe a)

-- List
deriving via (Default [a]) instance (GSubstituteSym typedSymbol sym a) => GSubstituteSym typedSymbol sym [a]

-- (,)
deriving via
  (Default (a, b))
  instance
    (GSubstituteSym typedSymbol sym a, GSubstituteSym typedSymbol sym b) =>
    GSubstituteSym typedSymbol sym (a, b)

-- (,,)
deriving via
  (Default (a, b, c))
  instance
    ( GSubstituteSym typedSymbol sym a,
      GSubstituteSym typedSymbol sym b,
      GSubstituteSym typedSymbol sym c
    ) =>
    GSubstituteSym typedSymbol sym (a, b, c)

-- (,,,)
deriving via
  (Default (a, b, c, d))
  instance
    ( GSubstituteSym typedSymbol sym a,
      GSubstituteSym typedSymbol sym b,
      GSubstituteSym typedSymbol sym c,
      GSubstituteSym typedSymbol sym d
    ) =>
    GSubstituteSym typedSymbol sym (a, b, c, d)

-- (,,,,)
deriving via
  (Default (a, b, c, d, e))
  instance
    ( GSubstituteSym typedSymbol sym a,
      GSubstituteSym typedSymbol sym b,
      GSubstituteSym typedSymbol sym c,
      GSubstituteSym typedSymbol sym d,
      GSubstituteSym typedSymbol sym e
    ) =>
    GSubstituteSym typedSymbol sym (a, b, c, d, e)

-- (,,,,,)
deriving via
  (Default (a, b, c, d, e, f))
  instance
    ( GSubstituteSym typedSymbol sym a,
      GSubstituteSym typedSymbol sym b,
      GSubstituteSym typedSymbol sym c,
      GSubstituteSym typedSymbol sym d,
      GSubstituteSym typedSymbol sym e,
      GSubstituteSym typedSymbol sym f
    ) =>
    GSubstituteSym typedSymbol sym (a, b, c, d, e, f)

-- (,,,,,,)
deriving via
  (Default (a, b, c, d, e, f, g))
  instance
    ( GSubstituteSym typedSymbol sym a,
      GSubstituteSym typedSymbol sym b,
      GSubstituteSym typedSymbol sym c,
      GSubstituteSym typedSymbol sym d,
      GSubstituteSym typedSymbol sym e,
      GSubstituteSym typedSymbol sym f,
      GSubstituteSym typedSymbol sym g
    ) =>
    GSubstituteSym typedSymbol sym (a, b, c, d, e, f, g)

-- (,,,,,,,)
deriving via
  (Default (a, b, c, d, e, f, g, h))
  instance
    ( GSubstituteSym typedSymbol sym a,
      GSubstituteSym typedSymbol sym b,
      GSubstituteSym typedSymbol sym c,
      GSubstituteSym typedSymbol sym d,
      GSubstituteSym typedSymbol sym e,
      GSubstituteSym typedSymbol sym f,
      GSubstituteSym typedSymbol sym g,
      GSubstituteSym typedSymbol sym h
    ) =>
    GSubstituteSym typedSymbol sym ((,,,,,,,) a b c d e f g h)

-- MaybeT
instance
  (GSubstituteSymSymbol typedSymbol sym, GSubstituteSym typedSymbol sym (m (Maybe a))) =>
  GSubstituteSym typedSymbol sym (MaybeT m a)
  where
  gsubstituteSym sym val (MaybeT v) = MaybeT $ gsubstituteSym sym val v

-- ExceptT
instance
  (GSubstituteSymSymbol typedSymbol sym, GSubstituteSym typedSymbol sym (m (Either e a))) =>
  GSubstituteSym typedSymbol sym (ExceptT e m a)
  where
  gsubstituteSym sym val (ExceptT v) = ExceptT $ gsubstituteSym sym val v

-- Sum
deriving via
  (Default (Sum f g a))
  instance
    (GSubstituteSym typedSymbol sym (f a), GSubstituteSym typedSymbol sym (g a)) =>
    GSubstituteSym typedSymbol sym (Sum f g a)

-- WriterT
instance
  (GSubstituteSymSymbol typedSymbol sym, GSubstituteSym typedSymbol sym (m (a, s))) =>
  GSubstituteSym typedSymbol sym (WriterLazy.WriterT s m a)
  where
  gsubstituteSym sym val (WriterLazy.WriterT v) = WriterLazy.WriterT $ gsubstituteSym sym val v

instance
  (GSubstituteSymSymbol typedSymbol sym, GSubstituteSym typedSymbol sym (m (a, s))) =>
  GSubstituteSym typedSymbol sym (WriterStrict.WriterT s m a)
  where
  gsubstituteSym sym val (WriterStrict.WriterT v) = WriterStrict.WriterT $ gsubstituteSym sym val v

-- Identity
instance GSubstituteSym typedSymbol sym a => GSubstituteSym typedSymbol sym (Identity a) where
  gsubstituteSym sym val (Identity a) = Identity $ gsubstituteSym sym val a

-- IdentityT
instance GSubstituteSym typedSymbol sym (m a) => GSubstituteSym typedSymbol sym (IdentityT m a) where
  gsubstituteSym sym val (IdentityT a) = IdentityT $ gsubstituteSym sym val a
