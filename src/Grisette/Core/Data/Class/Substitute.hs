{-# LANGUAGE CPP #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
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

    -- | This module does not contain the implementation for solvable (see "Grisette.Core#solvable")
    -- types, and the examples in this module rely on the implementations in
    -- the [grisette-symir](https://hackage.haskell.org/package/grisette-symir) package.

    -- * Substituting symbolic constants
    SubstituteSym (..),
    SubstituteSym' (..),
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
import Grisette.IR.SymPrim.Data.Prim.InternedTerm.Term
import Grisette.IR.SymPrim.Data.Prim.InternedTerm.TermSubstitution
import Grisette.IR.SymPrim.Data.Prim.InternedTerm.TermUtils
import {-# SOURCE #-} Grisette.IR.SymPrim.Data.SymPrim

-- $setup
-- >>> import Grisette.Core
-- >>> import Grisette.IR.SymPrim

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
-- > data X = ... deriving Generic deriving (SubstituteSym TypedSymbol Sym) via (Default X)
--
-- __Note 2:__ The @typedSymbol@ type is the typed symbol type for the symbolic
-- constants, and the @sym@ type is the solvable type. If you do not need to use
-- an alternative solvable type implementation, and will use the 'TypedSymbol'
-- and 'Sym' types provided by the [grisette-symir](https://hackage.haskell.org/package/grisette-symir) package, you can use the
-- specialized 'SubstituteSym' type synonym for the constraints and use the
-- specialized 'substituteSym' function to write code with fewer type annotations.
-- However, You still need @'SubstituteSym' TypedSymbol Symbol@ for implementing
-- or deriving the type class due to GHC's limitation.
class SubstituteSym a where
  -- Substitute a symbolic constant to some symbolic value
  --
  -- >>> substituteSym "a" ("c" &&~ "d" :: Sym Bool) ["a" &&~ "b" :: Sym Bool, "a"]
  -- [(&& (&& c d) b),(&& c d)]
  substituteSym :: TypedSymbol b -> Sym b -> a -> a

-- | Auxiliary class for 'SubstituteSym' instance derivation
class SubstituteSym' a where
  -- | Auxiliary function for 'substituteSym' derivation
  substituteSym' :: TypedSymbol b -> Sym b -> a c -> a c

instance
  ( Generic a,
    SubstituteSym' (Rep a)
  ) =>
  SubstituteSym (Default a)
  where
  substituteSym sym val = Default . to . substituteSym' sym val . from . unDefault

instance SubstituteSym' U1 where
  substituteSym' _ _ = id

instance SubstituteSym c => SubstituteSym' (K1 i c) where
  substituteSym' sym val (K1 v) = K1 $ substituteSym sym val v

instance SubstituteSym' a => SubstituteSym' (M1 i c a) where
  substituteSym' sym val (M1 v) = M1 $ substituteSym' sym val v

instance (SubstituteSym' a, SubstituteSym' b) => SubstituteSym' (a :+: b) where
  substituteSym' sym val (L1 l) = L1 $ substituteSym' sym val l
  substituteSym' sym val (R1 r) = R1 $ substituteSym' sym val r

instance (SubstituteSym' a, SubstituteSym' b) => SubstituteSym' (a :*: b) where
  substituteSym' sym val (a :*: b) = substituteSym' sym val a :*: substituteSym' sym val b

#define CONCRETE_SUBSTITUTESYM(type) \
instance  SubstituteSym type where \
  substituteSym _ _ = id

#if 1
CONCRETE_SUBSTITUTESYM(Bool)
CONCRETE_SUBSTITUTESYM(Integer)
CONCRETE_SUBSTITUTESYM(Char)
CONCRETE_SUBSTITUTESYM(Int)
CONCRETE_SUBSTITUTESYM(Int8)
CONCRETE_SUBSTITUTESYM(Int16)
CONCRETE_SUBSTITUTESYM(Int32)
CONCRETE_SUBSTITUTESYM(Int64)
CONCRETE_SUBSTITUTESYM(Word)
CONCRETE_SUBSTITUTESYM(Word8)
CONCRETE_SUBSTITUTESYM(Word16)
CONCRETE_SUBSTITUTESYM(Word32)
CONCRETE_SUBSTITUTESYM(Word64)
CONCRETE_SUBSTITUTESYM(B.ByteString)
#endif

instance SubstituteSym () where
  substituteSym _ _ = id

-- Either
deriving via
  (Default (Either a b))
  instance
    ( SubstituteSym a,
      SubstituteSym b
    ) =>
    SubstituteSym (Either a b)

-- Maybe
deriving via (Default (Maybe a)) instance (SubstituteSym a) => SubstituteSym (Maybe a)

-- List
deriving via (Default [a]) instance (SubstituteSym a) => SubstituteSym [a]

-- (,)
deriving via
  (Default (a, b))
  instance
    (SubstituteSym a, SubstituteSym b) =>
    SubstituteSym (a, b)

-- (,,)
deriving via
  (Default (a, b, c))
  instance
    ( SubstituteSym a,
      SubstituteSym b,
      SubstituteSym c
    ) =>
    SubstituteSym (a, b, c)

-- (,,,)
deriving via
  (Default (a, b, c, d))
  instance
    ( SubstituteSym a,
      SubstituteSym b,
      SubstituteSym c,
      SubstituteSym d
    ) =>
    SubstituteSym (a, b, c, d)

-- (,,,,)
deriving via
  (Default (a, b, c, d, e))
  instance
    ( SubstituteSym a,
      SubstituteSym b,
      SubstituteSym c,
      SubstituteSym d,
      SubstituteSym e
    ) =>
    SubstituteSym (a, b, c, d, e)

-- (,,,,,)
deriving via
  (Default (a, b, c, d, e, f))
  instance
    ( SubstituteSym a,
      SubstituteSym b,
      SubstituteSym c,
      SubstituteSym d,
      SubstituteSym e,
      SubstituteSym f
    ) =>
    SubstituteSym (a, b, c, d, e, f)

-- (,,,,,,)
deriving via
  (Default (a, b, c, d, e, f, g))
  instance
    ( SubstituteSym a,
      SubstituteSym b,
      SubstituteSym c,
      SubstituteSym d,
      SubstituteSym e,
      SubstituteSym f,
      SubstituteSym g
    ) =>
    SubstituteSym (a, b, c, d, e, f, g)

-- (,,,,,,,)
deriving via
  (Default (a, b, c, d, e, f, g, h))
  instance
    ( SubstituteSym a,
      SubstituteSym b,
      SubstituteSym c,
      SubstituteSym d,
      SubstituteSym e,
      SubstituteSym f,
      SubstituteSym g,
      SubstituteSym h
    ) =>
    SubstituteSym ((,,,,,,,) a b c d e f g h)

-- MaybeT
instance
  (SubstituteSym (m (Maybe a))) =>
  SubstituteSym (MaybeT m a)
  where
  substituteSym sym val (MaybeT v) = MaybeT $ substituteSym sym val v

-- ExceptT
instance
  (SubstituteSym (m (Either e a))) =>
  SubstituteSym (ExceptT e m a)
  where
  substituteSym sym val (ExceptT v) = ExceptT $ substituteSym sym val v

-- Sum
deriving via
  (Default (Sum f g a))
  instance
    (SubstituteSym (f a), SubstituteSym (g a)) =>
    SubstituteSym (Sum f g a)

-- WriterT
instance
  (SubstituteSym (m (a, s))) =>
  SubstituteSym (WriterLazy.WriterT s m a)
  where
  substituteSym sym val (WriterLazy.WriterT v) = WriterLazy.WriterT $ substituteSym sym val v

instance
  (SubstituteSym (m (a, s))) =>
  SubstituteSym (WriterStrict.WriterT s m a)
  where
  substituteSym sym val (WriterStrict.WriterT v) = WriterStrict.WriterT $ substituteSym sym val v

-- Identity
instance SubstituteSym a => SubstituteSym (Identity a) where
  substituteSym sym val (Identity a) = Identity $ substituteSym sym val a

-- IdentityT
instance SubstituteSym (m a) => SubstituteSym (IdentityT m a) where
  substituteSym sym val (IdentityT a) = IdentityT $ substituteSym sym val a

instance SubstituteSym (Sym a) where
  substituteSym sym (Sym val) (Sym x) =
    introSupportedPrimConstraint val $
      introSupportedPrimConstraint x $
        Sym $
          substTerm sym val x