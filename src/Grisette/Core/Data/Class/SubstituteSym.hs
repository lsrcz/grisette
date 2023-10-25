{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      :   Grisette.Core.Data.Class.SubstituteSym
-- Copyright   :   (c) Sirui Lu 2021-2023
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Core.Data.Class.SubstituteSym
  ( -- * Substituting symbolic constants
    SubstituteSym (..),
    SubstituteSym' (..),
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
    Generic (Rep, from, to),
    K1 (K1),
    M1 (M1),
    U1,
    type (:*:) ((:*:)),
    type (:+:) (L1, R1),
  )
import Generics.Deriving.Instances ()
import Grisette.Core.Data.BV (IntN, SomeIntN, SomeWordN, WordN)
import Grisette.IR.SymPrim.Data.Prim.InternedTerm.Term
  ( LinkedRep (underlyingTerm),
    SupportedPrim,
    TypedSymbol,
  )
import Grisette.IR.SymPrim.Data.Prim.InternedTerm.TermSubstitution (substTerm)
import Grisette.IR.SymPrim.Data.SymPrim
  ( SomeSymIntN (SomeSymIntN),
    SomeSymWordN (SomeSymWordN),
    SymBool (SymBool),
    SymIntN (SymIntN),
    SymInteger (SymInteger),
    SymWordN (SymWordN),
    type (-~>) (SymGeneralFun),
    type (=~>) (SymTabularFun),
  )

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
-- > data X = ... deriving Generic deriving SubstituteSym via (Default X)
class SubstituteSym a where
  -- Substitute a symbolic constant to some symbolic value
  --
  -- >>> substituteSym "a" ("c" &&~ "d" :: Sym Bool) ["a" &&~ "b" :: Sym Bool, "a"]
  -- [(&& (&& c d) b),(&& c d)]
  substituteSym :: (LinkedRep cb sb) => TypedSymbol cb -> sb -> a -> a

#define CONCRETE_SUBSTITUTESYM(type) \
instance SubstituteSym type where \
  substituteSym _ _ = id

#define CONCRETE_SUBSTITUTESYM_BV(type) \
instance (KnownNat n, 1 <= n) => SubstituteSym (type n) where \
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
CONCRETE_SUBSTITUTESYM(SomeWordN)
CONCRETE_SUBSTITUTESYM(SomeIntN)
CONCRETE_SUBSTITUTESYM(B.ByteString)
CONCRETE_SUBSTITUTESYM(T.Text)
CONCRETE_SUBSTITUTESYM_BV(WordN)
CONCRETE_SUBSTITUTESYM_BV(IntN)
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
instance (SubstituteSym a) => SubstituteSym (Identity a) where
  substituteSym sym val (Identity a) = Identity $ substituteSym sym val a

-- IdentityT
instance (SubstituteSym (m a)) => SubstituteSym (IdentityT m a) where
  substituteSym sym val (IdentityT a) = IdentityT $ substituteSym sym val a

#define SUBSTITUTE_SYM_SIMPLE(symtype) \
instance SubstituteSym symtype where \
  substituteSym sym v (symtype t) = symtype $ substTerm sym (underlyingTerm v) t

#define SUBSTITUTE_SYM_BV(symtype) \
instance (KnownNat n, 1 <= n) => SubstituteSym (symtype n) where \
  substituteSym sym v (symtype t) = symtype $ substTerm sym (underlyingTerm v) t

#define SUBSTITUTE_SYM_FUN(op, cons) \
instance (SupportedPrim ca, SupportedPrim cb, LinkedRep ca sa, LinkedRep cb sb) => SubstituteSym (sa op sb) where \
  substituteSym sym v (cons t) = cons $ substTerm sym (underlyingTerm v) t

#define SUBSTITUTE_SYM_BV_SOME(somety, origty) \
instance SubstituteSym somety where \
  substituteSym sym v (somety (origty t)) = somety $ origty $ substTerm sym (underlyingTerm v) t

#if 1
SUBSTITUTE_SYM_SIMPLE(SymBool)
SUBSTITUTE_SYM_SIMPLE(SymInteger)
SUBSTITUTE_SYM_BV(SymIntN)
SUBSTITUTE_SYM_BV(SymWordN)
SUBSTITUTE_SYM_FUN(=~>, SymTabularFun)
SUBSTITUTE_SYM_FUN(-~>, SymGeneralFun)
SUBSTITUTE_SYM_BV_SOME(SomeSymIntN, SymIntN)
SUBSTITUTE_SYM_BV_SOME(SomeSymWordN, SymWordN)
#endif

-- | Auxiliary class for 'SubstituteSym' instance derivation
class SubstituteSym' a where
  -- | Auxiliary function for 'substituteSym' derivation
  substituteSym' :: (LinkedRep cb sb) => TypedSymbol cb -> sb -> a c -> a c

instance
  ( Generic a,
    SubstituteSym' (Rep a)
  ) =>
  SubstituteSym (Default a)
  where
  substituteSym sym val = Default . to . substituteSym' sym val . from . unDefault

instance SubstituteSym' U1 where
  substituteSym' _ _ = id

instance (SubstituteSym c) => SubstituteSym' (K1 i c) where
  substituteSym' sym val (K1 v) = K1 $ substituteSym sym val v

instance (SubstituteSym' a) => SubstituteSym' (M1 i c a) where
  substituteSym' sym val (M1 v) = M1 $ substituteSym' sym val v

instance (SubstituteSym' a, SubstituteSym' b) => SubstituteSym' (a :+: b) where
  substituteSym' sym val (L1 l) = L1 $ substituteSym' sym val l
  substituteSym' sym val (R1 r) = R1 $ substituteSym' sym val r

instance (SubstituteSym' a, SubstituteSym' b) => SubstituteSym' (a :*: b) where
  substituteSym' sym val (a :*: b) = substituteSym' sym val a :*: substituteSym' sym val b
