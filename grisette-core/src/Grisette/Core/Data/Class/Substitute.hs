{-# LANGUAGE CPP #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Grisette.Core.Data.Class.Substitute
  ( SubstituteSymSymbol (..),
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

class SubstituteSymSymbol (typedSymbol :: * -> *) (sym :: * -> *) | sym -> typedSymbol

class SubstituteSymSymbol typedSymbol sym => SubstituteSym typedSymbol sym a | sym -> typedSymbol where
  substituteSym :: typedSymbol b -> sym b -> a -> a

class SubstituteSym' typedSymbol sym a | sym -> typedSymbol where
  substituteSym' :: typedSymbol b -> sym b -> a c -> a c

instance
  ( Generic a,
    SubstituteSymSymbol typedSymbol sym,
    SubstituteSym' typedSymbol sym (Rep a)
  ) =>
  SubstituteSym typedSymbol sym (Default a)
  where
  substituteSym sym val = Default . to . substituteSym' sym val . from . unDefault

instance SubstituteSymSymbol typedSymbol sym => SubstituteSym' typedSymbol sym U1 where
  substituteSym' _ _ = id

instance SubstituteSym typedSymbol sym c => SubstituteSym' typedSymbol sym (K1 i c) where
  substituteSym' sym val (K1 v) = K1 $ substituteSym sym val v

instance SubstituteSym' typedSymbol sym a => SubstituteSym' typedSymbol sym (M1 i c a) where
  substituteSym' sym val (M1 v) = M1 $ substituteSym' sym val v

instance (SubstituteSym' typedSymbol sym a, SubstituteSym' typedSymbol sym b) => SubstituteSym' typedSymbol sym (a :+: b) where
  substituteSym' sym val (L1 l) = L1 $ substituteSym' sym val l
  substituteSym' sym val (R1 r) = R1 $ substituteSym' sym val r

instance (SubstituteSym' typedSymbol sym a, SubstituteSym' typedSymbol sym b) => SubstituteSym' typedSymbol sym (a :*: b) where
  substituteSym' sym val (a :*: b) = substituteSym' sym val a :*: substituteSym' sym val b

#define CONCRETE_SUBSTITUTESYM(type) \
instance SubstituteSymSymbol typedSymbol sym => SubstituteSym typedSymbol sym type where \
  substituteSym _ _ = id

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

instance SubstituteSymSymbol typedSymbol sym => SubstituteSym typedSymbol sym () where
  substituteSym _ _ = id

-- Either
deriving via
  (Default (Either a b))
  instance
    ( SubstituteSym typedSymbol sym a,
      SubstituteSym typedSymbol sym b
    ) =>
    SubstituteSym typedSymbol sym (Either a b)

-- Maybe
deriving via (Default (Maybe a)) instance (SubstituteSym typedSymbol sym a) => SubstituteSym typedSymbol sym (Maybe a)

-- List
deriving via (Default [a]) instance (SubstituteSym typedSymbol sym a) => SubstituteSym typedSymbol sym [a]

-- (,)
deriving via
  (Default (a, b))
  instance
    (SubstituteSym typedSymbol sym a, SubstituteSym typedSymbol sym b) =>
    SubstituteSym typedSymbol sym (a, b)

-- (,,)
deriving via
  (Default (a, b, c))
  instance
    ( SubstituteSym typedSymbol sym a,
      SubstituteSym typedSymbol sym b,
      SubstituteSym typedSymbol sym c
    ) =>
    SubstituteSym typedSymbol sym (a, b, c)

-- (,,,)
deriving via
  (Default (a, b, c, d))
  instance
    ( SubstituteSym typedSymbol sym a,
      SubstituteSym typedSymbol sym b,
      SubstituteSym typedSymbol sym c,
      SubstituteSym typedSymbol sym d
    ) =>
    SubstituteSym typedSymbol sym (a, b, c, d)

-- (,,,,)
deriving via
  (Default (a, b, c, d, e))
  instance
    ( SubstituteSym typedSymbol sym a,
      SubstituteSym typedSymbol sym b,
      SubstituteSym typedSymbol sym c,
      SubstituteSym typedSymbol sym d,
      SubstituteSym typedSymbol sym e
    ) =>
    SubstituteSym typedSymbol sym (a, b, c, d, e)

-- (,,,,,)
deriving via
  (Default (a, b, c, d, e, f))
  instance
    ( SubstituteSym typedSymbol sym a,
      SubstituteSym typedSymbol sym b,
      SubstituteSym typedSymbol sym c,
      SubstituteSym typedSymbol sym d,
      SubstituteSym typedSymbol sym e,
      SubstituteSym typedSymbol sym f
    ) =>
    SubstituteSym typedSymbol sym (a, b, c, d, e, f)

-- (,,,,,,)
deriving via
  (Default (a, b, c, d, e, f, g))
  instance
    ( SubstituteSym typedSymbol sym a,
      SubstituteSym typedSymbol sym b,
      SubstituteSym typedSymbol sym c,
      SubstituteSym typedSymbol sym d,
      SubstituteSym typedSymbol sym e,
      SubstituteSym typedSymbol sym f,
      SubstituteSym typedSymbol sym g
    ) =>
    SubstituteSym typedSymbol sym (a, b, c, d, e, f, g)

-- (,,,,,,,)
deriving via
  (Default (a, b, c, d, e, f, g, h))
  instance
    ( SubstituteSym typedSymbol sym a,
      SubstituteSym typedSymbol sym b,
      SubstituteSym typedSymbol sym c,
      SubstituteSym typedSymbol sym d,
      SubstituteSym typedSymbol sym e,
      SubstituteSym typedSymbol sym f,
      SubstituteSym typedSymbol sym g,
      SubstituteSym typedSymbol sym h
    ) =>
    SubstituteSym typedSymbol sym ((,,,,,,,) a b c d e f g h)

-- MaybeT
instance
  (SubstituteSymSymbol typedSymbol sym, SubstituteSym typedSymbol sym (m (Maybe a))) =>
  SubstituteSym typedSymbol sym (MaybeT m a)
  where
  substituteSym sym val (MaybeT v) = MaybeT $ substituteSym sym val v

-- ExceptT
instance
  (SubstituteSymSymbol typedSymbol sym, SubstituteSym typedSymbol sym (m (Either e a))) =>
  SubstituteSym typedSymbol sym (ExceptT e m a)
  where
  substituteSym sym val (ExceptT v) = ExceptT $ substituteSym sym val v

-- Sum
deriving via
  (Default (Sum f g a))
  instance
    (SubstituteSym typedSymbol sym (f a), SubstituteSym typedSymbol sym (g a)) =>
    SubstituteSym typedSymbol sym (Sum f g a)

-- WriterT
instance
  (SubstituteSymSymbol typedSymbol sym, SubstituteSym typedSymbol sym (m (a, s))) =>
  SubstituteSym typedSymbol sym (WriterLazy.WriterT s m a)
  where
  substituteSym sym val (WriterLazy.WriterT v) = WriterLazy.WriterT $ substituteSym sym val v

instance
  (SubstituteSymSymbol typedSymbol sym, SubstituteSym typedSymbol sym (m (a, s))) =>
  SubstituteSym typedSymbol sym (WriterStrict.WriterT s m a)
  where
  substituteSym sym val (WriterStrict.WriterT v) = WriterStrict.WriterT $ substituteSym sym val v

-- Identity
instance SubstituteSym typedSymbol sym a => SubstituteSym typedSymbol sym (Identity a) where
  substituteSym sym val (Identity a) = Identity $ substituteSym sym val a

-- IdentityT
instance SubstituteSym typedSymbol sym (m a) => SubstituteSym typedSymbol sym (IdentityT m a) where
  substituteSym sym val (IdentityT a) = IdentityT $ substituteSym sym val a
