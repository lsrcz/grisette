{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      :   Grisette.Core.Data.Class.ToSym
-- Copyright   :   (c) Sirui Lu 2021-2023
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Core.Data.Class.ToSym
  ( -- * Converting to symbolic values
    ToSym (..),
  )
where

import Control.Monad.Identity
  ( Identity (Identity),
    IdentityT (IdentityT),
  )
import Control.Monad.Reader (ReaderT (ReaderT))
import qualified Control.Monad.State.Lazy as StateLazy
import qualified Control.Monad.State.Strict as StateStrict
import Control.Monad.Trans.Except (ExceptT (ExceptT))
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
  ( Default (Default),
    Generic (Rep, from, to),
    K1 (K1),
    M1 (M1),
    U1,
    V1,
    type (:*:) ((:*:)),
    type (:+:) (L1, R1),
  )
import Grisette.Core.Control.Exception (AssertionError, VerificationConditions)
import Grisette.Core.Data.BV
  ( IntN,
    WordN,
  )
import Grisette.Core.Data.Class.Solvable (Solvable (con))
import Grisette.SymPrim.GeneralFun (type (-->))
import Grisette.SymPrim.IntBitwidth (intBitwidthQ)
import Grisette.SymPrim.Prim.Term
  ( LinkedRep,
    SupportedPrim,
  )
import Grisette.SymPrim.SymBV
  ( SymIntN,
    SymWordN,
  )
import Grisette.SymPrim.SymBool (SymBool)
import Grisette.SymPrim.SymGeneralFun (type (-~>))
import Grisette.SymPrim.SymInteger (SymInteger)
import Grisette.SymPrim.SymTabularFun (type (=~>))
import Grisette.SymPrim.TabularFun (type (=->))

-- $setup
-- >>> import Grisette.SymPrim

-- | Convert a concrete value to symbolic value.
class ToSym a b where
  -- | Convert a concrete value to symbolic value.
  --
  -- >>> toSym False :: SymBool
  -- false
  --
  -- >>> toSym [False, True] :: [SymBool]
  -- [false,true]
  toSym :: a -> b

#define CONCRETE_TOSYM(type) \
instance ToSym type type where \
  toSym = id

#define CONCRETE_TOSYM_BV(type) \
instance (KnownNat n, 1 <= n) => ToSym (type n) (type n) where \
  toSym = id

#if 1
CONCRETE_TOSYM(Bool)
CONCRETE_TOSYM(Integer)
CONCRETE_TOSYM(Char)
CONCRETE_TOSYM(Int)
CONCRETE_TOSYM(Int8)
CONCRETE_TOSYM(Int16)
CONCRETE_TOSYM(Int32)
CONCRETE_TOSYM(Int64)
CONCRETE_TOSYM(Word)
CONCRETE_TOSYM(Word8)
CONCRETE_TOSYM(Word16)
CONCRETE_TOSYM(Word32)
CONCRETE_TOSYM(Word64)
CONCRETE_TOSYM(B.ByteString)
CONCRETE_TOSYM(T.Text)
CONCRETE_TOSYM_BV(IntN)
CONCRETE_TOSYM_BV(WordN)
#endif

-- Unit
instance ToSym () () where
  toSym = id

-- Either
deriving via (Default (Either e2 a2)) instance (ToSym e1 e2, ToSym a1 a2) => ToSym (Either e1 a1) (Either e2 a2)

-- Maybe
deriving via (Default (Maybe b)) instance (ToSym a b) => ToSym (Maybe a) (Maybe b)

-- List
deriving via (Default [b]) instance (ToSym a b) => ToSym [a] [b]

-- (,)
deriving via (Default (b1, b2)) instance (ToSym a1 b1, ToSym a2 b2) => ToSym (a1, a2) (b1, b2)

-- (,,)
deriving via (Default (b1, b2, b3)) instance (ToSym a1 b1, ToSym a2 b2, ToSym a3 b3) => ToSym (a1, a2, a3) (b1, b2, b3)

-- (,,,)
deriving via
  (Default (a2, b2, c2, d2))
  instance
    (ToSym a1 a2, ToSym b1 b2, ToSym c1 c2, ToSym d1 d2) => ToSym (a1, b1, c1, d1) (a2, b2, c2, d2)

-- (,,,,)
deriving via
  (Default (a2, b2, c2, d2, e2))
  instance
    (ToSym a1 a2, ToSym b1 b2, ToSym c1 c2, ToSym d1 d2, ToSym e1 e2) =>
    ToSym (a1, b1, c1, d1, e1) (a2, b2, c2, d2, e2)

-- (,,,,,)
deriving via
  (Default (a2, b2, c2, d2, e2, f2))
  instance
    (ToSym a1 a2, ToSym b1 b2, ToSym c1 c2, ToSym d1 d2, ToSym e1 e2, ToSym f1 f2) =>
    ToSym (a1, b1, c1, d1, e1, f1) (a2, b2, c2, d2, e2, f2)

-- (,,,,,,)
deriving via
  (Default (a2, b2, c2, d2, e2, f2, g2))
  instance
    (ToSym a1 a2, ToSym b1 b2, ToSym c1 c2, ToSym d1 d2, ToSym e1 e2, ToSym f1 f2, ToSym g1 g2) =>
    ToSym (a1, b1, c1, d1, e1, f1, g1) (a2, b2, c2, d2, e2, f2, g2)

-- (,,,,,,,)
deriving via
  (Default (a2, b2, c2, d2, e2, f2, g2, h2))
  instance
    (ToSym a1 a2, ToSym b1 b2, ToSym c1 c2, ToSym d1 d2, ToSym e1 e2, ToSym f1 f2, ToSym g1 g2, ToSym h1 h2) =>
    ToSym (a1, b1, c1, d1, e1, f1, g1, h1) (a2, b2, c2, d2, e2, f2, g2, h2)

-- function
instance (ToSym a b) => ToSym (v -> a) (v -> b) where
  toSym f = toSym . f

-- MaybeT
instance
  (ToSym (m1 (Maybe a)) (m2 (Maybe b))) =>
  ToSym (MaybeT m1 a) (MaybeT m2 b)
  where
  toSym (MaybeT v) = MaybeT $ toSym v

-- ExceptT
instance
  (ToSym (m1 (Either e1 a)) (m2 (Either e2 b))) =>
  ToSym (ExceptT e1 m1 a) (ExceptT e2 m2 b)
  where
  toSym (ExceptT v) = ExceptT $ toSym v

-- StateT
instance (ToSym (s1 -> m1 (a1, s1)) (s2 -> m2 (a2, s2))) => ToSym (StateLazy.StateT s1 m1 a1) (StateLazy.StateT s2 m2 a2) where
  toSym (StateLazy.StateT f1) = StateLazy.StateT $ toSym f1

instance (ToSym (s1 -> m1 (a1, s1)) (s2 -> m2 (a2, s2))) => ToSym (StateStrict.StateT s1 m1 a1) (StateStrict.StateT s2 m2 a2) where
  toSym (StateStrict.StateT f1) = StateStrict.StateT $ toSym f1

-- WriterT
instance (ToSym (m1 (a1, s1)) (m2 (a2, s2))) => ToSym (WriterLazy.WriterT s1 m1 a1) (WriterLazy.WriterT s2 m2 a2) where
  toSym (WriterLazy.WriterT f1) = WriterLazy.WriterT $ toSym f1

instance (ToSym (m1 (a1, s1)) (m2 (a2, s2))) => ToSym (WriterStrict.WriterT s1 m1 a1) (WriterStrict.WriterT s2 m2 a2) where
  toSym (WriterStrict.WriterT f1) = WriterStrict.WriterT $ toSym f1

-- ReaderT
instance (ToSym (s1 -> m1 a1) (s2 -> m2 a2)) => ToSym (ReaderT s1 m1 a1) (ReaderT s2 m2 a2) where
  toSym (ReaderT f1) = ReaderT $ toSym f1

-- Sum
deriving via
  (Default (Sum f1 g1 a1))
  instance
    (ToSym (f a) (f1 a1), ToSym (g a) (g1 a1)) => ToSym (Sum f g a) (Sum f1 g1 a1)

-- Identity
instance (ToSym a b) => ToSym (Identity a) (Identity b) where
  toSym (Identity a) = Identity $ toSym a

-- IdentityT
instance (ToSym (m a) (m1 b)) => ToSym (IdentityT m a) (IdentityT m1 b) where
  toSym (IdentityT v) = IdentityT $ toSym v

#define TO_SYM_SYMID_SIMPLE(symtype) \
instance ToSym symtype symtype where \
  toSym = id

#define TO_SYM_SYMID_BV(symtype) \
instance (KnownNat n, 1 <= n) => ToSym (symtype n) (symtype n) where \
  toSym = id

#define TO_SYM_SYMID_FUN(op) \
instance (SupportedPrim a, SupportedPrim b) => ToSym (a op b) (a op b) where \
  toSym = id

#if 1
TO_SYM_SYMID_SIMPLE(SymBool)
TO_SYM_SYMID_SIMPLE(SymInteger)
TO_SYM_SYMID_BV(SymIntN)
TO_SYM_SYMID_BV(SymWordN)
TO_SYM_SYMID_FUN(=~>)
TO_SYM_SYMID_FUN(-~>)
#endif

#define TO_SYM_FROMCON_SIMPLE(contype, symtype) \
instance ToSym contype symtype where \
  toSym = con

#define TO_SYM_FROMCON_BV(contype, symtype) \
instance (KnownNat n, 1 <= n) => ToSym (contype n) (symtype n) where \
  toSym = con

#define TO_SYM_FROMCON_FUN(conop, symop) \
instance (SupportedPrim (conop ca cb), LinkedRep ca sa, LinkedRep cb sb) => \
  ToSym (conop ca cb) (symop sa sb) where \
  toSym = con

#if 1
TO_SYM_FROMCON_SIMPLE(Bool, SymBool)
TO_SYM_FROMCON_SIMPLE(Integer, SymInteger)
TO_SYM_FROMCON_BV(IntN, SymIntN)
TO_SYM_FROMCON_BV(WordN, SymWordN)
TO_SYM_FROMCON_FUN((=->), (=~>))
TO_SYM_FROMCON_FUN((-->), (-~>))
#endif

#define TOSYM_MACHINE_INTEGER(int, bv) \
instance ToSym int (bv) where \
  toSym = fromIntegral

#if 1
TOSYM_MACHINE_INTEGER(Int8, SymIntN 8)
TOSYM_MACHINE_INTEGER(Int16, SymIntN 16)
TOSYM_MACHINE_INTEGER(Int32, SymIntN 32)
TOSYM_MACHINE_INTEGER(Int64, SymIntN 64)
TOSYM_MACHINE_INTEGER(Word8, SymWordN 8)
TOSYM_MACHINE_INTEGER(Word16, SymWordN 16)
TOSYM_MACHINE_INTEGER(Word32, SymWordN 32)
TOSYM_MACHINE_INTEGER(Word64, SymWordN 64)
TOSYM_MACHINE_INTEGER(Int, SymIntN $intBitwidthQ)
TOSYM_MACHINE_INTEGER(Word, SymWordN $intBitwidthQ)
#endif

-- Exception
deriving via
  (Default AssertionError)
  instance
    ToSym AssertionError AssertionError

deriving via
  (Default VerificationConditions)
  instance
    ToSym VerificationConditions VerificationConditions

instance (Generic a, Generic b, ToSym' (Rep a) (Rep b)) => ToSym a (Default b) where
  toSym = Default . to . toSym' . from

class ToSym' a b where
  toSym' :: a c -> b c

instance ToSym' U1 U1 where
  toSym' = id

instance ToSym' V1 V1 where
  toSym' = id

instance (ToSym a b) => ToSym' (K1 i a) (K1 i b) where
  toSym' (K1 a) = K1 $ toSym a

instance (ToSym' a b) => ToSym' (M1 i c1 a) (M1 i c2 b) where
  toSym' (M1 a) = M1 $ toSym' a

instance (ToSym' a1 a2, ToSym' b1 b2) => ToSym' (a1 :+: b1) (a2 :+: b2) where
  toSym' (L1 a) = L1 $ toSym' a
  toSym' (R1 b) = R1 $ toSym' b

instance (ToSym' a1 a2, ToSym' b1 b2) => ToSym' (a1 :*: b1) (a2 :*: b2) where
  toSym' (a :*: b) = toSym' a :*: toSym' b
