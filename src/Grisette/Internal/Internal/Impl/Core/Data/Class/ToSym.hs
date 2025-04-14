{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      :   Grisette.Internal.Internal.Impl.Core.Data.Class.ToSym
-- Copyright   :   (c) Sirui Lu 2021-2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Internal.Internal.Impl.Core.Data.Class.ToSym () where

import Control.Exception (ArithException)
import Control.Monad.Identity
  ( Identity (Identity, runIdentity),
    IdentityT (IdentityT),
  )
import Control.Monad.Reader (ReaderT (ReaderT))
import qualified Control.Monad.State.Lazy as StateLazy
import qualified Control.Monad.State.Strict as StateStrict
import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Trans.Maybe (MaybeT)
import qualified Control.Monad.Writer.Lazy as WriterLazy
import qualified Control.Monad.Writer.Strict as WriterStrict
import qualified Data.ByteString as B
import Data.Functor.Compose (Compose (Compose))
import Data.Functor.Const (Const)
import Data.Functor.Product (Product)
import Data.Functor.Sum (Sum)
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Monoid (Alt, Ap)
import qualified Data.Monoid as Monoid
import Data.Ord (Down)
import Data.Ratio (Ratio, denominator, numerator, (%))
import qualified Data.Text as T
import Data.Typeable (Typeable)
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.TypeNats (KnownNat, type (<=))
import Generics.Deriving
  ( Default (Default),
    Default1 (Default1),
    K1 (K1),
    M1 (M1),
    Par1 (Par1),
    Rec1 (Rec1),
    U1,
    V1,
    (:.:) (Comp1),
    type (:*:),
    type (:+:),
  )
import Grisette.Internal.Core.Control.Exception
  ( AssertionError,
    VerificationConditions,
  )
import Grisette.Internal.Core.Data.Class.BitCast (BitCast (bitCast))
import Grisette.Internal.Core.Data.Class.Solvable (Solvable (con))
import Grisette.Internal.Internal.Decl.Core.Data.Class.ToSym
  ( ToSym (toSym),
    ToSym1 (liftToSym),
    ToSym2 (liftToSym2),
    toSym1,
  )
import Grisette.Internal.Internal.Impl.Core.Data.Class.Mergeable ()
import Grisette.Internal.SymPrim.AlgReal (AlgReal)
import Grisette.Internal.SymPrim.BV
  ( IntN,
    WordN,
  )
import Grisette.Internal.SymPrim.FP
  ( FP,
    FPRoundingMode,
    NotRepresentableFPError,
    ValidFP,
  )
import Grisette.Internal.SymPrim.GeneralFun (type (-->))
import Grisette.Internal.SymPrim.IntBitwidth (intBitwidthQ)
import Grisette.Internal.SymPrim.Prim.Term
  ( LinkedRep,
    SupportedNonFuncPrim,
    SupportedPrim,
  )
import Grisette.Internal.SymPrim.SymAlgReal (SymAlgReal)
import Grisette.Internal.SymPrim.SymBV
  ( SymIntN,
    SymWordN,
  )
import Grisette.Internal.SymPrim.SymBool (SymBool)
import Grisette.Internal.SymPrim.SymFP
  ( SymFP,
    SymFP32,
    SymFP64,
    SymFPRoundingMode,
  )
import Grisette.Internal.SymPrim.SymGeneralFun (type (-~>))
import Grisette.Internal.SymPrim.SymInteger (SymInteger)
import Grisette.Internal.SymPrim.SymTabularFun (type (=~>))
import Grisette.Internal.SymPrim.TabularFun (type (=->))
import Grisette.Internal.TH.Derivation.Derive (derive)

-- $setup
-- >>> import Grisette.SymPrim

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
CONCRETE_TOSYM(Float)
CONCRETE_TOSYM(Double)
CONCRETE_TOSYM(B.ByteString)
CONCRETE_TOSYM(T.Text)
CONCRETE_TOSYM(FPRoundingMode)
CONCRETE_TOSYM_BV(IntN)
CONCRETE_TOSYM_BV(WordN)
CONCRETE_TOSYM(Monoid.All)
CONCRETE_TOSYM(Monoid.Any)
CONCRETE_TOSYM(Ordering)
#endif

instance (ValidFP eb sb) => ToSym (FP eb sb) (FP eb sb) where
  toSym = id

instance ToSym (a =-> b) (a =-> b) where
  toSym = id

instance ToSym (a --> b) (a --> b) where
  toSym = id

#define TO_SYM_SYMID_SIMPLE(symtype) \
instance ToSym symtype symtype where \
  toSym = id

#define TO_SYM_SYMID_BV(symtype) \
instance (KnownNat n, 1 <= n) => ToSym (symtype n) (symtype n) where \
  toSym = id

#define TO_SYM_SYMID_FUN(cop, op) \
instance (SupportedPrim (cop ca cb), LinkedRep ca sa, LinkedRep cb sb) => \
  ToSym (op sa sb) (op sa sb) where \
  toSym = id

#if 1
TO_SYM_SYMID_SIMPLE(SymBool)
TO_SYM_SYMID_SIMPLE(SymInteger)
TO_SYM_SYMID_SIMPLE(SymAlgReal)
TO_SYM_SYMID_BV(SymIntN)
TO_SYM_SYMID_BV(SymWordN)
TO_SYM_SYMID_FUN((=->), (=~>))
TO_SYM_SYMID_FUN((-->), (-~>))
TO_SYM_SYMID_SIMPLE(SymFPRoundingMode)
#endif

instance (ValidFP eb sb) => ToSym (SymFP eb sb) (SymFP eb sb) where
  toSym = id

#define TO_SYM_FROMCON_SIMPLE(contype, symtype) \
instance ToSym contype symtype where \
  toSym = con

#define TO_SYM_FROMCON_BV(contype, symtype) \
instance (KnownNat n, 1 <= n) => ToSym (contype n) (symtype n) where \
  toSym = con

#define TO_SYM_FROMCON_FUN(conop, symop) \
instance (SupportedPrim (conop ca cb), SupportedNonFuncPrim ca, LinkedRep ca sa, LinkedRep cb sb) => \
  ToSym (conop ca cb) (symop sa sb) where \
  toSym = con

#if 1
TO_SYM_FROMCON_SIMPLE(Bool, SymBool)
TO_SYM_FROMCON_SIMPLE(Integer, SymInteger)
TO_SYM_FROMCON_SIMPLE(AlgReal, SymAlgReal)
TO_SYM_FROMCON_BV(IntN, SymIntN)
TO_SYM_FROMCON_BV(WordN, SymWordN)
TO_SYM_FROMCON_FUN((=->), (=~>))
TO_SYM_FROMCON_FUN((-->), (-~>))
TO_SYM_FROMCON_SIMPLE(FPRoundingMode, SymFPRoundingMode)
#endif

instance (ValidFP eb sb) => ToSym (FP eb sb) (SymFP eb sb) where
  toSym = con

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

instance ToSym Float SymFP32 where
  toSym = con . bitCast
  {-# INLINE toSym #-}

instance ToSym Double SymFP64 where
  toSym = con . bitCast
  {-# INLINE toSym #-}

instance
  (Integral b, Typeable b, Show b, ToSym a b) =>
  ToSym (Ratio a) (Ratio b)
  where
  toSym r = toSym (numerator r) % toSym (denominator r)
  {-# INLINE toSym #-}

instance ToSym Rational SymAlgReal where
  toSym v = con (fromRational v)

-- Function
instance (ToSym b d, ToSym c a) => ToSym (a -> b) (c -> d) where
  toSym = toSym1
  {-# INLINE toSym #-}

instance (ToSym c a) => ToSym1 ((->) a) ((->) c) where
  liftToSym l f = l . f . toSym
  {-# INLINE liftToSym #-}

derive
  [ ''Either,
    ''(,),
    ''(,,),
    ''(,,,),
    ''(,,,,),
    ''(,,,,,),
    ''(,,,,,,),
    ''(,,,,,,,),
    ''(,,,,,,,,),
    ''(,,,,,,,,,),
    ''(,,,,,,,,,,),
    ''(,,,,,,,,,,,),
    ''(,,,,,,,,,,,,),
    ''(,,,,,,,,,,,,,),
    ''(,,,,,,,,,,,,,,)
  ]
  [''ToSym, ''ToSym1, ''ToSym2]

derive
  [ ''[],
    ''Maybe,
    ''Monoid.Dual,
    ''Monoid.Sum,
    ''Monoid.Product,
    ''Monoid.First,
    ''Monoid.Last,
    ''Down,
    ''ExceptT,
    ''MaybeT,
    ''WriterLazy.WriterT,
    ''WriterStrict.WriterT
  ]
  [''ToSym, ''ToSym1]

derive
  [ ''(),
    ''AssertionError,
    ''VerificationConditions,
    ''NotRepresentableFPError,
    ''ArithException
  ]
  [''ToSym]

-- StateT
instance
  (ToSym1 m1 m2, ToSym a1 a2) =>
  ToSym (StateLazy.StateT s m1 a1) (StateLazy.StateT s m2 a2)
  where
  toSym = toSym1
  {-# INLINE toSym #-}

instance
  (ToSym1 m1 m2) =>
  ToSym1 (StateLazy.StateT s m1) (StateLazy.StateT s m2)
  where
  liftToSym f (StateLazy.StateT f1) =
    StateLazy.StateT $ \s -> liftToSym (liftToSym2 f id) $ f1 s
  {-# INLINE liftToSym #-}

instance
  (ToSym1 m1 m2, ToSym a1 a2) =>
  ToSym (StateStrict.StateT s m1 a1) (StateStrict.StateT s m2 a2)
  where
  toSym = toSym1
  {-# INLINE toSym #-}

instance
  (ToSym1 m1 m2) =>
  ToSym1 (StateStrict.StateT s m1) (StateStrict.StateT s m2)
  where
  liftToSym f (StateStrict.StateT f1) =
    StateStrict.StateT $ \s -> liftToSym (liftToSym2 f id) $ f1 s
  {-# INLINE liftToSym #-}

-- ReaderT
instance
  (ToSym s2 s1, ToSym1 m1 m2, ToSym a1 a2) =>
  ToSym (ReaderT s1 m1 a1) (ReaderT s2 m2 a2)
  where
  toSym = toSym1
  {-# INLINE toSym #-}

instance
  (ToSym s2 s1, ToSym1 m1 m2) =>
  ToSym1 (ReaderT s1 m1) (ReaderT s2 m2)
  where
  liftToSym ::
    forall a b.
    (ToSym s2 s1, ToSym1 m1 m2) =>
    (a -> b) ->
    ReaderT s1 m1 a ->
    ReaderT s2 m2 b
  liftToSym f (ReaderT f1) =
    ReaderT $
      liftToSym (liftToSym f) f1
  {-# INLINE liftToSym #-}

-- IdentityT
instance (ToSym1 m m1, ToSym a b) => ToSym (IdentityT m a) (IdentityT m1 b) where
  toSym = toSym1
  {-# INLINE toSym #-}

instance (ToSym1 m m1) => ToSym1 (IdentityT m) (IdentityT m1) where
  liftToSym f (IdentityT v) = IdentityT $ liftToSym f v
  {-# INLINE liftToSym #-}

-- Identity
instance {-# INCOHERENT #-} (ToSym a b) => ToSym (Identity a) (Identity b) where
  toSym = toSym1
  {-# INLINE toSym #-}

instance {-# INCOHERENT #-} (ToSym a b) => ToSym a (Identity b) where
  toSym = Identity . toSym
  {-# INLINE toSym #-}

instance {-# INCOHERENT #-} (ToSym a b) => ToSym (Identity a) b where
  toSym = toSym . runIdentity
  {-# INLINE toSym #-}

instance ToSym1 Identity Identity where
  liftToSym f (Identity v) = Identity $ f v
  {-# INLINE liftToSym #-}

-- Product
deriving via
  (Default (Product l r a))
  instance
    (ToSym (l0 a0) (l a), ToSym (r0 a0) (r a)) =>
    ToSym (Product l0 r0 a0) (Product l r a)

deriving via
  (Default1 (Product l r))
  instance
    (ToSym1 l0 l, ToSym1 r0 r) => ToSym1 (Product l0 r0) (Product l r)

-- Sum
deriving via
  (Default (Sum l r a))
  instance
    (ToSym (l0 a0) (l a), ToSym (r0 a0) (r a)) =>
    ToSym (Sum l0 r0 a0) (Sum l r a)

deriving via
  (Default1 (Sum l r))
  instance
    (ToSym1 l0 l, ToSym1 r0 r) => ToSym1 (Sum l0 r0) (Sum l r)

-- Compose
deriving via
  (Default (Compose f g a))
  instance
    (ToSym (f0 (g0 a0)) (f (g a))) => ToSym (Compose f0 g0 a0) (Compose f g a)

instance
  (ToSym1 f0 f, ToSym1 g0 g) =>
  ToSym1 (Compose f0 g0) (Compose f g)
  where
  liftToSym ::
    forall a b.
    (ToSym1 f0 f, ToSym1 g0 g) =>
    (a -> b) ->
    Compose f0 g0 a ->
    Compose f g b
  liftToSym f (Compose v) = Compose $ liftToSym (liftToSym f) v
  {-# INLINE liftToSym #-}

-- Const
deriving via
  (Default (Const a b))
  instance
    (ToSym a0 a) => ToSym (Const a0 b0) (Const a b)

deriving via
  (Default1 (Const a))
  instance
    (ToSym a0 a) => ToSym1 (Const a0) (Const a)

-- Alt
deriving via
  (Default (Alt f a))
  instance
    (ToSym (f0 a0) (f a)) => ToSym (Alt f0 a0) (Alt f a)

deriving via
  (Default1 (Alt f))
  instance
    (ToSym1 f0 f) => ToSym1 (Alt f0) (Alt f)

-- Ap
deriving via
  (Default (Ap f a))
  instance
    (ToSym (f0 a0) (f a)) => ToSym (Ap f0 a0) (Ap f a)

deriving via
  (Default1 (Ap f))
  instance
    (ToSym1 f0 f) => ToSym1 (Ap f0) (Ap f)

-- Generic
deriving via (Default (U1 p)) instance ToSym (U1 p0) (U1 p)

deriving via (Default (V1 p)) instance ToSym (V1 p0) (V1 p)

deriving via
  (Default (K1 i c p))
  instance
    (ToSym c0 c) => ToSym (K1 i0 c0 p0) (K1 i c p)

deriving via
  (Default (M1 i c f p))
  instance
    (ToSym (f0 p0) (f p)) => ToSym (M1 i0 c0 f0 p0) (M1 i c f p)

deriving via
  (Default ((f :+: g) p))
  instance
    (ToSym (f0 p0) (f p), ToSym (g0 p0) (g p)) =>
    ToSym ((f0 :+: g0) p0) ((f :+: g) p)

deriving via
  (Default ((f :*: g) p))
  instance
    (ToSym (f0 p0) (f p), ToSym (g0 p0) (g p)) =>
    ToSym ((f0 :*: g0) p0) ((f :*: g) p)

deriving via
  (Default (Par1 p))
  instance
    (ToSym p0 p) => ToSym (Par1 p0) (Par1 p)

deriving via
  (Default (Rec1 f p))
  instance
    (ToSym (f0 p0) (f p)) => ToSym (Rec1 f0 p0) (Rec1 f p)

deriving via
  (Default ((f :.: g) p))
  instance
    (ToSym (f0 (g0 p0)) (f (g p))) => ToSym ((f0 :.: g0) p0) ((f :.: g) p)
