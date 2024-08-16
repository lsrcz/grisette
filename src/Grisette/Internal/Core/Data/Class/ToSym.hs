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
-- Module      :   Grisette.Internal.Core.Data.Class.ToSym
-- Copyright   :   (c) Sirui Lu 2021-2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Internal.Core.Data.Class.ToSym
  ( -- * Converting to symbolic values
    ToSym (..),
    ToSym1 (..),
    toSym1,
    ToSym2 (..),
    toSym2,

    -- * Generic 'ToSym'
    ToSymArgs (..),
    GToSym (..),
    genericToSym,
    genericLiftToSym,
  )
where

import Control.Monad.Identity
  ( Identity (Identity, runIdentity),
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
import Data.Functor.Compose (Compose (Compose))
import Data.Functor.Const (Const)
import Data.Functor.Product (Product)
import Data.Functor.Sum (Sum)
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Kind (Type)
import Data.Monoid (Alt, Ap)
import qualified Data.Monoid as Monoid
import Data.Ord (Down)
import qualified Data.Text as T
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.TypeNats (KnownNat, type (<=))
import Generics.Deriving
  ( Default (Default),
    Default1 (Default1),
    Generic (Rep, from, to),
    Generic1 (Rep1, from1, to1),
    K1 (K1),
    M1 (M1),
    Par1 (Par1),
    Rec1 (Rec1),
    U1 (U1),
    V1,
    (:.:) (Comp1),
    type (:*:) ((:*:)),
    type (:+:) (L1, R1),
  )
import Grisette.Internal.Core.Control.Exception
  ( AssertionError,
    VerificationConditions,
  )
import Grisette.Internal.Core.Data.Class.BitCast (BitCast (bitCast))
import Grisette.Internal.Core.Data.Class.Mergeable
  ( GMergeable,
    Mergeable,
    Mergeable1,
    Mergeable2,
    resolveMergeable1,
  )
import Grisette.Internal.Core.Data.Class.Solvable (Solvable (con))
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
import Grisette.Internal.TH.DeriveBuiltin (deriveBuiltins)
import Grisette.Internal.TH.DeriveInstanceProvider
  ( Strategy (ViaDefault, ViaDefault1),
  )
import Grisette.Internal.Utils.Derive (Arity0, Arity1)

-- $setup
-- >>> import Grisette.SymPrim

-- | Convert a concrete value to symbolic value.
class (Mergeable b) => ToSym a b where
  -- | Convert a concrete value to symbolic value.
  --
  -- >>> toSym False :: SymBool
  -- false
  --
  -- >>> toSym [False, True] :: [SymBool]
  -- [false,true]
  toSym :: a -> b

instance {-# INCOHERENT #-} (Mergeable a) => ToSym a a where
  toSym = id
  {-# INLINE toSym #-}

-- | Lifting of 'ToSym' to unary type constructors.
class
  (forall a b. (ToSym a b) => ToSym (f1 a) (f2 b), Mergeable1 f2) =>
  ToSym1 f1 f2
  where
  -- | Lift a conversion to symbolic function to unary type constructors.
  liftToSym :: (Mergeable b) => (a -> b) -> f1 a -> f2 b

-- | Lift the standard 'toSym' to unary type constructors.
toSym1 :: (ToSym1 f1 f2, ToSym a b) => f1 a -> f2 b
toSym1 = liftToSym toSym
{-# INLINE toSym1 #-}

-- | Lifting of 'ToSym' to binary type constructors.
class
  (forall a b. (ToSym a b) => ToSym1 (f1 a) (f2 b), Mergeable2 f2) =>
  ToSym2 f1 f2
  where
  -- | Lift conversion to symbolic functions to binary type constructors.
  liftToSym2 :: (a -> b) -> (c -> d) -> f1 a c -> f2 b d

-- | Lift the standard 'toSym' to binary type constructors.
toSym2 :: (ToSym2 f1 f2, ToSym a b, ToSym c d) => f1 a c -> f2 b d
toSym2 = liftToSym2 toSym toSym
{-# INLINE toSym2 #-}

-- Derivations

-- | The arguments to the generic 'toSym' function.
data family ToSymArgs arity a b :: Type

data instance ToSymArgs Arity0 _ _ = ToSymArgs0

data instance ToSymArgs Arity1 _ _ where
  ToSymArgs1 :: (Mergeable b) => (a -> b) -> ToSymArgs Arity1 a b

-- | The class of types that can be generically converted to symbolic values.
class GToSym arity f1 f2 where
  gtoSym :: ToSymArgs arity a b -> f1 a -> f2 b

instance GToSym arity V1 V1 where
  gtoSym _ _ = error "Impossible"
  {-# INLINE gtoSym #-}

instance GToSym arity U1 U1 where
  gtoSym _ _ = U1
  {-# INLINE gtoSym #-}

instance
  (GToSym arity a b, GToSym arity c d) =>
  GToSym arity (a :+: c) (b :+: d)
  where
  gtoSym args (L1 a) = L1 $ gtoSym args a
  gtoSym args (R1 b) = R1 $ gtoSym args b
  {-# INLINE gtoSym #-}

instance
  (GToSym arity a b, GToSym arity c d) =>
  GToSym arity (a :*: c) (b :*: d)
  where
  gtoSym args (a :*: c) = gtoSym args a :*: gtoSym args c
  {-# INLINE gtoSym #-}

instance (ToSym a b) => GToSym arity (K1 i a) (K1 i b) where
  gtoSym _ (K1 a) = K1 $ toSym a
  {-# INLINE gtoSym #-}

instance (GToSym arity f1 f2) => GToSym arity (M1 i c1 f1) (M1 i c2 f2) where
  gtoSym args (M1 a) = M1 $ gtoSym args a
  {-# INLINE gtoSym #-}

instance GToSym Arity1 Par1 Par1 where
  gtoSym (ToSymArgs1 f) (Par1 a) = Par1 $ f a
  {-# INLINE gtoSym #-}

instance (ToSym1 f1 f2) => GToSym Arity1 (Rec1 f1) (Rec1 f2) where
  gtoSym (ToSymArgs1 f) (Rec1 a) = Rec1 $ liftToSym f a
  {-# INLINE gtoSym #-}

instance
  (ToSym1 f1 f2, GToSym Arity1 g1 g2, Mergeable1 g2) =>
  GToSym Arity1 (f1 :.: g1) (f2 :.: g2)
  where
  gtoSym targs@ToSymArgs1 {} (Comp1 a) = Comp1 $ liftToSym (gtoSym targs) a
  {-# INLINE gtoSym #-}

-- | Generic 'toSym' function.
genericToSym ::
  (Generic a, Generic b, GToSym Arity0 (Rep a) (Rep b)) =>
  a ->
  b
genericToSym = to . gtoSym ToSymArgs0 . from
{-# INLINE genericToSym #-}

-- | Generic 'liftToSym' function.
genericLiftToSym ::
  (Generic1 f1, Generic1 f2, GToSym Arity1 (Rep1 f1) (Rep1 f2), Mergeable b) =>
  (a -> b) ->
  f1 a ->
  f2 b
genericLiftToSym f = to1 . gtoSym (ToSymArgs1 f) . from1
{-# INLINE genericLiftToSym #-}

instance
  ( Generic a,
    Generic b,
    GToSym Arity0 (Rep a) (Rep b),
    GMergeable Arity0 (Rep b)
  ) =>
  ToSym a (Default b)
  where
  toSym = Default . genericToSym
  {-# INLINE toSym #-}

instance
  ( Generic1 f1,
    Generic1 f2,
    GToSym Arity1 (Rep1 f1) (Rep1 f2),
    ToSym a b,
    GMergeable Arity1 (Rep1 f2)
  ) =>
  ToSym (f1 a) (Default1 f2 b)
  where
  toSym = toSym1

instance
  ( Generic1 f1,
    Generic1 f2,
    GToSym Arity1 (Rep1 f1) (Rep1 f2),
    GMergeable Arity1 (Rep1 f2)
  ) =>
  ToSym1 f1 (Default1 f2)
  where
  liftToSym f = Default1 . genericLiftToSym f
  {-# INLINE liftToSym #-}

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

deriveBuiltins
  (ViaDefault ''ToSym)
  [''ToSym]
  [ ''[],
    ''Maybe,
    ''Either,
    ''(),
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
    ''(,,,,,,,,,,,,,,),
    ''AssertionError,
    ''VerificationConditions,
    ''NotRepresentableFPError,
    ''Monoid.Dual,
    ''Monoid.Sum,
    ''Monoid.Product,
    ''Monoid.First,
    ''Monoid.Last,
    ''Down
  ]

deriveBuiltins
  (ViaDefault1 ''ToSym1)
  [''ToSym, ''ToSym1]
  [ ''[],
    ''Maybe,
    ''Either,
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
    ''(,,,,,,,,,,,,,,),
    ''Monoid.Dual,
    ''Monoid.Sum,
    ''Monoid.Product,
    ''Monoid.First,
    ''Monoid.Last,
    ''Down
  ]

-- ExceptT
instance
  (ToSym1 m1 m2, ToSym e1 e2, ToSym a b) =>
  ToSym (ExceptT e1 m1 a) (ExceptT e2 m2 b)
  where
  toSym = toSym1
  {-# INLINE toSym #-}

instance
  (ToSym1 m1 m2, ToSym e1 e2) =>
  ToSym1 (ExceptT e1 m1) (ExceptT e2 m2)
  where
  liftToSym f (ExceptT v) = ExceptT $ liftToSym (liftToSym f) v
  {-# INLINE liftToSym #-}

-- MaybeT
instance
  (ToSym1 m1 m2, ToSym a b) =>
  ToSym (MaybeT m1 a) (MaybeT m2 b)
  where
  toSym = toSym1
  {-# INLINE toSym #-}

instance
  (ToSym1 m1 m2) =>
  ToSym1 (MaybeT m1) (MaybeT m2)
  where
  liftToSym f (MaybeT v) = MaybeT $ liftToSym (liftToSym f) v
  {-# INLINE liftToSym #-}

-- WriterT
instance
  (ToSym1 m1 m2, ToSym a b, ToSym s1 s2) =>
  ToSym (WriterLazy.WriterT s1 m1 a) (WriterLazy.WriterT s2 m2 b)
  where
  toSym = toSym1
  {-# INLINE toSym #-}

instance
  (ToSym1 m1 m2, ToSym s1 s2) =>
  ToSym1 (WriterLazy.WriterT s1 m1) (WriterLazy.WriterT s2 m2)
  where
  liftToSym f (WriterLazy.WriterT v) =
    WriterLazy.WriterT $ liftToSym (liftToSym2 f toSym) v
  {-# INLINE liftToSym #-}

instance
  (ToSym1 m1 m2, ToSym a b, ToSym s1 s2) =>
  ToSym (WriterStrict.WriterT s1 m1 a) (WriterStrict.WriterT s2 m2 b)
  where
  toSym = toSym1
  {-# INLINE toSym #-}

instance
  (ToSym1 m1 m2, ToSym s1 s2) =>
  ToSym1 (WriterStrict.WriterT s1 m1) (WriterStrict.WriterT s2 m2)
  where
  liftToSym f (WriterStrict.WriterT v) =
    WriterStrict.WriterT $ liftToSym (liftToSym2 f toSym) v
  {-# INLINE liftToSym #-}

-- StateT
instance
  (ToSym1 m1 m2, ToSym a1 a2, Mergeable s) =>
  ToSym (StateLazy.StateT s m1 a1) (StateLazy.StateT s m2 a2)
  where
  toSym = toSym1
  {-# INLINE toSym #-}

instance
  (ToSym1 m1 m2, Mergeable s) =>
  ToSym1 (StateLazy.StateT s m1) (StateLazy.StateT s m2)
  where
  liftToSym f (StateLazy.StateT f1) =
    StateLazy.StateT $ \s -> liftToSym (liftToSym2 f id) $ f1 s
  {-# INLINE liftToSym #-}

instance
  (ToSym1 m1 m2, ToSym a1 a2, Mergeable s) =>
  ToSym (StateStrict.StateT s m1 a1) (StateStrict.StateT s m2 a2)
  where
  toSym = toSym1
  {-# INLINE toSym #-}

instance
  (ToSym1 m1 m2, Mergeable s) =>
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
  (ToSym s2 s1, ToSym1 m1 m2, Mergeable1 m2) =>
  ToSym1 (ReaderT s1 m1) (ReaderT s2 m2)
  where
  liftToSym ::
    forall a b.
    (ToSym s2 s1, ToSym1 m1 m2, Mergeable1 m2, Mergeable b) =>
    (a -> b) ->
    ReaderT s1 m1 a ->
    ReaderT s2 m2 b
  liftToSym f (ReaderT f1) =
    resolveMergeable1 @m2 @b $
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

-- Function
instance (ToSym b d, ToSym c a) => ToSym (a -> b) (c -> d) where
  toSym = toSym1
  {-# INLINE toSym #-}

instance (ToSym c a) => ToSym1 ((->) a) ((->) c) where
  liftToSym l f = l . f . toSym
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
    (ToSym1 f0 f, ToSym1 g0 g, Mergeable b) =>
    (a -> b) ->
    Compose f0 g0 a ->
    Compose f g b
  liftToSym f (Compose v) =
    resolveMergeable1 @g @b $ Compose $ liftToSym (liftToSym f) v
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

-- ToSym2
instance ToSym2 Either Either where
  liftToSym2 f _ (Left a) = Left $ f a
  liftToSym2 _ g (Right b) = Right $ g b
  {-# INLINE liftToSym2 #-}

instance ToSym2 (,) (,) where
  liftToSym2 f g (a, b) = (f a, g b)
  {-# INLINE liftToSym2 #-}

instance (ToSym a b) => ToSym2 ((,,) a) ((,,) b) where
  liftToSym2 f g (a, b, c) = (toSym a, f b, g c)
  {-# INLINE liftToSym2 #-}

instance (ToSym a c, ToSym b d) => ToSym2 ((,,,) a b) ((,,,) c d) where
  liftToSym2 f g (a, b, c, d) = (toSym a, toSym b, f c, g d)
  {-# INLINE liftToSym2 #-}
