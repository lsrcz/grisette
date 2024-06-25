{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Trustworthy #-}
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
import Data.Kind (Type)
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
import Grisette.Internal.Core.Data.Class.Solvable (Solvable (con))
import Grisette.Internal.SymPrim.BV
  ( IntN,
    WordN,
  )
import Grisette.Internal.SymPrim.FP (FP, FPRoundingMode, ValidFP)
import Grisette.Internal.SymPrim.GeneralFun (type (-->))
import Grisette.Internal.SymPrim.IntBitwidth (intBitwidthQ)
import Grisette.Internal.SymPrim.Prim.Term
  ( LinkedRep,
    SupportedPrim,
  )
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
class ToSym a b where
  -- | Convert a concrete value to symbolic value.
  --
  -- >>> toSym False :: SymBool
  -- false
  --
  -- >>> toSym [False, True] :: [SymBool]
  -- [false,true]
  toSym :: a -> b

class (forall a b. (ToSym a b) => ToSym (f1 a) (f2 b)) => ToSym1 f1 f2 where
  liftToSym :: (a -> b) -> f1 a -> f2 b

toSym1 :: (ToSym1 f1 f2, ToSym a b) => f1 a -> f2 b
toSym1 = liftToSym toSym
{-# INLINE toSym1 #-}

class (forall a b. (ToSym a b) => ToSym1 (f1 a) (f2 b)) => ToSym2 f1 f2 where
  liftToSym2 :: (a -> b) -> (c -> d) -> f1 a c -> f2 b d

toSym2 :: (ToSym2 f1 f2, ToSym a b, ToSym c d) => f1 a c -> f2 b d
toSym2 = liftToSym2 toSym toSym
{-# INLINE toSym2 #-}

-- Derivations

-- | The arguments to the generic 'toSym' function.
data family ToSymArgs arity a b :: Type

data instance ToSymArgs Arity0 _ _ = ToSymArgs0

newtype instance ToSymArgs Arity1 a b = ToSymArgs1 (a -> b)

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
  (ToSym1 f1 f2, GToSym Arity1 g1 g2) =>
  GToSym Arity1 (f1 :.: g1) (f2 :.: g2)
  where
  gtoSym targs (Comp1 a) = Comp1 $ liftToSym (gtoSym targs) a
  {-# INLINE gtoSym #-}

genericToSym ::
  (Generic a, Generic b, GToSym Arity0 (Rep a) (Rep b)) =>
  a ->
  b
genericToSym = to . gtoSym ToSymArgs0 . from
{-# INLINE genericToSym #-}

genericLiftToSym ::
  (Generic1 f1, Generic1 f2, GToSym Arity1 (Rep1 f1) (Rep1 f2)) =>
  (a -> b) ->
  f1 a ->
  f2 b
genericLiftToSym f = to1 . gtoSym (ToSymArgs1 f) . from1
{-# INLINE genericLiftToSym #-}

instance
  (Generic a, Generic b, GToSym Arity0 (Rep a) (Rep b)) =>
  ToSym a (Default b)
  where
  toSym = Default . genericToSym
  {-# INLINE toSym #-}

instance
  (Generic1 f1, Generic1 f2, GToSym Arity1 (Rep1 f1) (Rep1 f2), ToSym a b) =>
  ToSym (f1 a) (Default1 f2 b)
  where
  toSym = toSym1

instance
  (Generic1 f1, Generic1 f2, GToSym Arity1 (Rep1 f1) (Rep1 f2)) =>
  ToSym1 f1 (Default1 f2)
  where
  liftToSym f = Default1 . genericLiftToSym f
  {-# INLINE liftToSym #-}

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
    ''Identity
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
    ''Identity
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
  liftToSym f (ReaderT f1) = ReaderT $ liftToSym (liftToSym f) f1
  {-# INLINE liftToSym #-}

-- IdentityT
instance (ToSym1 m m1, ToSym a b) => ToSym (IdentityT m a) (IdentityT m1 b) where
  toSym = toSym1
  {-# INLINE toSym #-}

instance (ToSym1 m m1) => ToSym1 (IdentityT m) (IdentityT m1) where
  liftToSym f (IdentityT v) = IdentityT $ liftToSym f v
  {-# INLINE liftToSym #-}

-- Function
instance (ToSym b d, ToSym c a) => ToSym (a -> b) (c -> d) where
  toSym = toSym1
  {-# INLINE toSym #-}

instance (ToSym c a) => ToSym1 ((->) a) ((->) c) where
  liftToSym l f = l . f . toSym
  {-# INLINE liftToSym #-}

-- Sum
deriving via
  (Default (Sum f1 g1 a1))
  instance
    (ToSym (f a) (f1 a1), ToSym (g a) (g1 a1)) =>
    ToSym (Sum f g a) (Sum f1 g1 a1)

deriving via
  (Default1 (Sum f1 g1))
  instance
    (ToSym1 f f1, ToSym1 g g1) =>
    ToSym1 (Sum f g) (Sum f1 g1)

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
#endif

instance (ValidFP eb sb) => ToSym (FP eb sb) (FP eb sb) where
  toSym = id

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
