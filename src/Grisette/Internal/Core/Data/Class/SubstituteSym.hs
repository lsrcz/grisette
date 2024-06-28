{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      :   Grisette.Internal.Core.Data.Class.SubstituteSym
-- Copyright   :   (c) Sirui Lu 2021-2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Internal.Core.Data.Class.SubstituteSym
  ( -- * Substituting symbolic constants
    SubstituteSym (..),
    SubstituteSym1 (..),
    substituteSym1,
    SubstituteSym2 (..),
    substituteSym2,

    -- * Generic 'SubstituteSym'
    SubstituteSymArgs (..),
    GSubstituteSym (..),
    genericSubstituteSym,
    genericLiftSubstituteSym,
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
import Data.Kind (Type)
import qualified Data.Text as T
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.TypeNats (KnownNat, type (<=))
import Generics.Deriving
  ( Default (Default, unDefault),
    Default1 (Default1, unDefault1),
    Generic (Rep, from, to),
    Generic1 (Rep1, from1, to1),
    K1 (K1),
    M1 (M1),
    Par1 (Par1),
    Rec1 (Rec1),
    U1,
    V1,
    (:.:) (Comp1),
    type (:*:) ((:*:)),
    type (:+:) (L1, R1),
  )
import Generics.Deriving.Instances ()
import Grisette.Internal.Core.Control.Exception
  ( AssertionError,
    VerificationConditions,
  )
import Grisette.Internal.SymPrim.BV (IntN, WordN)
import Grisette.Internal.SymPrim.FP (FP, FPRoundingMode, ValidFP)
import Grisette.Internal.SymPrim.GeneralFun (substTerm, type (-->))
import Grisette.Internal.SymPrim.Prim.Term
  ( LinkedRep (underlyingTerm),
    SupportedPrim,
    TypedSymbol,
  )
import Grisette.Internal.SymPrim.SymBV
  ( SymIntN (SymIntN),
    SymWordN (SymWordN),
  )
import Grisette.Internal.SymPrim.SymBool (SymBool (SymBool))
import Grisette.Internal.SymPrim.SymFP
  ( SymFP (SymFP),
    SymFPRoundingMode (SymFPRoundingMode),
  )
import Grisette.Internal.SymPrim.SymGeneralFun (type (-~>) (SymGeneralFun))
import Grisette.Internal.SymPrim.SymInteger (SymInteger (SymInteger))
import Grisette.Internal.SymPrim.SymTabularFun (type (=~>) (SymTabularFun))
import Grisette.Internal.SymPrim.TabularFun (type (=->))
import Grisette.Internal.TH.DeriveBuiltin (deriveBuiltins)
import Grisette.Internal.TH.DeriveInstanceProvider
  ( Strategy (ViaDefault, ViaDefault1),
  )
import Grisette.Internal.Utils.Derive (Arity0, Arity1)

-- $setup
-- >>> import Grisette.Core
-- >>> import Grisette.SymPrim

-- | Substitution of symbolic constants.
--
-- >>> a = "a" :: TypedSymbol Bool
-- >>> v = "x" .&& "y" :: SymBool
-- >>> substituteSym a v (["a" .&& "b", "a"] :: [SymBool])
-- [(&& (&& x y) b),(&& x y)]
--
-- __Note 1:__ This type class can be derived for algebraic data types.
-- You may need the @DerivingVia@ and @DerivingStrategies@ extensions.
--
-- > data X = ... deriving Generic deriving SubstituteSym via (Default X)
class SubstituteSym a where
  -- Substitute a symbolic constant to some symbolic value
  --
  -- >>> substituteSym "a" ("c" .&& "d" :: Sym Bool) ["a" .&& "b" :: Sym Bool, "a"]
  -- [(&& (&& c d) b),(&& c d)]
  substituteSym :: (LinkedRep cb sb) => TypedSymbol cb -> sb -> a -> a

class
  (forall a. (SubstituteSym a) => SubstituteSym (f a)) =>
  SubstituteSym1 f
  where
  liftSubstituteSym ::
    (LinkedRep cb sb) =>
    (TypedSymbol cb -> sb -> a -> a) ->
    TypedSymbol cb ->
    sb ->
    f a ->
    f a

substituteSym1 ::
  (SubstituteSym1 f, SubstituteSym a, LinkedRep cb sb) =>
  TypedSymbol cb ->
  sb ->
  f a ->
  f a
substituteSym1 = liftSubstituteSym substituteSym

class
  (forall a. (SubstituteSym a) => SubstituteSym1 (f a)) =>
  SubstituteSym2 f
  where
  liftSubstituteSym2 ::
    (LinkedRep cb sb) =>
    (TypedSymbol cb -> sb -> a -> a) ->
    (TypedSymbol cb -> sb -> b -> b) ->
    TypedSymbol cb ->
    sb ->
    f a b ->
    f a b

substituteSym2 ::
  (SubstituteSym2 f, SubstituteSym a, SubstituteSym b, LinkedRep cb sb) =>
  TypedSymbol cb ->
  sb ->
  f a b ->
  f a b
substituteSym2 = liftSubstituteSym2 substituteSym substituteSym

-- Derivations

-- | The arguments to the generic 'substituteSym' function.
data family SubstituteSymArgs arity a cb sb :: Type

data instance SubstituteSymArgs Arity0 _ _ _ = SubstituteSymArgs0

newtype instance SubstituteSymArgs Arity1 a cb sb
  = SubstituteSymArgs1 (TypedSymbol cb -> sb -> a -> a)

class GSubstituteSym arity f where
  gsubstituteSym ::
    (LinkedRep cb sb) =>
    SubstituteSymArgs arity a cb sb ->
    TypedSymbol cb ->
    sb ->
    f a ->
    f a

instance GSubstituteSym arity V1 where
  gsubstituteSym _ _ _ = id
  {-# INLINE gsubstituteSym #-}

instance GSubstituteSym arity U1 where
  gsubstituteSym _ _ _ = id
  {-# INLINE gsubstituteSym #-}

instance (SubstituteSym a) => GSubstituteSym arity (K1 i a) where
  gsubstituteSym _ sym val (K1 v) = K1 $ substituteSym sym val v
  {-# INLINE gsubstituteSym #-}

instance (GSubstituteSym arity a) => GSubstituteSym arity (M1 i c a) where
  gsubstituteSym args sym val (M1 v) = M1 $ gsubstituteSym args sym val v
  {-# INLINE gsubstituteSym #-}

instance (GSubstituteSym arity a, GSubstituteSym arity b) => GSubstituteSym arity (a :*: b) where
  gsubstituteSym args sym val (a :*: b) =
    gsubstituteSym args sym val a :*: gsubstituteSym args sym val b
  {-# INLINE gsubstituteSym #-}

instance (GSubstituteSym arity a, GSubstituteSym arity b) => GSubstituteSym arity (a :+: b) where
  gsubstituteSym args sym val (L1 l) = L1 $ gsubstituteSym args sym val l
  gsubstituteSym args sym val (R1 r) = R1 $ gsubstituteSym args sym val r
  {-# INLINE gsubstituteSym #-}

instance (SubstituteSym1 a) => GSubstituteSym Arity1 (Rec1 a) where
  gsubstituteSym (SubstituteSymArgs1 f) sym val (Rec1 v) =
    Rec1 $ liftSubstituteSym f sym val v
  {-# INLINE gsubstituteSym #-}

instance GSubstituteSym Arity1 Par1 where
  gsubstituteSym (SubstituteSymArgs1 f) sym val (Par1 v) = Par1 $ f sym val v
  {-# INLINE gsubstituteSym #-}

instance
  (SubstituteSym1 f, GSubstituteSym Arity1 g) =>
  GSubstituteSym Arity1 (f :.: g)
  where
  gsubstituteSym targs sym val (Comp1 x) =
    Comp1 $ liftSubstituteSym (gsubstituteSym targs) sym val x
  {-# INLINE gsubstituteSym #-}

genericSubstituteSym ::
  (Generic a, GSubstituteSym Arity0 (Rep a), LinkedRep cb sb) =>
  TypedSymbol cb ->
  sb ->
  a ->
  a
genericSubstituteSym sym val =
  to . gsubstituteSym SubstituteSymArgs0 sym val . from
{-# INLINE genericSubstituteSym #-}

genericLiftSubstituteSym ::
  (Generic1 f, GSubstituteSym Arity1 (Rep1 f), LinkedRep cb sb) =>
  (TypedSymbol cb -> sb -> a -> a) ->
  TypedSymbol cb ->
  sb ->
  f a ->
  f a
genericLiftSubstituteSym f sym val =
  to1 . gsubstituteSym (SubstituteSymArgs1 f) sym val . from1
{-# INLINE genericLiftSubstituteSym #-}

instance
  (Generic a, GSubstituteSym Arity0 (Rep a)) =>
  SubstituteSym (Default a)
  where
  substituteSym sym val = Default . genericSubstituteSym sym val . unDefault
  {-# INLINE substituteSym #-}

instance
  (Generic1 f, GSubstituteSym Arity1 (Rep1 f), SubstituteSym a) =>
  SubstituteSym (Default1 f a)
  where
  substituteSym = substituteSym1
  {-# INLINE substituteSym #-}

instance
  (Generic1 f, GSubstituteSym Arity1 (Rep1 f)) =>
  SubstituteSym1 (Default1 f)
  where
  liftSubstituteSym f sym val =
    Default1 . genericLiftSubstituteSym f sym val . unDefault1
  {-# INLINE liftSubstituteSym #-}

-- Instances
deriveBuiltins
  (ViaDefault ''SubstituteSym)
  [''SubstituteSym]
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
  (ViaDefault1 ''SubstituteSym1)
  [''SubstituteSym, ''SubstituteSym1]
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
  (SubstituteSym1 m, SubstituteSym e, SubstituteSym a) =>
  SubstituteSym (ExceptT e m a)
  where
  substituteSym = substituteSym1
  {-# INLINE substituteSym #-}

instance
  (SubstituteSym1 m, SubstituteSym e) =>
  SubstituteSym1 (ExceptT e m)
  where
  liftSubstituteSym f sym val (ExceptT v) =
    ExceptT $ liftSubstituteSym (liftSubstituteSym f) sym val v
  {-# INLINE liftSubstituteSym #-}

-- MaybeT
instance
  (SubstituteSym1 m, SubstituteSym a) =>
  SubstituteSym (MaybeT m a)
  where
  substituteSym = substituteSym1
  {-# INLINE substituteSym #-}

instance
  (SubstituteSym1 m) =>
  SubstituteSym1 (MaybeT m)
  where
  liftSubstituteSym f sym val (MaybeT v) =
    MaybeT $ liftSubstituteSym (liftSubstituteSym f) sym val v
  {-# INLINE liftSubstituteSym #-}

-- WriterT
instance
  (SubstituteSym1 m, SubstituteSym a, SubstituteSym s) =>
  SubstituteSym (WriterLazy.WriterT s m a)
  where
  substituteSym = substituteSym1
  {-# INLINE substituteSym #-}

instance
  (SubstituteSym1 m, SubstituteSym s) =>
  SubstituteSym1 (WriterLazy.WriterT s m)
  where
  liftSubstituteSym f sym val (WriterLazy.WriterT v) =
    WriterLazy.WriterT $
      liftSubstituteSym (liftSubstituteSym2 f substituteSym) sym val v
  {-# INLINE liftSubstituteSym #-}

instance
  (SubstituteSym1 m, SubstituteSym a, SubstituteSym s) =>
  SubstituteSym (WriterStrict.WriterT s m a)
  where
  substituteSym = substituteSym1
  {-# INLINE substituteSym #-}

instance
  (SubstituteSym1 m, SubstituteSym s) =>
  SubstituteSym1 (WriterStrict.WriterT s m)
  where
  liftSubstituteSym f sym val (WriterStrict.WriterT v) =
    WriterStrict.WriterT $
      liftSubstituteSym (liftSubstituteSym2 f substituteSym) sym val v
  {-# INLINE liftSubstituteSym #-}

-- Sum
deriving via
  (Default (Sum f g a))
  instance
    (SubstituteSym (f a), SubstituteSym (g a)) =>
    SubstituteSym (Sum f g a)

deriving via
  (Default1 (Sum f g))
  instance
    (SubstituteSym1 f, SubstituteSym1 g) =>
    SubstituteSym1 (Sum f g)

-- IdentityT
instance
  (SubstituteSym1 m, SubstituteSym a) =>
  SubstituteSym (IdentityT m a)
  where
  substituteSym = substituteSym1
  {-# INLINE substituteSym #-}

instance (SubstituteSym1 m) => SubstituteSym1 (IdentityT m) where
  liftSubstituteSym f sym val (IdentityT a) =
    IdentityT $ liftSubstituteSym f sym val a
  {-# INLINE liftSubstituteSym #-}

-- SubstituteSym2
instance SubstituteSym2 Either where
  liftSubstituteSym2 f _ sym val (Left x) = Left $ f sym val x
  liftSubstituteSym2 _ g sym val (Right y) = Right $ g sym val y
  {-# INLINE liftSubstituteSym2 #-}

instance SubstituteSym2 (,) where
  liftSubstituteSym2 f g sym val (x, y) = (f sym val x, g sym val y)
  {-# INLINE liftSubstituteSym2 #-}

instance (SubstituteSym a) => SubstituteSym2 ((,,) a) where
  liftSubstituteSym2 f g sym val (x, y, z) =
    (substituteSym sym val x, f sym val y, g sym val z)
  {-# INLINE liftSubstituteSym2 #-}

instance (SubstituteSym a, SubstituteSym b) => SubstituteSym2 ((,,,) a b) where
  liftSubstituteSym2 f g sym val (x, y, z, w) =
    (substituteSym sym val x, substituteSym sym val y, f sym val z, g sym val w)
  {-# INLINE liftSubstituteSym2 #-}

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
CONCRETE_SUBSTITUTESYM(Float)
CONCRETE_SUBSTITUTESYM(Double)
CONCRETE_SUBSTITUTESYM(B.ByteString)
CONCRETE_SUBSTITUTESYM(T.Text)
CONCRETE_SUBSTITUTESYM_BV(WordN)
CONCRETE_SUBSTITUTESYM_BV(IntN)
CONCRETE_SUBSTITUTESYM(FPRoundingMode)
#endif

instance (ValidFP eb sb) => SubstituteSym (FP eb sb) where
  substituteSym _ _ = id

#define SUBSTITUTE_SYM_SIMPLE(symtype) \
instance SubstituteSym symtype where \
  substituteSym sym v (symtype t) = symtype $ substTerm sym (underlyingTerm v) t

#define SUBSTITUTE_SYM_BV(symtype) \
instance (KnownNat n, 1 <= n) => SubstituteSym (symtype n) where \
  substituteSym sym v (symtype t) = symtype $ substTerm sym (underlyingTerm v) t

#define SUBSTITUTE_SYM_FUN(cop, op, cons) \
instance (SupportedPrim (cop ca cb), LinkedRep ca sa, LinkedRep cb sb) => \
  SubstituteSym (op sa sb) where \
  substituteSym sym v (cons t) = cons $ substTerm sym (underlyingTerm v) t

#if 1
SUBSTITUTE_SYM_SIMPLE(SymBool)
SUBSTITUTE_SYM_SIMPLE(SymInteger)
SUBSTITUTE_SYM_BV(SymIntN)
SUBSTITUTE_SYM_BV(SymWordN)
SUBSTITUTE_SYM_FUN((=->), (=~>), SymTabularFun)
SUBSTITUTE_SYM_FUN((-->), (-~>), SymGeneralFun)
SUBSTITUTE_SYM_SIMPLE(SymFPRoundingMode)
#endif

instance (ValidFP eb sb) => SubstituteSym (SymFP eb sb) where
  substituteSym sym v (SymFP t) = SymFP $ substTerm sym (underlyingTerm v) t
