{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      :   Grisette.Internal.Internal.Impl.Core.Data.Class.ToCon
-- Copyright   :   (c) Sirui Lu 2021-2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Internal.Internal.Impl.Core.Data.Class.ToCon () where

import Control.Exception (ArithException)
import Control.Monad.Except (ExceptT (ExceptT))
import Control.Monad.Identity
  ( Identity (Identity, runIdentity),
    IdentityT (IdentityT),
  )
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
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.Generics
  ( K1 (K1),
    M1 (M1),
    Par1 (Par1),
    Rec1 (Rec1),
    U1,
    V1,
    (:.:) (Comp1),
    type (:*:),
    type (:+:),
  )
import GHC.TypeNats (KnownNat, type (<=))
import Generics.Deriving (Default (Default), Default1 (Default1))
import Generics.Deriving.Instances ()
import Grisette.Internal.Core.Control.Exception
  ( AssertionError,
    VerificationConditions,
  )
import Grisette.Internal.Core.Data.Class.BitCast (bitCastOrCanonical)
import Grisette.Internal.Core.Data.Class.Solvable
  ( Solvable (conView),
    pattern Con,
  )
import Grisette.Internal.Internal.Decl.Core.Data.Class.ToCon
  ( ToCon (toCon),
    ToCon1 (liftToCon),
    ToCon2,
    toCon1,
  )
import Grisette.Internal.SymPrim.AlgReal (AlgReal (AlgExactRational))
import Grisette.Internal.SymPrim.BV
  ( IntN (IntN),
    WordN (WordN),
  )
import Grisette.Internal.SymPrim.FP
  ( FP,
    FP32,
    FP64,
    FPRoundingMode,
    NotRepresentableFPError,
    ValidFP,
  )
import Grisette.Internal.SymPrim.GeneralFun (type (-->))
import Grisette.Internal.SymPrim.IntBitwidth (intBitwidthQ)
import Grisette.Internal.SymPrim.Prim.Term (LinkedRep)
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
import Grisette.Internal.SymPrim.SymGeneralFun (type (-~>) (SymGeneralFun))
import Grisette.Internal.SymPrim.SymInteger (SymInteger)
import Grisette.Internal.SymPrim.SymTabularFun (type (=~>) (SymTabularFun))
import Grisette.Internal.SymPrim.TabularFun (type (=->))
import Grisette.Internal.TH.Derivation.Derive (derive)

-- $setup
-- >>> import Grisette.Core
-- >>> import Grisette.SymPrim

#define CONCRETE_TOCON(type) \
instance ToCon type type where \
  toCon = Just

#define CONCRETE_TOCON_BV(type) \
instance (KnownNat n, 1 <= n) => ToCon (type n) (type n) where \
  toCon = Just

#if 1
CONCRETE_TOCON(Bool)
CONCRETE_TOCON(Integer)
CONCRETE_TOCON(Char)
CONCRETE_TOCON(Int)
CONCRETE_TOCON(Int8)
CONCRETE_TOCON(Int16)
CONCRETE_TOCON(Int32)
CONCRETE_TOCON(Int64)
CONCRETE_TOCON(Word)
CONCRETE_TOCON(Word8)
CONCRETE_TOCON(Word16)
CONCRETE_TOCON(Word32)
CONCRETE_TOCON(Word64)
CONCRETE_TOCON(Float)
CONCRETE_TOCON(Double)
CONCRETE_TOCON(B.ByteString)
CONCRETE_TOCON(T.Text)
CONCRETE_TOCON_BV(WordN)
CONCRETE_TOCON_BV(IntN)
CONCRETE_TOCON(FPRoundingMode)
CONCRETE_TOCON(Monoid.All)
CONCRETE_TOCON(Monoid.Any)
CONCRETE_TOCON(Ordering)
#endif

instance (ValidFP eb sb) => ToCon (FP eb sb) (FP eb sb) where
  toCon = Just

instance ToCon (a =-> b) (a =-> b) where
  toCon = Just

instance ToCon (a --> b) (a --> b) where
  toCon = Just

#define TO_CON_SYMID_SIMPLE(symtype) \
instance ToCon symtype symtype where \
  toCon = Just

#define TO_CON_SYMID_BV(symtype) \
instance ToCon (symtype n) (symtype n) where \
  toCon = Just

#define TO_CON_SYMID_FUN(op) \
instance ToCon (a op b) (a op b) where \
  toCon = Just

#if 1
TO_CON_SYMID_SIMPLE(SymBool)
TO_CON_SYMID_SIMPLE(SymInteger)
TO_CON_SYMID_SIMPLE(SymAlgReal)
TO_CON_SYMID_BV(SymIntN)
TO_CON_SYMID_BV(SymWordN)
TO_CON_SYMID_FUN(=~>)
TO_CON_SYMID_FUN(-~>)
TO_CON_SYMID_SIMPLE(SymFPRoundingMode)
#endif

instance (ValidFP eb sb) => ToCon (SymFP eb sb) (SymFP eb sb) where
  toCon = Just

#define TO_CON_FROMSYM_SIMPLE(contype, symtype) \
instance ToCon symtype contype where \
  toCon = conView

#define TO_CON_FROMSYM_BV(contype, symtype) \
instance (KnownNat n, 1 <= n) => ToCon (symtype n) (contype n) where \
  toCon = conView

#define TO_CON_FROMSYM_FUN(conop, symop, consop) \
instance (LinkedRep ca sa, LinkedRep cb sb) => ToCon (symop sa sb) (conop ca cb) where \
  toCon a@(consop _) = conView a

#if 1
-- TO_CON_FROMSYM_SIMPLE(Bool, SymBool)
TO_CON_FROMSYM_SIMPLE(Integer, SymInteger)
TO_CON_FROMSYM_SIMPLE(AlgReal, SymAlgReal)
TO_CON_FROMSYM_BV(IntN, SymIntN)
TO_CON_FROMSYM_BV(WordN, SymWordN)
TO_CON_FROMSYM_FUN((=->), (=~>), SymTabularFun)
TO_CON_FROMSYM_FUN((-->), (-~>), SymGeneralFun)
TO_CON_FROMSYM_SIMPLE(FPRoundingMode, SymFPRoundingMode)
#endif

instance (ValidFP eb sb) => ToCon (SymFP eb sb) (FP eb sb) where
  toCon = conView

#define TOCON_MACHINE_INTEGER(sbvw, bvw, n, int) \
instance ToCon (sbvw n) int where \
  toCon (Con (bvw v :: bvw n)) = Just $ fromIntegral v; \
  toCon _ = Nothing

#if 1
TOCON_MACHINE_INTEGER(SymIntN, IntN, 8, Int8)
TOCON_MACHINE_INTEGER(SymIntN, IntN, 16, Int16)
TOCON_MACHINE_INTEGER(SymIntN, IntN, 32, Int32)
TOCON_MACHINE_INTEGER(SymIntN, IntN, 64, Int64)
TOCON_MACHINE_INTEGER(SymWordN, WordN, 8, Word8)
TOCON_MACHINE_INTEGER(SymWordN, WordN, 16, Word16)
TOCON_MACHINE_INTEGER(SymWordN, WordN, 32, Word32)
TOCON_MACHINE_INTEGER(SymWordN, WordN, 64, Word64)
TOCON_MACHINE_INTEGER(SymIntN, IntN, $intBitwidthQ, Int)
TOCON_MACHINE_INTEGER(SymWordN, WordN, $intBitwidthQ, Word)
#endif

instance ToCon SymFP32 Float where
  toCon (Con (fp :: FP32)) = Just $ bitCastOrCanonical fp
  toCon _ = Nothing

instance ToCon SymFP64 Double where
  toCon (Con (fp :: FP64)) = Just $ bitCastOrCanonical fp
  toCon _ = Nothing

instance (ToCon a b, Integral b) => ToCon (Ratio a) (Ratio b) where
  toCon r = do
    n <- toCon (numerator r)
    d <- toCon (denominator r)
    return $ n % d

instance ToCon SymAlgReal Rational where
  toCon (Con (x :: AlgReal)) =
    case x of
      AlgExactRational r -> Just r
      _ -> Nothing
  toCon _ = Nothing

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
  [''ToCon, ''ToCon1, ''ToCon2]

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
  [''ToCon, ''ToCon1]

derive
  [ ''(),
    ''AssertionError,
    ''VerificationConditions,
    ''NotRepresentableFPError,
    ''ArithException
  ]
  [''ToCon]

-- IdentityT
instance
  (ToCon1 m m1, ToCon a b) =>
  ToCon (IdentityT m a) (IdentityT m1 b)
  where
  toCon = toCon1
  {-# INLINE toCon #-}

instance
  (ToCon1 m m1) =>
  ToCon1 (IdentityT m) (IdentityT m1)
  where
  liftToCon f (IdentityT a) = IdentityT <$> liftToCon f a
  {-# INLINE liftToCon #-}

-- Identity
instance {-# INCOHERENT #-} (ToCon a b) => ToCon (Identity a) (Identity b) where
  toCon = toCon1

instance {-# INCOHERENT #-} (ToCon a b) => ToCon (Identity a) b where
  toCon = toCon . runIdentity

instance {-# INCOHERENT #-} (ToCon a b) => ToCon a (Identity b) where
  toCon = fmap Identity . toCon

instance ToCon1 Identity Identity where
  liftToCon f (Identity a) = Identity <$> f a

-- Special
instance
  (ToCon (m1 (Either e1 a)) (Either e2 b)) =>
  ToCon (ExceptT e1 m1 a) (Either e2 b)
  where
  toCon (ExceptT v) = toCon v

-- Product
deriving via
  (Default (Product l r a))
  instance
    (ToCon (l0 a0) (l a), ToCon (r0 a0) (r a)) =>
    ToCon (Product l0 r0 a0) (Product l r a)

deriving via
  (Default1 (Product l r))
  instance
    (ToCon1 l0 l, ToCon1 r0 r) => ToCon1 (Product l0 r0) (Product l r)

-- Sum
deriving via
  (Default (Sum l r a))
  instance
    (ToCon (l0 a0) (l a), ToCon (r0 a0) (r a)) =>
    ToCon (Sum l0 r0 a0) (Sum l r a)

deriving via
  (Default1 (Sum l r))
  instance
    (ToCon1 l0 l, ToCon1 r0 r) => ToCon1 (Sum l0 r0) (Sum l r)

-- Compose
deriving via
  (Default (Compose f g a))
  instance
    (ToCon (f0 (g0 a0)) (f (g a))) => ToCon (Compose f0 g0 a0) (Compose f g a)

instance
  (ToCon1 f0 f, ToCon1 g0 g) =>
  ToCon1 (Compose f0 g0) (Compose f g)
  where
  liftToCon f (Compose a) = Compose <$> liftToCon (liftToCon f) a
  {-# INLINE liftToCon #-}

-- Const
deriving via
  (Default (Const a b))
  instance
    (ToCon a0 a) => ToCon (Const a0 b0) (Const a b)

deriving via
  (Default1 (Const a))
  instance
    (ToCon a0 a) => ToCon1 (Const a0) (Const a)

-- Alt
deriving via
  (Default (Alt f a))
  instance
    (ToCon (f0 a0) (f a)) => ToCon (Alt f0 a0) (Alt f a)

deriving via
  (Default1 (Alt f))
  instance
    (ToCon1 f0 f) => ToCon1 (Alt f0) (Alt f)

-- Ap
deriving via
  (Default (Ap f a))
  instance
    (ToCon (f0 a0) (f a)) => ToCon (Ap f0 a0) (Ap f a)

deriving via
  (Default1 (Ap f))
  instance
    (ToCon1 f0 f) => ToCon1 (Ap f0) (Ap f)

-- Generic
deriving via (Default (U1 p)) instance ToCon (U1 p0) (U1 p)

deriving via (Default (V1 p)) instance ToCon (V1 p0) (V1 p)

deriving via
  (Default (K1 i c p))
  instance
    (ToCon c0 c) => ToCon (K1 i0 c0 p0) (K1 i c p)

deriving via
  (Default (M1 i c f p))
  instance
    (ToCon (f0 p0) (f p)) => ToCon (M1 i0 c0 f0 p0) (M1 i c f p)

deriving via
  (Default ((f :+: g) p))
  instance
    (ToCon (f0 p0) (f p), ToCon (g0 p0) (g p)) =>
    ToCon ((f0 :+: g0) p0) ((f :+: g) p)

deriving via
  (Default ((f :*: g) p))
  instance
    (ToCon (f0 p0) (f p), ToCon (g0 p0) (g p)) =>
    ToCon ((f0 :*: g0) p0) ((f :*: g) p)

deriving via
  (Default (Par1 p))
  instance
    (ToCon p0 p) => ToCon (Par1 p0) (Par1 p)

deriving via
  (Default (Rec1 f p))
  instance
    (ToCon (f0 p0) (f p)) => ToCon (Rec1 f0 p0) (Rec1 f p)

deriving via
  (Default ((f :.: g) p))
  instance
    (ToCon (f0 (g0 p0)) (f (g p))) => ToCon ((f0 :.: g0) p0) ((f :.: g) p)
