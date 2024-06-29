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
-- Module      :   Grisette.Internal.Core.Data.Class.ExtractSymbolics
-- Copyright   :   (c) Sirui Lu 2021-2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Internal.Core.Data.Class.ExtractSymbolics
  ( -- * Extracting symbolic constant set from a value
    ExtractSymbolics (..),
    ExtractSymbolics1 (..),
    extractSymbolics1,
    ExtractSymbolics2 (..),
    extractSymbolics2,

    -- * Generic 'ExtractSymbolics'
    ExtractSymbolicsArgs (..),
    GExtractSymbolics (..),
    genericExtractSymbolics,
    genericLiftExtractSymbolics,
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
  ( Default (Default, unDefault),
    Default1 (Default1, unDefault1),
    Generic (Rep, from),
    Generic1 (Rep1, from1),
    K1 (K1),
    M1 (M1),
    Par1 (Par1),
    Rec1 (Rec1),
    U1,
    V1,
    type (:*:) ((:*:)),
    type (:+:) (L1, R1),
    type (:.:) (Comp1),
  )
import Grisette.Internal.Core.Control.Exception (AssertionError, VerificationConditions)
import Grisette.Internal.SymPrim.BV (IntN, WordN)
import Grisette.Internal.SymPrim.FP (FP, FPRoundingMode, ValidFP)
import Grisette.Internal.SymPrim.GeneralFun (type (-->))
import Grisette.Internal.SymPrim.Prim.Model
  ( SymbolSet (SymbolSet),
  )
import Grisette.Internal.SymPrim.Prim.Term
  ( LinkedRep,
    SupportedPrim,
  )
import Grisette.Internal.SymPrim.Prim.TermUtils (extractSymbolicsTerm)
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
-- >>> import Grisette.Lib.Base
-- >>> import Data.HashSet as HashSet
-- >>> import Data.List (sort)

-- | Extracts all the symbolic variables that are transitively contained in the given value.
--
-- >>> extractSymbolics ("a" :: SymBool) :: SymbolSet
-- SymbolSet {a :: Bool}
--
-- >>> extractSymbolics (mrgIf "a" (mrgReturn ["b"]) (mrgReturn ["c", "d"]) :: Union [SymBool]) :: SymbolSet
-- SymbolSet {a :: Bool, b :: Bool, c :: Bool, d :: Bool}
--
-- __Note 1:__ This type class can be derived for algebraic data types.
-- You may need the @DerivingVia@ and @DerivingStrategies@ extensions.
--
-- > data X = ... deriving Generic deriving ExtractSymbolics via (Default X)
class ExtractSymbolics a where
  extractSymbolics :: a -> SymbolSet

class
  (forall a. (ExtractSymbolics a) => ExtractSymbolics (f a)) =>
  ExtractSymbolics1 f
  where
  liftExtractSymbolics :: (a -> SymbolSet) -> f a -> SymbolSet

extractSymbolics1 :: (ExtractSymbolics1 f, ExtractSymbolics a) => f a -> SymbolSet
extractSymbolics1 = liftExtractSymbolics extractSymbolics
{-# INLINE extractSymbolics1 #-}

class
  (forall a. (ExtractSymbolics a) => ExtractSymbolics1 (f a)) =>
  ExtractSymbolics2 f
  where
  liftExtractSymbolics2 ::
    (a -> SymbolSet) -> (b -> SymbolSet) -> f a b -> SymbolSet

extractSymbolics2 ::
  (ExtractSymbolics2 f, ExtractSymbolics a, ExtractSymbolics b) =>
  f a b ->
  SymbolSet
extractSymbolics2 = liftExtractSymbolics2 extractSymbolics extractSymbolics
{-# INLINE extractSymbolics2 #-}

data family ExtractSymbolicsArgs arity a :: Type

data instance ExtractSymbolicsArgs Arity0 _ = ExtractSymbolicsArgs0

newtype instance ExtractSymbolicsArgs Arity1 a
  = ExtractSymbolicsArgs1 (a -> SymbolSet)

class GExtractSymbolics arity f where
  gextractSymbolics :: ExtractSymbolicsArgs arity a -> f a -> SymbolSet

instance GExtractSymbolics arity V1 where
  gextractSymbolics _ _ = mempty
  {-# INLINE gextractSymbolics #-}

instance GExtractSymbolics arity U1 where
  gextractSymbolics _ _ = mempty
  {-# INLINE gextractSymbolics #-}

instance (GExtractSymbolics arity a) => GExtractSymbolics arity (M1 i c a) where
  gextractSymbolics args (M1 x) = gextractSymbolics args x
  {-# INLINE gextractSymbolics #-}

instance (ExtractSymbolics a) => GExtractSymbolics arity (K1 i a) where
  gextractSymbolics _ (K1 x) = extractSymbolics x
  {-# INLINE gextractSymbolics #-}

instance
  (GExtractSymbolics arity a, GExtractSymbolics arity b) =>
  GExtractSymbolics arity (a :+: b)
  where
  gextractSymbolics args (L1 x) = gextractSymbolics args x
  gextractSymbolics args (R1 x) = gextractSymbolics args x
  {-# INLINE gextractSymbolics #-}

instance
  (GExtractSymbolics arity a, GExtractSymbolics arity b) =>
  GExtractSymbolics arity (a :*: b)
  where
  gextractSymbolics args (x :*: y) =
    gextractSymbolics args x <> gextractSymbolics args y
  {-# INLINE gextractSymbolics #-}

instance GExtractSymbolics Arity1 Par1 where
  gextractSymbolics (ExtractSymbolicsArgs1 f) (Par1 x) = f x
  {-# INLINE gextractSymbolics #-}

instance (ExtractSymbolics1 a) => GExtractSymbolics Arity1 (Rec1 a) where
  gextractSymbolics (ExtractSymbolicsArgs1 f) (Rec1 x) =
    liftExtractSymbolics f x
  {-# INLINE gextractSymbolics #-}

instance
  (ExtractSymbolics1 f, GExtractSymbolics Arity1 g) =>
  GExtractSymbolics Arity1 (f :.: g)
  where
  gextractSymbolics targs (Comp1 x) =
    liftExtractSymbolics (gextractSymbolics targs) x
  {-# INLINE gextractSymbolics #-}

genericExtractSymbolics ::
  (Generic a, GExtractSymbolics Arity0 (Rep a)) =>
  a ->
  SymbolSet
genericExtractSymbolics = gextractSymbolics ExtractSymbolicsArgs0 . from

genericLiftExtractSymbolics ::
  (Generic1 f, GExtractSymbolics Arity1 (Rep1 f)) =>
  (a -> SymbolSet) ->
  f a ->
  SymbolSet
genericLiftExtractSymbolics f =
  gextractSymbolics (ExtractSymbolicsArgs1 f) . from1

instance
  (Generic a, GExtractSymbolics Arity0 (Rep a)) =>
  ExtractSymbolics (Default a)
  where
  extractSymbolics = genericExtractSymbolics . unDefault
  {-# INLINE extractSymbolics #-}

instance
  (Generic1 f, GExtractSymbolics Arity1 (Rep1 f), ExtractSymbolics a) =>
  ExtractSymbolics (Default1 f a)
  where
  extractSymbolics = extractSymbolics1
  {-# INLINE extractSymbolics #-}

instance
  (Generic1 f, GExtractSymbolics Arity1 (Rep1 f)) =>
  ExtractSymbolics1 (Default1 f)
  where
  liftExtractSymbolics f = genericLiftExtractSymbolics f . unDefault1
  {-# INLINE liftExtractSymbolics #-}

#define CONCRETE_EXTRACT_SYMBOLICS(type) \
instance ExtractSymbolics type where \
  extractSymbolics _ = mempty

#define CONCRETE_EXTRACT_SYMBOLICS_BV(type) \
instance (KnownNat n, 1 <= n) => ExtractSymbolics (type n) where \
  extractSymbolics _ = mempty

#if 1
CONCRETE_EXTRACT_SYMBOLICS(Bool)
CONCRETE_EXTRACT_SYMBOLICS(Integer)
CONCRETE_EXTRACT_SYMBOLICS(Char)
CONCRETE_EXTRACT_SYMBOLICS(Int)
CONCRETE_EXTRACT_SYMBOLICS(Int8)
CONCRETE_EXTRACT_SYMBOLICS(Int16)
CONCRETE_EXTRACT_SYMBOLICS(Int32)
CONCRETE_EXTRACT_SYMBOLICS(Int64)
CONCRETE_EXTRACT_SYMBOLICS(Word)
CONCRETE_EXTRACT_SYMBOLICS(Word8)
CONCRETE_EXTRACT_SYMBOLICS(Word16)
CONCRETE_EXTRACT_SYMBOLICS(Word32)
CONCRETE_EXTRACT_SYMBOLICS(Word64)
CONCRETE_EXTRACT_SYMBOLICS(Float)
CONCRETE_EXTRACT_SYMBOLICS(Double)
CONCRETE_EXTRACT_SYMBOLICS(B.ByteString)
CONCRETE_EXTRACT_SYMBOLICS(T.Text)
CONCRETE_EXTRACT_SYMBOLICS(FPRoundingMode)
CONCRETE_EXTRACT_SYMBOLICS(Monoid.All)
CONCRETE_EXTRACT_SYMBOLICS(Monoid.Any)
CONCRETE_EXTRACT_SYMBOLICS(Ordering)
CONCRETE_EXTRACT_SYMBOLICS_BV(WordN)
CONCRETE_EXTRACT_SYMBOLICS_BV(IntN)
#endif

instance (ValidFP eb sb) => ExtractSymbolics (FP eb sb) where
  extractSymbolics _ = mempty

#define EXTRACT_SYMBOLICS_SIMPLE(symtype) \
instance ExtractSymbolics symtype where \
  extractSymbolics (symtype t) = SymbolSet $ extractSymbolicsTerm t

#define EXTRACT_SYMBOLICS_BV(symtype) \
instance (KnownNat n, 1 <= n) => ExtractSymbolics (symtype n) where \
  extractSymbolics (symtype t) = SymbolSet $ extractSymbolicsTerm t

#define EXTRACT_SYMBOLICS_FUN(cop, op, cons) \
instance (SupportedPrim (cop ca cb), LinkedRep ca sa, LinkedRep cb sb) => \
  ExtractSymbolics (op sa sb) where \
  extractSymbolics (cons t) = SymbolSet $ extractSymbolicsTerm t

#if 1
EXTRACT_SYMBOLICS_SIMPLE(SymBool)
EXTRACT_SYMBOLICS_SIMPLE(SymInteger)
EXTRACT_SYMBOLICS_SIMPLE(SymFPRoundingMode)
EXTRACT_SYMBOLICS_BV(SymIntN)
EXTRACT_SYMBOLICS_BV(SymWordN)
EXTRACT_SYMBOLICS_FUN((=->), (=~>), SymTabularFun)
EXTRACT_SYMBOLICS_FUN((-->), (-~>), SymGeneralFun)
#endif

instance (ValidFP eb fb) => ExtractSymbolics (SymFP eb fb) where
  extractSymbolics (SymFP t) = SymbolSet $ extractSymbolicsTerm t

-- Instances
deriveBuiltins
  (ViaDefault ''ExtractSymbolics)
  [''ExtractSymbolics]
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
    ''Identity,
    ''Monoid.Dual,
    ''Monoid.Sum,
    ''Monoid.Product,
    ''Monoid.First,
    ''Monoid.Last,
    ''Down
  ]

deriveBuiltins
  (ViaDefault1 ''ExtractSymbolics1)
  [''ExtractSymbolics, ''ExtractSymbolics1]
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
    ''Identity,
    ''Monoid.Dual,
    ''Monoid.Sum,
    ''Monoid.Product,
    ''Monoid.First,
    ''Monoid.Last,
    ''Down
  ]

-- ExceptT
instance
  (ExtractSymbolics1 m, ExtractSymbolics e, ExtractSymbolics a) =>
  ExtractSymbolics (ExceptT e m a)
  where
  extractSymbolics = extractSymbolics1
  {-# INLINE extractSymbolics #-}

instance
  (ExtractSymbolics1 m, ExtractSymbolics e) =>
  ExtractSymbolics1 (ExceptT e m)
  where
  liftExtractSymbolics f (ExceptT v) =
    liftExtractSymbolics (liftExtractSymbolics f) v
  {-# INLINE liftExtractSymbolics #-}

-- MaybeT
instance
  (ExtractSymbolics1 m, ExtractSymbolics a) =>
  ExtractSymbolics (MaybeT m a)
  where
  extractSymbolics = extractSymbolics1
  {-# INLINE extractSymbolics #-}

instance
  (ExtractSymbolics1 m) =>
  ExtractSymbolics1 (MaybeT m)
  where
  liftExtractSymbolics f (MaybeT v) =
    liftExtractSymbolics (liftExtractSymbolics f) v
  {-# INLINE liftExtractSymbolics #-}

-- WriterT
instance
  (ExtractSymbolics1 m, ExtractSymbolics w, ExtractSymbolics a) =>
  ExtractSymbolics (WriterLazy.WriterT w m a)
  where
  extractSymbolics = extractSymbolics1
  {-# INLINE extractSymbolics #-}

instance
  (ExtractSymbolics1 m, ExtractSymbolics w) =>
  ExtractSymbolics1 (WriterLazy.WriterT w m)
  where
  liftExtractSymbolics f (WriterLazy.WriterT v) =
    liftExtractSymbolics (liftExtractSymbolics2 f extractSymbolics) v
  {-# INLINE liftExtractSymbolics #-}

instance
  (ExtractSymbolics1 m, ExtractSymbolics w, ExtractSymbolics a) =>
  ExtractSymbolics (WriterStrict.WriterT w m a)
  where
  extractSymbolics = extractSymbolics1
  {-# INLINE extractSymbolics #-}

instance
  (ExtractSymbolics1 m, ExtractSymbolics w) =>
  ExtractSymbolics1 (WriterStrict.WriterT w m)
  where
  liftExtractSymbolics f (WriterStrict.WriterT v) =
    liftExtractSymbolics (liftExtractSymbolics2 f extractSymbolics) v
  {-# INLINE liftExtractSymbolics #-}

-- IdentityT
instance
  (ExtractSymbolics1 m, ExtractSymbolics a) =>
  ExtractSymbolics (IdentityT m a)
  where
  extractSymbolics = extractSymbolics1
  {-# INLINE extractSymbolics #-}

instance (ExtractSymbolics1 m) => ExtractSymbolics1 (IdentityT m) where
  liftExtractSymbolics f (IdentityT v) = liftExtractSymbolics f v
  {-# INLINE liftExtractSymbolics #-}

-- Product
deriving via
  (Default (Product l r a))
  instance
    (ExtractSymbolics (l a), ExtractSymbolics (r a)) =>
    ExtractSymbolics (Product l r a)

deriving via
  (Default1 (Product l r))
  instance
    (ExtractSymbolics1 l, ExtractSymbolics1 r) =>
    ExtractSymbolics1 (Product l r)

-- Sum
deriving via
  (Default (Sum l r a))
  instance
    (ExtractSymbolics (l a), ExtractSymbolics (r a)) =>
    ExtractSymbolics (Sum l r a)

deriving via
  (Default1 (Sum l r))
  instance
    (ExtractSymbolics1 l, ExtractSymbolics1 r) => ExtractSymbolics1 (Sum l r)

-- Compose
deriving via
  (Default (Compose f g a))
  instance
    (ExtractSymbolics (f (g a))) => ExtractSymbolics (Compose f g a)

instance
  (ExtractSymbolics1 f, ExtractSymbolics1 g) =>
  ExtractSymbolics1 (Compose f g)
  where
  liftExtractSymbolics f (Compose l) =
    liftExtractSymbolics (liftExtractSymbolics f) l
  {-# INLINE liftExtractSymbolics #-}

-- Const
deriving via
  (Default (Const a b))
  instance
    (ExtractSymbolics a) => ExtractSymbolics (Const a b)

deriving via
  (Default1 (Const a))
  instance
    (ExtractSymbolics a) => ExtractSymbolics1 (Const a)

-- Alt
deriving via
  (Default (Alt f a))
  instance
    (ExtractSymbolics (f a)) => ExtractSymbolics (Alt f a)

deriving via
  (Default1 (Alt f))
  instance
    (ExtractSymbolics1 f) => ExtractSymbolics1 (Alt f)

-- Ap
deriving via
  (Default (Ap f a))
  instance
    (ExtractSymbolics (f a)) => ExtractSymbolics (Ap f a)

deriving via
  (Default1 (Ap f))
  instance
    (ExtractSymbolics1 f) => ExtractSymbolics1 (Ap f)

-- Generic
deriving via (Default (U1 p)) instance ExtractSymbolics (U1 p)

deriving via (Default (V1 p)) instance ExtractSymbolics (V1 p)

deriving via
  (Default (K1 i c p))
  instance
    (ExtractSymbolics c) => ExtractSymbolics (K1 i c p)

deriving via
  (Default (M1 i c f p))
  instance
    (ExtractSymbolics (f p)) => ExtractSymbolics (M1 i c f p)

deriving via
  (Default ((f :+: g) p))
  instance
    (ExtractSymbolics (f p), ExtractSymbolics (g p)) =>
    ExtractSymbolics ((f :+: g) p)

deriving via
  (Default ((f :*: g) p))
  instance
    (ExtractSymbolics (f p), ExtractSymbolics (g p)) =>
    ExtractSymbolics ((f :*: g) p)

deriving via
  (Default (Par1 p))
  instance
    (ExtractSymbolics p) => ExtractSymbolics (Par1 p)

deriving via
  (Default (Rec1 f p))
  instance
    (ExtractSymbolics (f p)) => ExtractSymbolics (Rec1 f p)

deriving via
  (Default ((f :.: g) p))
  instance
    (ExtractSymbolics (f (g p))) => ExtractSymbolics ((f :.: g) p)

-- ExtractSymbolics2
instance ExtractSymbolics2 Either where
  liftExtractSymbolics2 f _ (Left x) = f x
  liftExtractSymbolics2 _ g (Right y) = g y
  {-# INLINE liftExtractSymbolics2 #-}

instance ExtractSymbolics2 (,) where
  liftExtractSymbolics2 f g (x, y) = f x <> g y
  {-# INLINE liftExtractSymbolics2 #-}

instance (ExtractSymbolics a) => ExtractSymbolics2 ((,,) a) where
  liftExtractSymbolics2 f g (x, y, z) = extractSymbolics x <> f y <> g z
  {-# INLINE liftExtractSymbolics2 #-}

instance
  (ExtractSymbolics a, ExtractSymbolics b) =>
  ExtractSymbolics2 ((,,,) a b)
  where
  liftExtractSymbolics2 f g (x, y, z, w) =
    extractSymbolics x <> extractSymbolics y <> f z <> g w
  {-# INLINE liftExtractSymbolics2 #-}
