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
-- Module      :   Grisette.Internal.Core.Data.Class.ExtractSym
-- Copyright   :   (c) Sirui Lu 2021-2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Internal.Core.Data.Class.ExtractSym
  ( -- * Extracting symbolic constant set from a value
    ExtractSym (..),
    ExtractSym1 (..),
    extractSym1,
    ExtractSym2 (..),
    extractSym2,

    -- * Generic 'ExtractSym'
    ExtractSymArgs (..),
    GExtractSym (..),
    genericExtractSym,
    genericLiftExtractSym,
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
import Grisette.Internal.SymPrim.Prim.TermUtils (extractTerm)
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
-- >>> extractSym ("a" :: SymBool) :: SymbolSet
-- SymbolSet {a :: Bool}
--
-- >>> extractSym (mrgIf "a" (mrgReturn ["b"]) (mrgReturn ["c", "d"]) :: Union [SymBool]) :: SymbolSet
-- SymbolSet {a :: Bool, b :: Bool, c :: Bool, d :: Bool}
--
-- __Note 1:__ This type class can be derived for algebraic data types.
-- You may need the @DerivingVia@ and @DerivingStrategies@ extensions.
--
-- > data X = ... deriving Generic deriving ExtractSym via (Default X)
class ExtractSym a where
  extractSym :: a -> SymbolSet

class
  (forall a. (ExtractSym a) => ExtractSym (f a)) =>
  ExtractSym1 f
  where
  liftExtractSym :: (a -> SymbolSet) -> f a -> SymbolSet

extractSym1 :: (ExtractSym1 f, ExtractSym a) => f a -> SymbolSet
extractSym1 = liftExtractSym extractSym
{-# INLINE extractSym1 #-}

class
  (forall a. (ExtractSym a) => ExtractSym1 (f a)) =>
  ExtractSym2 f
  where
  liftExtractSym2 ::
    (a -> SymbolSet) -> (b -> SymbolSet) -> f a b -> SymbolSet

extractSym2 ::
  (ExtractSym2 f, ExtractSym a, ExtractSym b) =>
  f a b ->
  SymbolSet
extractSym2 = liftExtractSym2 extractSym extractSym
{-# INLINE extractSym2 #-}

data family ExtractSymArgs arity a :: Type

data instance ExtractSymArgs Arity0 _ = ExtractSymArgs0

newtype instance ExtractSymArgs Arity1 a
  = ExtractSymArgs1 (a -> SymbolSet)

class GExtractSym arity f where
  gextractSym :: ExtractSymArgs arity a -> f a -> SymbolSet

instance GExtractSym arity V1 where
  gextractSym _ _ = mempty
  {-# INLINE gextractSym #-}

instance GExtractSym arity U1 where
  gextractSym _ _ = mempty
  {-# INLINE gextractSym #-}

instance (GExtractSym arity a) => GExtractSym arity (M1 i c a) where
  gextractSym args (M1 x) = gextractSym args x
  {-# INLINE gextractSym #-}

instance (ExtractSym a) => GExtractSym arity (K1 i a) where
  gextractSym _ (K1 x) = extractSym x
  {-# INLINE gextractSym #-}

instance
  (GExtractSym arity a, GExtractSym arity b) =>
  GExtractSym arity (a :+: b)
  where
  gextractSym args (L1 x) = gextractSym args x
  gextractSym args (R1 x) = gextractSym args x
  {-# INLINE gextractSym #-}

instance
  (GExtractSym arity a, GExtractSym arity b) =>
  GExtractSym arity (a :*: b)
  where
  gextractSym args (x :*: y) =
    gextractSym args x <> gextractSym args y
  {-# INLINE gextractSym #-}

instance GExtractSym Arity1 Par1 where
  gextractSym (ExtractSymArgs1 f) (Par1 x) = f x
  {-# INLINE gextractSym #-}

instance (ExtractSym1 a) => GExtractSym Arity1 (Rec1 a) where
  gextractSym (ExtractSymArgs1 f) (Rec1 x) =
    liftExtractSym f x
  {-# INLINE gextractSym #-}

instance
  (ExtractSym1 f, GExtractSym Arity1 g) =>
  GExtractSym Arity1 (f :.: g)
  where
  gextractSym targs (Comp1 x) =
    liftExtractSym (gextractSym targs) x
  {-# INLINE gextractSym #-}

genericExtractSym ::
  (Generic a, GExtractSym Arity0 (Rep a)) =>
  a ->
  SymbolSet
genericExtractSym = gextractSym ExtractSymArgs0 . from

genericLiftExtractSym ::
  (Generic1 f, GExtractSym Arity1 (Rep1 f)) =>
  (a -> SymbolSet) ->
  f a ->
  SymbolSet
genericLiftExtractSym f =
  gextractSym (ExtractSymArgs1 f) . from1

instance
  (Generic a, GExtractSym Arity0 (Rep a)) =>
  ExtractSym (Default a)
  where
  extractSym = genericExtractSym . unDefault
  {-# INLINE extractSym #-}

instance
  (Generic1 f, GExtractSym Arity1 (Rep1 f), ExtractSym a) =>
  ExtractSym (Default1 f a)
  where
  extractSym = extractSym1
  {-# INLINE extractSym #-}

instance
  (Generic1 f, GExtractSym Arity1 (Rep1 f)) =>
  ExtractSym1 (Default1 f)
  where
  liftExtractSym f = genericLiftExtractSym f . unDefault1
  {-# INLINE liftExtractSym #-}

#define CONCRETE_EXTRACT_SYMBOLICS(type) \
instance ExtractSym type where \
  extractSym _ = mempty

#define CONCRETE_EXTRACT_SYMBOLICS_BV(type) \
instance (KnownNat n, 1 <= n) => ExtractSym (type n) where \
  extractSym _ = mempty

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

instance (ValidFP eb sb) => ExtractSym (FP eb sb) where
  extractSym _ = mempty

#define EXTRACT_SYMBOLICS_SIMPLE(symtype) \
instance ExtractSym symtype where \
  extractSym (symtype t) = SymbolSet $ extractTerm t

#define EXTRACT_SYMBOLICS_BV(symtype) \
instance (KnownNat n, 1 <= n) => ExtractSym (symtype n) where \
  extractSym (symtype t) = SymbolSet $ extractTerm t

#define EXTRACT_SYMBOLICS_FUN(cop, op, cons) \
instance (SupportedPrim (cop ca cb), LinkedRep ca sa, LinkedRep cb sb) => \
  ExtractSym (op sa sb) where \
  extractSym (cons t) = SymbolSet $ extractTerm t

#if 1
EXTRACT_SYMBOLICS_SIMPLE(SymBool)
EXTRACT_SYMBOLICS_SIMPLE(SymInteger)
EXTRACT_SYMBOLICS_SIMPLE(SymFPRoundingMode)
EXTRACT_SYMBOLICS_BV(SymIntN)
EXTRACT_SYMBOLICS_BV(SymWordN)
EXTRACT_SYMBOLICS_FUN((=->), (=~>), SymTabularFun)
EXTRACT_SYMBOLICS_FUN((-->), (-~>), SymGeneralFun)
#endif

instance (ValidFP eb fb) => ExtractSym (SymFP eb fb) where
  extractSym (SymFP t) = SymbolSet $ extractTerm t

-- Instances
deriveBuiltins
  (ViaDefault ''ExtractSym)
  [''ExtractSym]
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
  (ViaDefault1 ''ExtractSym1)
  [''ExtractSym, ''ExtractSym1]
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
  (ExtractSym1 m, ExtractSym e, ExtractSym a) =>
  ExtractSym (ExceptT e m a)
  where
  extractSym = extractSym1
  {-# INLINE extractSym #-}

instance
  (ExtractSym1 m, ExtractSym e) =>
  ExtractSym1 (ExceptT e m)
  where
  liftExtractSym f (ExceptT v) =
    liftExtractSym (liftExtractSym f) v
  {-# INLINE liftExtractSym #-}

-- MaybeT
instance
  (ExtractSym1 m, ExtractSym a) =>
  ExtractSym (MaybeT m a)
  where
  extractSym = extractSym1
  {-# INLINE extractSym #-}

instance
  (ExtractSym1 m) =>
  ExtractSym1 (MaybeT m)
  where
  liftExtractSym f (MaybeT v) =
    liftExtractSym (liftExtractSym f) v
  {-# INLINE liftExtractSym #-}

-- WriterT
instance
  (ExtractSym1 m, ExtractSym w, ExtractSym a) =>
  ExtractSym (WriterLazy.WriterT w m a)
  where
  extractSym = extractSym1
  {-# INLINE extractSym #-}

instance
  (ExtractSym1 m, ExtractSym w) =>
  ExtractSym1 (WriterLazy.WriterT w m)
  where
  liftExtractSym f (WriterLazy.WriterT v) =
    liftExtractSym (liftExtractSym2 f extractSym) v
  {-# INLINE liftExtractSym #-}

instance
  (ExtractSym1 m, ExtractSym w, ExtractSym a) =>
  ExtractSym (WriterStrict.WriterT w m a)
  where
  extractSym = extractSym1
  {-# INLINE extractSym #-}

instance
  (ExtractSym1 m, ExtractSym w) =>
  ExtractSym1 (WriterStrict.WriterT w m)
  where
  liftExtractSym f (WriterStrict.WriterT v) =
    liftExtractSym (liftExtractSym2 f extractSym) v
  {-# INLINE liftExtractSym #-}

-- IdentityT
instance
  (ExtractSym1 m, ExtractSym a) =>
  ExtractSym (IdentityT m a)
  where
  extractSym = extractSym1
  {-# INLINE extractSym #-}

instance (ExtractSym1 m) => ExtractSym1 (IdentityT m) where
  liftExtractSym f (IdentityT v) = liftExtractSym f v
  {-# INLINE liftExtractSym #-}

-- Product
deriving via
  (Default (Product l r a))
  instance
    (ExtractSym (l a), ExtractSym (r a)) =>
    ExtractSym (Product l r a)

deriving via
  (Default1 (Product l r))
  instance
    (ExtractSym1 l, ExtractSym1 r) =>
    ExtractSym1 (Product l r)

-- Sum
deriving via
  (Default (Sum l r a))
  instance
    (ExtractSym (l a), ExtractSym (r a)) =>
    ExtractSym (Sum l r a)

deriving via
  (Default1 (Sum l r))
  instance
    (ExtractSym1 l, ExtractSym1 r) => ExtractSym1 (Sum l r)

-- Compose
deriving via
  (Default (Compose f g a))
  instance
    (ExtractSym (f (g a))) => ExtractSym (Compose f g a)

instance
  (ExtractSym1 f, ExtractSym1 g) =>
  ExtractSym1 (Compose f g)
  where
  liftExtractSym f (Compose l) =
    liftExtractSym (liftExtractSym f) l
  {-# INLINE liftExtractSym #-}

-- Const
deriving via
  (Default (Const a b))
  instance
    (ExtractSym a) => ExtractSym (Const a b)

deriving via
  (Default1 (Const a))
  instance
    (ExtractSym a) => ExtractSym1 (Const a)

-- Alt
deriving via
  (Default (Alt f a))
  instance
    (ExtractSym (f a)) => ExtractSym (Alt f a)

deriving via
  (Default1 (Alt f))
  instance
    (ExtractSym1 f) => ExtractSym1 (Alt f)

-- Ap
deriving via
  (Default (Ap f a))
  instance
    (ExtractSym (f a)) => ExtractSym (Ap f a)

deriving via
  (Default1 (Ap f))
  instance
    (ExtractSym1 f) => ExtractSym1 (Ap f)

-- Generic
deriving via (Default (U1 p)) instance ExtractSym (U1 p)

deriving via (Default (V1 p)) instance ExtractSym (V1 p)

deriving via
  (Default (K1 i c p))
  instance
    (ExtractSym c) => ExtractSym (K1 i c p)

deriving via
  (Default (M1 i c f p))
  instance
    (ExtractSym (f p)) => ExtractSym (M1 i c f p)

deriving via
  (Default ((f :+: g) p))
  instance
    (ExtractSym (f p), ExtractSym (g p)) =>
    ExtractSym ((f :+: g) p)

deriving via
  (Default ((f :*: g) p))
  instance
    (ExtractSym (f p), ExtractSym (g p)) =>
    ExtractSym ((f :*: g) p)

deriving via
  (Default (Par1 p))
  instance
    (ExtractSym p) => ExtractSym (Par1 p)

deriving via
  (Default (Rec1 f p))
  instance
    (ExtractSym (f p)) => ExtractSym (Rec1 f p)

deriving via
  (Default ((f :.: g) p))
  instance
    (ExtractSym (f (g p))) => ExtractSym ((f :.: g) p)

-- ExtractSym2
instance ExtractSym2 Either where
  liftExtractSym2 f _ (Left x) = f x
  liftExtractSym2 _ g (Right y) = g y
  {-# INLINE liftExtractSym2 #-}

instance ExtractSym2 (,) where
  liftExtractSym2 f g (x, y) = f x <> g y
  {-# INLINE liftExtractSym2 #-}

instance (ExtractSym a) => ExtractSym2 ((,,) a) where
  liftExtractSym2 f g (x, y, z) = extractSym x <> f y <> g z
  {-# INLINE liftExtractSym2 #-}

instance
  (ExtractSym a, ExtractSym b) =>
  ExtractSym2 ((,,,) a b)
  where
  liftExtractSym2 f g (x, y, z, w) =
    extractSym x <> extractSym y <> f z <> g w
  {-# INLINE liftExtractSym2 #-}
