{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeApplications #-}
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
    extractSymMaybe1,
    extractSym1,
    ExtractSym2 (..),
    extractSymMaybe2,
    extractSym2,

    -- * Generic 'ExtractSym'
    ExtractSymArgs (..),
    GExtractSym (..),
    genericExtractSymMaybe,
    genericLiftExtractSymMaybe,
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
import qualified Data.HashSet as HS
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Kind (Type)
import Data.Maybe (fromJust)
import Data.Monoid (Alt, Ap)
import qualified Data.Monoid as Monoid
import Data.Ord (Down)
import qualified Data.Text as T
import Data.Typeable (type (:~~:) (HRefl))
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
import Grisette.Internal.Core.Control.Exception
  ( AssertionError,
    VerificationConditions,
  )
import Grisette.Internal.SymPrim.AlgReal (AlgReal)
import Grisette.Internal.SymPrim.BV (BitwidthMismatch, IntN, WordN)
import Grisette.Internal.SymPrim.FP (FP, FPRoundingMode, NotRepresentableFPError, ValidFP)
import Grisette.Internal.SymPrim.GeneralFun (type (-->) (GeneralFun))
import Grisette.Internal.SymPrim.Prim.Model
  ( AnySymbolSet,
    SymbolSet (SymbolSet),
  )
import Grisette.Internal.SymPrim.Prim.Term
  ( IsSymbolKind (decideSymbolKind),
    LinkedRep,
    SupportedPrim,
    SymRep (SymType),
    SymbolKind,
    someTypedSymbol,
  )
import Grisette.Internal.SymPrim.Prim.TermUtils (extractTerm)
import Grisette.Internal.SymPrim.SymAlgReal (SymAlgReal (SymAlgReal))
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
import Grisette.Internal.SymPrim.TabularFun (type (=->) (TabularFun))
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

-- | Extracts all the symbols (symbolic constants) that are transitively
-- contained in the given value.
--
-- >>> extractSym ("a" :: SymBool)
-- SymbolSet {a :: Bool}
--
-- >>> extractSym (mrgIf "a" (mrgReturn ["b"]) (mrgReturn ["c", "d"]) :: Union [SymBool])
-- SymbolSet {a :: Bool, b :: Bool, c :: Bool, d :: Bool}
--
-- __Note 1:__ This type class can be derived for algebraic data types.
-- You may need the @DerivingVia@ and @DerivingStrategies@ extensions.
--
-- > data X = ... deriving Generic deriving ExtractSym via (Default X)
class ExtractSym a where
  extractSym :: a -> AnySymbolSet
  extractSym = fromJust . extractSymMaybe
  {-# INLINE extractSym #-}
  extractSymMaybe :: (IsSymbolKind knd) => a -> Maybe (SymbolSet knd)

-- | Lifting of 'ExtractSym' to unary type constructors.
class
  (forall a. (ExtractSym a) => ExtractSym (f a)) =>
  ExtractSym1 f
  where
  -- | Lifts the 'extractSymMaybe' function to unary type constructors.
  liftExtractSymMaybe ::
    (IsSymbolKind knd) =>
    (a -> Maybe (SymbolSet knd)) ->
    f a ->
    Maybe (SymbolSet knd)

-- | Lift the standard 'extractSym' to unary type constructors.
extractSym1 ::
  (ExtractSym1 f, ExtractSym a, IsSymbolKind knd) =>
  f a ->
  SymbolSet knd
extractSym1 = fromJust . liftExtractSymMaybe extractSymMaybe
{-# INLINE extractSym1 #-}

-- | Lift the standard 'extractSymMaybe' to unary type constructors.
extractSymMaybe1 ::
  (ExtractSym1 f, ExtractSym a, IsSymbolKind knd) =>
  f a ->
  Maybe (SymbolSet knd)
extractSymMaybe1 = liftExtractSymMaybe extractSymMaybe
{-# INLINE extractSymMaybe1 #-}

-- | Lifting of 'ExtractSym' to binary type constructors.
class
  (forall a. (ExtractSym a) => ExtractSym1 (f a)) =>
  ExtractSym2 f
  where
  -- | Lifts the 'extractSymMaybe' function to binary type constructors.
  liftExtractSymMaybe2 ::
    (IsSymbolKind knd) =>
    (a -> Maybe (SymbolSet knd)) ->
    (b -> Maybe (SymbolSet knd)) ->
    f a b ->
    Maybe (SymbolSet knd)

-- | Lift the standard 'extractSym' to binary type constructors.
extractSym2 ::
  (ExtractSym2 f, ExtractSym a, ExtractSym b, IsSymbolKind knd) =>
  f a b ->
  SymbolSet knd
extractSym2 = fromJust . liftExtractSymMaybe2 extractSymMaybe extractSymMaybe

-- | Lift the standard 'extractSymMaybe' to binary type constructors.
extractSymMaybe2 ::
  (ExtractSym2 f, ExtractSym a, ExtractSym b, IsSymbolKind knd) =>
  f a b ->
  Maybe (SymbolSet knd)
extractSymMaybe2 = liftExtractSymMaybe2 extractSymMaybe extractSymMaybe
{-# INLINE extractSymMaybe2 #-}

-- Derivations

-- | The arguments to the generic 'extractSym' function.
data family ExtractSymArgs arity (knd :: SymbolKind) a :: Type

data instance ExtractSymArgs Arity0 _ _ = ExtractSymArgs0

newtype instance ExtractSymArgs Arity1 knd a
  = ExtractSymArgs1 (a -> Maybe (SymbolSet knd))

-- | The class of types that can generically extract the symbols.
class GExtractSym arity f where
  gextractSymMaybe ::
    (IsSymbolKind knd) =>
    ExtractSymArgs arity knd a ->
    f a ->
    Maybe (SymbolSet knd)

instance GExtractSym arity V1 where
  gextractSymMaybe _ _ = Just mempty
  {-# INLINE gextractSymMaybe #-}

instance GExtractSym arity U1 where
  gextractSymMaybe _ _ = Just mempty
  {-# INLINE gextractSymMaybe #-}

instance (GExtractSym arity a) => GExtractSym arity (M1 i c a) where
  gextractSymMaybe args (M1 x) = gextractSymMaybe args x
  {-# INLINE gextractSymMaybe #-}

instance (ExtractSym a) => GExtractSym arity (K1 i a) where
  gextractSymMaybe _ (K1 x) = extractSymMaybe x
  {-# INLINE gextractSymMaybe #-}

instance
  (GExtractSym arity a, GExtractSym arity b) =>
  GExtractSym arity (a :+: b)
  where
  gextractSymMaybe args (L1 x) = gextractSymMaybe args x
  gextractSymMaybe args (R1 x) = gextractSymMaybe args x
  {-# INLINE gextractSymMaybe #-}

instance
  (GExtractSym arity a, GExtractSym arity b) =>
  GExtractSym arity (a :*: b)
  where
  gextractSymMaybe args (x :*: y) =
    gextractSymMaybe args x <> gextractSymMaybe args y
  {-# INLINE gextractSymMaybe #-}

instance GExtractSym Arity1 Par1 where
  gextractSymMaybe (ExtractSymArgs1 f) (Par1 x) = f x
  {-# INLINE gextractSymMaybe #-}

instance (ExtractSym1 a) => GExtractSym Arity1 (Rec1 a) where
  gextractSymMaybe (ExtractSymArgs1 f) (Rec1 x) =
    liftExtractSymMaybe f x
  {-# INLINE gextractSymMaybe #-}

instance
  (ExtractSym1 f, GExtractSym Arity1 g) =>
  GExtractSym Arity1 (f :.: g)
  where
  gextractSymMaybe targs (Comp1 x) =
    liftExtractSymMaybe (gextractSymMaybe targs) x
  {-# INLINE gextractSymMaybe #-}

-- | Generic 'extractSym' function.
genericExtractSymMaybe ::
  (Generic a, GExtractSym Arity0 (Rep a), IsSymbolKind knd) =>
  a ->
  Maybe (SymbolSet knd)
genericExtractSymMaybe = gextractSymMaybe ExtractSymArgs0 . from

-- | Generic 'liftExtractSymMaybe' function.
genericLiftExtractSymMaybe ::
  (Generic1 f, GExtractSym Arity1 (Rep1 f), IsSymbolKind knd) =>
  (a -> Maybe (SymbolSet knd)) ->
  f a ->
  Maybe (SymbolSet knd)
genericLiftExtractSymMaybe f =
  gextractSymMaybe (ExtractSymArgs1 f) . from1

instance
  (Generic a, GExtractSym Arity0 (Rep a)) =>
  ExtractSym (Default a)
  where
  extractSymMaybe = genericExtractSymMaybe . unDefault
  {-# INLINE extractSymMaybe #-}

instance
  (Generic1 f, GExtractSym Arity1 (Rep1 f), ExtractSym a) =>
  ExtractSym (Default1 f a)
  where
  extractSymMaybe = extractSymMaybe1
  {-# INLINE extractSymMaybe #-}

instance
  (Generic1 f, GExtractSym Arity1 (Rep1 f)) =>
  ExtractSym1 (Default1 f)
  where
  liftExtractSymMaybe f = genericLiftExtractSymMaybe f . unDefault1
  {-# INLINE liftExtractSymMaybe #-}

#define CONCRETE_EXTRACT_SYMBOLICS(type) \
instance ExtractSym type where \
  extractSymMaybe _ = return mempty

#define CONCRETE_EXTRACT_SYMBOLICS_BV(type) \
instance (KnownNat n, 1 <= n) => ExtractSym (type n) where \
  extractSymMaybe _ = return mempty

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
CONCRETE_EXTRACT_SYMBOLICS(AlgReal)
#endif

instance (ValidFP eb sb) => ExtractSym (FP eb sb) where
  extractSymMaybe _ = return mempty

#define EXTRACT_SYMBOLICS_SIMPLE(symtype) \
instance ExtractSym symtype where \
  extractSymMaybe :: \
    forall knd. (IsSymbolKind knd) => symtype -> Maybe (SymbolSet knd); \
  extractSymMaybe (symtype t) = \
    case decideSymbolKind @knd of\
      Left HRefl -> SymbolSet <$> extractTerm HS.empty t; \
      Right HRefl -> SymbolSet <$> extractTerm HS.empty t

#define EXTRACT_SYMBOLICS_BV(symtype) \
instance (KnownNat n, 1 <= n) => ExtractSym (symtype n) where \
  extractSymMaybe :: \
    forall knd. (IsSymbolKind knd) => symtype n -> Maybe (SymbolSet knd); \
  extractSymMaybe (symtype t) = \
    case decideSymbolKind @knd of\
      Left HRefl -> SymbolSet <$> extractTerm HS.empty t; \
      Right HRefl -> SymbolSet <$> extractTerm HS.empty t

#define EXTRACT_SYMBOLICS_FUN(cop, op, cons) \
instance (SupportedPrim (cop ca cb), LinkedRep ca sa, LinkedRep cb sb) => \
  ExtractSym (op sa sb) where \
  extractSymMaybe :: \
    forall knd. (IsSymbolKind knd) => op sa sb -> Maybe (SymbolSet knd); \
  extractSymMaybe (cons t) = \
    case decideSymbolKind @knd of \
      Left HRefl -> Nothing; \
      Right HRefl -> SymbolSet <$> extractTerm HS.empty t

#if 1
EXTRACT_SYMBOLICS_SIMPLE(SymBool)
EXTRACT_SYMBOLICS_SIMPLE(SymInteger)
EXTRACT_SYMBOLICS_SIMPLE(SymFPRoundingMode)
EXTRACT_SYMBOLICS_SIMPLE(SymAlgReal)
EXTRACT_SYMBOLICS_BV(SymIntN)
EXTRACT_SYMBOLICS_BV(SymWordN)
EXTRACT_SYMBOLICS_FUN((=->), (=~>), SymTabularFun)
EXTRACT_SYMBOLICS_FUN((-->), (-~>), SymGeneralFun)
#endif

instance (ValidFP eb fb) => ExtractSym (SymFP eb fb) where
  extractSymMaybe ::
    forall knd. (IsSymbolKind knd) => SymFP eb fb -> Maybe (SymbolSet knd)
  extractSymMaybe (SymFP t) =
    case decideSymbolKind @knd of
      Left HRefl -> SymbolSet <$> extractTerm HS.empty t
      Right HRefl -> SymbolSet <$> extractTerm HS.empty t

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
    ''BitwidthMismatch,
    ''NotRepresentableFPError,
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
  extractSymMaybe = extractSymMaybe1
  {-# INLINE extractSymMaybe #-}

instance
  (ExtractSym1 m, ExtractSym e) =>
  ExtractSym1 (ExceptT e m)
  where
  liftExtractSymMaybe f (ExceptT v) =
    liftExtractSymMaybe (liftExtractSymMaybe f) v
  {-# INLINE liftExtractSymMaybe #-}

-- MaybeT
instance
  (ExtractSym1 m, ExtractSym a) =>
  ExtractSym (MaybeT m a)
  where
  extractSymMaybe = extractSymMaybe1
  {-# INLINE extractSymMaybe #-}

instance
  (ExtractSym1 m) =>
  ExtractSym1 (MaybeT m)
  where
  liftExtractSymMaybe f (MaybeT v) =
    liftExtractSymMaybe (liftExtractSymMaybe f) v
  {-# INLINE liftExtractSymMaybe #-}

-- WriterT
instance
  (ExtractSym1 m, ExtractSym w, ExtractSym a) =>
  ExtractSym (WriterLazy.WriterT w m a)
  where
  extractSymMaybe = extractSymMaybe1
  {-# INLINE extractSymMaybe #-}

instance
  (ExtractSym1 m, ExtractSym w) =>
  ExtractSym1 (WriterLazy.WriterT w m)
  where
  liftExtractSymMaybe f (WriterLazy.WriterT v) =
    liftExtractSymMaybe (liftExtractSymMaybe2 f extractSymMaybe) v
  {-# INLINE liftExtractSymMaybe #-}

instance
  (ExtractSym1 m, ExtractSym w, ExtractSym a) =>
  ExtractSym (WriterStrict.WriterT w m a)
  where
  extractSymMaybe = extractSymMaybe1
  {-# INLINE extractSymMaybe #-}

instance
  (ExtractSym1 m, ExtractSym w) =>
  ExtractSym1 (WriterStrict.WriterT w m)
  where
  liftExtractSymMaybe f (WriterStrict.WriterT v) =
    liftExtractSymMaybe (liftExtractSymMaybe2 f extractSymMaybe) v
  {-# INLINE liftExtractSymMaybe #-}

-- IdentityT
instance
  (ExtractSym1 m, ExtractSym a) =>
  ExtractSym (IdentityT m a)
  where
  extractSymMaybe = extractSymMaybe1
  {-# INLINE extractSymMaybe #-}

instance (ExtractSym1 m) => ExtractSym1 (IdentityT m) where
  liftExtractSymMaybe f (IdentityT v) = liftExtractSymMaybe f v
  {-# INLINE liftExtractSymMaybe #-}

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
  liftExtractSymMaybe f (Compose l) =
    liftExtractSymMaybe (liftExtractSymMaybe f) l
  {-# INLINE liftExtractSymMaybe #-}

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
  liftExtractSymMaybe2 f _ (Left x) = f x
  liftExtractSymMaybe2 _ g (Right y) = g y
  {-# INLINE liftExtractSymMaybe2 #-}

instance ExtractSym2 (,) where
  liftExtractSymMaybe2 f g (x, y) = f x <> g y
  {-# INLINE liftExtractSymMaybe2 #-}

instance (ExtractSym a) => ExtractSym2 ((,,) a) where
  liftExtractSymMaybe2 f g (x, y, z) = extractSymMaybe x <> f y <> g z
  {-# INLINE liftExtractSymMaybe2 #-}

instance
  (ExtractSym a, ExtractSym b) =>
  ExtractSym2 ((,,,) a b)
  where
  liftExtractSymMaybe2 f g (x, y, z, w) =
    extractSymMaybe x <> extractSymMaybe y <> f z <> g w
  {-# INLINE liftExtractSymMaybe2 #-}

instance (ExtractSym a, ExtractSym b) => ExtractSym (a =-> b) where
  extractSymMaybe (TabularFun s t) =
    extractSymMaybe s <> extractSymMaybe t

instance (ExtractSym (SymType b)) => ExtractSym (a --> b) where
  extractSymMaybe :: forall knd. (IsSymbolKind knd) => (a --> b) -> Maybe (SymbolSet knd)
  extractSymMaybe (GeneralFun t f) =
    case decideSymbolKind @knd of
      Left HRefl -> Nothing
      Right HRefl -> SymbolSet <$> extractTerm (HS.singleton $ someTypedSymbol t) f
