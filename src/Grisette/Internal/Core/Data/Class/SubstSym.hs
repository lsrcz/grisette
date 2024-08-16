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
-- Module      :   Grisette.Internal.Core.Data.Class.SubstSym
-- Copyright   :   (c) Sirui Lu 2021-2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Internal.Core.Data.Class.SubstSym
  ( -- * Substituting symbolic constants
    SubstSym (..),
    SubstSym1 (..),
    substSym1,
    SubstSym2 (..),
    substSym2,

    -- * Generic 'SubstSym'
    SubstSymArgs (..),
    GSubstSym (..),
    genericSubstSym,
    genericLiftSubstSym,
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
import Data.Monoid (Alt, Ap)
import qualified Data.Monoid as Monoid
import Data.Ord (Down)
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
import Grisette.Internal.SymPrim.AlgReal (AlgReal)
import Grisette.Internal.SymPrim.BV (IntN, WordN)
import Grisette.Internal.SymPrim.FP
  ( FP,
    FPRoundingMode,
    NotRepresentableFPError,
    ValidFP,
  )
import Grisette.Internal.SymPrim.GeneralFun (substTerm, type (-->) (GeneralFun))
import Grisette.Internal.SymPrim.Prim.Term
  ( IsSymbolKind,
    LinkedRep (underlyingTerm),
    SymRep (SymType),
    SymbolKind,
    TypedSymbol,
    someTypedSymbol,
  )
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

-- | Substitution of symbols (symbolic constants) to a symbolic value.
--
-- >>> a = "a" :: TypedAnySymbol Bool
-- >>> v = "x" .&& "y" :: SymBool
-- >>> substSym a v (["a" .&& "b", "a"] :: [SymBool])
-- [(&& (&& x y) b),(&& x y)]
--
-- __Note 1:__ This type class can be derived for algebraic data types.
-- You may need the @DerivingVia@ and @DerivingStrategies@ extensions.
--
-- > data X = ... deriving Generic deriving SubstSym via (Default X)
class SubstSym a where
  -- Substitute a symbolic constant to some symbolic value
  --
  -- >>> substSym "a" ("c" .&& "d" :: Sym Bool) ["a" .&& "b" :: Sym Bool, "a"]
  -- [(&& (&& c d) b),(&& c d)]
  substSym ::
    (LinkedRep cb sb, IsSymbolKind knd) =>
    TypedSymbol knd cb ->
    sb ->
    a ->
    a

-- | Lifting of 'SubstSym' to unary type constructors.
class
  (forall a. (SubstSym a) => SubstSym (f a)) =>
  SubstSym1 f
  where
  -- | Lift a symbol substitution function to unary type constructors.
  liftSubstSym ::
    (LinkedRep cb sb, IsSymbolKind knd) =>
    (TypedSymbol knd cb -> sb -> a -> a) ->
    TypedSymbol knd cb ->
    sb ->
    f a ->
    f a

-- | Lifting the standard 'substSym' to unary type constructors.
substSym1 ::
  (SubstSym1 f, SubstSym a, LinkedRep cb sb, IsSymbolKind knd) =>
  TypedSymbol knd cb ->
  sb ->
  f a ->
  f a
substSym1 = liftSubstSym substSym

-- | Lifting of 'SubstSym' to binary type constructors.
class
  (forall a. (SubstSym a) => SubstSym1 (f a)) =>
  SubstSym2 f
  where
  -- | Lift a symbol substitution function to binary type constructors.
  liftSubstSym2 ::
    (LinkedRep cb sb, IsSymbolKind knd) =>
    (TypedSymbol knd cb -> sb -> a -> a) ->
    (TypedSymbol knd cb -> sb -> b -> b) ->
    TypedSymbol knd cb ->
    sb ->
    f a b ->
    f a b

-- | Lifting the standard 'substSym' to binary type constructors.
substSym2 ::
  (SubstSym2 f, SubstSym a, SubstSym b, LinkedRep cb sb, IsSymbolKind knd) =>
  TypedSymbol knd cb ->
  sb ->
  f a b ->
  f a b
substSym2 = liftSubstSym2 substSym substSym

-- Derivations

-- | The arguments to the generic 'substSym' function.
data family SubstSymArgs arity (knd :: SymbolKind) a cb sb :: Type

data instance SubstSymArgs Arity0 _ _ _ _ = SubstSymArgs0

newtype instance SubstSymArgs Arity1 knd a cb sb
  = SubstSymArgs1 (TypedSymbol knd cb -> sb -> a -> a)

-- | The class of types where we can generically substitute the symbols in a
-- value.
class GSubstSym arity f where
  gsubstSym ::
    (LinkedRep cb sb, IsSymbolKind knd) =>
    SubstSymArgs arity knd a cb sb ->
    TypedSymbol knd cb ->
    sb ->
    f a ->
    f a

instance GSubstSym arity V1 where
  gsubstSym _ _ _ = id
  {-# INLINE gsubstSym #-}

instance GSubstSym arity U1 where
  gsubstSym _ _ _ = id
  {-# INLINE gsubstSym #-}

instance (SubstSym a) => GSubstSym arity (K1 i a) where
  gsubstSym _ sym val (K1 v) = K1 $ substSym sym val v
  {-# INLINE gsubstSym #-}

instance (GSubstSym arity a) => GSubstSym arity (M1 i c a) where
  gsubstSym args sym val (M1 v) = M1 $ gsubstSym args sym val v
  {-# INLINE gsubstSym #-}

instance (GSubstSym arity a, GSubstSym arity b) => GSubstSym arity (a :*: b) where
  gsubstSym args sym val (a :*: b) =
    gsubstSym args sym val a :*: gsubstSym args sym val b
  {-# INLINE gsubstSym #-}

instance (GSubstSym arity a, GSubstSym arity b) => GSubstSym arity (a :+: b) where
  gsubstSym args sym val (L1 l) = L1 $ gsubstSym args sym val l
  gsubstSym args sym val (R1 r) = R1 $ gsubstSym args sym val r
  {-# INLINE gsubstSym #-}

instance (SubstSym1 a) => GSubstSym Arity1 (Rec1 a) where
  gsubstSym (SubstSymArgs1 f) sym val (Rec1 v) =
    Rec1 $ liftSubstSym f sym val v
  {-# INLINE gsubstSym #-}

instance GSubstSym Arity1 Par1 where
  gsubstSym (SubstSymArgs1 f) sym val (Par1 v) = Par1 $ f sym val v
  {-# INLINE gsubstSym #-}

instance
  (SubstSym1 f, GSubstSym Arity1 g) =>
  GSubstSym Arity1 (f :.: g)
  where
  gsubstSym targs sym val (Comp1 x) =
    Comp1 $ liftSubstSym (gsubstSym targs) sym val x
  {-# INLINE gsubstSym #-}

-- | Generic 'substSym' function.
genericSubstSym ::
  (Generic a, GSubstSym Arity0 (Rep a), LinkedRep cb sb, IsSymbolKind knd) =>
  TypedSymbol knd cb ->
  sb ->
  a ->
  a
genericSubstSym sym val =
  to . gsubstSym SubstSymArgs0 sym val . from
{-# INLINE genericSubstSym #-}

-- | Generic 'liftSubstSym' function.
genericLiftSubstSym ::
  (Generic1 f, GSubstSym Arity1 (Rep1 f), LinkedRep cb sb, IsSymbolKind knd) =>
  (TypedSymbol knd cb -> sb -> a -> a) ->
  TypedSymbol knd cb ->
  sb ->
  f a ->
  f a
genericLiftSubstSym f sym val =
  to1 . gsubstSym (SubstSymArgs1 f) sym val . from1
{-# INLINE genericLiftSubstSym #-}

instance
  (Generic a, GSubstSym Arity0 (Rep a)) =>
  SubstSym (Default a)
  where
  substSym sym val = Default . genericSubstSym sym val . unDefault
  {-# INLINE substSym #-}

instance
  (Generic1 f, GSubstSym Arity1 (Rep1 f), SubstSym a) =>
  SubstSym (Default1 f a)
  where
  substSym = substSym1
  {-# INLINE substSym #-}

instance
  (Generic1 f, GSubstSym Arity1 (Rep1 f)) =>
  SubstSym1 (Default1 f)
  where
  liftSubstSym f sym val =
    Default1 . genericLiftSubstSym f sym val . unDefault1
  {-# INLINE liftSubstSym #-}

#define CONCRETE_SUBSTITUTESYM(type) \
instance SubstSym type where \
  substSym _ _ = id

#define CONCRETE_SUBSTITUTESYM_BV(type) \
instance (KnownNat n, 1 <= n) => SubstSym (type n) where \
  substSym _ _ = id

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
CONCRETE_SUBSTITUTESYM(Monoid.All)
CONCRETE_SUBSTITUTESYM(Monoid.Any)
CONCRETE_SUBSTITUTESYM(Ordering)
CONCRETE_SUBSTITUTESYM_BV(WordN)
CONCRETE_SUBSTITUTESYM_BV(IntN)
CONCRETE_SUBSTITUTESYM(FPRoundingMode)
CONCRETE_SUBSTITUTESYM(AlgReal)
#endif

instance (ValidFP eb sb) => SubstSym (FP eb sb) where
  substSym _ _ = id

#define SUBSTITUTE_SYM_SIMPLE(symtype) \
instance SubstSym symtype where \
  substSym sym v (symtype t) = \
    symtype $ substTerm sym (underlyingTerm v) HS.empty t

#define SUBSTITUTE_SYM_BV(symtype) \
instance (KnownNat n, 1 <= n) => SubstSym (symtype n) where \
  substSym sym v (symtype t) = \
    symtype $ substTerm sym (underlyingTerm v) HS.empty t

#define SUBSTITUTE_SYM_FUN(op, cons) \
instance SubstSym (op sa sb) where \
  substSym sym v (cons t) = \
    cons $ substTerm sym (underlyingTerm v) HS.empty t

#if 1
SUBSTITUTE_SYM_SIMPLE(SymBool)
SUBSTITUTE_SYM_SIMPLE(SymInteger)
SUBSTITUTE_SYM_SIMPLE(SymAlgReal)
SUBSTITUTE_SYM_BV(SymIntN)
SUBSTITUTE_SYM_BV(SymWordN)
SUBSTITUTE_SYM_FUN((=~>), SymTabularFun)
SUBSTITUTE_SYM_FUN((-~>), SymGeneralFun)
SUBSTITUTE_SYM_SIMPLE(SymFPRoundingMode)
#endif

instance (ValidFP eb sb) => SubstSym (SymFP eb sb) where
  substSym sym v (SymFP t) = SymFP $ substTerm sym (underlyingTerm v) HS.empty t

-- Instances
deriveBuiltins
  (ViaDefault ''SubstSym)
  [''SubstSym]
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
    ''Identity,
    ''Monoid.Dual,
    ''Monoid.Sum,
    ''Monoid.Product,
    ''Monoid.First,
    ''Monoid.Last,
    ''Down
  ]

deriveBuiltins
  (ViaDefault1 ''SubstSym1)
  [''SubstSym, ''SubstSym1]
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
  (SubstSym1 m, SubstSym e, SubstSym a) =>
  SubstSym (ExceptT e m a)
  where
  substSym = substSym1
  {-# INLINE substSym #-}

instance
  (SubstSym1 m, SubstSym e) =>
  SubstSym1 (ExceptT e m)
  where
  liftSubstSym f sym val (ExceptT v) =
    ExceptT $ liftSubstSym (liftSubstSym f) sym val v
  {-# INLINE liftSubstSym #-}

-- MaybeT
instance
  (SubstSym1 m, SubstSym a) =>
  SubstSym (MaybeT m a)
  where
  substSym = substSym1
  {-# INLINE substSym #-}

instance
  (SubstSym1 m) =>
  SubstSym1 (MaybeT m)
  where
  liftSubstSym f sym val (MaybeT v) =
    MaybeT $ liftSubstSym (liftSubstSym f) sym val v
  {-# INLINE liftSubstSym #-}

-- WriterT
instance
  (SubstSym1 m, SubstSym a, SubstSym s) =>
  SubstSym (WriterLazy.WriterT s m a)
  where
  substSym = substSym1
  {-# INLINE substSym #-}

instance
  (SubstSym1 m, SubstSym s) =>
  SubstSym1 (WriterLazy.WriterT s m)
  where
  liftSubstSym f sym val (WriterLazy.WriterT v) =
    WriterLazy.WriterT $
      liftSubstSym (liftSubstSym2 f substSym) sym val v
  {-# INLINE liftSubstSym #-}

instance
  (SubstSym1 m, SubstSym a, SubstSym s) =>
  SubstSym (WriterStrict.WriterT s m a)
  where
  substSym = substSym1
  {-# INLINE substSym #-}

instance
  (SubstSym1 m, SubstSym s) =>
  SubstSym1 (WriterStrict.WriterT s m)
  where
  liftSubstSym f sym val (WriterStrict.WriterT v) =
    WriterStrict.WriterT $
      liftSubstSym (liftSubstSym2 f substSym) sym val v
  {-# INLINE liftSubstSym #-}

-- IdentityT
instance
  (SubstSym1 m, SubstSym a) =>
  SubstSym (IdentityT m a)
  where
  substSym = substSym1
  {-# INLINE substSym #-}

instance (SubstSym1 m) => SubstSym1 (IdentityT m) where
  liftSubstSym f sym val (IdentityT a) =
    IdentityT $ liftSubstSym f sym val a
  {-# INLINE liftSubstSym #-}

-- Product
deriving via
  (Default (Product l r a))
  instance
    (SubstSym (l a), SubstSym (r a)) => SubstSym (Product l r a)

deriving via
  (Default1 (Product l r))
  instance
    (SubstSym1 l, SubstSym1 r) => SubstSym1 (Product l r)

-- Sum
deriving via
  (Default (Sum l r a))
  instance
    (SubstSym (l a), SubstSym (r a)) => SubstSym (Sum l r a)

deriving via
  (Default1 (Sum l r))
  instance
    (SubstSym1 l, SubstSym1 r) => SubstSym1 (Sum l r)

-- Compose
deriving via
  (Default (Compose f g a))
  instance
    (SubstSym (f (g a))) => SubstSym (Compose f g a)

instance
  (SubstSym1 f, SubstSym1 g) =>
  SubstSym1 (Compose f g)
  where
  liftSubstSym f sym val (Compose x) =
    Compose $ liftSubstSym (liftSubstSym f) sym val x
  {-# INLINE liftSubstSym #-}

-- Const
deriving via
  (Default (Const a b))
  instance
    (SubstSym a) => SubstSym (Const a b)

deriving via
  (Default1 (Const a))
  instance
    (SubstSym a) => SubstSym1 (Const a)

-- Alt
deriving via
  (Default (Alt f a))
  instance
    (SubstSym (f a)) => SubstSym (Alt f a)

deriving via
  (Default1 (Alt f))
  instance
    (SubstSym1 f) => SubstSym1 (Alt f)

-- Ap
deriving via
  (Default (Ap f a))
  instance
    (SubstSym (f a)) => SubstSym (Ap f a)

deriving via
  (Default1 (Ap f))
  instance
    (SubstSym1 f) => SubstSym1 (Ap f)

-- Generic
deriving via (Default (U1 p)) instance SubstSym (U1 p)

deriving via (Default (V1 p)) instance SubstSym (V1 p)

deriving via
  (Default (K1 i c p))
  instance
    (SubstSym c) => SubstSym (K1 i c p)

deriving via
  (Default (M1 i c f p))
  instance
    (SubstSym (f p)) => SubstSym (M1 i c f p)

deriving via
  (Default ((f :+: g) p))
  instance
    (SubstSym (f p), SubstSym (g p)) => SubstSym ((f :+: g) p)

deriving via
  (Default ((f :*: g) p))
  instance
    (SubstSym (f p), SubstSym (g p)) => SubstSym ((f :*: g) p)

deriving via
  (Default (Par1 p))
  instance
    (SubstSym p) => SubstSym (Par1 p)

deriving via
  (Default (Rec1 f p))
  instance
    (SubstSym (f p)) => SubstSym (Rec1 f p)

deriving via
  (Default ((f :.: g) p))
  instance
    (SubstSym (f (g p))) => SubstSym ((f :.: g) p)

-- SubstSym2
instance SubstSym2 Either where
  liftSubstSym2 f _ sym val (Left x) = Left $ f sym val x
  liftSubstSym2 _ g sym val (Right y) = Right $ g sym val y
  {-# INLINE liftSubstSym2 #-}

instance SubstSym2 (,) where
  liftSubstSym2 f g sym val (x, y) = (f sym val x, g sym val y)
  {-# INLINE liftSubstSym2 #-}

instance (SubstSym a) => SubstSym2 ((,,) a) where
  liftSubstSym2 f g sym val (x, y, z) =
    (substSym sym val x, f sym val y, g sym val z)
  {-# INLINE liftSubstSym2 #-}

instance (SubstSym a, SubstSym b) => SubstSym2 ((,,,) a b) where
  liftSubstSym2 f g sym val (x, y, z, w) =
    (substSym sym val x, substSym sym val y, f sym val z, g sym val w)
  {-# INLINE liftSubstSym2 #-}

instance (SubstSym a, SubstSym b) => SubstSym (a =-> b) where
  substSym sym val (TabularFun f d) =
    TabularFun (substSym sym val f) (substSym sym val d)
  {-# INLINE substSym #-}

instance (SubstSym (SymType b)) => SubstSym (a --> b) where
  substSym sym val (GeneralFun s t) =
    GeneralFun s $
      substTerm sym (underlyingTerm val) (HS.singleton $ someTypedSymbol s) t
  {-# INLINE substSym #-}
