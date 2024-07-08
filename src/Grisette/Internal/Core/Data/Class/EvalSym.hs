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
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      :   Grisette.Internal.Core.Data.Class.EvalSym
-- Copyright   :   (c) Sirui Lu 2021-2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Internal.Core.Data.Class.EvalSym
  ( -- * Evaluating symbolic values with model
    EvalSym (..),
    evalSymToCon,
    EvalSym1 (..),
    evalSym1,
    evalSymToCon1,
    EvalSym2 (..),
    evalSym2,
    evalSymToCon2,

    -- * Generic 'EvalSym'
    EvalSymArgs (..),
    GEvalSym (..),
    genericEvalSym,
    genericLiftEvalSym,
  )
where

import Control.Monad.Except (ExceptT (ExceptT), runExceptT)
import Control.Monad.Identity
  ( Identity (Identity),
    IdentityT (IdentityT),
  )
import Control.Monad.Trans.Maybe (MaybeT (MaybeT, runMaybeT))
import qualified Control.Monad.Writer.Lazy as WriterLazy
import qualified Control.Monad.Writer.Strict as WriterStrict
import qualified Data.ByteString as B
import Data.Functor.Compose (Compose (Compose))
import Data.Functor.Const (Const)
import Data.Functor.Product (Product)
import Data.Functor.Sum (Sum)
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Kind (Type)
import Data.Maybe (fromJust)
import Data.Monoid (Alt, Ap)
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
import qualified Generics.Deriving.Monoid as Monoid
import Grisette.Internal.Core.Control.Exception
  ( AssertionError,
    VerificationConditions,
  )
import Grisette.Internal.Core.Data.Class.ToCon
  ( ToCon (toCon),
    ToCon1,
    ToCon2,
    toCon1,
    toCon2,
  )
import Grisette.Internal.SymPrim.BV (IntN, WordN)
import Grisette.Internal.SymPrim.FP (FP, FPRoundingMode, ValidFP)
import Grisette.Internal.SymPrim.GeneralFun (type (-->))
import Grisette.Internal.SymPrim.Prim.Model (Model, evaluateTerm)
import Grisette.Internal.SymPrim.Prim.Term (LinkedRep, SupportedPrim)
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
import Grisette.Internal.SymPrim.TabularFun (type (=->))
import Grisette.Internal.TH.DeriveBuiltin (deriveBuiltins)
import Grisette.Internal.TH.DeriveInstanceProvider
  ( Strategy (ViaDefault, ViaDefault1),
  )
import Grisette.Internal.Utils.Derive (Arity0, Arity1)

-- $setup
-- >>> import Grisette.Core
-- >>> import Grisette.SymPrim
-- >>> import Data.Proxy
-- >>> :set -XTypeApplications

-- | Evaluating symbolic values with some model. This would substitute the
-- symbols (symbolic constants) with the values in the model.
--
-- >>> let model = insertValue "a" (1 :: Integer) emptyModel :: Model
-- >>> evalSym False model ([ssym "a", ssym "b"] :: [SymInteger])
-- [1,b]
--
-- If we set the first argument true, the missing symbols will be filled in
-- with some default values:
--
-- >>> evalSym True model ([ssym "a", ssym "b"] :: [SymInteger])
-- [1,0]
--
-- __Note 1:__ This type class can be derived for algebraic data types.
-- You may need the @DerivingVia@ and @DerivingStrategies@ extensions.
--
-- > data X = ... deriving Generic deriving EvalSym via (Default X)
class EvalSym a where
  -- | Evaluate a symbolic value with some model, possibly fill in values for
  -- the missing symbols.
  evalSym :: Bool -> Model -> a -> a

-- | Evaluate a symbolic value with some model, fill in values for the missing
-- symbols, and convert the result to a concrete value.
--
-- >>> let model = insertValue "a" (1 :: Integer) emptyModel :: Model
-- >>> evalSymToCon model ([ssym "a", ssym "b"] :: [SymInteger]) :: [Integer]
-- [1,0]
evalSymToCon :: (ToCon a b, EvalSym a) => Model -> a -> b
evalSymToCon model a = fromJust $ toCon $ evalSym True model a

-- | Lifting of 'EvalSym' to unary type constructors.
class (forall a. (EvalSym a) => EvalSym (f a)) => EvalSym1 f where
  -- | Lift the 'evalSym' function to unary type constructors.
  liftEvalSym :: (Bool -> Model -> a -> a) -> (Bool -> Model -> f a -> f a)

-- | Lifting the standard 'evalSym' to unary type constructors.
evalSym1 :: (EvalSym1 f, EvalSym a) => Bool -> Model -> f a -> f a
evalSym1 = liftEvalSym evalSym
{-# INLINE evalSym1 #-}

-- | Evaluate and convert to concrete values with lifted standard 'evalSym' for
-- unary type constructors. See 'evalSymToCon'.
evalSymToCon1 ::
  (EvalSym1 f, EvalSym a, ToCon1 f g, ToCon a b) =>
  Model ->
  f a ->
  g b
evalSymToCon1 model a = fromJust $ toCon1 $ evalSym1 True model a
{-# INLINE evalSymToCon1 #-}

-- | Lifting of 'EvalSym1' to binary type constructors.
class (forall a. (EvalSym a) => EvalSym1 (f a)) => EvalSym2 f where
  -- | Lift the 'evalSym' function to binary type constructors.
  liftEvalSym2 ::
    (Bool -> Model -> a -> a) ->
    (Bool -> Model -> b -> b) ->
    (Bool -> Model -> f a b -> f a b)

-- | Lifting the standard 'evalSym' to binary type constructors.
evalSym2 ::
  (EvalSym2 f, EvalSym a, EvalSym b) =>
  Bool ->
  Model ->
  f a b ->
  f a b
evalSym2 = liftEvalSym2 evalSym evalSym
{-# INLINE evalSym2 #-}

-- | Evaluate and convert to concrete values with lifted standard 'evalSym' for
-- binary type constructors. See 'evalSymToCon'.
evalSymToCon2 ::
  ( EvalSym2 f,
    EvalSym a,
    EvalSym c,
    ToCon2 f g,
    ToCon a b,
    ToCon c d
  ) =>
  Model ->
  f a c ->
  g b d
evalSymToCon2 model a = fromJust $ toCon2 $ evalSym2 True model a
{-# INLINE evalSymToCon2 #-}

-- Derivations

-- | The arguments to the generic 'evalSym' function.
data family EvalSymArgs arity a :: Type

data instance EvalSymArgs Arity0 _ = EvalSymArgs0

newtype instance EvalSymArgs Arity1 a
  = EvalSymArgs1 (Bool -> Model -> a -> a)

-- | The class of types that can be generically evaluated.
class GEvalSym arity f where
  gevalSym :: EvalSymArgs arity a -> Bool -> Model -> f a -> f a

instance GEvalSym arity V1 where
  gevalSym _ _ _ = id
  {-# INLINE gevalSym #-}

instance GEvalSym arity U1 where
  gevalSym _ _ _ = id
  {-# INLINE gevalSym #-}

instance
  (GEvalSym arity a, GEvalSym arity b) =>
  GEvalSym arity (a :*: b)
  where
  gevalSym args fillDefault model (a :*: b) =
    gevalSym args fillDefault model a
      :*: gevalSym args fillDefault model b
  {-# INLINE gevalSym #-}

instance
  (GEvalSym arity a, GEvalSym arity b) =>
  GEvalSym arity (a :+: b)
  where
  gevalSym args fillDefault model (L1 l) =
    L1 $ gevalSym args fillDefault model l
  gevalSym args fillDefault model (R1 r) =
    R1 $ gevalSym args fillDefault model r
  {-# INLINE gevalSym #-}

instance (GEvalSym arity a) => GEvalSym arity (M1 i c a) where
  gevalSym args fillDefault model (M1 x) =
    M1 $ gevalSym args fillDefault model x
  {-# INLINE gevalSym #-}

instance (EvalSym a) => GEvalSym arity (K1 i a) where
  gevalSym _ fillDefault model (K1 x) = K1 $ evalSym fillDefault model x
  {-# INLINE gevalSym #-}

instance GEvalSym Arity1 Par1 where
  gevalSym (EvalSymArgs1 f) fillDefault model (Par1 x) =
    Par1 $ f fillDefault model x
  {-# INLINE gevalSym #-}

instance (EvalSym1 a) => GEvalSym Arity1 (Rec1 a) where
  gevalSym (EvalSymArgs1 f) fillDefault model (Rec1 x) =
    Rec1 $ liftEvalSym f fillDefault model x
  {-# INLINE gevalSym #-}

instance
  (EvalSym1 f, GEvalSym Arity1 g) =>
  GEvalSym Arity1 (f :.: g)
  where
  gevalSym targs fillDefault model (Comp1 x) =
    Comp1 $ liftEvalSym (gevalSym targs) fillDefault model x
  {-# INLINE gevalSym #-}

-- | Generic 'evalSym' function.
genericEvalSym ::
  (Generic a, GEvalSym Arity0 (Rep a)) => Bool -> Model -> a -> a
genericEvalSym fillDefault model =
  to . gevalSym EvalSymArgs0 fillDefault model . from
{-# INLINE genericEvalSym #-}

-- | Generic 'liftEvalSym' function.
genericLiftEvalSym ::
  (Generic1 f, GEvalSym Arity1 (Rep1 f)) =>
  (Bool -> Model -> a -> a) ->
  Bool ->
  Model ->
  f a ->
  f a
genericLiftEvalSym f fillDefault model =
  to1 . gevalSym (EvalSymArgs1 f) fillDefault model . from1
{-# INLINE genericLiftEvalSym #-}

instance
  (Generic a, GEvalSym Arity0 (Rep a)) =>
  EvalSym (Default a)
  where
  evalSym fillDefault model =
    Default . genericEvalSym fillDefault model . unDefault
  {-# INLINE evalSym #-}

instance
  (Generic1 f, GEvalSym Arity1 (Rep1 f), EvalSym a) =>
  EvalSym (Default1 f a)
  where
  evalSym = evalSym1
  {-# INLINE evalSym #-}

instance
  (Generic1 f, GEvalSym Arity1 (Rep1 f)) =>
  EvalSym1 (Default1 f)
  where
  liftEvalSym f fillDefault model =
    Default1 . genericLiftEvalSym f fillDefault model . unDefault1
  {-# INLINE liftEvalSym #-}

#define CONCRETE_EVALUATESYM(type) \
instance EvalSym type where \
  evalSym _ _ = id

#define CONCRETE_EVALUATESYM_BV(type) \
instance (KnownNat n, 1 <= n) => EvalSym (type n) where \
  evalSym _ _ = id

#if 1
CONCRETE_EVALUATESYM(Bool)
CONCRETE_EVALUATESYM(Integer)
CONCRETE_EVALUATESYM(Char)
CONCRETE_EVALUATESYM(Int)
CONCRETE_EVALUATESYM(Int8)
CONCRETE_EVALUATESYM(Int16)
CONCRETE_EVALUATESYM(Int32)
CONCRETE_EVALUATESYM(Int64)
CONCRETE_EVALUATESYM(Word)
CONCRETE_EVALUATESYM(Word8)
CONCRETE_EVALUATESYM(Word16)
CONCRETE_EVALUATESYM(Word32)
CONCRETE_EVALUATESYM(Word64)
CONCRETE_EVALUATESYM(Float)
CONCRETE_EVALUATESYM(Double)
CONCRETE_EVALUATESYM(B.ByteString)
CONCRETE_EVALUATESYM(T.Text)
CONCRETE_EVALUATESYM(FPRoundingMode)
CONCRETE_EVALUATESYM(Monoid.All)
CONCRETE_EVALUATESYM(Monoid.Any)
CONCRETE_EVALUATESYM(Ordering)
CONCRETE_EVALUATESYM_BV(IntN)
CONCRETE_EVALUATESYM_BV(WordN)
#endif

instance (ValidFP eb fb) => EvalSym (FP eb fb) where
  evalSym _ _ = id

-- Symbolic primitives
#define EVALUATE_SYM_SIMPLE(symtype) \
instance EvalSym symtype where \
  evalSym fillDefault model (symtype t) = \
    symtype $ evaluateTerm fillDefault model t

#define EVALUATE_SYM_BV(symtype) \
instance (KnownNat n, 1 <= n) => EvalSym (symtype n) where \
  evalSym fillDefault model (symtype t) = \
    symtype $ evaluateTerm fillDefault model t

#define EVALUATE_SYM_FUN(cop, op, cons) \
instance (SupportedPrim (cop ca cb), LinkedRep ca sa, LinkedRep cb sb) => \
  EvalSym (op sa sb) where \
  evalSym fillDefault model (cons t) = \
    cons $ evaluateTerm fillDefault model t

#if 1
EVALUATE_SYM_SIMPLE(SymBool)
EVALUATE_SYM_SIMPLE(SymInteger)
EVALUATE_SYM_SIMPLE(SymFPRoundingMode)
EVALUATE_SYM_SIMPLE(SymAlgReal)
EVALUATE_SYM_BV(SymIntN)
EVALUATE_SYM_BV(SymWordN)
EVALUATE_SYM_FUN((=->), (=~>), SymTabularFun)
EVALUATE_SYM_FUN((-->), (-~>), SymGeneralFun)
#endif

instance (ValidFP eb sb) => EvalSym (SymFP eb sb) where
  evalSym fillDefault model (SymFP t) =
    SymFP $ evaluateTerm fillDefault model t

-- Instances
deriveBuiltins
  (ViaDefault ''EvalSym)
  [''EvalSym]
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
  (ViaDefault1 ''EvalSym1)
  [''EvalSym, ''EvalSym1]
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
  (EvalSym1 m, EvalSym e, EvalSym a) =>
  EvalSym (ExceptT e m a)
  where
  evalSym = evalSym1
  {-# INLINE evalSym #-}

instance (EvalSym1 m, EvalSym e) => EvalSym1 (ExceptT e m) where
  liftEvalSym f fillDefault model =
    ExceptT . liftEvalSym (liftEvalSym f) fillDefault model . runExceptT
  {-# INLINE liftEvalSym #-}

-- MaybeT
instance (EvalSym1 m, EvalSym a) => EvalSym (MaybeT m a) where
  evalSym = evalSym1
  {-# INLINE evalSym #-}

instance (EvalSym1 m) => EvalSym1 (MaybeT m) where
  liftEvalSym f fillDefault model =
    MaybeT . liftEvalSym (liftEvalSym f) fillDefault model . runMaybeT
  {-# INLINE liftEvalSym #-}

-- WriterT
instance
  (EvalSym1 m, EvalSym s, EvalSym a) =>
  EvalSym (WriterLazy.WriterT s m a)
  where
  evalSym = evalSym1
  {-# INLINE evalSym #-}

instance
  (EvalSym1 m, EvalSym s) =>
  EvalSym1 (WriterLazy.WriterT s m)
  where
  liftEvalSym f fillDefault model =
    WriterLazy.WriterT
      . liftEvalSym (liftEvalSym2 f evalSym) fillDefault model
      . WriterLazy.runWriterT
  {-# INLINE liftEvalSym #-}

instance
  (EvalSym1 m, EvalSym s, EvalSym a) =>
  EvalSym (WriterStrict.WriterT s m a)
  where
  evalSym = evalSym1
  {-# INLINE evalSym #-}

instance
  (EvalSym1 m, EvalSym s) =>
  EvalSym1 (WriterStrict.WriterT s m)
  where
  liftEvalSym f fillDefault model =
    WriterStrict.WriterT
      . liftEvalSym (liftEvalSym2 f evalSym) fillDefault model
      . WriterStrict.runWriterT
  {-# INLINE liftEvalSym #-}

-- IdentityT
instance (EvalSym1 m, EvalSym a) => EvalSym (IdentityT m a) where
  evalSym = evalSym1
  {-# INLINE evalSym #-}

instance (EvalSym1 m) => EvalSym1 (IdentityT m) where
  liftEvalSym f fillDefault model (IdentityT a) =
    IdentityT $ liftEvalSym f fillDefault model a
  {-# INLINE liftEvalSym #-}

-- Product
deriving via
  (Default (Product l r a))
  instance
    (EvalSym (l a), EvalSym (r a)) => EvalSym (Product l r a)

deriving via
  (Default1 (Product l r))
  instance
    (EvalSym1 l, EvalSym1 r) => EvalSym1 (Product l r)

-- Sum
deriving via
  (Default (Sum l r a))
  instance
    (EvalSym (l a), EvalSym (r a)) => EvalSym (Sum l r a)

deriving via
  (Default1 (Sum l r))
  instance
    (EvalSym1 l, EvalSym1 r) => EvalSym1 (Sum l r)

-- Compose
deriving via
  (Default (Compose f g a))
  instance
    (EvalSym (f (g a))) => EvalSym (Compose f g a)

instance (EvalSym1 f, EvalSym1 g) => EvalSym1 (Compose f g) where
  liftEvalSym f fillDefault m (Compose l) =
    Compose $ liftEvalSym (liftEvalSym f) fillDefault m l
  {-# INLINE liftEvalSym #-}

-- Const
deriving via (Default (Const a b)) instance (EvalSym a) => EvalSym (Const a b)

deriving via (Default1 (Const a)) instance (EvalSym a) => EvalSym1 (Const a)

-- Alt
deriving via (Default (Alt f a)) instance (EvalSym (f a)) => EvalSym (Alt f a)

deriving via (Default1 (Alt f)) instance (EvalSym1 f) => EvalSym1 (Alt f)

-- Ap
deriving via (Default (Ap f a)) instance (EvalSym (f a)) => EvalSym (Ap f a)

deriving via (Default1 (Ap f)) instance (EvalSym1 f) => EvalSym1 (Ap f)

-- Generic
deriving via (Default (U1 p)) instance EvalSym (U1 p)

deriving via (Default (V1 p)) instance EvalSym (V1 p)

deriving via
  (Default (K1 i c p))
  instance
    (EvalSym c) => EvalSym (K1 i c p)

deriving via
  (Default (M1 i c f p))
  instance
    (EvalSym (f p)) => EvalSym (M1 i c f p)

deriving via
  (Default ((f :+: g) p))
  instance
    (EvalSym (f p), EvalSym (g p)) => EvalSym ((f :+: g) p)

deriving via
  (Default ((f :*: g) p))
  instance
    (EvalSym (f p), EvalSym (g p)) => EvalSym ((f :*: g) p)

deriving via
  (Default (Par1 p))
  instance
    (EvalSym p) => EvalSym (Par1 p)

deriving via
  (Default (Rec1 f p))
  instance
    (EvalSym (f p)) => EvalSym (Rec1 f p)

deriving via
  (Default ((f :.: g) p))
  instance
    (EvalSym (f (g p))) => EvalSym ((f :.: g) p)

instance EvalSym2 Either where
  liftEvalSym2 f _ fillDefault model (Left a) = Left $ f fillDefault model a
  liftEvalSym2 _ g fillDefault model (Right a) =
    Right $ g fillDefault model a
  {-# INLINE liftEvalSym2 #-}

instance EvalSym2 (,) where
  liftEvalSym2 f g fillDefault model (a, b) =
    (f fillDefault model a, g fillDefault model b)
  {-# INLINE liftEvalSym2 #-}

instance (EvalSym a) => EvalSym2 ((,,) a) where
  liftEvalSym2 f g fillDefault model (a, b, c) =
    ( evalSym fillDefault model a,
      f fillDefault model b,
      g fillDefault model c
    )
  {-# INLINE liftEvalSym2 #-}

instance (EvalSym a, EvalSym b) => EvalSym2 ((,,,) a b) where
  liftEvalSym2 f g fillDefault model (a, b, c, d) =
    ( evalSym fillDefault model a,
      evalSym fillDefault model b,
      f fillDefault model c,
      g fillDefault model d
    )
  {-# INLINE liftEvalSym2 #-}
