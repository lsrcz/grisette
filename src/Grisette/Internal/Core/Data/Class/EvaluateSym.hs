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
-- Module      :   Grisette.Internal.Core.Data.Class.EvaluateSym
-- Copyright   :   (c) Sirui Lu 2021-2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Internal.Core.Data.Class.EvaluateSym
  ( -- * Evaluating symbolic values with model
    EvaluateSym (..),
    evaluateSymToCon,
    EvaluateSym1 (..),
    evaluateSym1,
    evaluateSymToCon1,
    EvaluateSym2 (..),
    evaluateSym2,
    evaluateSymToCon2,

    -- * Generic 'EvaluateSym'
    EvaluateSymArgs (..),
    GEvaluateSym (..),
    genericEvaluateSym,
    genericLiftEvaluateSym,
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
import Data.Functor.Sum (Sum)
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Kind (Type)
import Data.Maybe (fromJust)
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
import Grisette.Internal.TH.Derivation
  ( Strategy (ViaDefault, ViaDefault1),
    deriveFunctorArgBuiltins,
    deriveSimpleBuiltin1s,
  )
import Grisette.Internal.Utils.Derive (Arity0, Arity1)

-- $setup
-- >>> import Grisette.Core
-- >>> import Grisette.SymPrim
-- >>> import Data.Proxy
-- >>> :set -XTypeApplications

-- | Evaluating symbolic values with some model.
--
-- >>> let model = insertValue "a" (1 :: Integer) emptyModel :: Model
-- >>> evaluateSym False model ([ssym "a", ssym "b"] :: [SymInteger])
-- [1,b]
--
-- If we set the first argument true, the missing variables will be filled in
-- with some default values:
--
-- >>> evaluateSym True model ([ssym "a", ssym "b"] :: [SymInteger])
-- [1,0]
--
-- __Note 1:__ This type class can be derived for algebraic data types.
-- You may need the @DerivingVia@ and @DerivingStrategies@ extensions.
--
-- > data X = ... deriving Generic deriving EvaluateSym via (Default X)
class EvaluateSym a where
  -- | Evaluate a symbolic variable with some model, possibly fill in values for
  -- the missing variables.
  evaluateSym :: Bool -> Model -> a -> a

-- | Evaluate a symbolic variable with some model, fill in values for the
-- missing variables, and transform to concrete ones
--
-- >>> let model = insertValue "a" (1 :: Integer) emptyModel :: Model
-- >>> evaluateSymToCon model ([ssym "a", ssym "b"] :: [SymInteger]) :: [Integer]
-- [1,0]
evaluateSymToCon :: (ToCon a b, EvaluateSym a) => Model -> a -> b
evaluateSymToCon model a = fromJust $ toCon $ evaluateSym True model a

class (forall a. (EvaluateSym a) => EvaluateSym (f a)) => EvaluateSym1 f where
  liftEvaluateSym :: (Bool -> Model -> a -> a) -> (Bool -> Model -> f a -> f a)

evaluateSym1 :: (EvaluateSym1 f, EvaluateSym a) => Bool -> Model -> f a -> f a
evaluateSym1 = liftEvaluateSym evaluateSym
{-# INLINE evaluateSym1 #-}

evaluateSymToCon1 ::
  (EvaluateSym1 f, EvaluateSym a, ToCon1 f g, ToCon a b) =>
  Model ->
  f a ->
  g b
evaluateSymToCon1 model a = fromJust $ toCon1 $ evaluateSym1 True model a
{-# INLINE evaluateSymToCon1 #-}

class (forall a. (EvaluateSym a) => EvaluateSym1 (f a)) => EvaluateSym2 f where
  liftEvaluateSym2 ::
    (Bool -> Model -> a -> a) ->
    (Bool -> Model -> b -> b) ->
    (Bool -> Model -> f a b -> f a b)

evaluateSym2 ::
  (EvaluateSym2 f, EvaluateSym a, EvaluateSym b) =>
  Bool ->
  Model ->
  f a b ->
  f a b
evaluateSym2 = liftEvaluateSym2 evaluateSym evaluateSym
{-# INLINE evaluateSym2 #-}

evaluateSymToCon2 ::
  ( EvaluateSym2 f,
    EvaluateSym a,
    EvaluateSym c,
    ToCon2 f g,
    ToCon a b,
    ToCon c d
  ) =>
  Model ->
  f a c ->
  g b d
evaluateSymToCon2 model a = fromJust $ toCon2 $ evaluateSym2 True model a
{-# INLINE evaluateSymToCon2 #-}

-- Derivations

-- | The arguments to the generic 'evaluateSym' function.
data family EvaluateSymArgs arity a :: Type

data instance EvaluateSymArgs Arity0 _ = EvaluateSymArgs0

newtype instance EvaluateSymArgs Arity1 a
  = EvaluateSymArgs1 (Bool -> Model -> a -> a)

-- | The class of types that can be generically evaluated.
class GEvaluateSym arity f where
  gevaluateSym :: EvaluateSymArgs arity a -> Bool -> Model -> f a -> f a

instance GEvaluateSym arity V1 where
  gevaluateSym _ _ _ = id
  {-# INLINE gevaluateSym #-}

instance GEvaluateSym arity U1 where
  gevaluateSym _ _ _ = id
  {-# INLINE gevaluateSym #-}

instance
  (GEvaluateSym arity a, GEvaluateSym arity b) =>
  GEvaluateSym arity (a :*: b)
  where
  gevaluateSym args fillDefault model (a :*: b) =
    gevaluateSym args fillDefault model a
      :*: gevaluateSym args fillDefault model b
  {-# INLINE gevaluateSym #-}

instance
  (GEvaluateSym arity a, GEvaluateSym arity b) =>
  GEvaluateSym arity (a :+: b)
  where
  gevaluateSym args fillDefault model (L1 l) =
    L1 $ gevaluateSym args fillDefault model l
  gevaluateSym args fillDefault model (R1 r) =
    R1 $ gevaluateSym args fillDefault model r
  {-# INLINE gevaluateSym #-}

instance (GEvaluateSym arity a) => GEvaluateSym arity (M1 i c a) where
  gevaluateSym args fillDefault model (M1 x) =
    M1 $ gevaluateSym args fillDefault model x
  {-# INLINE gevaluateSym #-}

instance (EvaluateSym a) => GEvaluateSym arity (K1 i a) where
  gevaluateSym _ fillDefault model (K1 x) = K1 $ evaluateSym fillDefault model x
  {-# INLINE gevaluateSym #-}

instance GEvaluateSym Arity1 Par1 where
  gevaluateSym (EvaluateSymArgs1 f) fillDefault model (Par1 x) =
    Par1 $ f fillDefault model x
  {-# INLINE gevaluateSym #-}

instance (EvaluateSym1 a) => GEvaluateSym Arity1 (Rec1 a) where
  gevaluateSym (EvaluateSymArgs1 f) fillDefault model (Rec1 x) =
    Rec1 $ liftEvaluateSym f fillDefault model x
  {-# INLINE gevaluateSym #-}

instance
  (EvaluateSym1 f, GEvaluateSym Arity1 g) =>
  GEvaluateSym Arity1 (f :.: g)
  where
  gevaluateSym targs fillDefault model (Comp1 x) =
    Comp1 $ liftEvaluateSym (gevaluateSym targs) fillDefault model x
  {-# INLINE gevaluateSym #-}

genericEvaluateSym ::
  (Generic a, GEvaluateSym Arity0 (Rep a)) => Bool -> Model -> a -> a
genericEvaluateSym fillDefault model =
  to . gevaluateSym EvaluateSymArgs0 fillDefault model . from
{-# INLINE genericEvaluateSym #-}

genericLiftEvaluateSym ::
  (Generic1 f, GEvaluateSym Arity1 (Rep1 f)) =>
  (Bool -> Model -> a -> a) ->
  Bool ->
  Model ->
  f a ->
  f a
genericLiftEvaluateSym f fillDefault model =
  to1 . gevaluateSym (EvaluateSymArgs1 f) fillDefault model . from1
{-# INLINE genericLiftEvaluateSym #-}

instance
  (Generic a, GEvaluateSym Arity0 (Rep a)) =>
  EvaluateSym (Default a)
  where
  evaluateSym fillDefault model =
    Default . genericEvaluateSym fillDefault model . unDefault
  {-# INLINE evaluateSym #-}

instance
  (Generic1 f, GEvaluateSym Arity1 (Rep1 f), EvaluateSym a) =>
  EvaluateSym (Default1 f a)
  where
  evaluateSym = evaluateSym1
  {-# INLINE evaluateSym #-}

instance
  (Generic1 f, GEvaluateSym Arity1 (Rep1 f)) =>
  EvaluateSym1 (Default1 f)
  where
  liftEvaluateSym f fillDefault model =
    Default1 . genericLiftEvaluateSym f fillDefault model . unDefault1
  {-# INLINE liftEvaluateSym #-}

-- Instances
deriveFunctorArgBuiltins
  (ViaDefault ''EvaluateSym)
  ''EvaluateSym
  ''EvaluateSym1
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

deriveSimpleBuiltin1s
  (ViaDefault1 ''EvaluateSym1)
  ''EvaluateSym
  ''EvaluateSym1
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
  (EvaluateSym1 m, EvaluateSym e, EvaluateSym a) =>
  EvaluateSym (ExceptT e m a)
  where
  evaluateSym = evaluateSym1
  {-# INLINE evaluateSym #-}

instance (EvaluateSym1 m, EvaluateSym e) => EvaluateSym1 (ExceptT e m) where
  liftEvaluateSym f fillDefault model =
    ExceptT . liftEvaluateSym (liftEvaluateSym f) fillDefault model . runExceptT
  {-# INLINE liftEvaluateSym #-}

-- MaybeT
instance (EvaluateSym1 m, EvaluateSym a) => EvaluateSym (MaybeT m a) where
  evaluateSym = evaluateSym1
  {-# INLINE evaluateSym #-}

instance (EvaluateSym1 m) => EvaluateSym1 (MaybeT m) where
  liftEvaluateSym f fillDefault model =
    MaybeT . liftEvaluateSym (liftEvaluateSym f) fillDefault model . runMaybeT
  {-# INLINE liftEvaluateSym #-}

-- WriterT
instance
  (EvaluateSym1 m, EvaluateSym s, EvaluateSym a) =>
  EvaluateSym (WriterLazy.WriterT s m a)
  where
  evaluateSym = evaluateSym1
  {-# INLINE evaluateSym #-}

instance
  (EvaluateSym1 m, EvaluateSym s) =>
  EvaluateSym1 (WriterLazy.WriterT s m)
  where
  liftEvaluateSym f fillDefault model =
    WriterLazy.WriterT
      . liftEvaluateSym (liftEvaluateSym2 f evaluateSym) fillDefault model
      . WriterLazy.runWriterT
  {-# INLINE liftEvaluateSym #-}

instance
  (EvaluateSym1 m, EvaluateSym s, EvaluateSym a) =>
  EvaluateSym (WriterStrict.WriterT s m a)
  where
  evaluateSym = evaluateSym1
  {-# INLINE evaluateSym #-}

instance
  (EvaluateSym1 m, EvaluateSym s) =>
  EvaluateSym1 (WriterStrict.WriterT s m)
  where
  liftEvaluateSym f fillDefault model =
    WriterStrict.WriterT
      . liftEvaluateSym (liftEvaluateSym2 f evaluateSym) fillDefault model
      . WriterStrict.runWriterT
  {-# INLINE liftEvaluateSym #-}

-- Sum
deriving via
  (Default (Sum f g a))
  instance
    (EvaluateSym (f a), EvaluateSym (g a)) => EvaluateSym (Sum f g a)

deriving via
  (Default1 (Sum f g))
  instance
    (EvaluateSym1 f, EvaluateSym1 g) => EvaluateSym1 (Sum f g)

-- IdentityT
instance (EvaluateSym1 m, EvaluateSym a) => EvaluateSym (IdentityT m a) where
  evaluateSym = evaluateSym1
  {-# INLINE evaluateSym #-}

instance (EvaluateSym1 m) => EvaluateSym1 (IdentityT m) where
  liftEvaluateSym f fillDefault model (IdentityT a) =
    IdentityT $ liftEvaluateSym f fillDefault model a
  {-# INLINE liftEvaluateSym #-}

instance EvaluateSym2 Either where
  liftEvaluateSym2 f _ fillDefault model (Left a) = Left $ f fillDefault model a
  liftEvaluateSym2 _ g fillDefault model (Right a) =
    Right $ g fillDefault model a
  {-# INLINE liftEvaluateSym2 #-}

instance EvaluateSym2 (,) where
  liftEvaluateSym2 f g fillDefault model (a, b) =
    (f fillDefault model a, g fillDefault model b)
  {-# INLINE liftEvaluateSym2 #-}

instance (EvaluateSym a) => EvaluateSym2 ((,,) a) where
  liftEvaluateSym2 f g fillDefault model (a, b, c) =
    ( evaluateSym fillDefault model a,
      f fillDefault model b,
      g fillDefault model c
    )
  {-# INLINE liftEvaluateSym2 #-}

instance (EvaluateSym a, EvaluateSym b) => EvaluateSym2 ((,,,) a b) where
  liftEvaluateSym2 f g fillDefault model (a, b, c, d) =
    ( evaluateSym fillDefault model a,
      evaluateSym fillDefault model b,
      f fillDefault model c,
      g fillDefault model d
    )
  {-# INLINE liftEvaluateSym2 #-}

#define CONCRETE_EVALUATESYM(type) \
instance EvaluateSym type where \
  evaluateSym _ _ = id

#define CONCRETE_EVALUATESYM_BV(type) \
instance (KnownNat n, 1 <= n) => EvaluateSym (type n) where \
  evaluateSym _ _ = id

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
CONCRETE_EVALUATESYM_BV(IntN)
CONCRETE_EVALUATESYM_BV(WordN)
#endif

instance (ValidFP eb fb) => EvaluateSym (FP eb fb) where
  evaluateSym _ _ = id

-- Symbolic primitives
#define EVALUATE_SYM_SIMPLE(symtype) \
instance EvaluateSym symtype where \
  evaluateSym fillDefault model (symtype t) = \
    symtype $ evaluateTerm fillDefault model t

#define EVALUATE_SYM_BV(symtype) \
instance (KnownNat n, 1 <= n) => EvaluateSym (symtype n) where \
  evaluateSym fillDefault model (symtype t) = \
    symtype $ evaluateTerm fillDefault model t

#define EVALUATE_SYM_FUN(cop, op, cons) \
instance (SupportedPrim (cop ca cb), LinkedRep ca sa, LinkedRep cb sb) => \
  EvaluateSym (op sa sb) where \
  evaluateSym fillDefault model (cons t) = \
    cons $ evaluateTerm fillDefault model t

#if 1
EVALUATE_SYM_SIMPLE(SymBool)
EVALUATE_SYM_SIMPLE(SymInteger)
EVALUATE_SYM_SIMPLE(SymFPRoundingMode)
EVALUATE_SYM_BV(SymIntN)
EVALUATE_SYM_BV(SymWordN)
EVALUATE_SYM_FUN((=->), (=~>), SymTabularFun)
EVALUATE_SYM_FUN((-->), (-~>), SymGeneralFun)
#endif

instance (ValidFP eb sb) => EvaluateSym (SymFP eb sb) where
  evaluateSym fillDefault model (SymFP t) =
    SymFP $ evaluateTerm fillDefault model t
