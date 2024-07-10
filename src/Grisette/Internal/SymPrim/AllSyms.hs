{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      :   Grisette.Internal.SymPrim.AllSyms
-- Copyright   :   (c) Sirui Lu 2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Internal.SymPrim.AllSyms
  ( -- * Get all symbolic primitive values in a value
    SomeSym (..),
    AllSyms (..),
    AllSyms1 (..),
    allSymsS1,
    AllSyms2 (..),
    allSymsS2,
    allSymsSize,
    symSize,
    symsSize,

    -- * Generic 'AllSyms'
    AllSymsArgs (..),
    GAllSyms (..),
    genericAllSymsS,
    genericLiftAllSymsS,
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
import GHC.Generics
  ( Generic (Rep, from),
    Generic1 (Rep1, from1),
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
import GHC.TypeNats (KnownNat, type (<=))
import Generics.Deriving
  ( Default (Default, unDefault),
    Default1 (Default1, unDefault1),
  )
import Grisette.Internal.Core.Control.Exception
  ( AssertionError,
    VerificationConditions,
  )
import Grisette.Internal.SymPrim.BV (IntN, WordN)
import Grisette.Internal.SymPrim.FP (FP, FPRoundingMode, ValidFP)
import Grisette.Internal.SymPrim.Prim.SomeTerm
  ( SomeTerm (SomeTerm),
  )
import Grisette.Internal.SymPrim.Prim.Term
  ( LinkedRep (underlyingTerm),
    pformat,
  )
import Grisette.Internal.SymPrim.Prim.TermUtils
  ( someTermsSize,
    termSize,
    termsSize,
  )
import Grisette.Internal.TH.DeriveBuiltin (deriveBuiltins)
import Grisette.Internal.TH.DeriveInstanceProvider
  ( Strategy (ViaDefault, ViaDefault1),
  )
import Grisette.Internal.Utils.Derive (Arity0, Arity1)

-- $setup
-- >>> import Grisette.Core
-- >>> import Grisette.SymPrim
-- >>> import Grisette.Backend
-- >>> import Data.Proxy

-- | Some symbolic value with 'LinkedRep' constraint.
data SomeSym where
  SomeSym :: (LinkedRep con sym) => sym -> SomeSym

instance Show SomeSym where
  show (SomeSym s) = pformat $ underlyingTerm s

-- | Extract all symbolic primitive values that are represented as SMT terms.
--
-- >>> allSyms (["a" + 1 :: SymInteger, -"b"], "c" :: SymBool)
-- [(+ 1 a),(- b),c]
--
-- This is usually used for getting a statistical summary of the size of
-- a symbolic value with 'allSymsSize'.
--
-- __Note:__ This type class can be derived for algebraic data types. You may
-- need the @DerivingVia@ and @DerivingStrategies@ extenstions.
--
-- > data X = ... deriving Generic deriving AllSyms via (Default X)
class AllSyms a where
  -- | Convert a value to a list of symbolic primitive values. It should
  -- prepend to an existing list of symbolic primitive values.
  allSymsS :: a -> [SomeSym] -> [SomeSym]
  allSymsS a l = allSyms a ++ l

  -- | Specialized 'allSymsS' that prepends to an empty list.
  allSyms :: a -> [SomeSym]
  allSyms a = allSymsS a []

  {-# MINIMAL allSymsS | allSyms #-}

-- | Get the sum of the sizes of a list of symbolic terms.
-- Duplicate sub-terms are counted for only once.
--
-- >>> symsSize [1, "a" :: SymInteger, "a" + 1 :: SymInteger]
-- 3
symsSize :: forall con sym. (LinkedRep con sym) => [sym] -> Int
symsSize = termsSize . fmap (underlyingTerm @con)
{-# INLINE symsSize #-}

-- | Get the size of a symbolic term.
-- Duplicate sub-terms are counted for only once.
--
-- >>> symSize (1 :: SymInteger)
-- 1
-- >>> symSize ("a" :: SymInteger)
-- 1
-- >>> symSize ("a" + 1 :: SymInteger)
-- 3
-- >>> symSize (("a" + 1) * ("a" + 1) :: SymInteger)
-- 4
symSize :: forall con sym. (LinkedRep con sym) => sym -> Int
symSize = termSize . underlyingTerm @con
{-# INLINE symSize #-}

someUnderlyingTerm :: SomeSym -> SomeTerm
someUnderlyingTerm (SomeSym s) = SomeTerm $ underlyingTerm s

someSymsSize :: [SomeSym] -> Int
someSymsSize = someTermsSize . fmap someUnderlyingTerm
{-# INLINE someSymsSize #-}

-- | Get the total size of symbolic terms in a value.
-- Duplicate sub-terms are counted for only once.
--
-- >>> allSymsSize ("a" :: SymInteger, "a" + "b" :: SymInteger, ("a" + "b") * "c" :: SymInteger)
-- 5
--
-- The 5 terms are @a@, @b@, @(+ a b)@, @c@, and @(* (+ a b) c)@.
allSymsSize :: (AllSyms a) => a -> Int
allSymsSize = someSymsSize . allSyms

-- | Lifting of the 'AllSyms' class to unary type constructors.
class (forall a. (AllSyms a) => AllSyms (f a)) => AllSyms1 f where
  -- | Lift the 'allSymsS' function to unary type constructors.
  liftAllSymsS :: (a -> [SomeSym] -> [SomeSym]) -> f a -> [SomeSym] -> [SomeSym]

-- | Lift the standard 'allSymsS' function to unary type constructors.
allSymsS1 :: (AllSyms1 f, AllSyms a) => f a -> [SomeSym] -> [SomeSym]
allSymsS1 = liftAllSymsS allSymsS
{-# INLINE allSymsS1 #-}

-- | Lifting of the 'AllSyms' class to binary type constructors.
class (forall a. (AllSyms a) => AllSyms1 (f a)) => AllSyms2 f where
  -- | Lift the 'allSymsS' function to binary type constructors.
  liftAllSymsS2 ::
    (a -> [SomeSym] -> [SomeSym]) ->
    (b -> [SomeSym] -> [SomeSym]) ->
    f a b ->
    [SomeSym] ->
    [SomeSym]

-- | Lift the standard 'allSymsS' function to binary type constructors.
allSymsS2 ::
  (AllSyms2 f, AllSyms a, AllSyms b) => f a b -> [SomeSym] -> [SomeSym]
allSymsS2 = liftAllSymsS2 allSymsS allSymsS
{-# INLINE allSymsS2 #-}

-- Derivation

-- | The arguments to the generic 'AllSyms' function.
data family AllSymsArgs arity a :: Type

data instance AllSymsArgs Arity0 _ = AllSymsArgs0

newtype instance AllSymsArgs Arity1 a
  = AllSymsArgs1 (a -> [SomeSym] -> [SomeSym])

-- | The class of types that can generically extract all symbolic primitives.
class GAllSyms arity f where
  gallSymsS :: AllSymsArgs arity a -> f a -> [SomeSym] -> [SomeSym]

instance GAllSyms arity V1 where
  gallSymsS _ _ = id

instance GAllSyms arity U1 where
  gallSymsS _ _ = id

instance (AllSyms c) => GAllSyms arity (K1 i c) where
  gallSymsS _ (K1 x) = allSymsS x

instance (GAllSyms arity a) => GAllSyms arity (M1 i c a) where
  gallSymsS args (M1 x) = gallSymsS args x

instance (GAllSyms arity a, GAllSyms arity b) => GAllSyms arity (a :+: b) where
  gallSymsS args (L1 l) = gallSymsS args l
  gallSymsS args (R1 r) = gallSymsS args r

instance (GAllSyms arity a, GAllSyms arity b) => GAllSyms arity (a :*: b) where
  gallSymsS args (a :*: b) = gallSymsS args a . gallSymsS args b

instance GAllSyms Arity1 Par1 where
  gallSymsS (AllSymsArgs1 f) (Par1 x) = f x

instance (AllSyms1 f) => GAllSyms Arity1 (Rec1 f) where
  gallSymsS (AllSymsArgs1 f) (Rec1 x) = liftAllSymsS f x

instance (AllSyms1 f, GAllSyms Arity1 g) => GAllSyms Arity1 (f :.: g) where
  gallSymsS targs (Comp1 x) = liftAllSymsS (gallSymsS targs) x

-- | Generic 'allSymsS' function.
genericAllSymsS ::
  (Generic a, GAllSyms Arity0 (Rep a)) =>
  a ->
  [SomeSym] ->
  [SomeSym]
genericAllSymsS x = gallSymsS AllSymsArgs0 (from x)
{-# INLINE genericAllSymsS #-}

-- | Generic 'liftAllSymsS' function.
genericLiftAllSymsS ::
  (Generic1 f, GAllSyms Arity1 (Rep1 f)) =>
  (a -> [SomeSym] -> [SomeSym]) ->
  f a ->
  [SomeSym] ->
  [SomeSym]
genericLiftAllSymsS f x = gallSymsS (AllSymsArgs1 f) (from1 x)
{-# INLINE genericLiftAllSymsS #-}

instance (Generic a, GAllSyms Arity0 (Rep a)) => AllSyms (Default a) where
  allSymsS = genericAllSymsS . unDefault
  {-# INLINE allSymsS #-}

instance
  (Generic1 f, GAllSyms Arity1 (Rep1 f), AllSyms a) =>
  AllSyms (Default1 f a)
  where
  allSymsS = allSymsS1
  {-# INLINE allSymsS #-}

instance (Generic1 f, GAllSyms Arity1 (Rep1 f)) => AllSyms1 (Default1 f) where
  liftAllSymsS f = genericLiftAllSymsS f . unDefault1
  {-# INLINE liftAllSymsS #-}

-- Instances
deriveBuiltins
  (ViaDefault ''AllSyms)
  [''AllSyms]
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
  (ViaDefault1 ''AllSyms1)
  [''AllSyms, ''AllSyms1]
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
  (AllSyms1 m, AllSyms e, AllSyms a) =>
  AllSyms (ExceptT e m a)
  where
  allSymsS = allSymsS1
  {-# INLINE allSymsS #-}

instance
  (AllSyms1 m, AllSyms e) =>
  AllSyms1 (ExceptT e m)
  where
  liftAllSymsS f (ExceptT v) = liftAllSymsS (liftAllSymsS f) v
  {-# INLINE liftAllSymsS #-}

-- MaybeT
instance (AllSyms1 m, AllSyms a) => AllSyms (MaybeT m a) where
  allSymsS = allSymsS1
  {-# INLINE allSymsS #-}

instance (AllSyms1 m) => AllSyms1 (MaybeT m) where
  liftAllSymsS f (MaybeT v) = liftAllSymsS (liftAllSymsS f) v
  {-# INLINE liftAllSymsS #-}

-- WriterT
instance
  (AllSyms1 m, AllSyms a, AllSyms s) =>
  AllSyms (WriterLazy.WriterT s m a)
  where
  allSymsS = allSymsS1
  {-# INLINE allSymsS #-}

instance
  (AllSyms1 m, AllSyms s) =>
  AllSyms1 (WriterLazy.WriterT s m)
  where
  liftAllSymsS f (WriterLazy.WriterT v) =
    liftAllSymsS (liftAllSymsS2 f allSymsS) v
  {-# INLINE liftAllSymsS #-}

instance
  (AllSyms1 m, AllSyms a, AllSyms s) =>
  AllSyms (WriterStrict.WriterT s m a)
  where
  allSymsS = allSymsS1
  {-# INLINE allSymsS #-}

instance
  (AllSyms1 m, AllSyms s) =>
  AllSyms1 (WriterStrict.WriterT s m)
  where
  liftAllSymsS f (WriterStrict.WriterT v) =
    liftAllSymsS (liftAllSymsS2 f allSymsS) v
  {-# INLINE liftAllSymsS #-}

-- Sum
deriving via
  (Default (Sum f g a))
  instance
    (AllSyms (f a), AllSyms (g a)) =>
    AllSyms (Sum f g a)

deriving via
  (Default1 (Sum f g))
  instance
    (AllSyms1 f, AllSyms1 g) =>
    AllSyms1 (Sum f g)

-- IdentityT
instance
  (AllSyms1 m, AllSyms a) =>
  AllSyms (IdentityT m a)
  where
  allSymsS = allSymsS1
  {-# INLINE allSymsS #-}

instance (AllSyms1 m) => AllSyms1 (IdentityT m) where
  liftAllSymsS f (IdentityT a) = liftAllSymsS f a
  {-# INLINE liftAllSymsS #-}

-- AllSyms2
instance AllSyms2 Either where
  liftAllSymsS2 f _ (Left x) = f x
  liftAllSymsS2 _ g (Right y) = g y
  {-# INLINE liftAllSymsS2 #-}

instance AllSyms2 (,) where
  liftAllSymsS2 f g (x, y) = f x . g y
  {-# INLINE liftAllSymsS2 #-}

instance (AllSyms a) => AllSyms2 ((,,) a) where
  liftAllSymsS2 f g (x, y, z) = allSymsS x . f y . g z
  {-# INLINE liftAllSymsS2 #-}

instance (AllSyms a, AllSyms b) => AllSyms2 ((,,,) a b) where
  liftAllSymsS2 f g (x, y, z, w) = allSymsS x . allSymsS y . f z . g w
  {-# INLINE liftAllSymsS2 #-}

#define CONCRETE_ALLSYMS(type) \
instance AllSyms type where \
  allSymsS _ = id; \
  {-# INLINE allSymsS #-}

#define CONCRETE_ALLSYMS_BV(type) \
instance (KnownNat n, 1 <= n) => AllSyms (type n) where \
  allSymsS _ = id; \
  {-# INLINE allSymsS #-}

#if 1
CONCRETE_ALLSYMS(Bool)
CONCRETE_ALLSYMS(Integer)
CONCRETE_ALLSYMS(Char)
CONCRETE_ALLSYMS(Int)
CONCRETE_ALLSYMS(Int8)
CONCRETE_ALLSYMS(Int16)
CONCRETE_ALLSYMS(Int32)
CONCRETE_ALLSYMS(Int64)
CONCRETE_ALLSYMS(Word)
CONCRETE_ALLSYMS(Word8)
CONCRETE_ALLSYMS(Word16)
CONCRETE_ALLSYMS(Word32)
CONCRETE_ALLSYMS(Word64)
CONCRETE_ALLSYMS(Float)
CONCRETE_ALLSYMS(Double)
CONCRETE_ALLSYMS(B.ByteString)
CONCRETE_ALLSYMS(T.Text)
CONCRETE_ALLSYMS(FPRoundingMode)
CONCRETE_ALLSYMS_BV(WordN)
CONCRETE_ALLSYMS_BV(IntN)
#endif

instance (ValidFP eb sb) => AllSyms (FP eb sb) where
  allSymsS _ = id
  {-# INLINE allSymsS #-}
