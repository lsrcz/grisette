{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      :   Grisette.Internal.Core.Data.Class.SymEq
-- Copyright   :   (c) Sirui Lu 2021-2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Internal.Core.Data.Class.SymEq
  ( -- * Symbolic equality
    SymEq (..),
    SymEq1 (..),
    symEq1,
    SymEq2 (..),
    symEq2,
    pairwiseSymDistinct,

    -- * More 'Eq' helper
    distinct,

    -- * Generic 'SymEq'
    SymEqArgs (..),
    GSymEq (..),
    genericSymEq,
    genericLiftSymEq,
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
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Monoid (Alt, Ap)
import qualified Data.Monoid as Monoid
import Data.Ord (Down)
import Data.Ratio (Ratio, denominator, numerator)
import qualified Data.Text as T
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.TypeNats (KnownNat, type (<=))
import Generics.Deriving
  ( Default (Default),
    Default1 (Default1),
    Generic (Rep, from),
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
import Grisette.Internal.Core.Control.Exception
  ( AssertionError,
    VerificationConditions,
  )
import Grisette.Internal.Core.Data.Class.LogicalOp (LogicalOp (symNot, (.&&)))
import Grisette.Internal.Core.Data.Class.Solvable (Solvable (con))
import Grisette.Internal.SymPrim.AlgReal (AlgReal)
import Grisette.Internal.SymPrim.BV (IntN, WordN)
import Grisette.Internal.SymPrim.FP
  ( FP,
    FPRoundingMode,
    NotRepresentableFPError,
    ValidFP,
  )
import Grisette.Internal.SymPrim.Prim.Term
  ( SupportedPrim (pevalDistinctTerm),
    pevalEqTerm,
    underlyingTerm,
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
import Grisette.Internal.SymPrim.SymInteger (SymInteger (SymInteger))
import Grisette.Internal.TH.DeriveBuiltin (deriveBuiltins)
import Grisette.Internal.TH.DeriveInstanceProvider
  ( Strategy (ViaDefault, ViaDefault1),
  )
import Grisette.Internal.Utils.Derive (Arity0, Arity1)

-- | Check if all elements in a list are distinct.
--
-- Note that empty or singleton lists are always distinct.
--
-- >>> distinct []
-- True
-- >>> distinct [1]
-- True
-- >>> distinct [1, 2, 3]
-- True
-- >>> distinct [1, 2, 2]
-- False
distinct :: (Eq a) => [a] -> Bool
distinct [] = True
distinct [_] = True
distinct (x : xs) = go x xs && distinct xs
  where
    go _ [] = True
    go x' (y : ys) = x' /= y .&& go x' ys

-- $setup
-- >>> import Grisette.Core
-- >>> import Grisette.SymPrim

-- | Symbolic equality. Note that we can't use Haskell's 'Eq' class since
-- symbolic comparison won't necessarily return a concrete 'Bool' value.
--
-- >>> let a = 1 :: SymInteger
-- >>> let b = 2 :: SymInteger
-- >>> a .== b
-- false
-- >>> a ./= b
-- true
--
-- >>> let a = "a" :: SymInteger
-- >>> let b = "b" :: SymInteger
-- >>> a .== b
-- (= a b)
-- >>> a ./= b
-- (distinct a b)
--
-- __Note:__ This type class can be derived for algebraic data types.
-- You may need the @DerivingVia@ and @DerivingStrategies@ extensions.
--
-- > data X = ... deriving Generic deriving SymEq via (Default X)
class SymEq a where
  (.==) :: a -> a -> SymBool
  a .== b = symNot $ a ./= b
  {-# INLINE (.==) #-}
  infix 4 .==

  (./=) :: a -> a -> SymBool
  a ./= b = symNot $ a .== b
  {-# INLINE (./=) #-}
  infix 4 ./=

  -- | Check if all elements in a list are distinct, under the symbolic equality
  -- semantics.
  symDistinct :: [a] -> SymBool
  symDistinct = pairwiseSymDistinct

  {-# MINIMAL (.==) | (./=) #-}

-- | Default pairwise symbolic distinct implementation.
pairwiseSymDistinct :: (SymEq a) => [a] -> SymBool
pairwiseSymDistinct [] = con True
pairwiseSymDistinct [_] = con True
pairwiseSymDistinct (x : xs) = go x xs .&& pairwiseSymDistinct xs
  where
    go _ [] = con True
    go x' (y : ys) = x' ./= y .&& go x' ys

-- | Lifting of the 'SymEq' class to unary type constructors.
--
-- Any instance should be subject to the following law that canonicity is
-- preserved:
--
-- @liftSymEq (.==)@ should be equivalent to @(.==)@, under the symbolic
-- semantics.
--
-- This class therefore represents the generalization of 'SymEq' by decomposing
-- its main method into a canonical lifting on a canonical inner method, so that
-- the lifting can be reused for other arguments than the canonical one.
class (forall a. (SymEq a) => SymEq (f a)) => SymEq1 f where
  -- | Lift a symbolic equality test through the type constructor.
  --
  -- The function will usually be applied to an symbolic equality function, but
  -- the more general type ensures that the implementation uses it to compare
  -- elements of the first container with elements of the second.
  liftSymEq :: (a -> b -> SymBool) -> f a -> f b -> SymBool

-- | Lift the standard @('.==')@ function through the type constructor.
symEq1 :: (SymEq a, SymEq1 f) => f a -> f a -> SymBool
symEq1 = liftSymEq (.==)

-- | Lifting of the 'SymEq' class to binary type constructors.
class (forall a. (SymEq a) => SymEq1 (f a)) => SymEq2 f where
  -- | Lift symbolic equality tests through the type constructor.
  --
  -- The function will usually be applied to an symbolic equality function, but
  -- the more general type ensures that the implementation uses it to compare
  -- elements of the first container with elements of the second.
  liftSymEq2 ::
    (a -> b -> SymBool) ->
    (c -> d -> SymBool) ->
    f a c ->
    f b d ->
    SymBool

-- | Lift the standard @('.==')@ function through the type constructor.
symEq2 :: (SymEq a, SymEq b, SymEq2 f) => f a b -> f a b -> SymBool
symEq2 = liftSymEq2 (.==) (.==)

-- Derivations

-- | The arguments to the generic equality function.
data family SymEqArgs arity a b :: Type

data instance SymEqArgs Arity0 _ _ = SymEqArgs0

newtype instance SymEqArgs Arity1 a b = SymEqArgs1 (a -> b -> SymBool)

-- | The class of types that can be generically compared for symbolic equality.
class GSymEq arity f where
  gsymEq :: SymEqArgs arity a b -> f a -> f b -> SymBool

instance GSymEq arity V1 where
  gsymEq _ _ _ = con True
  {-# INLINE gsymEq #-}

instance GSymEq arity U1 where
  gsymEq _ _ _ = con True
  {-# INLINE gsymEq #-}

instance (GSymEq arity a, GSymEq arity b) => GSymEq arity (a :*: b) where
  gsymEq args (a1 :*: b1) (a2 :*: b2) = gsymEq args a1 a2 .&& gsymEq args b1 b2
  {-# INLINE gsymEq #-}

instance (GSymEq arity a, GSymEq arity b) => GSymEq arity (a :+: b) where
  gsymEq args (L1 a1) (L1 a2) = gsymEq args a1 a2
  gsymEq args (R1 b1) (R1 b2) = gsymEq args b1 b2
  gsymEq _ _ _ = con False
  {-# INLINE gsymEq #-}

instance (GSymEq arity a) => GSymEq arity (M1 i c a) where
  gsymEq args (M1 a1) (M1 a2) = gsymEq args a1 a2
  {-# INLINE gsymEq #-}

instance (SymEq a) => GSymEq arity (K1 i a) where
  gsymEq _ (K1 a) (K1 b) = a .== b
  {-# INLINE gsymEq #-}

instance GSymEq Arity1 Par1 where
  gsymEq (SymEqArgs1 e) (Par1 a) (Par1 b) = e a b
  {-# INLINE gsymEq #-}

instance (SymEq1 f) => GSymEq Arity1 (Rec1 f) where
  gsymEq (SymEqArgs1 e) (Rec1 a) (Rec1 b) = liftSymEq e a b
  {-# INLINE gsymEq #-}

instance (SymEq1 f, GSymEq Arity1 g) => GSymEq Arity1 (f :.: g) where
  gsymEq targs (Comp1 a) (Comp1 b) = liftSymEq (gsymEq targs) a b
  {-# INLINE gsymEq #-}

instance (Generic a, GSymEq Arity0 (Rep a)) => SymEq (Default a) where
  Default l .== Default r = genericSymEq l r
  {-# INLINE (.==) #-}

-- | Generic @('.==')@ function.
genericSymEq :: (Generic a, GSymEq Arity0 (Rep a)) => a -> a -> SymBool
genericSymEq l r = gsymEq SymEqArgs0 (from l) (from r)
{-# INLINE genericSymEq #-}

instance (Generic1 f, GSymEq Arity1 (Rep1 f), SymEq a) => SymEq (Default1 f a) where
  (.==) = symEq1
  {-# INLINE (.==) #-}

instance (Generic1 f, GSymEq Arity1 (Rep1 f)) => SymEq1 (Default1 f) where
  liftSymEq f (Default1 l) (Default1 r) = genericLiftSymEq f l r
  {-# INLINE liftSymEq #-}

-- | Generic 'liftSymEq' function.
genericLiftSymEq ::
  (Generic1 f, GSymEq Arity1 (Rep1 f)) =>
  (a -> b -> SymBool) ->
  f a ->
  f b ->
  SymBool
genericLiftSymEq f l r = gsymEq (SymEqArgs1 f) (from1 l) (from1 r)
{-# INLINE genericLiftSymEq #-}

#define CONCRETE_SEQ(type) \
instance SymEq type where \
  l .== r = con $ l == r; \
  {-# INLINE (.==) #-}

#define CONCRETE_SEQ_BV(type) \
instance (KnownNat n, 1 <= n) => SymEq (type n) where \
  l .== r = con $ l == r; \
  {-# INLINE (.==) #-}

#if 1
CONCRETE_SEQ(Bool)
CONCRETE_SEQ(Integer)
CONCRETE_SEQ(Char)
CONCRETE_SEQ(Int)
CONCRETE_SEQ(Int8)
CONCRETE_SEQ(Int16)
CONCRETE_SEQ(Int32)
CONCRETE_SEQ(Int64)
CONCRETE_SEQ(Word)
CONCRETE_SEQ(Word8)
CONCRETE_SEQ(Word16)
CONCRETE_SEQ(Word32)
CONCRETE_SEQ(Word64)
CONCRETE_SEQ(Float)
CONCRETE_SEQ(Double)
CONCRETE_SEQ(B.ByteString)
CONCRETE_SEQ(T.Text)
CONCRETE_SEQ(FPRoundingMode)
CONCRETE_SEQ(Monoid.All)
CONCRETE_SEQ(Monoid.Any)
CONCRETE_SEQ(Ordering)
CONCRETE_SEQ_BV(WordN)
CONCRETE_SEQ_BV(IntN)
CONCRETE_SEQ(AlgReal)
#endif

instance (SymEq a) => SymEq (Ratio a) where
  a .== b = numerator a .== numerator b .&& denominator a .== denominator b
  {-# INLINE (.==) #-}

instance (ValidFP eb sb) => SymEq (FP eb sb) where
  l .== r = con $ l == r
  {-# INLINE (.==) #-}

-- Symbolic types
#define SEQ_SIMPLE(symtype) \
instance SymEq symtype where \
  (symtype l) .== (symtype r) = SymBool $ pevalEqTerm l r; \
  {-# INLINE (.==) #-}; \
  l ./= r = symDistinct [l, r]; \
  {-# INLINE (./=) #-}; \
  symDistinct [] = con True; \
  symDistinct [_] = con True; \
  symDistinct (l:ls) = SymBool $ \
    pevalDistinctTerm (underlyingTerm l :| (underlyingTerm <$> ls))

#define SEQ_BV(symtype) \
instance (KnownNat n, 1 <= n) => SymEq (symtype n) where \
  (symtype l) .== (symtype r) = SymBool $ pevalEqTerm l r; \
  {-# INLINE (.==) #-}; \
  l ./= r = symDistinct [l, r]; \
  {-# INLINE (./=) #-}; \
  symDistinct [] = con True; \
  symDistinct [_] = con True; \
  symDistinct (l:ls) = SymBool $ \
    pevalDistinctTerm (underlyingTerm l :| (underlyingTerm <$> ls))

#if 1
SEQ_SIMPLE(SymBool)
SEQ_SIMPLE(SymInteger)
SEQ_SIMPLE(SymFPRoundingMode)
SEQ_SIMPLE(SymAlgReal)
SEQ_BV(SymIntN)
SEQ_BV(SymWordN)
#endif

instance (ValidFP eb sb) => SymEq (SymFP eb sb) where
  (SymFP l) .== (SymFP r) = SymBool $ pevalEqTerm l r
  {-# INLINE (.==) #-}

-- Instances
deriveBuiltins
  (ViaDefault ''SymEq)
  [''SymEq]
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
  (ViaDefault1 ''SymEq1)
  [''SymEq, ''SymEq1]
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
instance (SymEq1 m, SymEq e, SymEq a) => SymEq (ExceptT e m a) where
  (.==) = symEq1
  {-# INLINE (.==) #-}

instance (SymEq1 m, SymEq e) => SymEq1 (ExceptT e m) where
  liftSymEq f (ExceptT l) (ExceptT r) = liftSymEq (liftSymEq f) l r
  {-# INLINE liftSymEq #-}

-- MaybeT
instance (SymEq1 m, SymEq a) => SymEq (MaybeT m a) where
  (.==) = symEq1
  {-# INLINE (.==) #-}

instance (SymEq1 m) => SymEq1 (MaybeT m) where
  liftSymEq f (MaybeT l) (MaybeT r) = liftSymEq (liftSymEq f) l r
  {-# INLINE liftSymEq #-}

-- Writer
instance (SymEq1 m, SymEq w, SymEq a) => SymEq (WriterLazy.WriterT w m a) where
  (.==) = symEq1
  {-# INLINE (.==) #-}

instance (SymEq1 m, SymEq w) => SymEq1 (WriterLazy.WriterT w m) where
  liftSymEq f (WriterLazy.WriterT l) (WriterLazy.WriterT r) =
    liftSymEq (liftSymEq2 f (.==)) l r
  {-# INLINE liftSymEq #-}

instance (SymEq1 m, SymEq w, SymEq a) => SymEq (WriterStrict.WriterT w m a) where
  (.==) = symEq1
  {-# INLINE (.==) #-}

instance (SymEq1 m, SymEq w) => SymEq1 (WriterStrict.WriterT w m) where
  liftSymEq f (WriterStrict.WriterT l) (WriterStrict.WriterT r) =
    liftSymEq (liftSymEq2 f (.==)) l r
  {-# INLINE liftSymEq #-}

-- IdentityT
instance (SymEq1 m, SymEq a) => SymEq (IdentityT m a) where
  (.==) = symEq1
  {-# INLINE (.==) #-}

instance (SymEq1 m) => SymEq1 (IdentityT m) where
  liftSymEq f (IdentityT l) (IdentityT r) = liftSymEq f l r
  {-# INLINE liftSymEq #-}

-- Product
deriving via
  (Default (Product l r a))
  instance
    (SymEq (l a), SymEq (r a)) => SymEq (Product l r a)

deriving via
  (Default1 (Product l r))
  instance
    (SymEq1 l, SymEq1 r) => SymEq1 (Product l r)

-- Sum
deriving via
  (Default (Sum l r a))
  instance
    (SymEq (l a), SymEq (r a)) => SymEq (Sum l r a)

deriving via
  (Default1 (Sum l r))
  instance
    (SymEq1 l, SymEq1 r) => SymEq1 (Sum l r)

-- Compose
deriving via
  (Default (Compose f g a))
  instance
    (SymEq (f (g a))) => SymEq (Compose f g a)

instance (SymEq1 f, SymEq1 g) => SymEq1 (Compose f g) where
  liftSymEq f (Compose l) (Compose r) = liftSymEq (liftSymEq f) l r

-- Const
deriving via (Default (Const a b)) instance (SymEq a) => SymEq (Const a b)

deriving via (Default1 (Const a)) instance (SymEq a) => SymEq1 (Const a)

-- Alt
deriving via (Default (Alt f a)) instance (SymEq (f a)) => SymEq (Alt f a)

deriving via (Default1 (Alt f)) instance (SymEq1 f) => SymEq1 (Alt f)

-- Ap
deriving via (Default (Ap f a)) instance (SymEq (f a)) => SymEq (Ap f a)

deriving via (Default1 (Ap f)) instance (SymEq1 f) => SymEq1 (Ap f)

-- Generic
deriving via (Default (U1 p)) instance SymEq (U1 p)

deriving via (Default (V1 p)) instance SymEq (V1 p)

deriving via
  (Default (K1 i c p))
  instance
    (SymEq c) => SymEq (K1 i c p)

deriving via
  (Default (M1 i c f p))
  instance
    (SymEq (f p)) => SymEq (M1 i c f p)

deriving via
  (Default ((f :+: g) p))
  instance
    (SymEq (f p), SymEq (g p)) => SymEq ((f :+: g) p)

deriving via
  (Default ((f :*: g) p))
  instance
    (SymEq (f p), SymEq (g p)) => SymEq ((f :*: g) p)

deriving via
  (Default (Par1 p))
  instance
    (SymEq p) => SymEq (Par1 p)

deriving via
  (Default (Rec1 f p))
  instance
    (SymEq (f p)) => SymEq (Rec1 f p)

deriving via
  (Default ((f :.: g) p))
  instance
    (SymEq (f (g p))) => SymEq ((f :.: g) p)

instance SymEq2 Either where
  liftSymEq2 f _ (Left l) (Left r) = f l r
  liftSymEq2 _ g (Right l) (Right r) = g l r
  liftSymEq2 _ _ _ _ = con False
  {-# INLINE liftSymEq2 #-}

instance SymEq2 (,) where
  liftSymEq2 f g (l1, l2) (r1, r2) = f l1 r1 .&& g l2 r2
  {-# INLINE liftSymEq2 #-}

instance (SymEq a) => SymEq2 ((,,) a) where
  liftSymEq2 f g (l1, l2, l3) (r1, r2, r3) = l1 .== r1 .&& f l2 r2 .&& g l3 r3
  {-# INLINE liftSymEq2 #-}

instance (SymEq a, SymEq b) => SymEq2 ((,,,) a b) where
  liftSymEq2 f g (l1, l2, l3, l4) (r1, r2, r3, r4) =
    l1 .== r1 .&& l2 .== r2 .&& f l3 r3 .&& g l4 r4
  {-# INLINE liftSymEq2 #-}
