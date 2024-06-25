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
-- Module      :   Grisette.Internal.Core.Data.Class.SEq
-- Copyright   :   (c) Sirui Lu 2021-2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Internal.Core.Data.Class.SEq
  ( -- * Symbolic equality
    SEq (..),
    SEq1 (..),
    seq1,
    SEq2 (..),
    seq2,

    -- * Generic 'SEq'
    SEqArgs (..),
    GSEq (..),
    genericSEq,
    genericLiftSEq,
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
import Grisette.Internal.SymPrim.BV (IntN, WordN)
import Grisette.Internal.SymPrim.FP (FP, FPRoundingMode, ValidFP)
import Grisette.Internal.SymPrim.Prim.Term (pevalEqTerm)
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

-- $setup
-- >>> import Grisette.Core
-- >>> import Grisette.SymPrim
-- >>> :set -XDataKinds
-- >>> :set -XBinaryLiterals
-- >>> :set -XFlexibleContexts
-- >>> :set -XFlexibleInstances
-- >>> :set -XFunctionalDependencies

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
-- >>> a ./= b
-- (! (= a b))
-- >>> a ./= b
-- (! (= a b))
--
-- __Note:__ This type class can be derived for algebraic data types.
-- You may need the @DerivingVia@ and @DerivingStrategies@ extensions.
--
-- > data X = ... deriving Generic deriving SEq via (Default X)
class SEq a where
  (.==) :: a -> a -> SymBool
  a .== b = symNot $ a ./= b
  {-# INLINE (.==) #-}
  infix 4 .==

  (./=) :: a -> a -> SymBool
  a ./= b = symNot $ a .== b
  {-# INLINE (./=) #-}
  infix 4 ./=
  {-# MINIMAL (.==) | (./=) #-}

-- | Lifting of the 'SEq' class to unary type constructors.
--
-- Any instance should be subject to the following law that canonicity is
-- preserved:
--
-- @liftSEq (.==)@ should be equivalent to @(.==)@, under the symbolic
-- semantics.
--
-- This class therefore represents the generalization of 'SEq' by decomposing
-- its main method into a canonical lifting on a canonical inner method, so that
-- the lifting can be reused for other arguments than the canonical one.
class (forall a. (SEq a) => SEq (f a)) => SEq1 f where
  -- | Lift a symbolic equality test through the type constructor.
  --
  -- The function will usually be applied to an symbolic equality function, but
  -- the more general type ensures that the implementation uses it to compare
  -- elements of the first container with elements of the second.
  liftSEq :: (a -> b -> SymBool) -> f a -> f b -> SymBool

-- | Lift the standard @('.==')@ function through the type constructor.
seq1 :: (SEq a, SEq1 f) => f a -> f a -> SymBool
seq1 = liftSEq (.==)

-- | Lifting of the 'SEq' class to binary type constructors.
class (forall a. (SEq a) => SEq1 (f a)) => SEq2 f where
  -- | Lift symbolic equality tests through the type constructor.
  --
  -- The function will usually be applied to an symbolic equality function, but
  -- the more general type ensures that the implementation uses it to compare
  -- elements of the first container with elements of the second.
  liftSEq2 ::
    (a -> b -> SymBool) ->
    (c -> d -> SymBool) ->
    f a c ->
    f b d ->
    SymBool

-- | Lift the standard @('.==')@ function through the type constructor.
seq2 :: (SEq a, SEq b, SEq2 f) => f a b -> f a b -> SymBool
seq2 = liftSEq2 (.==) (.==)

-- Derivations

-- | The arguments to the generic equality function.
data family SEqArgs arity a b :: Type

data instance SEqArgs Arity0 _ _ = SEqArgs0

newtype instance SEqArgs Arity1 a b = SEqArgs1 (a -> b -> SymBool)

-- | The class of types that can be generically compared for symbolic equality.
class GSEq arity f where
  gseq :: SEqArgs arity a b -> f a -> f b -> SymBool

instance GSEq arity V1 where
  gseq _ _ _ = con True
  {-# INLINE gseq #-}

instance GSEq arity U1 where
  gseq _ _ _ = con True
  {-# INLINE gseq #-}

instance (GSEq arity a, GSEq arity b) => GSEq arity (a :*: b) where
  gseq args (a1 :*: b1) (a2 :*: b2) = gseq args a1 a2 .&& gseq args b1 b2
  {-# INLINE gseq #-}

instance (GSEq arity a, GSEq arity b) => GSEq arity (a :+: b) where
  gseq args (L1 a1) (L1 a2) = gseq args a1 a2
  gseq args (R1 b1) (R1 b2) = gseq args b1 b2
  gseq _ _ _ = con False
  {-# INLINE gseq #-}

instance (GSEq arity a) => GSEq arity (M1 i c a) where
  gseq args (M1 a1) (M1 a2) = gseq args a1 a2
  {-# INLINE gseq #-}

instance (SEq a) => GSEq arity (K1 i a) where
  gseq _ (K1 a) (K1 b) = a .== b
  {-# INLINE gseq #-}

instance GSEq Arity1 Par1 where
  gseq (SEqArgs1 e) (Par1 a) (Par1 b) = e a b
  {-# INLINE gseq #-}

instance (SEq1 f) => GSEq Arity1 (Rec1 f) where
  gseq (SEqArgs1 e) (Rec1 a) (Rec1 b) = liftSEq e a b
  {-# INLINE gseq #-}

instance (SEq1 f, GSEq Arity1 g) => GSEq Arity1 (f :.: g) where
  gseq targs (Comp1 a) (Comp1 b) = liftSEq (gseq targs) a b
  {-# INLINE gseq #-}

instance (Generic a, GSEq Arity0 (Rep a)) => SEq (Default a) where
  Default l .== Default r = genericSEq l r
  {-# INLINE (.==) #-}

-- | Generic @('.==')@ function.
genericSEq :: (Generic a, GSEq Arity0 (Rep a)) => a -> a -> SymBool
genericSEq l r = gseq SEqArgs0 (from l) (from r)
{-# INLINE genericSEq #-}

instance (Generic1 f, GSEq Arity1 (Rep1 f), SEq a) => SEq (Default1 f a) where
  (.==) = seq1
  {-# INLINE (.==) #-}

instance (Generic1 f, GSEq Arity1 (Rep1 f)) => SEq1 (Default1 f) where
  liftSEq f (Default1 l) (Default1 r) = genericLiftSEq f l r
  {-# INLINE liftSEq #-}

-- | Generic 'liftSEq' function.
genericLiftSEq ::
  (Generic1 f, GSEq Arity1 (Rep1 f)) =>
  (a -> b -> SymBool) ->
  f a ->
  f b ->
  SymBool
genericLiftSEq f l r = gseq (SEqArgs1 f) (from1 l) (from1 r)
{-# INLINE genericLiftSEq #-}

-- Instances
deriveBuiltins
  (ViaDefault ''SEq)
  [''SEq]
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
  (ViaDefault1 ''SEq1)
  [''SEq, ''SEq1]
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
instance (SEq1 m, SEq e, SEq a) => SEq (ExceptT e m a) where
  (.==) = seq1
  {-# INLINE (.==) #-}

instance (SEq1 m, SEq e) => SEq1 (ExceptT e m) where
  liftSEq f (ExceptT l) (ExceptT r) = liftSEq (liftSEq f) l r
  {-# INLINE liftSEq #-}

-- MaybeT
instance (SEq1 m, SEq a) => SEq (MaybeT m a) where
  (.==) = seq1
  {-# INLINE (.==) #-}

instance (SEq1 m) => SEq1 (MaybeT m) where
  liftSEq f (MaybeT l) (MaybeT r) = liftSEq (liftSEq f) l r
  {-# INLINE liftSEq #-}

-- Writer
instance (SEq1 m, SEq w, SEq a) => SEq (WriterLazy.WriterT w m a) where
  (.==) = seq1
  {-# INLINE (.==) #-}

instance (SEq1 m, SEq w) => SEq1 (WriterLazy.WriterT w m) where
  liftSEq f (WriterLazy.WriterT l) (WriterLazy.WriterT r) =
    liftSEq (liftSEq2 f (.==)) l r
  {-# INLINE liftSEq #-}

instance (SEq1 m, SEq w, SEq a) => SEq (WriterStrict.WriterT w m a) where
  (.==) = seq1
  {-# INLINE (.==) #-}

instance (SEq1 m, SEq w) => SEq1 (WriterStrict.WriterT w m) where
  liftSEq f (WriterStrict.WriterT l) (WriterStrict.WriterT r) =
    liftSEq (liftSEq2 f (.==)) l r
  {-# INLINE liftSEq #-}

-- Sum
deriving via
  (Default (Sum f g a))
  instance
    (SEq (f a), SEq (g a)) => SEq (Sum f g a)

deriving via
  (Default1 (Sum f g))
  instance
    (SEq1 f, SEq1 g) => SEq1 (Sum f g)

-- IdentityT
instance (SEq1 m, SEq a) => SEq (IdentityT m a) where
  (.==) = seq1
  {-# INLINE (.==) #-}

instance (SEq1 m) => SEq1 (IdentityT m) where
  liftSEq f (IdentityT l) (IdentityT r) = liftSEq f l r
  {-# INLINE liftSEq #-}

instance SEq2 Either where
  liftSEq2 f _ (Left l) (Left r) = f l r
  liftSEq2 _ g (Right l) (Right r) = g l r
  liftSEq2 _ _ _ _ = con False
  {-# INLINE liftSEq2 #-}

instance SEq2 (,) where
  liftSEq2 f g (l1, l2) (r1, r2) = f l1 r1 .&& g l2 r2
  {-# INLINE liftSEq2 #-}

instance (SEq a) => SEq2 ((,,) a) where
  liftSEq2 f g (l1, l2, l3) (r1, r2, r3) = l1 .== r1 .&& f l2 r2 .&& g l3 r3
  {-# INLINE liftSEq2 #-}

instance (SEq a, SEq b) => SEq2 ((,,,) a b) where
  liftSEq2 f g (l1, l2, l3, l4) (r1, r2, r3, r4) =
    l1 .== r1 .&& l2 .== r2 .&& f l3 r3 .&& g l4 r4
  {-# INLINE liftSEq2 #-}

#define CONCRETE_SEQ(type) \
instance SEq type where \
  l .== r = con $ l == r; \
  {-# INLINE (.==) #-}

#define CONCRETE_SEQ_BV(type) \
instance (KnownNat n, 1 <= n) => SEq (type n) where \
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
CONCRETE_SEQ_BV(WordN)
CONCRETE_SEQ_BV(IntN)
#endif

instance (ValidFP eb sb) => SEq (FP eb sb) where
  l .== r = con $ l == r
  {-# INLINE (.==) #-}

-- Symbolic types
#define SEQ_SIMPLE(symtype) \
instance SEq symtype where \
  (symtype l) .== (symtype r) = SymBool $ pevalEqTerm l r; \
  {-# INLINE (.==) #-}

#define SEQ_BV(symtype) \
instance (KnownNat n, 1 <= n) => SEq (symtype n) where \
  (symtype l) .== (symtype r) = SymBool $ pevalEqTerm l r; \
  {-# INLINE (.==) #-}

#if 1
SEQ_SIMPLE(SymBool)
SEQ_SIMPLE(SymInteger)
SEQ_SIMPLE(SymFPRoundingMode)
SEQ_BV(SymIntN)
SEQ_BV(SymWordN)
#endif

instance (ValidFP eb sb) => SEq (SymFP eb sb) where
  (SymFP l) .== (SymFP r) = SymBool $ pevalEqTerm l r
  {-# INLINE (.==) #-}
