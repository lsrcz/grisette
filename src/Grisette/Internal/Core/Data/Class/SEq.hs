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
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      :   Grisette.Internal.Core.Data.Class.Bool
-- Copyright   :   (c) Sirui Lu 2021-2023
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
    SEqArgs (..),
    GSEq (..),
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

class (forall a. (SEq a) => SEq (f a)) => SEq1 f where
  liftSEq :: (a -> b -> SymBool) -> f a -> f b -> SymBool

seq1 :: (SEq a, SEq1 f) => f a -> f a -> SymBool
seq1 = liftSEq (.==)

class (forall a. (SEq a) => SEq1 (f a)) => SEq2 f where
  liftSEq2 ::
    (a -> b -> SymBool) ->
    (c -> d -> SymBool) ->
    f a c ->
    f b d ->
    SymBool

seq2 :: (SEq a, SEq b, SEq2 f) => f a b -> f a b -> SymBool
seq2 = liftSEq2 (.==) (.==)

-- SEq instances
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

-- List
deriving via (Default [a]) instance (SEq a) => SEq [a]

-- Maybe
deriving via (Default (Maybe a)) instance (SEq a) => SEq (Maybe a)

-- Either
deriving via (Default (Either e a)) instance (SEq e, SEq a) => SEq (Either e a)

-- ExceptT
instance (SEq (m (Either e a))) => SEq (ExceptT e m a) where
  (ExceptT a) .== (ExceptT b) = a .== b
  {-# INLINE (.==) #-}

-- MaybeT
instance (SEq (m (Maybe a))) => SEq (MaybeT m a) where
  (MaybeT a) .== (MaybeT b) = a .== b
  {-# INLINE (.==) #-}

-- ()
instance SEq () where
  _ .== _ = con True
  {-# INLINE (.==) #-}

-- (,)
deriving via (Default (a, b)) instance (SEq a, SEq b) => SEq (a, b)

-- (,,)
deriving via (Default (a, b, c)) instance (SEq a, SEq b, SEq c) => SEq (a, b, c)

-- (,,,)
deriving via
  (Default (a, b, c, d))
  instance
    (SEq a, SEq b, SEq c, SEq d) =>
    SEq (a, b, c, d)

-- (,,,,)
deriving via
  (Default (a, b, c, d, e))
  instance
    (SEq a, SEq b, SEq c, SEq d, SEq e) =>
    SEq (a, b, c, d, e)

-- (,,,,,)
deriving via
  (Default (a, b, c, d, e, f))
  instance
    (SEq a, SEq b, SEq c, SEq d, SEq e, SEq f) =>
    SEq (a, b, c, d, e, f)

-- (,,,,,,)
deriving via
  (Default (a, b, c, d, e, f, g))
  instance
    (SEq a, SEq b, SEq c, SEq d, SEq e, SEq f, SEq g) =>
    SEq (a, b, c, d, e, f, g)

-- (,,,,,,,)
deriving via
  (Default (a, b, c, d, e, f, g, h))
  instance
    (SEq a, SEq b, SEq c, SEq d, SEq e, SEq f, SEq g, SEq h) =>
    SEq (a, b, c, d, e, f, g, h)

-- Sum
deriving via
  (Default (Sum f g a))
  instance
    (SEq (f a), SEq (g a)) => SEq (Sum f g a)

-- Writer
instance (SEq (m (a, s))) => SEq (WriterLazy.WriterT s m a) where
  (WriterLazy.WriterT l) .== (WriterLazy.WriterT r) = l .== r
  {-# INLINE (.==) #-}

instance (SEq (m (a, s))) => SEq (WriterStrict.WriterT s m a) where
  (WriterStrict.WriterT l) .== (WriterStrict.WriterT r) = l .== r
  {-# INLINE (.==) #-}

-- Identity
instance (SEq a) => SEq (Identity a) where
  (Identity l) .== (Identity r) = l .== r
  {-# INLINE (.==) #-}

-- IdentityT
instance (SEq (m a)) => SEq (IdentityT m a) where
  (IdentityT l) .== (IdentityT r) = l .== r
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

-- Exceptions
deriving via (Default AssertionError) instance SEq AssertionError

deriving via (Default VerificationConditions) instance SEq VerificationConditions

data family SEqArgs arity a b :: Type

data instance SEqArgs Arity0 _ _ = SEqArgs0

newtype instance SEqArgs Arity1 a b = SEqArgs1 (a -> b -> SymBool)

-- | Auxiliary class for 'SEq' instance derivation
class GSEq arity f where
  -- | Auxiliary function for '(..==) derivation
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
  Default l .== Default r = gseq SEqArgs0 (from l) (from r)
  {-# INLINE (.==) #-}

instance (Generic1 f, GSEq Arity1 (Rep1 f), SEq a) => SEq (Default1 f a) where
  (.==) = seq1
  {-# INLINE (.==) #-}

instance (Generic1 f, GSEq Arity1 (Rep1 f)) => SEq1 (Default1 f) where
  liftSEq f (Default1 l) (Default1 r) = gseq (SEqArgs1 f) (from1 l) (from1 r)
  {-# INLINE liftSEq #-}
