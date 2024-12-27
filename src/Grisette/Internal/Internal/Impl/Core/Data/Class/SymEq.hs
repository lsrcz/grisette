{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -ddump-splices -ddump-to-file -ddump-file-prefix=symeq #-}

module Grisette.Internal.Internal.Impl.Core.Data.Class.SymEq () where

import Control.Monad.Except (ExceptT)
import Control.Monad.Identity
  ( Identity,
    IdentityT (IdentityT),
  )
import Control.Monad.Trans.Maybe (MaybeT)
import qualified Control.Monad.Writer.Lazy as WriterLazy
import qualified Control.Monad.Writer.Strict as WriterStrict
import qualified Data.ByteString as B
import Data.Functor.Compose (Compose (Compose))
import Data.Functor.Const (Const)
import Data.Functor.Product (Product)
import Data.Functor.Sum (Sum)
import Data.Int (Int16, Int32, Int64, Int8)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Monoid (Alt, Ap)
import qualified Data.Monoid as Monoid
import Data.Ord (Down)
import Data.Proxy (Proxy)
import Data.Ratio (Ratio, denominator, numerator)
import qualified Data.Text as T
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.TypeNats (KnownNat, type (<=))
import Generics.Deriving
  ( Default (Default),
    Default1 (Default1),
    K1 (K1),
    M1 (M1),
    Par1 (Par1),
    Rec1 (Rec1),
    U1,
    V1,
    (:.:) (Comp1),
    type (:*:),
    type (:+:),
  )
import Grisette.Internal.Core.Control.Exception
  ( AssertionError,
    VerificationConditions,
  )
import Grisette.Internal.Core.Data.Class.LogicalOp (LogicalOp ((.&&)))
import Grisette.Internal.Core.Data.Class.Solvable (Solvable (con))
import Grisette.Internal.Internal.Decl.Core.Data.Class.SymEq
  ( SymEq (symDistinct, (./=), (.==)),
    SymEq1 (liftSymEq),
    SymEq2,
    symEq1,
  )
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
import Grisette.Internal.TH.GADT.DeriveGADT (deriveGADT)

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

instance SymEq (Proxy a) where
  _ .== _ = con True
  {-# INLINE (.==) #-}

instance SymEq1 Proxy where
  liftSymEq _ _ _ = con True
  {-# INLINE liftSymEq #-}

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

deriveGADT
  [ ''(),
    ''AssertionError,
    ''VerificationConditions,
    ''NotRepresentableFPError
  ]
  [''SymEq]

deriveGADT
  [ ''Either,
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
    ''(,,,,,,,,,,,,,,)
  ]
  [''SymEq, ''SymEq1, ''SymEq2]

deriveGADT
  [ ''[],
    ''Maybe,
    ''Identity,
    ''Monoid.Dual,
    ''Monoid.First,
    ''Monoid.Last,
    ''Monoid.Sum,
    ''Monoid.Product,
    ''Down,
    ''ExceptT,
    ''MaybeT,
    ''WriterLazy.WriterT,
    ''WriterStrict.WriterT
  ]
  [''SymEq, ''SymEq1]

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

-- instance SymEq2 Either where
--   liftSymEq2 f _ (Left l) (Left r) = f l r
--   liftSymEq2 _ g (Right l) (Right r) = g l r
--   liftSymEq2 _ _ _ _ = con False
--   {-# INLINE liftSymEq2 #-}

{-
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
-}