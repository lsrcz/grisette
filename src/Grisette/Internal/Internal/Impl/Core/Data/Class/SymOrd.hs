{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      :   Grisette.Internal.Internal.Impl.Core.Data.Class.SymOrd
-- Copyright   :   (c) Sirui Lu 2021-2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Internal.Internal.Impl.Core.Data.Class.SymOrd () where

import Control.Exception (ArithException)
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
import Data.Monoid (Alt, Ap)
import qualified Data.Monoid as Monoid
import Data.Ord (Down (Down))
import Data.Proxy (Proxy)
import Data.Ratio (Ratio, denominator, numerator)
import qualified Data.Text as T
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.TypeLits (KnownNat, type (<=))
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
import Grisette.Internal.Core.Control.Monad.Union (Union)
import Grisette.Internal.Core.Data.Class.AsKey (AsKey (AsKey), AsKey1 (AsKey1))
import Grisette.Internal.Core.Data.Class.LogicalOp
  ( LogicalOp (symNot, (.&&), (.||)),
  )
import Grisette.Internal.Core.Data.Class.SimpleMergeable
  ( mrgIf,
  )
import Grisette.Internal.Core.Data.Class.Solvable (Solvable (con))
import Grisette.Internal.Core.Data.Class.SymEq
  ( SymEq ((.==)),
  )
import Grisette.Internal.Core.Data.Class.TryMerge
  ( mrgSingle,
    tryMerge,
  )
import Grisette.Internal.Core.Data.Class.UnionView
  ( simpleMerge,
  )
import Grisette.Internal.Internal.Decl.Core.Data.Class.SymOrd
  ( SymOrd (symCompare, (.<), (.<=), (.>), (.>=)),
    SymOrd1 (liftSymCompare),
    SymOrd2,
    symCompare1,
  )
import Grisette.Internal.SymPrim.AlgReal (AlgReal)
import Grisette.Internal.SymPrim.BV (IntN, WordN)
import Grisette.Internal.SymPrim.FP (FP, FPRoundingMode, NotRepresentableFPError, ValidFP)
import Grisette.Internal.SymPrim.Prim.Term
  ( PEvalOrdTerm
      ( pevalLeOrdTerm,
        pevalLtOrdTerm
      ),
    pevalGeOrdTerm,
    pevalGtOrdTerm,
  )
import Grisette.Internal.SymPrim.SymAlgReal (SymAlgReal (SymAlgReal))
import Grisette.Internal.SymPrim.SymBV
  ( SymIntN (SymIntN),
    SymWordN (SymWordN),
  )
import Grisette.Internal.SymPrim.SymBool (SymBool (SymBool))
import Grisette.Internal.SymPrim.SymFP (SymFP (SymFP))
import Grisette.Internal.SymPrim.SymInteger (SymInteger (SymInteger))
import Grisette.Internal.TH.Derivation.Derive (derive)

#define CONCRETE_SORD(type) \
instance SymOrd type where \
  l .<= r = con $ l <= r; \
  l .< r = con $ l < r; \
  l .>= r = con $ l >= r; \
  l .> r = con $ l > r; \
  symCompare l r = mrgSingle $ compare l r; \
  {-# INLINE (.<=) #-}; \
  {-# INLINE (.<) #-}; \
  {-# INLINE (.>=) #-}; \
  {-# INLINE (.>) #-}; \
  {-# INLINE symCompare #-}

#define CONCRETE_SORD_BV(type) \
instance (KnownNat n, 1 <= n) => SymOrd (type n) where \
  l .<= r = con $ l <= r; \
  l .< r = con $ l < r; \
  l .>= r = con $ l >= r; \
  l .> r = con $ l > r; \
  symCompare l r = mrgSingle $ compare l r; \
  {-# INLINE (.<=) #-}; \
  {-# INLINE (.<) #-}; \
  {-# INLINE (.>=) #-}; \
  {-# INLINE (.>) #-}; \
  {-# INLINE symCompare #-}

#if 1
CONCRETE_SORD(Bool)
CONCRETE_SORD(Integer)
CONCRETE_SORD(Char)
CONCRETE_SORD(Int)
CONCRETE_SORD(Int8)
CONCRETE_SORD(Int16)
CONCRETE_SORD(Int32)
CONCRETE_SORD(Int64)
CONCRETE_SORD(Word)
CONCRETE_SORD(Word8)
CONCRETE_SORD(Word16)
CONCRETE_SORD(Word32)
CONCRETE_SORD(Word64)
CONCRETE_SORD(Float)
CONCRETE_SORD(Double)
CONCRETE_SORD(B.ByteString)
CONCRETE_SORD(T.Text)
CONCRETE_SORD(FPRoundingMode)
CONCRETE_SORD(Monoid.All)
CONCRETE_SORD(Monoid.Any)
CONCRETE_SORD(Ordering)
CONCRETE_SORD_BV(WordN)
CONCRETE_SORD_BV(IntN)
CONCRETE_SORD(AlgReal)
#endif

instance SymOrd (Proxy a) where
  _ .<= _ = con True
  {-# INLINE (.<=) #-}
  _ .< _ = con False
  {-# INLINE (.<) #-}
  _ .>= _ = con True
  {-# INLINE (.>=) #-}
  _ .> _ = con False
  {-# INLINE (.>) #-}
  symCompare _ _ = mrgSingle EQ
  {-# INLINE symCompare #-}

instance SymOrd1 Proxy where
  liftSymCompare _ _ _ = mrgSingle EQ
  {-# INLINE liftSymCompare #-}

instance (SymOrd a, Integral a) => SymOrd (Ratio a) where
  a .<= b = numerator a * denominator b .<= numerator b * denominator a
  {-# INLINE (.<=) #-}
  a .< b = numerator a * denominator b .< numerator b * denominator a
  {-# INLINE (.<) #-}

instance (ValidFP eb sb) => SymOrd (FP eb sb) where
  l .<= r = con $ l <= r
  {-# INLINE (.<=) #-}
  l .< r = con $ l < r
  {-# INLINE (.<) #-}
  l .>= r = con $ l >= r
  {-# INLINE (.>=) #-}
  l .> r = con $ l > r
  {-# INLINE (.>) #-}

-- SymOrd
#define SORD_SIMPLE(symtype) \
instance SymOrd symtype where \
  (symtype a) .<= (symtype b) = SymBool $ pevalLeOrdTerm a b; \
  (symtype a) .< (symtype b) = SymBool $ pevalLtOrdTerm a b; \
  (symtype a) .>= (symtype b) = SymBool $ pevalGeOrdTerm a b; \
  (symtype a) .> (symtype b) = SymBool $ pevalGtOrdTerm a b; \
  a `symCompare` b = mrgIf \
    (a .< b) \
    (mrgSingle LT) \
    (mrgIf (a .== b) (mrgSingle EQ) (mrgSingle GT)); \
  {-# INLINE (.<=) #-}; \
  {-# INLINE (.<) #-}; \
  {-# INLINE (.>=) #-}; \
  {-# INLINE (.>) #-}; \
  {-# INLINE symCompare #-}

#define SORD_BV(symtype) \
instance (KnownNat n, 1 <= n) => SymOrd (symtype n) where \
  (symtype a) .<= (symtype b) = SymBool $ pevalLeOrdTerm a b; \
  (symtype a) .< (symtype b) = SymBool $ pevalLtOrdTerm a b; \
  (symtype a) .>= (symtype b) = SymBool $ pevalGeOrdTerm a b; \
  (symtype a) .> (symtype b) = SymBool $ pevalGtOrdTerm a b; \
  a `symCompare` b = mrgIf \
    (a .< b) \
    (mrgSingle LT) \
    (mrgIf (a .== b) (mrgSingle EQ) (mrgSingle GT)); \
  {-# INLINE (.<=) #-}; \
  {-# INLINE (.<) #-}; \
  {-# INLINE (.>=) #-}; \
  {-# INLINE (.>) #-}; \
  {-# INLINE symCompare #-}

instance (ValidFP eb sb) => SymOrd (SymFP eb sb) where
  (SymFP a) .<= (SymFP b) = SymBool $ pevalLeOrdTerm a b
  {-# INLINE (.<=) #-}
  (SymFP a) .< (SymFP b) = SymBool $ pevalLtOrdTerm a b
  {-# INLINE (.<) #-}
  (SymFP a) .>= (SymFP b) = SymBool $ pevalGeOrdTerm a b
  {-# INLINE (.>=) #-}
  (SymFP a) .> (SymFP b) = SymBool $ pevalGtOrdTerm a b
  {-# INLINE (.>) #-}

instance SymOrd SymBool where
  l .<= r = symNot l .|| r
  {-# INLINE (.<=) #-}
  l .< r = symNot l .&& r
  {-# INLINE (.<) #-}
  l .>= r = l .|| symNot r
  {-# INLINE (.>=) #-}
  l .> r = l .&& symNot r
  {-# INLINE (.>) #-}
  symCompare l r =
    mrgIf
      (symNot l .&& r)
      (mrgSingle LT)
      (mrgIf (l .== r) (mrgSingle EQ) (mrgSingle GT))
  {-# INLINE symCompare #-}

#if 1
SORD_SIMPLE(SymInteger)
SORD_SIMPLE(SymAlgReal)
SORD_BV(SymIntN)
SORD_BV(SymWordN)
#endif

-- Union
instance (SymOrd a) => SymOrd (Union a) where
  x .<= y = simpleMerge $ do
    x1 <- x
    y1 <- y
    mrgSingle $ x1 .<= y1
  x .< y = simpleMerge $ do
    x1 <- x
    y1 <- y
    mrgSingle $ x1 .< y1
  x .>= y = simpleMerge $ do
    x1 <- x
    y1 <- y
    mrgSingle $ x1 .>= y1
  x .> y = simpleMerge $ do
    x1 <- x
    y1 <- y
    mrgSingle $ x1 .> y1
  x `symCompare` y = tryMerge $ do
    x1 <- x
    y1 <- y
    x1 `symCompare` y1

instance SymOrd1 Union where
  liftSymCompare f x y = tryMerge $ do
    x1 <- x
    y1 <- y
    f x1 y1

derive
  [ ''(),
    ''AssertionError,
    ''VerificationConditions,
    ''NotRepresentableFPError,
    ''ArithException
  ]
  [''SymOrd]

derive
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
  [''SymOrd, ''SymOrd1, ''SymOrd2]

derive
  [ ''Maybe,
    ''Identity,
    ''Monoid.Dual,
    ''Monoid.First,
    ''Monoid.Last,
    ''Monoid.Sum,
    ''Monoid.Product,
    ''ExceptT,
    ''MaybeT,
    ''WriterLazy.WriterT,
    ''WriterStrict.WriterT
  ]
  [''SymOrd, ''SymOrd1]

symCompareSingleList :: (SymOrd a) => Bool -> Bool -> [a] -> [a] -> SymBool
symCompareSingleList isLess isStrict = go
  where
    go [] [] = con (not isStrict)
    go (x : xs) (y : ys) =
      (if isLess then x .< y else x .> y) .|| (x .== y .&& go xs ys)
    go [] _ = if isLess then con True else con False
    go _ [] = if isLess then con False else con True

symLiftCompareList ::
  (a -> b -> Union Ordering) -> [a] -> [b] -> Union Ordering
symLiftCompareList _ [] [] = mrgSingle EQ
symLiftCompareList f (x : xs) (y : ys) = do
  oxy <- f x y
  case oxy of
    LT -> mrgSingle LT
    EQ -> symLiftCompareList f xs ys
    GT -> mrgSingle GT
symLiftCompareList _ [] _ = mrgSingle LT
symLiftCompareList _ _ [] = mrgSingle GT

-- []
instance (SymOrd a) => SymOrd [a] where
  {-# INLINE (.<=) #-}
  {-# INLINE (.<) #-}
  {-# INLINE symCompare #-}
  {-# INLINE (.>=) #-}
  {-# INLINE (.>) #-}
  (.<=) = symCompareSingleList True False
  (.<) = symCompareSingleList True True
  (.>=) = symCompareSingleList False False
  (.>) = symCompareSingleList False True
  symCompare = symLiftCompareList symCompare

instance SymOrd1 [] where
  liftSymCompare = symLiftCompareList
  {-# INLINE liftSymCompare #-}

-- IdentityT
instance (SymOrd1 m, SymOrd a) => SymOrd (IdentityT m a) where
  symCompare = symCompare1
  {-# INLINE symCompare #-}

instance (SymOrd1 m) => SymOrd1 (IdentityT m) where
  liftSymCompare f (IdentityT l) (IdentityT r) = liftSymCompare f l r
  {-# INLINE liftSymCompare #-}

-- Product
deriving via
  (Default (Product l r a))
  instance
    (SymOrd (l a), SymOrd (r a)) => SymOrd (Product l r a)

deriving via
  (Default1 (Product l r))
  instance
    (SymOrd1 l, SymOrd1 r) => SymOrd1 (Product l r)

-- Sum
deriving via
  (Default (Sum l r a))
  instance
    (SymOrd (l a), SymOrd (r a)) => SymOrd (Sum l r a)

deriving via
  (Default1 (Sum l r))
  instance
    (SymOrd1 l, SymOrd1 r) => SymOrd1 (Sum l r)

-- Compose
deriving via
  (Default (Compose f g a))
  instance
    (SymOrd (f (g a))) => SymOrd (Compose f g a)

instance (SymOrd1 f, SymOrd1 g) => SymOrd1 (Compose f g) where
  liftSymCompare f (Compose l) (Compose r) =
    liftSymCompare (liftSymCompare f) l r

-- Const
deriving via (Default (Const a b)) instance (SymOrd a) => SymOrd (Const a b)

deriving via (Default1 (Const a)) instance (SymOrd a) => SymOrd1 (Const a)

-- Alt
deriving via (Default (Alt f a)) instance (SymOrd (f a)) => SymOrd (Alt f a)

deriving via (Default1 (Alt f)) instance (SymOrd1 f) => SymOrd1 (Alt f)

-- Ap
deriving via (Default (Ap f a)) instance (SymOrd (f a)) => SymOrd (Ap f a)

deriving via (Default1 (Ap f)) instance (SymOrd1 f) => SymOrd1 (Ap f)

-- Generic
deriving via (Default (U1 p)) instance SymOrd (U1 p)

deriving via (Default (V1 p)) instance SymOrd (V1 p)

deriving via
  (Default (K1 i c p))
  instance
    (SymOrd c) => SymOrd (K1 i c p)

deriving via
  (Default (M1 i c f p))
  instance
    (SymOrd (f p)) => SymOrd (M1 i c f p)

deriving via
  (Default ((f :+: g) p))
  instance
    (SymOrd (f p), SymOrd (g p)) => SymOrd ((f :+: g) p)

deriving via
  (Default ((f :*: g) p))
  instance
    (SymOrd (f p), SymOrd (g p)) => SymOrd ((f :*: g) p)

deriving via
  (Default (Par1 p))
  instance
    (SymOrd p) => SymOrd (Par1 p)

deriving via
  (Default (Rec1 f p))
  instance
    (SymOrd (f p)) => SymOrd (Rec1 f p)

deriving via
  (Default ((f :.: g) p))
  instance
    (SymOrd (f (g p))) => SymOrd ((f :.: g) p)

-- Down
instance (SymOrd a) => SymOrd (Down a) where
  symCompare = symCompare1
  {-# INLINE symCompare #-}

instance SymOrd1 Down where
  liftSymCompare comp (Down l) (Down r) = do
    res <- comp l r
    case res of
      LT -> mrgSingle GT
      EQ -> mrgSingle EQ
      GT -> mrgSingle LT
  {-# INLINE liftSymCompare #-}

instance (SymOrd a) => SymOrd (AsKey a) where
  symCompare (AsKey l) (AsKey r) = symCompare l r
  {-# INLINE symCompare #-}

instance (SymOrd1 f, SymOrd a) => SymOrd (AsKey1 f a) where
  symCompare = symCompare1
  {-# INLINE symCompare #-}

instance (SymOrd1 f) => SymOrd1 (AsKey1 f) where
  liftSymCompare f (AsKey1 l) (AsKey1 r) = liftSymCompare f l r
  {-# INLINE liftSymCompare #-}
