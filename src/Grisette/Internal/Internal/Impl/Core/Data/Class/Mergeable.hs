{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- {-# OPTIONS_GHC -ddump-splices -ddump-to-file -ddump-file-prefix=mergeable1 #-}

-- |
-- Module      :   Grisette.Internal.Internal.Impl.Core.Data.Class.Mergeable
-- Copyright   :   (c) Sirui Lu 2021-2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Internal.Internal.Impl.Core.Data.Class.Mergeable () where

import Control.Exception
  ( ArithException
      ( Denormal,
        DivideByZero,
        LossOfPrecision,
        Overflow,
        RatioZeroDenominator,
        Underflow
      ),
  )
import Control.Monad.Cont (ContT (ContT))
import Control.Monad.Except (ExceptT)
import Control.Monad.Identity
  ( Identity,
    IdentityT (IdentityT, runIdentityT),
  )
import qualified Control.Monad.RWS.Lazy as RWSLazy
import qualified Control.Monad.RWS.Strict as RWSStrict
import Control.Monad.Reader (ReaderT (ReaderT, runReaderT))
import qualified Control.Monad.State.Lazy as StateLazy
import qualified Control.Monad.State.Strict as StateStrict
import Control.Monad.Trans.Maybe (MaybeT)
import qualified Control.Monad.Writer.Lazy as WriterLazy
import qualified Control.Monad.Writer.Strict as WriterStrict
import qualified Data.ByteString as B
import Data.Functor.Compose (Compose (Compose, getCompose))
import Data.Functor.Const (Const)
import Data.Functor.Product (Product)
import Data.Functor.Sum (Sum)
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Monoid (Alt, Ap, Endo (Endo, appEndo))
import qualified Data.Monoid as Monoid
import Data.Ord (Down)
import Data.Ratio (Ratio)
import qualified Data.Text as T
import Data.Typeable (Proxy, Typeable)
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.TypeNats (KnownNat, type (+), type (<=))
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
import Grisette.Internal.Core.Data.Class.BitCast (bitCastOrCanonical)
import Grisette.Internal.Core.Data.Class.ITEOp (ITEOp (symIte))
import Grisette.Internal.Internal.Decl.Core.Data.Class.Mergeable
  ( Mergeable (rootStrategy),
    Mergeable1 (liftRootStrategy),
    Mergeable2 (liftRootStrategy2),
    Mergeable3 (liftRootStrategy3),
    MergingStrategy (NoStrategy, SimpleStrategy, SortedStrategy),
    StrategyList (StrategyList),
    buildStrategyList,
    rootStrategy1,
    wrapStrategy,
  )
import Grisette.Internal.SymPrim.AlgReal (AlgReal, AlgRealPoly, RealPoint)
import Grisette.Internal.SymPrim.BV
  ( IntN,
    WordN,
  )
import Grisette.Internal.SymPrim.FP
  ( FP,
    FPRoundingMode,
    NotRepresentableFPError,
    ValidFP,
    withValidFPProofs,
  )
import Grisette.Internal.SymPrim.GeneralFun (type (-->))
import Grisette.Internal.SymPrim.SymAlgReal (SymAlgReal)
import Grisette.Internal.SymPrim.SymBV (SymIntN, SymWordN)
import Grisette.Internal.SymPrim.SymFP (SymFP, SymFPRoundingMode)
import Grisette.Internal.SymPrim.SymGeneralFun (type (-~>))
import Grisette.Internal.SymPrim.SymInteger (SymInteger)
import Grisette.Internal.SymPrim.SymTabularFun (type (=~>))
import Grisette.Internal.SymPrim.TabularFun (type (=->))
import Grisette.Internal.TH.GADT.DeriveGADT (deriveGADT)
import Unsafe.Coerce (unsafeCoerce)

#define CONCRETE_ORD_MERGEABLE(type) \
instance Mergeable type where \
  rootStrategy = \
    let sub = SimpleStrategy $ \_ t _ -> t \
     in SortedStrategy id $ const sub

#define CONCRETE_ORD_MERGEABLE_BV(type) \
instance (KnownNat n, 1 <= n) => Mergeable (type n) where \
  rootStrategy = \
    let sub = SimpleStrategy $ \_ t _ -> t \
     in SortedStrategy id $ const sub

#if 1
CONCRETE_ORD_MERGEABLE(Bool)
CONCRETE_ORD_MERGEABLE(Integer)
CONCRETE_ORD_MERGEABLE(Char)
CONCRETE_ORD_MERGEABLE(Int)
CONCRETE_ORD_MERGEABLE(Int8)
CONCRETE_ORD_MERGEABLE(Int16)
CONCRETE_ORD_MERGEABLE(Int32)
CONCRETE_ORD_MERGEABLE(Int64)
CONCRETE_ORD_MERGEABLE(Word)
CONCRETE_ORD_MERGEABLE(Word8)
CONCRETE_ORD_MERGEABLE(Word16)
CONCRETE_ORD_MERGEABLE(Word32)
CONCRETE_ORD_MERGEABLE(Word64)
CONCRETE_ORD_MERGEABLE(Float)
CONCRETE_ORD_MERGEABLE(Double)
CONCRETE_ORD_MERGEABLE(B.ByteString)
CONCRETE_ORD_MERGEABLE(T.Text)
CONCRETE_ORD_MERGEABLE(FPRoundingMode)
CONCRETE_ORD_MERGEABLE(Monoid.All)
CONCRETE_ORD_MERGEABLE(Monoid.Any)
CONCRETE_ORD_MERGEABLE_BV(WordN)
CONCRETE_ORD_MERGEABLE_BV(IntN)
#endif

instance Mergeable (Proxy a) where
  rootStrategy = SimpleStrategy $ \_ t _ -> t

instance Mergeable1 Proxy where
  liftRootStrategy _ = SimpleStrategy $ \_ t _ -> t
  {-# INLINE liftRootStrategy #-}

instance (Integral a, Typeable a, Show a) => Mergeable (Ratio a) where
  rootStrategy =
    let sub = SimpleStrategy $ \_ t _ -> t
     in SortedStrategy id $ const sub

instance (ValidFP eb sb) => Mergeable (FP eb sb) where
  rootStrategy =
    let sub = SimpleStrategy $ \_ t _ -> t
     in withValidFPProofs @eb @sb
          $ SortedStrategy
            (\fp -> (bitCastOrCanonical fp :: WordN (eb + sb)))
          $ const sub

instance Mergeable (a =-> b) where
  rootStrategy = NoStrategy

instance Mergeable (a --> b) where
  rootStrategy = SimpleStrategy symIte

#define MERGEABLE_SIMPLE(symtype) \
instance Mergeable symtype where \
  rootStrategy = SimpleStrategy symIte

#define MERGEABLE_BV(symtype) \
instance (KnownNat n, 1 <= n) => Mergeable (symtype n) where \
  rootStrategy = SimpleStrategy symIte

#define MERGEABLE_FUN(cop, op, consop) \
instance Mergeable (op sa sb) where \
  rootStrategy = SimpleStrategy symIte

#if 1
MERGEABLE_SIMPLE(SymInteger)
MERGEABLE_SIMPLE(SymFPRoundingMode)
MERGEABLE_SIMPLE(SymAlgReal)
MERGEABLE_BV(SymIntN)
MERGEABLE_BV(SymWordN)
MERGEABLE_FUN((=->), (=~>), SymTabularFun)
MERGEABLE_FUN((-->), (-~>), SymGeneralFun)
#endif

instance (ValidFP eb sb) => Mergeable (SymFP eb sb) where
  rootStrategy = SimpleStrategy symIte

-- function
instance (Mergeable b) => Mergeable (a -> b) where
  rootStrategy = case rootStrategy @b of
    SimpleStrategy m -> SimpleStrategy $ \cond t f v -> m cond (t v) (f v)
    _ -> NoStrategy
  {-# INLINE rootStrategy #-}

instance Mergeable1 ((->) a) where
  liftRootStrategy ms = case ms of
    SimpleStrategy m -> SimpleStrategy $ \cond t f v -> m cond (t v) (f v)
    _ -> NoStrategy
  {-# INLINE liftRootStrategy #-}

instance Mergeable2 ((->)) where
  liftRootStrategy2 _ ms = case ms of
    SimpleStrategy m -> SimpleStrategy $ \cond t f v -> m cond (t v) (f v)
    _ -> NoStrategy
  {-# INLINE liftRootStrategy2 #-}

-- List

instance (Mergeable a) => Mergeable [a] where
  rootStrategy = case rootStrategy :: MergingStrategy a of
    SimpleStrategy m ->
      SortedStrategy length $ \_ ->
        SimpleStrategy $ \cond -> zipWith (m cond)
    NoStrategy ->
      SortedStrategy length $ const NoStrategy
    _ -> SortedStrategy length $ \_ ->
      SortedStrategy (buildStrategyList rootStrategy) $
        \(StrategyList _ strategies) ->
          let s :: [MergingStrategy a] = unsafeCoerce strategies
              allSimple = all (\case SimpleStrategy _ -> True; _ -> False) s
           in if allSimple
                then SimpleStrategy $ \cond l r ->
                  ( \case
                      (SimpleStrategy f, l1, r1) -> f cond l1 r1
                      _ -> error "impossible"
                  )
                    <$> zip3 s l r
                else NoStrategy
  {-# INLINE rootStrategy #-}

instance Mergeable1 [] where
  liftRootStrategy (ms :: MergingStrategy a) = case ms of
    SimpleStrategy m ->
      SortedStrategy length $ \_ ->
        SimpleStrategy $ \cond -> zipWith (m cond)
    NoStrategy ->
      SortedStrategy length $ const NoStrategy
    _ -> SortedStrategy length $ \_ ->
      SortedStrategy (buildStrategyList ms) $ \(StrategyList _ strategies) ->
        let s :: [MergingStrategy a] = unsafeCoerce strategies
            allSimple = all (\case SimpleStrategy _ -> True; _ -> False) s
         in if allSimple
              then SimpleStrategy $ \cond l r ->
                ( \case
                    (SimpleStrategy f, l1, r1) -> f cond l1 r1
                    _ -> error "impossible"
                )
                  <$> zip3 s l r
              else NoStrategy
  {-# INLINE liftRootStrategy #-}

instance Mergeable () where
  rootStrategy = SimpleStrategy $ \_ t _ -> t

deriveGADT
  [ ''Either,
    ''(,)
  ]
  [''Mergeable, ''Mergeable1, ''Mergeable2]

deriveGADT
  [ ''(,,),
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
  [''Mergeable, ''Mergeable1, ''Mergeable2, ''Mergeable3]

deriveGADT
  [ ''Maybe,
    ''Identity,
    ''Monoid.Dual,
    ''Monoid.Sum,
    ''Monoid.Product,
    ''Monoid.First,
    ''Monoid.Last,
    ''Down,
    ''MaybeT,
    ''ExceptT,
    ''WriterLazy.WriterT,
    ''WriterStrict.WriterT,
    ''StateLazy.StateT,
    ''StateStrict.StateT
  ]
  [''Mergeable, ''Mergeable1]

deriveGADT
  [ ''AssertionError,
    ''VerificationConditions,
    ''NotRepresentableFPError,
    ''AlgRealPoly,
    ''RealPoint,
    ''AlgReal
  ]
  [''Mergeable]

-- Reader -- separately implemented as we don't need Mergeable s
instance
  (Mergeable a, Mergeable1 m) =>
  Mergeable (ReaderT s m a)
  where
  rootStrategy = rootStrategy1
  {-# INLINE rootStrategy #-}

instance (Mergeable1 m) => Mergeable1 (ReaderT s m) where
  liftRootStrategy m =
    wrapStrategy
      (liftRootStrategy (liftRootStrategy m))
      ReaderT
      runReaderT
  {-# INLINE liftRootStrategy #-}

-- IdentityT
instance (Mergeable1 m, Mergeable a) => Mergeable (IdentityT m a) where
  rootStrategy = rootStrategy1
  {-# INLINE rootStrategy #-}

instance (Mergeable1 m) => Mergeable1 (IdentityT m) where
  liftRootStrategy m = wrapStrategy (liftRootStrategy m) IdentityT runIdentityT
  {-# INLINE liftRootStrategy #-}

-- ContT -- separately implemented as we don't need Mergeable a
instance (Mergeable1 m, Mergeable r) => Mergeable (ContT r m a) where
  rootStrategy =
    wrapStrategy
      (liftRootStrategy rootStrategy1)
      ContT
      (\(ContT v) -> v)
  {-# INLINE rootStrategy #-}

instance (Mergeable1 m, Mergeable r) => Mergeable1 (ContT r m) where
  liftRootStrategy _ =
    wrapStrategy
      (liftRootStrategy rootStrategy1)
      ContT
      (\(ContT v) -> v)
  {-# INLINE liftRootStrategy #-}

-- RWS -- separately implemented as we don't need Mergeable r
instance
  (Mergeable s, Mergeable w, Mergeable a, Mergeable1 m) =>
  Mergeable (RWSLazy.RWST r w s m a)
  where
  rootStrategy = rootStrategy1
  {-# INLINE rootStrategy #-}

instance
  (Mergeable s, Mergeable w, Mergeable1 m) =>
  Mergeable1 (RWSLazy.RWST r w s m)
  where
  liftRootStrategy m =
    wrapStrategy
      ( liftRootStrategy . liftRootStrategy . liftRootStrategy $
          liftRootStrategy3 m rootStrategy rootStrategy
      )
      RWSLazy.RWST
      (\(RWSLazy.RWST rws) -> rws)
  {-# INLINE liftRootStrategy #-}

instance
  (Mergeable s, Mergeable w, Mergeable a, Mergeable1 m) =>
  Mergeable (RWSStrict.RWST r w s m a)
  where
  rootStrategy = rootStrategy1
  {-# INLINE rootStrategy #-}

instance
  (Mergeable s, Mergeable w, Mergeable1 m) =>
  Mergeable1 (RWSStrict.RWST r w s m)
  where
  liftRootStrategy m =
    wrapStrategy
      ( liftRootStrategy . liftRootStrategy . liftRootStrategy $
          liftRootStrategy3 m rootStrategy rootStrategy
      )
      RWSStrict.RWST
      (\(RWSStrict.RWST rws) -> rws)
  {-# INLINE liftRootStrategy #-}

-- Product
deriving via
  (Default (Product l r a))
  instance
    (Mergeable (l a), Mergeable (r a)) => Mergeable (Product l r a)

deriving via
  (Default1 (Product l r))
  instance
    (Mergeable1 l, Mergeable1 r) => Mergeable1 (Product l r)

-- Sum
deriving via
  (Default (Sum l r a))
  instance
    (Mergeable (l a), Mergeable (r a)) => Mergeable (Sum l r a)

deriving via
  (Default1 (Sum l r))
  instance
    (Mergeable1 l, Mergeable1 r) => Mergeable1 (Sum l r)

-- Compose
deriving via
  (Default (Compose f g a))
  instance
    (Mergeable (f (g a))) => Mergeable (Compose f g a)

instance (Mergeable1 f, Mergeable1 g) => Mergeable1 (Compose f g) where
  liftRootStrategy s =
    wrapStrategy (liftRootStrategy (liftRootStrategy s)) Compose getCompose
  {-# INLINE liftRootStrategy #-}

-- Const
deriving via
  (Default (Const a b))
  instance
    (Mergeable a) => Mergeable (Const a b)

deriving via
  (Default1 (Const a))
  instance
    (Mergeable a) => Mergeable1 (Const a)

-- Alt
deriving via
  (Default (Alt f a))
  instance
    (Mergeable (f a)) => Mergeable (Alt f a)

deriving via
  (Default1 (Alt f))
  instance
    (Mergeable1 f) => Mergeable1 (Alt f)

-- Ap
deriving via
  (Default (Ap f a))
  instance
    (Mergeable (f a)) => Mergeable (Ap f a)

deriving via
  (Default1 (Ap f))
  instance
    (Mergeable1 f) => Mergeable1 (Ap f)

-- Endo
instance (Mergeable a) => Mergeable (Endo a) where
  rootStrategy = rootStrategy1
  {-# INLINE rootStrategy #-}

instance Mergeable1 Endo where
  liftRootStrategy strategy =
    wrapStrategy (liftRootStrategy strategy) Endo appEndo

-- Generic
deriving via (Default (U1 p)) instance Mergeable (U1 p)

deriving via (Default (V1 p)) instance Mergeable (V1 p)

deriving via
  (Default (K1 i c p))
  instance
    (Mergeable c) => Mergeable (K1 i c p)

deriving via
  (Default (M1 i c f p))
  instance
    (Mergeable (f p)) => Mergeable (M1 i c f p)

deriving via
  (Default ((f :+: g) p))
  instance
    (Mergeable (f p), Mergeable (g p)) => Mergeable ((f :+: g) p)

deriving via
  (Default ((f :*: g) p))
  instance
    (Mergeable (f p), Mergeable (g p)) => Mergeable ((f :*: g) p)

deriving via
  (Default (Par1 p))
  instance
    (Mergeable p) => Mergeable (Par1 p)

deriving via
  (Default (Rec1 f p))
  instance
    (Mergeable (f p)) => Mergeable (Rec1 f p)

deriving via
  (Default ((f :.: g) p))
  instance
    (Mergeable (f (g p))) => Mergeable ((f :.: g) p)

-- Exceptions
instance Mergeable ArithException where
  rootStrategy =
    SortedStrategy
      ( \case
          Overflow -> 0 :: Int
          Underflow -> 1 :: Int
          LossOfPrecision -> 2 :: Int
          DivideByZero -> 3 :: Int
          Denormal -> 4 :: Int
          RatioZeroDenominator -> 5 :: Int
      )
      (const $ SimpleStrategy $ \_ l _ -> l)
