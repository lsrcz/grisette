{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
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
-- Module      :   Grisette.Internal.Internal.Impl.SymPrim.AllSyms
-- Copyright   :   (c) Sirui Lu 2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Internal.Internal.Impl.SymPrim.AllSyms () where

import Control.Monad.Except (ExceptT)
import Control.Monad.Identity
  ( Identity,
    IdentityT (IdentityT),
  )
import Control.Monad.Trans.Maybe (MaybeT)
import qualified Control.Monad.Writer.Lazy as WriterLazy
import qualified Control.Monad.Writer.Strict as WriterStrict
import qualified Data.ByteString as B
import Data.Functor.Sum (Sum)
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Proxy (Proxy)
import Data.Ratio (Ratio, denominator, numerator)
import qualified Data.Text as T
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.TypeNats (KnownNat, type (<=))
import Generics.Deriving
  ( Default (Default),
    Default1 (Default1),
  )
import Grisette.Internal.Core.Control.Exception
  ( AssertionError,
    VerificationConditions,
  )
import Grisette.Internal.Internal.Decl.SymPrim.AllSyms
  ( AllSyms (allSymsS),
    AllSyms1 (liftAllSymsS),
    AllSyms2,
    allSymsS1,
  )
import Grisette.Internal.SymPrim.AlgReal (AlgReal)
import Grisette.Internal.SymPrim.BV (IntN, WordN)
import Grisette.Internal.SymPrim.FP (FP, FPRoundingMode, ValidFP)
import Grisette.Internal.TH.GADT.DeriveGADT (deriveGADT)

-- $setup
-- >>> import Grisette.Core
-- >>> import Grisette.SymPrim
-- >>> import Grisette.Backend
-- >>> import Data.Proxy

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
  [''AllSyms, ''AllSyms1, ''AllSyms2]
deriveGADT
  [ ''[],
    ''Maybe,
    ''Identity,
    ''ExceptT,
    ''MaybeT,
    ''WriterLazy.WriterT,
    ''WriterStrict.WriterT
  ]
  [''AllSyms, ''AllSyms1]
deriveGADT
  [ ''(),
    ''AssertionError,
    ''VerificationConditions
  ]
  [''AllSyms]

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
CONCRETE_ALLSYMS(AlgReal)
#endif

instance AllSyms (Proxy a) where
  allSymsS _ = id
  {-# INLINE allSymsS #-}

instance AllSyms1 Proxy where
  liftAllSymsS _ _ = id
  {-# INLINE liftAllSymsS #-}

instance (AllSyms a) => AllSyms (Ratio a) where
  allSymsS r = allSymsS (numerator r) . allSymsS (denominator r)
  {-# INLINE allSymsS #-}

instance (ValidFP eb sb) => AllSyms (FP eb sb) where
  allSymsS _ = id
  {-# INLINE allSymsS #-}
