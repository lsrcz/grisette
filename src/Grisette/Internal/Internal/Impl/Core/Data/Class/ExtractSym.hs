{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- {-# OPTIONS_GHC -ddump-splices -ddump-to-file -ddump-file-prefix=extractsym #-}

-- |
-- Module      :   Grisette.Internal.Internal.Impl.Core.Data.Class.ExtractSym
-- Copyright   :   (c) Sirui Lu 2021-2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Internal.Internal.Impl.Core.Data.Class.ExtractSym () where

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
import qualified Data.HashSet as HS
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Monoid (Alt, Ap)
import qualified Data.Monoid as Monoid
import Data.Ord (Down)
import Data.Ratio (Ratio, denominator, numerator)
import qualified Data.Text as T
import Data.Typeable (Proxy, type (:~~:) (HRefl))
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
    type (:*:),
    type (:+:),
    type (:.:) (Comp1),
  )
import Grisette.Internal.Core.Control.Exception
  ( AssertionError,
    VerificationConditions,
  )
import Grisette.Internal.Internal.Decl.Core.Data.Class.ExtractSym
  ( ExtractSym (extractSymMaybe),
    ExtractSym1 (liftExtractSymMaybe),
    ExtractSym2,
    extractSymMaybe1,
  )
import Grisette.Internal.SymPrim.AlgReal (AlgReal)
import Grisette.Internal.SymPrim.BV (IntN, WordN)
import Grisette.Internal.SymPrim.FP
  ( FP,
    FPRoundingMode,
    NotRepresentableFPError,
    ValidFP,
  )
import Grisette.Internal.SymPrim.GeneralFun (type (-->) (GeneralFun))
import Grisette.Internal.SymPrim.Prim.Model
  ( SymbolSet (SymbolSet),
  )
import Grisette.Internal.SymPrim.Prim.Term
  ( IsSymbolKind (decideSymbolKind),
    SymRep (SymType),
    someTypedSymbol,
  )
import Grisette.Internal.SymPrim.Prim.TermUtils (extractTerm)
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
import Grisette.Internal.SymPrim.TabularFun (type (=->) (TabularFun))
import Grisette.Internal.TH.GADT.DeriveGADT (deriveGADT)

-- $setup
-- >>> import Grisette.Core
-- >>> import Grisette.SymPrim
-- >>> import Grisette.Lib.Base
-- >>> import Data.HashSet as HashSet
-- >>> import Data.List (sort)

#define CONCRETE_EXTRACT_SYMBOLICS(type) \
instance ExtractSym type where \
  extractSymMaybe _ = return mempty

#define CONCRETE_EXTRACT_SYMBOLICS_BV(type) \
instance (KnownNat n, 1 <= n) => ExtractSym (type n) where \
  extractSymMaybe _ = return mempty

#if 1
CONCRETE_EXTRACT_SYMBOLICS(Bool)
CONCRETE_EXTRACT_SYMBOLICS(Integer)
CONCRETE_EXTRACT_SYMBOLICS(Char)
CONCRETE_EXTRACT_SYMBOLICS(Int)
CONCRETE_EXTRACT_SYMBOLICS(Int8)
CONCRETE_EXTRACT_SYMBOLICS(Int16)
CONCRETE_EXTRACT_SYMBOLICS(Int32)
CONCRETE_EXTRACT_SYMBOLICS(Int64)
CONCRETE_EXTRACT_SYMBOLICS(Word)
CONCRETE_EXTRACT_SYMBOLICS(Word8)
CONCRETE_EXTRACT_SYMBOLICS(Word16)
CONCRETE_EXTRACT_SYMBOLICS(Word32)
CONCRETE_EXTRACT_SYMBOLICS(Word64)
CONCRETE_EXTRACT_SYMBOLICS(Float)
CONCRETE_EXTRACT_SYMBOLICS(Double)
CONCRETE_EXTRACT_SYMBOLICS(B.ByteString)
CONCRETE_EXTRACT_SYMBOLICS(T.Text)
CONCRETE_EXTRACT_SYMBOLICS(FPRoundingMode)
CONCRETE_EXTRACT_SYMBOLICS(Monoid.All)
CONCRETE_EXTRACT_SYMBOLICS(Monoid.Any)
CONCRETE_EXTRACT_SYMBOLICS(Ordering)
CONCRETE_EXTRACT_SYMBOLICS_BV(WordN)
CONCRETE_EXTRACT_SYMBOLICS_BV(IntN)
CONCRETE_EXTRACT_SYMBOLICS(AlgReal)
#endif

instance ExtractSym (Proxy a) where
  extractSymMaybe _ = return mempty
  {-# INLINE extractSymMaybe #-}

instance ExtractSym1 Proxy where
  liftExtractSymMaybe _ _ = return mempty
  {-# INLINE liftExtractSymMaybe #-}

instance (ExtractSym a) => ExtractSym (Ratio a) where
  extractSymMaybe a =
    extractSymMaybe (numerator a) <> extractSymMaybe (denominator a)
  {-# INLINE extractSymMaybe #-}

instance (ValidFP eb sb) => ExtractSym (FP eb sb) where
  extractSymMaybe _ = return mempty

#define EXTRACT_SYMBOLICS_SIMPLE(symtype) \
instance ExtractSym symtype where \
  extractSymMaybe :: \
    forall knd. (IsSymbolKind knd) => symtype -> Maybe (SymbolSet knd); \
  extractSymMaybe (symtype t) = \
    case decideSymbolKind @knd of\
      Left HRefl -> SymbolSet <$> extractTerm HS.empty t; \
      Right HRefl -> SymbolSet <$> extractTerm HS.empty t

#define EXTRACT_SYMBOLICS_BV(symtype) \
instance (KnownNat n, 1 <= n) => ExtractSym (symtype n) where \
  extractSymMaybe :: \
    forall knd. (IsSymbolKind knd) => symtype n -> Maybe (SymbolSet knd); \
  extractSymMaybe (symtype t) = \
    case decideSymbolKind @knd of\
      Left HRefl -> SymbolSet <$> extractTerm HS.empty t; \
      Right HRefl -> SymbolSet <$> extractTerm HS.empty t

#define EXTRACT_SYMBOLICS_FUN(op, cons) \
instance ExtractSym (op sa sb) where \
  extractSymMaybe :: \
    forall knd. (IsSymbolKind knd) => op sa sb -> Maybe (SymbolSet knd); \
  extractSymMaybe (cons t) = \
    case decideSymbolKind @knd of \
      Left HRefl -> Nothing; \
      Right HRefl -> SymbolSet <$> extractTerm HS.empty t

#if 1
EXTRACT_SYMBOLICS_SIMPLE(SymBool)
EXTRACT_SYMBOLICS_SIMPLE(SymInteger)
EXTRACT_SYMBOLICS_SIMPLE(SymFPRoundingMode)
EXTRACT_SYMBOLICS_SIMPLE(SymAlgReal)
EXTRACT_SYMBOLICS_BV(SymIntN)
EXTRACT_SYMBOLICS_BV(SymWordN)
EXTRACT_SYMBOLICS_FUN((=~>), SymTabularFun)
EXTRACT_SYMBOLICS_FUN((-~>), SymGeneralFun)
#endif

instance (ValidFP eb fb) => ExtractSym (SymFP eb fb) where
  extractSymMaybe ::
    forall knd. (IsSymbolKind knd) => SymFP eb fb -> Maybe (SymbolSet knd)
  extractSymMaybe (SymFP t) =
    case decideSymbolKind @knd of
      Left HRefl -> SymbolSet <$> extractTerm HS.empty t
      Right HRefl -> SymbolSet <$> extractTerm HS.empty t

deriveGADT
  [ ''(),
    ''AssertionError,
    ''VerificationConditions,
    ''NotRepresentableFPError
  ]
  [''ExtractSym]

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
  [''ExtractSym, ''ExtractSym1, ''ExtractSym2]

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
  [''ExtractSym, ''ExtractSym1]

-- IdentityT
instance
  (ExtractSym1 m, ExtractSym a) =>
  ExtractSym (IdentityT m a)
  where
  extractSymMaybe = extractSymMaybe1
  {-# INLINE extractSymMaybe #-}

instance (ExtractSym1 m) => ExtractSym1 (IdentityT m) where
  liftExtractSymMaybe f (IdentityT v) = liftExtractSymMaybe f v
  {-# INLINE liftExtractSymMaybe #-}

-- Product
deriving via
  (Default (Product l r a))
  instance
    (ExtractSym (l a), ExtractSym (r a)) =>
    ExtractSym (Product l r a)

deriving via
  (Default1 (Product l r))
  instance
    (ExtractSym1 l, ExtractSym1 r) =>
    ExtractSym1 (Product l r)

-- Sum
deriving via
  (Default (Sum l r a))
  instance
    (ExtractSym (l a), ExtractSym (r a)) =>
    ExtractSym (Sum l r a)

deriving via
  (Default1 (Sum l r))
  instance
    (ExtractSym1 l, ExtractSym1 r) => ExtractSym1 (Sum l r)

-- Compose
deriving via
  (Default (Compose f g a))
  instance
    (ExtractSym (f (g a))) => ExtractSym (Compose f g a)

instance
  (ExtractSym1 f, ExtractSym1 g) =>
  ExtractSym1 (Compose f g)
  where
  liftExtractSymMaybe f (Compose l) =
    liftExtractSymMaybe (liftExtractSymMaybe f) l
  {-# INLINE liftExtractSymMaybe #-}

-- Const
deriving via
  (Default (Const a b))
  instance
    (ExtractSym a) => ExtractSym (Const a b)

deriving via
  (Default1 (Const a))
  instance
    (ExtractSym a) => ExtractSym1 (Const a)

-- Alt
deriving via
  (Default (Alt f a))
  instance
    (ExtractSym (f a)) => ExtractSym (Alt f a)

deriving via
  (Default1 (Alt f))
  instance
    (ExtractSym1 f) => ExtractSym1 (Alt f)

-- Ap
deriving via
  (Default (Ap f a))
  instance
    (ExtractSym (f a)) => ExtractSym (Ap f a)

deriving via
  (Default1 (Ap f))
  instance
    (ExtractSym1 f) => ExtractSym1 (Ap f)

-- Generic
deriving via (Default (U1 p)) instance ExtractSym (U1 p)

deriving via (Default (V1 p)) instance ExtractSym (V1 p)

deriving via
  (Default (K1 i c p))
  instance
    (ExtractSym c) => ExtractSym (K1 i c p)

deriving via
  (Default (M1 i c f p))
  instance
    (ExtractSym (f p)) => ExtractSym (M1 i c f p)

deriving via
  (Default ((f :+: g) p))
  instance
    (ExtractSym (f p), ExtractSym (g p)) =>
    ExtractSym ((f :+: g) p)

deriving via
  (Default ((f :*: g) p))
  instance
    (ExtractSym (f p), ExtractSym (g p)) =>
    ExtractSym ((f :*: g) p)

deriving via
  (Default (Par1 p))
  instance
    (ExtractSym p) => ExtractSym (Par1 p)

deriving via
  (Default (Rec1 f p))
  instance
    (ExtractSym (f p)) => ExtractSym (Rec1 f p)

deriving via
  (Default ((f :.: g) p))
  instance
    (ExtractSym (f (g p))) => ExtractSym ((f :.: g) p)

instance (ExtractSym a, ExtractSym b) => ExtractSym (a =-> b) where
  extractSymMaybe (TabularFun s t) =
    extractSymMaybe s <> extractSymMaybe t

instance (ExtractSym (SymType b)) => ExtractSym (a --> b) where
  extractSymMaybe :: forall knd. (IsSymbolKind knd) => (a --> b) -> Maybe (SymbolSet knd)
  extractSymMaybe (GeneralFun t f) =
    case decideSymbolKind @knd of
      Left HRefl -> Nothing
      Right HRefl -> SymbolSet <$> extractTerm (HS.singleton $ someTypedSymbol t) f
