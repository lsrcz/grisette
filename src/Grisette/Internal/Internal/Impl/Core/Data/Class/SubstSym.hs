{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
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
-- Module      :   Grisette.Internal.Internal.Impl.Core.Data.Class.SubstSym
-- Copyright   :   (c) Sirui Lu 2021-2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Internal.Internal.Impl.Core.Data.Class.SubstSym () where

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
import Data.Proxy (Proxy)
import Data.Ratio (Ratio, denominator, numerator, (%))
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
import Generics.Deriving.Instances ()
import Grisette.Internal.Core.Control.Exception
  ( AssertionError,
    VerificationConditions,
  )
import Grisette.Internal.Internal.Decl.Core.Data.Class.SubstSym
  ( SubstSym (substSym),
    SubstSym1 (liftSubstSym),
    SubstSym2,
    substSym1,
  )
import Grisette.Internal.SymPrim.AlgReal (AlgReal)
import Grisette.Internal.SymPrim.BV (IntN, WordN)
import Grisette.Internal.SymPrim.FP
  ( FP,
    FPRoundingMode,
    NotRepresentableFPError,
    ValidFP,
  )
import Grisette.Internal.SymPrim.GeneralFun (substTerm, type (-->) (GeneralFun))
import Grisette.Internal.SymPrim.Prim.Term
  ( LinkedRep (underlyingTerm),
    SymRep (SymType),
    someTypedSymbol,
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
import Grisette.Internal.SymPrim.SymGeneralFun (type (-~>) (SymGeneralFun))
import Grisette.Internal.SymPrim.SymInteger (SymInteger (SymInteger))
import Grisette.Internal.SymPrim.SymTabularFun (type (=~>) (SymTabularFun))
import Grisette.Internal.SymPrim.TabularFun (type (=->) (TabularFun))
import Grisette.Internal.TH.Derivation.Derive (derive)

#define CONCRETE_SUBSTITUTESYM(type) \
instance SubstSym type where \
  substSym _ _ = id

#define CONCRETE_SUBSTITUTESYM_BV(type) \
instance (KnownNat n, 1 <= n) => SubstSym (type n) where \
  substSym _ _ = id

#if 1
CONCRETE_SUBSTITUTESYM(Bool)
CONCRETE_SUBSTITUTESYM(Integer)
CONCRETE_SUBSTITUTESYM(Char)
CONCRETE_SUBSTITUTESYM(Int)
CONCRETE_SUBSTITUTESYM(Int8)
CONCRETE_SUBSTITUTESYM(Int16)
CONCRETE_SUBSTITUTESYM(Int32)
CONCRETE_SUBSTITUTESYM(Int64)
CONCRETE_SUBSTITUTESYM(Word)
CONCRETE_SUBSTITUTESYM(Word8)
CONCRETE_SUBSTITUTESYM(Word16)
CONCRETE_SUBSTITUTESYM(Word32)
CONCRETE_SUBSTITUTESYM(Word64)
CONCRETE_SUBSTITUTESYM(Float)
CONCRETE_SUBSTITUTESYM(Double)
CONCRETE_SUBSTITUTESYM(B.ByteString)
CONCRETE_SUBSTITUTESYM(T.Text)
CONCRETE_SUBSTITUTESYM(Monoid.All)
CONCRETE_SUBSTITUTESYM(Monoid.Any)
CONCRETE_SUBSTITUTESYM(Ordering)
CONCRETE_SUBSTITUTESYM_BV(WordN)
CONCRETE_SUBSTITUTESYM_BV(IntN)
CONCRETE_SUBSTITUTESYM(FPRoundingMode)
CONCRETE_SUBSTITUTESYM(AlgReal)
#endif

instance SubstSym (Proxy a) where
  substSym _ _ = id
  {-# INLINE substSym #-}

instance SubstSym1 Proxy where
  liftSubstSym _ _ _ = id
  {-# INLINE liftSubstSym #-}

instance (Integral a, SubstSym a) => SubstSym (Ratio a) where
  substSym sym val a =
    substSym sym val (numerator a) % substSym sym val (denominator a)
  {-# INLINE substSym #-}

instance (ValidFP eb sb) => SubstSym (FP eb sb) where
  substSym _ _ = id

#define SUBSTITUTE_SYM_SIMPLE(symtype) \
instance SubstSym symtype where \
  substSym sym v (symtype t) = \
    symtype $ substTerm sym (underlyingTerm v) HS.empty t

#define SUBSTITUTE_SYM_BV(symtype) \
instance (KnownNat n, 1 <= n) => SubstSym (symtype n) where \
  substSym sym v (symtype t) = \
    symtype $ substTerm sym (underlyingTerm v) HS.empty t

#define SUBSTITUTE_SYM_FUN(op, cons) \
instance SubstSym (op sa sb) where \
  substSym sym v (cons t) = \
    cons $ substTerm sym (underlyingTerm v) HS.empty t

#if 1
SUBSTITUTE_SYM_SIMPLE(SymBool)
SUBSTITUTE_SYM_SIMPLE(SymInteger)
SUBSTITUTE_SYM_SIMPLE(SymAlgReal)
SUBSTITUTE_SYM_BV(SymIntN)
SUBSTITUTE_SYM_BV(SymWordN)
SUBSTITUTE_SYM_FUN((=~>), SymTabularFun)
SUBSTITUTE_SYM_FUN((-~>), SymGeneralFun)
SUBSTITUTE_SYM_SIMPLE(SymFPRoundingMode)
#endif

instance (ValidFP eb sb) => SubstSym (SymFP eb sb) where
  substSym sym v (SymFP t) = SymFP $ substTerm sym (underlyingTerm v) HS.empty t

derive
  [ ''(),
    ''AssertionError,
    ''VerificationConditions,
    ''NotRepresentableFPError
  ]
  [''SubstSym]

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
  [''SubstSym, ''SubstSym1, ''SubstSym2]

derive
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
  [''SubstSym, ''SubstSym1]

-- IdentityT
instance
  (SubstSym1 m, SubstSym a) =>
  SubstSym (IdentityT m a)
  where
  substSym = substSym1
  {-# INLINE substSym #-}

instance (SubstSym1 m) => SubstSym1 (IdentityT m) where
  liftSubstSym f sym val (IdentityT a) =
    IdentityT $ liftSubstSym f sym val a
  {-# INLINE liftSubstSym #-}

-- Product
deriving via
  (Default (Product l r a))
  instance
    (SubstSym (l a), SubstSym (r a)) => SubstSym (Product l r a)

deriving via
  (Default1 (Product l r))
  instance
    (SubstSym1 l, SubstSym1 r) => SubstSym1 (Product l r)

-- Sum
deriving via
  (Default (Sum l r a))
  instance
    (SubstSym (l a), SubstSym (r a)) => SubstSym (Sum l r a)

deriving via
  (Default1 (Sum l r))
  instance
    (SubstSym1 l, SubstSym1 r) => SubstSym1 (Sum l r)

-- Compose
deriving via
  (Default (Compose f g a))
  instance
    (SubstSym (f (g a))) => SubstSym (Compose f g a)

instance
  (SubstSym1 f, SubstSym1 g) =>
  SubstSym1 (Compose f g)
  where
  liftSubstSym f sym val (Compose x) =
    Compose $ liftSubstSym (liftSubstSym f) sym val x
  {-# INLINE liftSubstSym #-}

-- Const
deriving via
  (Default (Const a b))
  instance
    (SubstSym a) => SubstSym (Const a b)

deriving via
  (Default1 (Const a))
  instance
    (SubstSym a) => SubstSym1 (Const a)

-- Alt
deriving via
  (Default (Alt f a))
  instance
    (SubstSym (f a)) => SubstSym (Alt f a)

deriving via
  (Default1 (Alt f))
  instance
    (SubstSym1 f) => SubstSym1 (Alt f)

-- Ap
deriving via
  (Default (Ap f a))
  instance
    (SubstSym (f a)) => SubstSym (Ap f a)

deriving via
  (Default1 (Ap f))
  instance
    (SubstSym1 f) => SubstSym1 (Ap f)

-- Generic
deriving via (Default (U1 p)) instance SubstSym (U1 p)

deriving via (Default (V1 p)) instance SubstSym (V1 p)

deriving via
  (Default (K1 i c p))
  instance
    (SubstSym c) => SubstSym (K1 i c p)

deriving via
  (Default (M1 i c f p))
  instance
    (SubstSym (f p)) => SubstSym (M1 i c f p)

deriving via
  (Default ((f :+: g) p))
  instance
    (SubstSym (f p), SubstSym (g p)) => SubstSym ((f :+: g) p)

deriving via
  (Default ((f :*: g) p))
  instance
    (SubstSym (f p), SubstSym (g p)) => SubstSym ((f :*: g) p)

deriving via
  (Default (Par1 p))
  instance
    (SubstSym p) => SubstSym (Par1 p)

deriving via
  (Default (Rec1 f p))
  instance
    (SubstSym (f p)) => SubstSym (Rec1 f p)

deriving via
  (Default ((f :.: g) p))
  instance
    (SubstSym (f (g p))) => SubstSym ((f :.: g) p)

instance (SubstSym a, SubstSym b) => SubstSym (a =-> b) where
  substSym sym val (TabularFun f d) =
    TabularFun (substSym sym val f) (substSym sym val d)
  {-# INLINE substSym #-}

instance (SubstSym (SymType b)) => SubstSym (a --> b) where
  substSym sym val (GeneralFun s t) =
    GeneralFun s $
      substTerm sym (underlyingTerm val) (HS.singleton $ someTypedSymbol s) t
  {-# INLINE substSym #-}
