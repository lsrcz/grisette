{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
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
-- Module      :   Grisette.Internal.Core.Data.Class.EvalSym
-- Copyright   :   (c) Sirui Lu 2021-2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Internal.Internal.Impl.Core.Data.Class.EvalSym () where

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
import qualified Data.HashSet as HS
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Monoid (Alt, Ap)
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
import qualified Generics.Deriving.Monoid as Monoid
import Grisette.Internal.Core.Control.Exception
  ( AssertionError,
    VerificationConditions,
  )
import Grisette.Internal.Core.Data.Class.AsKey (AsKey (AsKey), AsKey1 (AsKey1))
import Grisette.Internal.Internal.Decl.Core.Data.Class.EvalSym
  ( EvalSym (evalSym),
    EvalSym1 (liftEvalSym),
    EvalSym2,
    evalSym1,
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
import Grisette.Internal.SymPrim.Prim.Model (evalTerm)
import Grisette.Internal.SymPrim.Prim.Term
  ( SymRep (SymType),
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

#define CONCRETE_EVALUATESYM(type) \
instance EvalSym type where \
  evalSym _ _ = id

#define CONCRETE_EVALUATESYM_BV(type) \
instance (KnownNat n, 1 <= n) => EvalSym (type n) where \
  evalSym _ _ = id

#if 1
CONCRETE_EVALUATESYM(Bool)
CONCRETE_EVALUATESYM(Integer)
CONCRETE_EVALUATESYM(Char)
CONCRETE_EVALUATESYM(Int)
CONCRETE_EVALUATESYM(Int8)
CONCRETE_EVALUATESYM(Int16)
CONCRETE_EVALUATESYM(Int32)
CONCRETE_EVALUATESYM(Int64)
CONCRETE_EVALUATESYM(Word)
CONCRETE_EVALUATESYM(Word8)
CONCRETE_EVALUATESYM(Word16)
CONCRETE_EVALUATESYM(Word32)
CONCRETE_EVALUATESYM(Word64)
CONCRETE_EVALUATESYM(Float)
CONCRETE_EVALUATESYM(Double)
CONCRETE_EVALUATESYM(B.ByteString)
CONCRETE_EVALUATESYM(T.Text)
CONCRETE_EVALUATESYM(FPRoundingMode)
CONCRETE_EVALUATESYM(Monoid.All)
CONCRETE_EVALUATESYM(Monoid.Any)
CONCRETE_EVALUATESYM(Ordering)
CONCRETE_EVALUATESYM_BV(IntN)
CONCRETE_EVALUATESYM_BV(WordN)
CONCRETE_EVALUATESYM(AlgReal)
#endif

instance EvalSym (Proxy a) where
  evalSym _ _ = id
  {-# INLINE evalSym #-}

instance EvalSym1 Proxy where
  liftEvalSym _ _ _ = id
  {-# INLINE liftEvalSym #-}

instance (Integral a, EvalSym a) => EvalSym (Ratio a) where
  evalSym fillDefault model r =
    evalSym fillDefault model (numerator r)
      % evalSym fillDefault model (denominator r)

instance (ValidFP eb fb) => EvalSym (FP eb fb) where
  evalSym _ _ = id

-- Symbolic primitives
#define EVALUATE_SYM_SIMPLE(symtype) \
instance EvalSym symtype where \
  evalSym fillDefault model (symtype t) = \
    symtype $ evalTerm fillDefault model HS.empty t

#define EVALUATE_SYM_BV(symtype) \
instance (KnownNat n, 1 <= n) => EvalSym (symtype n) where \
  evalSym fillDefault model (symtype t) = \
    symtype $ evalTerm fillDefault model HS.empty t

#define EVALUATE_SYM_FUN(cop, op, cons) \
instance EvalSym (op sa sb) where \
  evalSym fillDefault model (cons t) = \
    cons $ evalTerm fillDefault model HS.empty t

#if 1
EVALUATE_SYM_SIMPLE(SymBool)
EVALUATE_SYM_SIMPLE(SymInteger)
EVALUATE_SYM_SIMPLE(SymFPRoundingMode)
EVALUATE_SYM_SIMPLE(SymAlgReal)
EVALUATE_SYM_BV(SymIntN)
EVALUATE_SYM_BV(SymWordN)
EVALUATE_SYM_FUN((=->), (=~>), SymTabularFun)
EVALUATE_SYM_FUN((-->), (-~>), SymGeneralFun)
#endif

instance (ValidFP eb sb) => EvalSym (SymFP eb sb) where
  evalSym fillDefault model (SymFP t) =
    SymFP $ evalTerm fillDefault model HS.empty t

derive
  [ ''(),
    ''AssertionError,
    ''VerificationConditions,
    ''NotRepresentableFPError,
    ''ArithException
  ]
  [''EvalSym]

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
  [''EvalSym, ''EvalSym1, ''EvalSym2]

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
  [''EvalSym, ''EvalSym1]

-- IdentityT
instance (EvalSym1 m, EvalSym a) => EvalSym (IdentityT m a) where
  evalSym = evalSym1
  {-# INLINE evalSym #-}

instance (EvalSym1 m) => EvalSym1 (IdentityT m) where
  liftEvalSym f fillDefault model (IdentityT a) =
    IdentityT $ liftEvalSym f fillDefault model a
  {-# INLINE liftEvalSym #-}

-- Product
deriving via
  (Default (Product l r a))
  instance
    (EvalSym (l a), EvalSym (r a)) => EvalSym (Product l r a)

deriving via
  (Default1 (Product l r))
  instance
    (EvalSym1 l, EvalSym1 r) => EvalSym1 (Product l r)

-- Sum
deriving via
  (Default (Sum l r a))
  instance
    (EvalSym (l a), EvalSym (r a)) => EvalSym (Sum l r a)

deriving via
  (Default1 (Sum l r))
  instance
    (EvalSym1 l, EvalSym1 r) => EvalSym1 (Sum l r)

-- Compose
deriving via
  (Default (Compose f g a))
  instance
    (EvalSym (f (g a))) => EvalSym (Compose f g a)

instance (EvalSym1 f, EvalSym1 g) => EvalSym1 (Compose f g) where
  liftEvalSym f fillDefault m (Compose l) =
    Compose $ liftEvalSym (liftEvalSym f) fillDefault m l
  {-# INLINE liftEvalSym #-}

-- Const
deriving via (Default (Const a b)) instance (EvalSym a) => EvalSym (Const a b)

deriving via (Default1 (Const a)) instance (EvalSym a) => EvalSym1 (Const a)

-- Alt
deriving via (Default (Alt f a)) instance (EvalSym (f a)) => EvalSym (Alt f a)

deriving via (Default1 (Alt f)) instance (EvalSym1 f) => EvalSym1 (Alt f)

-- Ap
deriving via (Default (Ap f a)) instance (EvalSym (f a)) => EvalSym (Ap f a)

deriving via (Default1 (Ap f)) instance (EvalSym1 f) => EvalSym1 (Ap f)

-- Generic
deriving via (Default (U1 p)) instance EvalSym (U1 p)

deriving via (Default (V1 p)) instance EvalSym (V1 p)

deriving via
  (Default (K1 i c p))
  instance
    (EvalSym c) => EvalSym (K1 i c p)

deriving via
  (Default (M1 i c f p))
  instance
    (EvalSym (f p)) => EvalSym (M1 i c f p)

deriving via
  (Default ((f :+: g) p))
  instance
    (EvalSym (f p), EvalSym (g p)) => EvalSym ((f :+: g) p)

deriving via
  (Default ((f :*: g) p))
  instance
    (EvalSym (f p), EvalSym (g p)) => EvalSym ((f :*: g) p)

deriving via
  (Default (Par1 p))
  instance
    (EvalSym p) => EvalSym (Par1 p)

deriving via
  (Default (Rec1 f p))
  instance
    (EvalSym (f p)) => EvalSym (Rec1 f p)

deriving via
  (Default ((f :.: g) p))
  instance
    (EvalSym (f (g p))) => EvalSym ((f :.: g) p)

instance (EvalSym a, EvalSym b) => EvalSym (a =-> b) where
  evalSym fillDefault model (TabularFun s t) =
    TabularFun
      (evalSym fillDefault model s)
      (evalSym fillDefault model t)

instance (EvalSym (SymType b)) => EvalSym (a --> b) where
  evalSym fillDefault model (GeneralFun s t) =
    GeneralFun s $
      evalTerm fillDefault model (HS.singleton $ someTypedSymbol s) t

instance (EvalSym a) => EvalSym (AsKey a) where
  evalSym fillDefault model (AsKey t) = AsKey $ evalSym fillDefault model t
  {-# INLINE evalSym #-}

instance (EvalSym1 f, EvalSym a) => EvalSym (AsKey1 f a) where
  evalSym fillDefault model (AsKey1 t) = AsKey1 $ evalSym fillDefault model t

instance (EvalSym1 a) => EvalSym1 (AsKey1 a) where
  liftEvalSym f fillDefault model (AsKey1 t) = AsKey1 $ liftEvalSym f fillDefault model t
  {-# INLINE liftEvalSym #-}
