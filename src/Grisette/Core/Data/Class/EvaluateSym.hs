{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      :   Grisette.Core.Data.Class.EvaluateSym
-- Copyright   :   (c) Sirui Lu 2021-2023
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Core.Data.Class.EvaluateSym
  ( -- * Evaluating symbolic values with model
    EvaluateSym (..),
    evaluateSymToCon,
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
import Data.Maybe (fromJust)
import qualified Data.Text as T
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.TypeNats (KnownNat, type (<=))
import Generics.Deriving
  ( Default (Default, unDefault),
    Generic (Rep, from, to),
    K1 (K1),
    M1 (M1),
    U1,
    type (:*:) ((:*:)),
    type (:+:) (L1, R1),
  )
import Generics.Deriving.Instances ()
import Grisette.Core.Data.BV (IntN, SomeIntN, SomeWordN, WordN)
import Grisette.Core.Data.Class.ToCon (ToCon (toCon))
import Grisette.IR.SymPrim.Data.Prim.InternedTerm.Term (LinkedRep, SupportedPrim)
import Grisette.IR.SymPrim.Data.Prim.Model (Model, evaluateTerm)
import {-# SOURCE #-} Grisette.IR.SymPrim.Data.SymPrim
  ( SomeSymIntN (SomeSymIntN),
    SomeSymWordN (SomeSymWordN),
    SymBool (SymBool),
    SymIntN (SymIntN),
    SymInteger (SymInteger),
    SymWordN (SymWordN),
    type (-~>) (SymGeneralFun),
    type (=~>) (SymTabularFun),
  )

-- $setup
-- >>> import Grisette.Core
-- >>> import Grisette.IR.SymPrim
-- >>> import Data.Proxy
-- >>> :set -XTypeApplications

-- | Evaluating symbolic values with some model.
--
-- >>> let model = insertValue (SimpleSymbol "a") (1 :: Integer) emptyModel :: Model
-- >>> evaluateSym False model ([ssym "a", ssym "b"] :: [SymInteger])
-- [1,b]
--
-- If we set the first argument true, the missing variables will be filled in with
-- some default values:
--
-- >>> evaluateSym True model ([ssym "a", ssym "b"] :: [SymInteger])
-- [1,0]
--
-- __Note 1:__ This type class can be derived for algebraic data types.
-- You may need the @DerivingVia@ and @DerivingStrategies@ extensions.
--
-- > data X = ... deriving Generic deriving EvaluateSym via (Default X)
class EvaluateSym a where
  -- | Evaluate a symbolic variable with some model, possibly fill in values for the missing variables.
  evaluateSym :: Bool -> Model -> a -> a

-- | Evaluate a symbolic variable with some model, fill in values for the missing variables,
-- and transform to concrete ones
--
-- >>> let model = insertValue (SimpleSymbol "a") (1 :: Integer) emptyModel :: Model
-- >>> evaluateSymToCon model ([ssym "a", ssym "b"] :: [SymInteger]) :: [Integer]
-- [1,0]
evaluateSymToCon :: (ToCon a b, EvaluateSym a) => Model -> a -> b
evaluateSymToCon model a = fromJust $ toCon $ evaluateSym True model a

-- instances

#define CONCRETE_EVALUATESYM(type) \
instance EvaluateSym type where \
  evaluateSym _ _ = id

#define CONCRETE_EVALUATESYM_BV(type) \
instance (KnownNat n, 1 <= n) => EvaluateSym (type n) where \
  evaluateSym _ _ = id

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
CONCRETE_EVALUATESYM(SomeIntN)
CONCRETE_EVALUATESYM(SomeWordN)
CONCRETE_EVALUATESYM(B.ByteString)
CONCRETE_EVALUATESYM(T.Text)
CONCRETE_EVALUATESYM_BV(IntN)
CONCRETE_EVALUATESYM_BV(WordN)
#endif

-- ()
instance EvaluateSym () where
  evaluateSym _ _ = id

-- Either
deriving via (Default (Either a b)) instance (EvaluateSym a, EvaluateSym b) => EvaluateSym (Either a b)

-- Maybe
deriving via (Default (Maybe a)) instance (EvaluateSym a) => EvaluateSym (Maybe a)

-- List
deriving via (Default [a]) instance (EvaluateSym a) => EvaluateSym [a]

-- (,)
deriving via (Default (a, b)) instance (EvaluateSym a, EvaluateSym b) => EvaluateSym (a, b)

-- (,,)
deriving via (Default (a, b, c)) instance (EvaluateSym a, EvaluateSym b, EvaluateSym c) => EvaluateSym (a, b, c)

-- (,,,)
deriving via
  (Default (a, b, c, d))
  instance
    (EvaluateSym a, EvaluateSym b, EvaluateSym c, EvaluateSym d) => EvaluateSym (a, b, c, d)

-- (,,,,)
deriving via
  (Default (a, b, c, d, e))
  instance
    (EvaluateSym a, EvaluateSym b, EvaluateSym c, EvaluateSym d, EvaluateSym e) =>
    EvaluateSym (a, b, c, d, e)

-- (,,,,,)
deriving via
  (Default (a, b, c, d, e, f))
  instance
    (EvaluateSym a, EvaluateSym b, EvaluateSym c, EvaluateSym d, EvaluateSym e, EvaluateSym f) =>
    EvaluateSym (a, b, c, d, e, f)

-- (,,,,,,)
deriving via
  (Default (a, b, c, d, e, f, g))
  instance
    ( EvaluateSym a,
      EvaluateSym b,
      EvaluateSym c,
      EvaluateSym d,
      EvaluateSym e,
      EvaluateSym f,
      EvaluateSym g
    ) =>
    EvaluateSym (a, b, c, d, e, f, g)

-- (,,,,,,,)
deriving via
  (Default (a, b, c, d, e, f, g, h))
  instance
    ( EvaluateSym a,
      EvaluateSym b,
      EvaluateSym c,
      EvaluateSym d,
      EvaluateSym e,
      EvaluateSym f,
      EvaluateSym g,
      EvaluateSym h
    ) =>
    EvaluateSym ((,,,,,,,) a b c d e f g h)

-- MaybeT
instance (EvaluateSym (m (Maybe a))) => EvaluateSym (MaybeT m a) where
  evaluateSym fillDefault model (MaybeT v) = MaybeT $ evaluateSym fillDefault model v

-- ExceptT
instance (EvaluateSym (m (Either e a))) => EvaluateSym (ExceptT e m a) where
  evaluateSym fillDefault model (ExceptT v) = ExceptT $ evaluateSym fillDefault model v

-- Sum
deriving via
  (Default (Sum f g a))
  instance
    (EvaluateSym (f a), EvaluateSym (g a)) => EvaluateSym (Sum f g a)

-- WriterT
instance (EvaluateSym (m (a, s))) => EvaluateSym (WriterLazy.WriterT s m a) where
  evaluateSym fillDefault model (WriterLazy.WriterT v) = WriterLazy.WriterT $ evaluateSym fillDefault model v

instance (EvaluateSym (m (a, s))) => EvaluateSym (WriterStrict.WriterT s m a) where
  evaluateSym fillDefault model (WriterStrict.WriterT v) = WriterStrict.WriterT $ evaluateSym fillDefault model v

-- Identity
instance (EvaluateSym a) => EvaluateSym (Identity a) where
  evaluateSym fillDefault model (Identity a) = Identity $ evaluateSym fillDefault model a

-- IdentityT
instance (EvaluateSym (m a)) => EvaluateSym (IdentityT m a) where
  evaluateSym fillDefault model (IdentityT a) = IdentityT $ evaluateSym fillDefault model a

-- Symbolic primitives
#define EVALUATE_SYM_SIMPLE(symtype) \
instance EvaluateSym symtype where \
  evaluateSym fillDefault model (symtype t) = symtype $ evaluateTerm fillDefault model t

#define EVALUATE_SYM_BV(symtype) \
instance (KnownNat n, 1 <= n) => EvaluateSym (symtype n) where \
  evaluateSym fillDefault model (symtype t) = symtype $ evaluateTerm fillDefault model t

#define EVALUATE_SYM_FUN(op, cons) \
instance (SupportedPrim ca, SupportedPrim cb, LinkedRep ca sa, LinkedRep cb sb) => EvaluateSym (sa op sb) where \
  evaluateSym fillDefault model (cons t) = cons $ evaluateTerm fillDefault model t

#define EVALUATE_SYM_BV_SOME(somety, origty) \
instance EvaluateSym somety where \
  evaluateSym fillDefault model (somety (origty t)) = somety $ origty $ evaluateTerm fillDefault model t

#if 1
EVALUATE_SYM_SIMPLE(SymBool)
EVALUATE_SYM_SIMPLE(SymInteger)
EVALUATE_SYM_BV(SymIntN)
EVALUATE_SYM_BV(SymWordN)
EVALUATE_SYM_FUN(=~>, SymTabularFun)
EVALUATE_SYM_FUN(-~>, SymGeneralFun)
EVALUATE_SYM_BV_SOME(SomeSymIntN, SymIntN)
EVALUATE_SYM_BV_SOME(SomeSymWordN, SymWordN)
#endif

instance (Generic a, EvaluateSym' (Rep a)) => EvaluateSym (Default a) where
  evaluateSym fillDefault model = Default . to . evaluateSym' fillDefault model . from . unDefault

class EvaluateSym' a where
  evaluateSym' :: Bool -> Model -> a c -> a c

instance EvaluateSym' U1 where
  evaluateSym' _ _ = id

instance (EvaluateSym c) => EvaluateSym' (K1 i c) where
  evaluateSym' fillDefault model (K1 v) = K1 $ evaluateSym fillDefault model v

instance (EvaluateSym' a) => EvaluateSym' (M1 i c a) where
  evaluateSym' fillDefault model (M1 v) = M1 $ evaluateSym' fillDefault model v

instance (EvaluateSym' a, EvaluateSym' b) => EvaluateSym' (a :+: b) where
  evaluateSym' fillDefault model (L1 l) = L1 $ evaluateSym' fillDefault model l
  evaluateSym' fillDefault model (R1 r) = R1 $ evaluateSym' fillDefault model r

instance (EvaluateSym' a, EvaluateSym' b) => EvaluateSym' (a :*: b) where
  evaluateSym' fillDefault model (a :*: b) = evaluateSym' fillDefault model a :*: evaluateSym' fillDefault model b
