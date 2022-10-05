{-# LANGUAGE CPP #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Grisette.Core.Data.Class.Evaluate
  ( EvaluateSym (..),
    evaluateSymToCon,
  )
where

import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.Trans.Maybe
import qualified Control.Monad.Writer.Lazy as WriterLazy
import qualified Control.Monad.Writer.Strict as WriterStrict
import qualified Data.ByteString as B
import Data.Functor.Sum
import Data.Int
import Data.Maybe
import Data.Word
import Generics.Deriving
import Generics.Deriving.Instances ()
import Grisette.Core.Data.Class.ModelOps
import Grisette.Core.Data.Class.ToCon

-- $setup
-- >>> import Grisette.Core
-- >>> import Grisette.IR.SymPrim
-- >>> import Data.Proxy
-- >>> :set -XTypeApplications

-- | Evaluating symbolic variables with some model.
--
-- Usually the model is created with the solver, rather than by hand.
class EvaluateSym model a where
  -- | Evaluate a symbolic variable with some model, possibly fill in values for the missing variables.
  --
  -- >>> let model = insertValue emptyModel (termSymbol (Proxy @Integer) (SimpleSymbol "a")) (1 :: Integer) :: Model
  -- >>> evaluateSym False model ([ssymb "a", ssymb "b"] :: [SymInteger])
  -- [1I,b]
  -- >>> evaluateSym True model ([ssymb "a", ssymb "b"] :: [SymInteger])
  -- [1I,0I]
  evaluateSym :: Bool -> model -> a -> a

instance (Generic a, EvaluateSym' model (Rep a)) => EvaluateSym model (Default a) where
  evaluateSym fillDefault model = Default . to . evaluateSym' fillDefault model . from . unDefault

class EvaluateSym' model a where
  evaluateSym' :: Bool -> model -> a c -> a c

instance EvaluateSym' model U1 where
  evaluateSym' _ _ = id

instance EvaluateSym model c => EvaluateSym' model (K1 i c) where
  evaluateSym' fillDefault model (K1 v) = K1 $ evaluateSym fillDefault model v

instance EvaluateSym' model a => EvaluateSym' model (M1 i c a) where
  evaluateSym' fillDefault model (M1 v) = M1 $ evaluateSym' fillDefault model v

instance (EvaluateSym' model a, EvaluateSym' model b) => EvaluateSym' model (a :+: b) where
  evaluateSym' fillDefault model (L1 l) = L1 $ evaluateSym' fillDefault model l
  evaluateSym' fillDefault model (R1 r) = R1 $ evaluateSym' fillDefault model r

instance (EvaluateSym' model a, EvaluateSym' model b) => EvaluateSym' model (a :*: b) where
  evaluateSym' fillDefault model (a :*: b) = evaluateSym' fillDefault model a :*: evaluateSym' fillDefault model b

-- | Evaluate a symbolic variable with some model, fill in values for the missing variables,
-- and transform to concrete ones
--
-- >>> let model = insertValue emptyModel (termSymbol (Proxy @Integer) (SimpleSymbol "a")) (1 :: Integer) :: Model
-- >>> evaluateSymToCon model ([ssymb "a", ssymb "b"] :: [SymInteger]) :: [Integer]
-- [1,0]
evaluateSymToCon :: (ToCon a b, EvaluateSym model a) => model -> a -> b
evaluateSymToCon model a = fromJust $ toCon $ evaluateSym True model a

-- instances

#define CONCRETE_EVALUATESYM(type) \
instance EvaluateSym model type where \
  evaluateSym _ _ = id

CONCRETE_EVALUATESYM (Bool)
CONCRETE_EVALUATESYM (Integer)
CONCRETE_EVALUATESYM (Char)
CONCRETE_EVALUATESYM (Int)
CONCRETE_EVALUATESYM (Int8)
CONCRETE_EVALUATESYM (Int16)
CONCRETE_EVALUATESYM (Int32)
CONCRETE_EVALUATESYM (Int64)
CONCRETE_EVALUATESYM (Word)
CONCRETE_EVALUATESYM (Word8)
CONCRETE_EVALUATESYM (Word16)
CONCRETE_EVALUATESYM (Word32)
CONCRETE_EVALUATESYM (Word64)
CONCRETE_EVALUATESYM (B.ByteString)

-- ()
instance EvaluateSym model () where
  evaluateSym _ _ = id

-- Either
deriving via (Default (Either a b)) instance (EvaluateSym model a, EvaluateSym model b) => EvaluateSym model (Either a b)

-- Maybe
deriving via (Default (Maybe a)) instance (EvaluateSym model a) => EvaluateSym model (Maybe a)

-- List
deriving via (Default [a]) instance (EvaluateSym model a) => EvaluateSym model [a]

-- (,)
deriving via (Default (a, b)) instance (EvaluateSym model a, EvaluateSym model b) => EvaluateSym model (a, b)

-- (,,)
deriving via (Default (a, b, c)) instance (EvaluateSym model a, EvaluateSym model b, EvaluateSym model c) => EvaluateSym model (a, b, c)

-- (,,,)
deriving via
  (Default (a, b, c, d))
  instance
    (EvaluateSym model a, EvaluateSym model b, EvaluateSym model c, EvaluateSym model d) => EvaluateSym model (a, b, c, d)

-- (,,,,)
deriving via
  (Default (a, b, c, d, e))
  instance
    (EvaluateSym model a, EvaluateSym model b, EvaluateSym model c, EvaluateSym model d, EvaluateSym model e) =>
    EvaluateSym model (a, b, c, d, e)

-- (,,,,,)
deriving via
  (Default (a, b, c, d, e, f))
  instance
    (EvaluateSym model a, EvaluateSym model b, EvaluateSym model c, EvaluateSym model d, EvaluateSym model e, EvaluateSym model f) =>
    EvaluateSym model (a, b, c, d, e, f)

-- (,,,,,,)
deriving via
  (Default (a, b, c, d, e, f, g))
  instance
    ( EvaluateSym model a,
      EvaluateSym model b,
      EvaluateSym model c,
      EvaluateSym model d,
      EvaluateSym model e,
      EvaluateSym model f,
      EvaluateSym model g
    ) =>
    EvaluateSym model (a, b, c, d, e, f, g)

-- (,,,,,,,)
deriving via
  (Default (a, b, c, d, e, f, g, h))
  instance
    ( EvaluateSym model a,
      EvaluateSym model b,
      EvaluateSym model c,
      EvaluateSym model d,
      EvaluateSym model e,
      EvaluateSym model f,
      EvaluateSym model g,
      EvaluateSym model h
    ) =>
    EvaluateSym model ((,,,,,,,) a b c d e f g h)

-- MaybeT
instance (EvaluateSym model (m (Maybe a))) => EvaluateSym model (MaybeT m a) where
  evaluateSym fillDefault model (MaybeT v) = MaybeT $ evaluateSym fillDefault model v

-- ExceptT
instance (EvaluateSym model (m (Either e a))) => EvaluateSym model (ExceptT e m a) where
  evaluateSym fillDefault model (ExceptT v) = ExceptT $ evaluateSym fillDefault model v

-- Sum
deriving via
  (Default (Sum f g a))
  instance
    (EvaluateSym model (f a), EvaluateSym model (g a)) => EvaluateSym model (Sum f g a)

-- WriterT
instance EvaluateSym model (m (a, s)) => EvaluateSym model (WriterLazy.WriterT s m a) where
  evaluateSym fillDefault model (WriterLazy.WriterT v) = WriterLazy.WriterT $ evaluateSym fillDefault model v

instance EvaluateSym model (m (a, s)) => EvaluateSym model (WriterStrict.WriterT s m a) where
  evaluateSym fillDefault model (WriterStrict.WriterT v) = WriterStrict.WriterT $ evaluateSym fillDefault model v

-- Identity
instance EvaluateSym model a => EvaluateSym model (Identity a) where
  evaluateSym fillDefault model (Identity a) = Identity $ evaluateSym fillDefault model a

-- IdentityT
instance EvaluateSym model (m a) => EvaluateSym model (IdentityT m a) where
  evaluateSym fillDefault model (IdentityT a) = IdentityT $ evaluateSym fillDefault model a
