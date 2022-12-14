{-# LANGUAGE CPP #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Grisette.Core.Data.Class.Evaluate
  ( GEvaluateSym (..),
    gevaluateSymToCon,
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
class GEvaluateSym model a where
  -- | Evaluate a symbolic variable with some model, possibly fill in values for the missing variables.
  --
  -- >>> let model = insertValue (SimpleSymbol "a") (1 :: Integer) emptyModel :: Model
  -- >>> gevaluateSym False model ([ssymb "a", ssymb "b"] :: [SymInteger])
  -- [1I,b]
  -- >>> gevaluateSym True model ([ssymb "a", ssymb "b"] :: [SymInteger])
  -- [1I,0I]
  gevaluateSym :: Bool -> model -> a -> a

instance (Generic a, GEvaluateSym' model (Rep a)) => GEvaluateSym model (Default a) where
  gevaluateSym fillDefault model = Default . to . gevaluateSym' fillDefault model . from . unDefault

class GEvaluateSym' model a where
  gevaluateSym' :: Bool -> model -> a c -> a c

instance GEvaluateSym' model U1 where
  gevaluateSym' _ _ = id

instance GEvaluateSym model c => GEvaluateSym' model (K1 i c) where
  gevaluateSym' fillDefault model (K1 v) = K1 $ gevaluateSym fillDefault model v

instance GEvaluateSym' model a => GEvaluateSym' model (M1 i c a) where
  gevaluateSym' fillDefault model (M1 v) = M1 $ gevaluateSym' fillDefault model v

instance (GEvaluateSym' model a, GEvaluateSym' model b) => GEvaluateSym' model (a :+: b) where
  gevaluateSym' fillDefault model (L1 l) = L1 $ gevaluateSym' fillDefault model l
  gevaluateSym' fillDefault model (R1 r) = R1 $ gevaluateSym' fillDefault model r

instance (GEvaluateSym' model a, GEvaluateSym' model b) => GEvaluateSym' model (a :*: b) where
  gevaluateSym' fillDefault model (a :*: b) = gevaluateSym' fillDefault model a :*: gevaluateSym' fillDefault model b

-- | Evaluate a symbolic variable with some model, fill in values for the missing variables,
-- and transform to concrete ones
--
-- >>> let model = insertValue (SimpleSymbol "a") (1 :: Integer) emptyModel :: Model
-- >>> gevaluateSymToCon model ([ssymb "a", ssymb "b"] :: [SymInteger]) :: [Integer]
-- [1,0]
gevaluateSymToCon :: (ToCon a b, GEvaluateSym model a) => model -> a -> b
gevaluateSymToCon model a = fromJust $ toCon $ gevaluateSym True model a

-- instances

#define CONCRETE_EVALUATESYM(type) \
instance GEvaluateSym model type where \
  gevaluateSym _ _ = id

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
CONCRETE_EVALUATESYM(B.ByteString)
#endif

-- ()
instance GEvaluateSym model () where
  gevaluateSym _ _ = id

-- Either
deriving via (Default (Either a b)) instance (GEvaluateSym model a, GEvaluateSym model b) => GEvaluateSym model (Either a b)

-- Maybe
deriving via (Default (Maybe a)) instance (GEvaluateSym model a) => GEvaluateSym model (Maybe a)

-- List
deriving via (Default [a]) instance (GEvaluateSym model a) => GEvaluateSym model [a]

-- (,)
deriving via (Default (a, b)) instance (GEvaluateSym model a, GEvaluateSym model b) => GEvaluateSym model (a, b)

-- (,,)
deriving via (Default (a, b, c)) instance (GEvaluateSym model a, GEvaluateSym model b, GEvaluateSym model c) => GEvaluateSym model (a, b, c)

-- (,,,)
deriving via
  (Default (a, b, c, d))
  instance
    (GEvaluateSym model a, GEvaluateSym model b, GEvaluateSym model c, GEvaluateSym model d) => GEvaluateSym model (a, b, c, d)

-- (,,,,)
deriving via
  (Default (a, b, c, d, e))
  instance
    (GEvaluateSym model a, GEvaluateSym model b, GEvaluateSym model c, GEvaluateSym model d, GEvaluateSym model e) =>
    GEvaluateSym model (a, b, c, d, e)

-- (,,,,,)
deriving via
  (Default (a, b, c, d, e, f))
  instance
    (GEvaluateSym model a, GEvaluateSym model b, GEvaluateSym model c, GEvaluateSym model d, GEvaluateSym model e, GEvaluateSym model f) =>
    GEvaluateSym model (a, b, c, d, e, f)

-- (,,,,,,)
deriving via
  (Default (a, b, c, d, e, f, g))
  instance
    ( GEvaluateSym model a,
      GEvaluateSym model b,
      GEvaluateSym model c,
      GEvaluateSym model d,
      GEvaluateSym model e,
      GEvaluateSym model f,
      GEvaluateSym model g
    ) =>
    GEvaluateSym model (a, b, c, d, e, f, g)

-- (,,,,,,,)
deriving via
  (Default (a, b, c, d, e, f, g, h))
  instance
    ( GEvaluateSym model a,
      GEvaluateSym model b,
      GEvaluateSym model c,
      GEvaluateSym model d,
      GEvaluateSym model e,
      GEvaluateSym model f,
      GEvaluateSym model g,
      GEvaluateSym model h
    ) =>
    GEvaluateSym model ((,,,,,,,) a b c d e f g h)

-- MaybeT
instance (GEvaluateSym model (m (Maybe a))) => GEvaluateSym model (MaybeT m a) where
  gevaluateSym fillDefault model (MaybeT v) = MaybeT $ gevaluateSym fillDefault model v

-- ExceptT
instance (GEvaluateSym model (m (Either e a))) => GEvaluateSym model (ExceptT e m a) where
  gevaluateSym fillDefault model (ExceptT v) = ExceptT $ gevaluateSym fillDefault model v

-- Sum
deriving via
  (Default (Sum f g a))
  instance
    (GEvaluateSym model (f a), GEvaluateSym model (g a)) => GEvaluateSym model (Sum f g a)

-- WriterT
instance GEvaluateSym model (m (a, s)) => GEvaluateSym model (WriterLazy.WriterT s m a) where
  gevaluateSym fillDefault model (WriterLazy.WriterT v) = WriterLazy.WriterT $ gevaluateSym fillDefault model v

instance GEvaluateSym model (m (a, s)) => GEvaluateSym model (WriterStrict.WriterT s m a) where
  gevaluateSym fillDefault model (WriterStrict.WriterT v) = WriterStrict.WriterT $ gevaluateSym fillDefault model v

-- Identity
instance GEvaluateSym model a => GEvaluateSym model (Identity a) where
  gevaluateSym fillDefault model (Identity a) = Identity $ gevaluateSym fillDefault model a

-- IdentityT
instance GEvaluateSym model (m a) => GEvaluateSym model (IdentityT m a) where
  gevaluateSym fillDefault model (IdentityT a) = IdentityT $ gevaluateSym fillDefault model a
