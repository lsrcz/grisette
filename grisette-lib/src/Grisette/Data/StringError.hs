{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Grisette.Data.StringError
  ( StringError (..),
  )
where

import Grisette.Core.Data.Class.Bool
import Grisette.Core.Data.Class.Error
import Grisette.Core.Data.Class.Integer
import Grisette.Core.Data.Class.Mergeable
import Grisette.Core.Data.Class.PrimWrapper
import Grisette.Core.Data.Class.ToCon
import Grisette.Core.Data.Class.ToSym

newtype StringError = StringError String deriving (Eq, Ord)

instance Show StringError where
  show (StringError str) = str

instance (SymBoolOp bool) => Mergeable bool StringError where
  mergingStrategy = SortedStrategy (\(StringError s) -> s) (\_ -> SimpleStrategy $ \_ t _ -> t)

instance (SymBoolOp bool) => SEq bool StringError where
  l ==~ r = conc $ l == r

instance TransformError ArithException StringError where
  transformError = StringError . show

instance ToSym StringError StringError where
  toSym = id

instance ToCon StringError StringError where
  toCon = Just
