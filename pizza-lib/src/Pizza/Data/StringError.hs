{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Pizza.Data.StringError
  ( StringError (..),
  )
where

import Pizza.Core.Data.Class.Bool
import Pizza.Core.Data.Class.Error
import Pizza.Core.Data.Class.Integer
import Pizza.Core.Data.Class.Mergeable
import Pizza.Core.Data.Class.PrimWrapper
import Pizza.Core.Data.Class.ToCon
import Pizza.Core.Data.Class.ToSym

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
