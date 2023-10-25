{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Grisette.IR.SymPrim.Data.SymPrim
  ( SymBool (..),
    SymInteger (..),
    SymIntN (..),
    SymWordN (..),
    type (=~>) (..),
    type (-~>) (..),
  )
where

import Data.String (IsString)
import GHC.TypeNats (KnownNat, Nat, type (<=))
import Grisette.Core.Data.BV (IntN, WordN)
import {-# SOURCE #-} Grisette.IR.SymPrim.Data.Prim.InternedTerm.Term
  ( LinkedRep,
    SupportedPrim,
    Term,
    type (-->),
  )
import Grisette.IR.SymPrim.Data.TabularFun (type (=->))

newtype SymBool = SymBool {underlyingBoolTerm :: Term Bool}

newtype SymIntN (n :: Nat) = SymIntN {underlyingIntNTerm :: Term (IntN n)}

instance (KnownNat n, 1 <= n) => IsString (SymIntN n)

instance (KnownNat n, 1 <= n) => IsString (SymWordN n)

newtype SymWordN (n :: Nat) = SymWordN {underlyingWordNTerm :: Term (WordN n)}

data sa =~> sb where
  SymTabularFun :: (LinkedRep ca sa, LinkedRep cb sb) => Term (ca =-> cb) -> sa =~> sb

data sa -~> sb where
  SymGeneralFun :: (LinkedRep ca sa, LinkedRep cb sb) => Term (ca --> cb) -> sa -~> sb

instance
  (SupportedPrim ca, SupportedPrim cb, LinkedRep ca sa, LinkedRep cb sb) =>
  IsString (sa =~> sb)

instance
  (SupportedPrim ca, SupportedPrim cb, LinkedRep ca sa, LinkedRep cb sb) =>
  IsString (sa -~> sb)

instance IsString SymBool

newtype SymInteger = SymInteger {underlyingIntegerTerm :: Term Integer}

instance IsString SymInteger
