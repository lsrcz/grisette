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
    SomeSymIntN (..),
    SomeSymWordN (..),
    type (=~>) (..),
    type (-~>) (..),
    SomeSym (..),
    AllSyms (..),
    unarySomeSymIntN,
    unarySomeSymIntNR1,
    binSomeSymIntN,
    binSomeSymIntNR1,
    binSomeSymIntNR2,
    unarySomeSymWordN,
    unarySomeSymWordNR1,
    binSomeSymWordN,
    binSomeSymWordNR1,
    binSomeSymWordNR2,
  )
where

import Control.DeepSeq (NFData)
import Data.Hashable (Hashable)
import Data.String (IsString)
import GHC.TypeNats (KnownNat, Nat, type (<=))
import Grisette.Core.Data.BV (IntN, WordN)
import Grisette.Core.Data.Class.ExtractSymbolics
  ( ExtractSymbolics,
  )
import {-# SOURCE #-} Grisette.IR.SymPrim.Data.Prim.InternedTerm.Term
  ( LinkedRep,
    SupportedPrim,
    Term,
    type (-->),
  )
import Grisette.IR.SymPrim.Data.TabularFun (type (=->))
import Language.Haskell.TH.Syntax (Lift)

newtype SymBool = SymBool {underlyingBoolTerm :: Term Bool}

newtype SymIntN (n :: Nat) = SymIntN {underlyingIntNTerm :: Term (IntN n)}

instance (KnownNat n, 1 <= n) => IsString (SymIntN n)

instance (KnownNat n, 1 <= n) => IsString (SymWordN n)

newtype SymWordN (n :: Nat) = SymWordN {underlyingWordNTerm :: Term (WordN n)}

data SomeSymIntN where
  SomeSymIntN :: (KnownNat n, 1 <= n) => SymIntN n -> SomeSymIntN

data SomeSymWordN where
  SomeSymWordN :: (KnownNat n, 1 <= n) => SymWordN n -> SomeSymWordN

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

unarySomeSymIntN :: (forall n. (KnownNat n, 1 <= n) => SymIntN n -> r) -> String -> SomeSymIntN -> r
unarySomeSymIntNR1 :: (forall n. (KnownNat n, 1 <= n) => SymIntN n -> SymIntN n) -> String -> SomeSymIntN -> SomeSymIntN
binSomeSymIntN :: (forall n. (KnownNat n, 1 <= n) => SymIntN n -> SymIntN n -> r) -> String -> SomeSymIntN -> SomeSymIntN -> r
binSomeSymIntNR1 :: (forall n. (KnownNat n, 1 <= n) => SymIntN n -> SymIntN n -> SymIntN n) -> String -> SomeSymIntN -> SomeSymIntN -> SomeSymIntN
binSomeSymIntNR2 :: (forall n. (KnownNat n, 1 <= n) => SymIntN n -> SymIntN n -> (SymIntN n, SymIntN n)) -> String -> SomeSymIntN -> SomeSymIntN -> (SomeSymIntN, SomeSymIntN)
unarySomeSymWordN :: (forall n. (KnownNat n, 1 <= n) => SymWordN n -> r) -> String -> SomeSymWordN -> r
unarySomeSymWordNR1 :: (forall n. (KnownNat n, 1 <= n) => SymWordN n -> SymWordN n) -> String -> SomeSymWordN -> SomeSymWordN
binSomeSymWordN :: (forall n. (KnownNat n, 1 <= n) => SymWordN n -> SymWordN n -> r) -> String -> SomeSymWordN -> SomeSymWordN -> r
binSomeSymWordNR1 :: (forall n. (KnownNat n, 1 <= n) => SymWordN n -> SymWordN n -> SymWordN n) -> String -> SomeSymWordN -> SomeSymWordN -> SomeSymWordN
binSomeSymWordNR2 :: (forall n. (KnownNat n, 1 <= n) => SymWordN n -> SymWordN n -> (SymWordN n, SymWordN n)) -> String -> SomeSymWordN -> SomeSymWordN -> (SomeSymWordN, SomeSymWordN)

instance LinkedRep Bool SymBool

instance Eq SymBool

instance Lift SymBool

instance NFData SymBool

instance Show SymBool

instance Hashable SymBool

instance ExtractSymbolics SymBool

instance IsString SymBool

newtype SymInteger = SymInteger {underlyingIntegerTerm :: Term Integer}

instance Eq SymInteger

instance Lift SymInteger

instance NFData SymInteger

instance Show SymInteger

instance Hashable SymInteger

instance ExtractSymbolics SymInteger

instance IsString SymInteger

data SomeSym where
  SomeSym :: (LinkedRep con sym) => sym -> SomeSym

class AllSyms a where
  allSymsS :: a -> [SomeSym] -> [SomeSym]
  allSymsS a l = allSyms a ++ l
  allSyms :: a -> [SomeSym]
  allSyms a = allSymsS a []
  {-# MINIMAL allSymsS | allSyms #-}
