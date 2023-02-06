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

import Control.DeepSeq
import Data.Hashable
import GHC.Generics
import GHC.TypeNats
import Grisette.Core.Data.Class.Evaluate
import Grisette.Core.Data.Class.ExtractSymbolics
import Grisette.Core.Data.Class.Solvable
import Grisette.IR.SymPrim.Data.BV
import {-# SOURCE #-} Grisette.IR.SymPrim.Data.Prim.InternedTerm.Term
import Grisette.IR.SymPrim.Data.TabularFun
import Language.Haskell.TH.Syntax

newtype SymBool = SymBool {underlyingBoolTerm :: Term Bool}

newtype SymIntN (n :: Nat) = SymIntN {underlyingIntNTerm :: Term (IntN n)}

newtype SymWordN (n :: Nat) = SymWordN {underlyingWordNTerm :: Term (WordN n)}

data SomeSymIntN where
  SomeSymIntN :: (KnownNat n, 1 <= n) => SymIntN n -> SomeSymIntN

data SomeSymWordN where
  SomeSymWordN :: (KnownNat n, 1 <= n) => SymWordN n -> SomeSymWordN

data sa =~> sb where
  SymTabularFun :: (LinkedRep ca sa, LinkedRep cb sb) => Term (ca =-> cb) -> sa =~> sb

data sa -~> sb where
  SymGeneralFun :: (LinkedRep ca sa, LinkedRep cb sb) => Term (ca --> cb) -> sa -~> sb

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

instance Solvable Bool SymBool

instance Eq SymBool

instance Lift SymBool

instance NFData SymBool

instance Show SymBool

instance Hashable SymBool

instance EvaluateSym SymBool

instance ExtractSymbolics SymBool

newtype SymInteger = SymInteger {underlyingIntegerTerm :: Term Integer}

instance Solvable Integer SymInteger

instance Eq SymInteger

instance Lift SymInteger

instance NFData SymInteger

instance Show SymInteger

instance Hashable SymInteger

instance EvaluateSym SymInteger

instance ExtractSymbolics SymInteger

instance (KnownNat n, 1 <= n) => Solvable (WordN n) (SymWordN n)

instance (KnownNat n, 1 <= n) => Solvable (IntN n) (SymIntN n)

instance (SupportedPrim ca, SupportedPrim cb, LinkedRep ca sa, LinkedRep cb sb) => Solvable (ca --> cb) (sa -~> sb)

instance (SupportedPrim ca, SupportedPrim cb, LinkedRep ca sa, LinkedRep cb sb) => Solvable (ca =-> cb) (sa =~> sb)
