{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE QuantifiedConstraints #-}

module Grisette.Backend.SBV.Data.SMT.TermRewritingGen where

import Data.Bits
import Data.Data
import Data.Kind
import GHC.TypeLits
import Grisette.Core.Data.Class.BitVector
import Grisette.IR.SymPrim.Data.Prim.InternedTerm.InternedCtors
import Grisette.IR.SymPrim.Data.Prim.InternedTerm.Term
import Grisette.IR.SymPrim.Data.Prim.InternedTerm.TermUtils
import Grisette.IR.SymPrim.Data.Prim.PartialEval.BV
import Grisette.IR.SymPrim.Data.Prim.PartialEval.Bits
import Grisette.IR.SymPrim.Data.Prim.PartialEval.Bool
import Grisette.IR.SymPrim.Data.Prim.PartialEval.Integral
import Grisette.IR.SymPrim.Data.Prim.PartialEval.Num
import Test.Tasty.QuickCheck

class (SupportedPrim b) => TermRewritingSpec a b | a -> b where
  norewriteVer :: a -> Term b
  rewriteVer :: a -> Term b
  wrap :: Term b -> Term b -> a
  same :: a -> Term Bool
  counterExample :: a -> Term Bool
  counterExample = notTerm . same
  symSpec :: String -> a
  symSpec s = wrap (ssymTerm s) (ssymTerm s)
  conSpec :: b -> a
  conSpec v = wrap (conTerm v) (conTerm v)

constructUnarySpec ::
  forall a av b bv.
  ( TermRewritingSpec a av,
    TermRewritingSpec b bv
  ) =>
  (Term av -> Term bv) ->
  (Term av -> Term bv) ->
  a ->
  b
constructUnarySpec construct partial a =
  wrap (construct $ norewriteVer a) (partial $ rewriteVer a)

constructUnarySpec' ::
  forall a av b bv tag.
  ( TermRewritingSpec a av,
    TermRewritingSpec b bv,
    UnaryOp tag av bv
  ) =>
  tag ->
  a ->
  b
constructUnarySpec' tag = constructUnarySpec @a @av @b @bv (constructUnary tag) (partialEvalUnary tag)

constructBinarySpec ::
  forall a av b bv c cv.
  ( TermRewritingSpec a av,
    TermRewritingSpec b bv,
    TermRewritingSpec c cv
  ) =>
  (Term av -> Term bv -> Term cv) ->
  (Term av -> Term bv -> Term cv) ->
  a ->
  b ->
  c
constructBinarySpec construct partial a b =
  wrap
    (construct (norewriteVer a) (norewriteVer b))
    (partial (rewriteVer a) (rewriteVer b))

constructBinarySpec' ::
  forall a av b bv c cv tag.
  ( TermRewritingSpec a av,
    TermRewritingSpec b bv,
    TermRewritingSpec c cv,
    BinaryOp tag av bv cv
  ) =>
  tag ->
  a ->
  b ->
  c
constructBinarySpec' tag = constructBinarySpec @a @av @b @bv @c @cv (constructBinary tag) (partialEvalBinary tag)

constructTernarySpec ::
  forall a av b bv c cv d dv.
  ( TermRewritingSpec a av,
    TermRewritingSpec b bv,
    TermRewritingSpec c cv,
    TermRewritingSpec d dv
  ) =>
  (Term av -> Term bv -> Term cv -> Term dv) ->
  (Term av -> Term bv -> Term cv -> Term dv) ->
  a ->
  b ->
  c ->
  d
constructTernarySpec construct partial a b c =
  wrap
    (construct (norewriteVer a) (norewriteVer b) (norewriteVer c))
    (partial (rewriteVer a) (rewriteVer b) (rewriteVer c))

constructTernarySpec' ::
  forall a av b bv c cv d dv tag.
  ( TermRewritingSpec a av,
    TermRewritingSpec b bv,
    TermRewritingSpec c cv,
    TermRewritingSpec d dv,
    TernaryOp tag av bv cv dv
  ) =>
  tag ->
  a ->
  b ->
  c ->
  d
constructTernarySpec' tag =
  constructTernarySpec @a @av @b @bv @c @cv @d @dv
    (constructTernary tag)
    (partialEvalTernary tag)

notSpec :: (TermRewritingSpec a Bool) => a -> a
notSpec = constructUnarySpec notTerm pevalNotTerm

andSpec :: (TermRewritingSpec a Bool) => a -> a -> a
andSpec = constructBinarySpec andTerm pevalAndTerm

orSpec :: (TermRewritingSpec a Bool) => a -> a -> a
orSpec = constructBinarySpec orTerm pevalOrTerm

eqvSpec :: (TermRewritingSpec a av, TermRewritingSpec b Bool) => a -> a -> b
eqvSpec = constructBinarySpec eqvTerm pevalEqvTerm

iteSpec :: (TermRewritingSpec a Bool, TermRewritingSpec b bv) => a -> b -> b -> b
iteSpec = constructTernarySpec iteTerm pevalITETerm

addNumSpec :: (TermRewritingSpec a av, Num av) => a -> a -> a
addNumSpec = constructBinarySpec addNumTerm pevalAddNumTerm

uminusNumSpec :: (TermRewritingSpec a av, Num av) => a -> a
uminusNumSpec = constructUnarySpec uminusNumTerm pevalUMinusNumTerm

timesNumSpec :: (TermRewritingSpec a av, Num av) => a -> a -> a
timesNumSpec = constructBinarySpec timesNumTerm pevalTimesNumTerm

absNumSpec :: (TermRewritingSpec a av, Num av) => a -> a
absNumSpec = constructUnarySpec absNumTerm pevalAbsNumTerm

signumNumSpec :: (TermRewritingSpec a av, Num av) => a -> a
signumNumSpec = constructUnarySpec signumNumTerm pevalSignumNumTerm

ltNumSpec :: (TermRewritingSpec a av, Num av, Ord av, TermRewritingSpec b Bool) => a -> a -> b
ltNumSpec = constructBinarySpec ltNumTerm pevalLtNumTerm

leNumSpec :: (TermRewritingSpec a av, Num av, Ord av, TermRewritingSpec b Bool) => a -> a -> b
leNumSpec = constructBinarySpec leNumTerm pevalLeNumTerm

andBitsSpec :: (TermRewritingSpec a av, Bits av) => a -> a -> a
andBitsSpec = constructBinarySpec andBitsTerm pevalAndBitsTerm

orBitsSpec :: (TermRewritingSpec a av, Bits av) => a -> a -> a
orBitsSpec = constructBinarySpec orBitsTerm pevalOrBitsTerm

xorBitsSpec :: (TermRewritingSpec a av, Bits av) => a -> a -> a
xorBitsSpec = constructBinarySpec xorBitsTerm pevalXorBitsTerm

complementBitsSpec :: (TermRewritingSpec a av, Bits av) => a -> a
complementBitsSpec = constructUnarySpec complementBitsTerm pevalComplementBitsTerm

shiftBitsSpec :: (TermRewritingSpec a av, Bits av) => a -> Int -> a
shiftBitsSpec a n = constructUnarySpec (`shiftBitsTerm` n) (`pevalShiftBitsTerm` n) a

rotateBitsSpec :: (TermRewritingSpec a av, Bits av) => a -> Int -> a
rotateBitsSpec a n = constructUnarySpec (`rotateBitsTerm` n) (`pevalRotateBitsTerm` n) a

bvconcatSpec ::
  ( TermRewritingSpec a (bv an),
    TermRewritingSpec b (bv bn),
    TermRewritingSpec c (bv (an + bn)),
    forall n. (KnownNat n, 1 <= n) => SupportedPrim (bv n),
    Typeable bv,
    KnownNat an,
    KnownNat bn,
    KnownNat (an + bn),
    1 <= an,
    1 <= bn,
    1 <= an + bn,
    SizedBV bv
  ) =>
  a ->
  b ->
  c
bvconcatSpec = constructBinarySpec bvconcatTerm pevalBVConcatTerm

bvselectSpec ::
  ( TermRewritingSpec a (bv an),
    TermRewritingSpec b (bv bn),
    forall n. (KnownNat n, 1 <= n) => SupportedPrim (bv n),
    Typeable bv,
    KnownNat an,
    KnownNat ix,
    KnownNat bn,
    1 <= an,
    1 <= bn,
    0 <= ix,
    ix + bn <= an,
    SizedBV bv
  ) =>
  proxy ix ->
  proxy bn ->
  a ->
  b
bvselectSpec p1 p2 = constructUnarySpec (bvselectTerm p1 p2) (pevalBVSelectTerm p1 p2)

bvextendSpec ::
  ( TermRewritingSpec a (bv an),
    TermRewritingSpec b (bv bn),
    forall n. (KnownNat n, 1 <= n) => SupportedPrim (bv n),
    Typeable bv,
    KnownNat an,
    KnownNat bn,
    1 <= an,
    1 <= bn,
    an <= bn,
    SizedBV bv
  ) =>
  Bool ->
  proxy bn ->
  a ->
  b
bvextendSpec signed p = constructUnarySpec (bvextendTerm signed p) (pevalBVExtendTerm signed p)

divIntegralSpec :: (TermRewritingSpec a b, Integral b) => a -> a -> a
divIntegralSpec = constructBinarySpec divIntegralTerm pevalDivIntegralTerm

modIntegralSpec :: (TermRewritingSpec a b, Integral b) => a -> a -> a
modIntegralSpec = constructBinarySpec modIntegralTerm pevalModIntegralTerm

quotIntegralSpec :: (TermRewritingSpec a b, Integral b) => a -> a -> a
quotIntegralSpec = constructBinarySpec quotIntegralTerm pevalQuotIntegralTerm

remIntegralSpec :: (TermRewritingSpec a b, Integral b) => a -> a -> a
remIntegralSpec = constructBinarySpec remIntegralTerm pevalRemIntegralTerm

divBoundedIntegralSpec :: (TermRewritingSpec a b, Bounded b, Integral b) => a -> a -> a
divBoundedIntegralSpec = constructBinarySpec divBoundedIntegralTerm pevalDivBoundedIntegralTerm

modBoundedIntegralSpec :: (TermRewritingSpec a b, Bounded b, Integral b) => a -> a -> a
modBoundedIntegralSpec = constructBinarySpec modBoundedIntegralTerm pevalModBoundedIntegralTerm

quotBoundedIntegralSpec :: (TermRewritingSpec a b, Bounded b, Integral b) => a -> a -> a
quotBoundedIntegralSpec = constructBinarySpec quotBoundedIntegralTerm pevalQuotBoundedIntegralTerm

remBoundedIntegralSpec :: (TermRewritingSpec a b, Bounded b, Integral b) => a -> a -> a
remBoundedIntegralSpec = constructBinarySpec remBoundedIntegralTerm pevalRemBoundedIntegralTerm

data BoolOnlySpec = BoolOnlySpec (Term Bool) (Term Bool)

instance Show BoolOnlySpec where
  show (BoolOnlySpec n r) = "BoolOnlySpec { no: " ++ pformat n ++ ", re: " ++ pformat r ++ " }"

instance TermRewritingSpec BoolOnlySpec Bool where
  norewriteVer (BoolOnlySpec n _) = n
  rewriteVer (BoolOnlySpec _ r) = r
  wrap = BoolOnlySpec
  same s = eqvTerm (norewriteVer s) (rewriteVer s)

boolonly :: Int -> Gen BoolOnlySpec
boolonly 0 =
  let s =
        oneof $
          return . symSpec . (++ "bool")
            <$> ["a", "b", "c", "d", "e", "f", "g"]
      r = oneof $ return . conSpec <$> [True, False]
   in oneof [r, s]
boolonly n | n > 0 = do
  v1 <- boolonly (n - 1)
  v2 <- boolonly (n - 1)
  v3 <- boolonly (n - 1)
  oneof
    [ return $ notSpec v1,
      return $ andSpec v1 v2,
      return $ orSpec v1 v2,
      return $ eqvSpec v1 v2,
      return $ iteSpec v1 v2 v3
    ]
boolonly _ = error "Should never be called"

instance Arbitrary BoolOnlySpec where
  arbitrary = sized boolonly

data BoolWithLIASpec = BoolWithLIASpec (Term Bool) (Term Bool)

instance Show BoolWithLIASpec where
  show (BoolWithLIASpec n r) = "BoolWithLIASpec { no: " ++ pformat n ++ ", re: " ++ pformat r ++ " }"

instance TermRewritingSpec BoolWithLIASpec Bool where
  norewriteVer (BoolWithLIASpec n _) = n
  rewriteVer (BoolWithLIASpec _ r) = r
  wrap = BoolWithLIASpec
  same s = eqvTerm (norewriteVer s) (rewriteVer s)

data LIAWithBoolSpec = LIAWithBoolSpec (Term Integer) (Term Integer)

instance Show LIAWithBoolSpec where
  show (LIAWithBoolSpec n r) =
    "LIAWithBoolSpec { no: " ++ pformat n ++ ", re: " ++ pformat r ++ " }"

instance TermRewritingSpec LIAWithBoolSpec Integer where
  norewriteVer (LIAWithBoolSpec n _) = n
  rewriteVer (LIAWithBoolSpec _ r) = r
  wrap = LIAWithBoolSpec
  same s = eqvTerm (norewriteVer s) (rewriteVer s)

boolWithLIA :: Int -> Gen BoolWithLIASpec
boolWithLIA 0 =
  let s =
        oneof $
          return . symSpec . (++ "bool")
            <$> ["a", "b", "c", "d", "e", "f", "g"]
      r = oneof $ return . conSpec <$> [True, False]
   in oneof [r, s]
boolWithLIA n | n > 0 = do
  v1 <- boolWithLIA (n - 1)
  v2 <- boolWithLIA (n - 1)
  v3 <- boolWithLIA (n - 1)
  v1i <- liaWithBool (n - 1)
  v2i <- liaWithBool (n - 1)
  frequency
    [ (1, return $ notSpec v1),
      (1, return $ andSpec v1 v2),
      (1, return $ orSpec v1 v2),
      (1, return $ eqvSpec v1 v2),
      (5, return $ eqvSpec v1i v2i),
      (5, return $ ltNumSpec v1i v2i),
      (5, return $ leNumSpec v1i v2i),
      (1, return $ iteSpec v1 v2 v3)
    ]
boolWithLIA _ = error "Should never be called"

liaWithBool :: Int -> Gen LIAWithBoolSpec
liaWithBool 0 =
  let s =
        oneof $
          return . symSpec . (++ "int")
            <$> ["a", "b", "c", "d", "e", "f", "g"]
      r = conSpec <$> arbitrary
   in oneof [r, s]
liaWithBool n | n > 0 = do
  v1b <- boolWithLIA (n - 1)
  v1i <- liaWithBool (n - 1)
  v2i <- liaWithBool (n - 1)
  oneof
    [ return $ uminusNumSpec v1i,
      return $ absNumSpec v1i,
      return $ signumNumSpec v1i,
      return $ addNumSpec v1i v2i,
      return $ iteSpec v1b v1i v2i
    ]
liaWithBool _ = error "Should never be called"

instance Arbitrary BoolWithLIASpec where
  arbitrary = sized boolWithLIA

instance Arbitrary LIAWithBoolSpec where
  arbitrary = sized liaWithBool

data FixedSizedBVWithBoolSpec bv = FixedSizedBVWithBoolSpec (Term (bv 4)) (Term (bv 4))

instance (SupportedPrim (bv 4)) => Show (FixedSizedBVWithBoolSpec bv) where
  show (FixedSizedBVWithBoolSpec n r) = "FixedSizedBVWithBoolSpec { no: " ++ pformat n ++ ", re: " ++ pformat r ++ " }"

instance (SupportedPrim (bv 4)) => TermRewritingSpec (FixedSizedBVWithBoolSpec bv) (bv 4) where
  norewriteVer (FixedSizedBVWithBoolSpec n _) = n
  rewriteVer (FixedSizedBVWithBoolSpec _ r) = r
  wrap = FixedSizedBVWithBoolSpec
  same s = eqvTerm (norewriteVer s) (rewriteVer s)

data BoolWithFixedSizedBVSpec (bv :: Nat -> Type) = BoolWithFixedSizedBVSpec (Term Bool) (Term Bool)

instance Show (BoolWithFixedSizedBVSpec bv) where
  show (BoolWithFixedSizedBVSpec n r) =
    "BoolWithFixedSizedBVSpec { no: " ++ pformat n ++ ", re: " ++ pformat r ++ " }"

instance TermRewritingSpec (BoolWithFixedSizedBVSpec bv) Bool where
  norewriteVer (BoolWithFixedSizedBVSpec n _) = n
  rewriteVer (BoolWithFixedSizedBVSpec _ r) = r
  wrap = BoolWithFixedSizedBVSpec
  same s = eqvTerm (norewriteVer s) (rewriteVer s)

boolWithFSBV :: forall proxy bv. (SupportedPrim (bv 4), Ord (bv 4), Num (bv 4), Bits (bv 4)) => proxy bv -> Int -> Gen (BoolWithFixedSizedBVSpec bv)
boolWithFSBV _ 0 =
  let s =
        oneof $
          return . symSpec . (++ "bool")
            <$> ["a", "b", "c", "d", "e", "f", "g"]
      r = oneof $ return . conSpec <$> [True, False]
   in oneof [r, s]
boolWithFSBV p n | n > 0 = do
  v1 <- boolWithFSBV p (n - 1)
  v2 <- boolWithFSBV p (n - 1)
  v3 <- boolWithFSBV p (n - 1)
  v1i <- fsbvWithBool p (n - 1)
  v2i <- fsbvWithBool p (n - 1)
  frequency
    [ (1, return $ notSpec v1),
      (1, return $ andSpec v1 v2),
      (1, return $ orSpec v1 v2),
      (1, return $ eqvSpec v1 v2),
      (5, return $ eqvSpec v1i v2i),
      (5, return $ ltNumSpec v1i v2i),
      (5, return $ leNumSpec v1i v2i),
      (1, return $ iteSpec v1 v2 v3)
    ]
boolWithFSBV _ _ = error "Should never be called"

fsbvWithBool ::
  forall proxy bv.
  (SupportedPrim (bv 4), Ord (bv 4), Num (bv 4), Bits (bv 4)) =>
  proxy bv ->
  Int ->
  Gen (FixedSizedBVWithBoolSpec bv)
fsbvWithBool _ 0 =
  let s =
        oneof $
          return . symSpec . (++ "int")
            <$> ["a", "b", "c", "d", "e", "f", "g"]
      r = conSpec . fromInteger <$> arbitrary
   in oneof [r, s]
fsbvWithBool p n | n > 0 = do
  v1b <- boolWithFSBV p (n - 1)
  v1i <- fsbvWithBool p (n - 1)
  v2i <- fsbvWithBool p (n - 1)
  i <- arbitrary
  oneof
    [ return $ uminusNumSpec v1i,
      return $ absNumSpec v1i,
      return $ signumNumSpec v1i,
      return $ addNumSpec v1i v2i,
      return $ timesNumSpec v1i v2i,
      return $ andBitsSpec v1i v2i,
      return $ orBitsSpec v1i v2i,
      return $ xorBitsSpec v1i v2i,
      return $ complementBitsSpec v1i,
      return $ shiftBitsSpec v1i i,
      return $ rotateBitsSpec v1i i,
      return $ iteSpec v1b v1i v2i
    ]
fsbvWithBool _ _ = error "Should never be called"

instance (SupportedPrim (bv 4), Ord (bv 4), Num (bv 4), Bits (bv 4)) => Arbitrary (BoolWithFixedSizedBVSpec bv) where
  arbitrary = sized (boolWithFSBV (Proxy @bv))

instance (SupportedPrim (bv 4), Ord (bv 4), Num (bv 4), Bits (bv 4)) => Arbitrary (FixedSizedBVWithBoolSpec bv) where
  arbitrary = sized (fsbvWithBool Proxy)

data DifferentSizeBVSpec bv (n :: Nat) = DifferentSizeBVSpec (Term (bv n)) (Term (bv n))

instance (SupportedPrim (bv n)) => Show (DifferentSizeBVSpec bv n) where
  show (DifferentSizeBVSpec n r) = "DSizeBVSpec { no: " ++ pformat n ++ ", re: " ++ pformat r ++ " }"

instance (SupportedPrim (bv n)) => TermRewritingSpec (DifferentSizeBVSpec bv n) (bv n) where
  norewriteVer (DifferentSizeBVSpec n _) = n
  rewriteVer (DifferentSizeBVSpec _ r) = r
  wrap = DifferentSizeBVSpec
  same s = eqvTerm (norewriteVer s) (rewriteVer s)

type SupportedBV bv (n :: Nat) =
  (SupportedPrim (bv n), Ord (bv n), Num (bv n), Bits (bv n))

dsbv1 ::
  forall proxy bv.
  ( forall n. (KnownNat n, 1 <= n) => SupportedPrim (bv n),
    SupportedBV bv 1,
    SupportedBV bv 2,
    SupportedBV bv 3,
    SupportedBV bv 4,
    Typeable bv,
    SizedBV bv
  ) =>
  proxy bv ->
  Int ->
  Gen (DifferentSizeBVSpec bv 1)
dsbv1 _ 0 =
  let s =
        oneof $
          return . symSpec . (++ "bv1")
            <$> ["a", "b", "c", "d", "e", "f", "g"]
      r = conSpec . fromInteger <$> arbitrary
   in oneof [r, s]
dsbv1 p depth | depth > 0 = do
  v1 <- dsbv1 p (depth - 1)
  v1' <- dsbv1 p (depth - 1)
  v2 <- dsbv2 p (depth - 1)
  v3 <- dsbv3 p (depth - 1)
  v4 <- dsbv4 p (depth - 1)
  i <- arbitrary
  oneof
    [ return $ uminusNumSpec v1,
      return $ absNumSpec v1,
      return $ signumNumSpec v1,
      return $ addNumSpec v1 v1',
      return $ timesNumSpec v1 v1',
      return $ andBitsSpec v1 v1',
      return $ orBitsSpec v1 v1',
      return $ xorBitsSpec v1 v1',
      return $ complementBitsSpec v1,
      return $ shiftBitsSpec v1 i,
      return $ rotateBitsSpec v1 i,
      return $ bvselectSpec (Proxy @0) (Proxy @1) v4,
      return $ bvselectSpec (Proxy @1) (Proxy @1) v4,
      return $ bvselectSpec (Proxy @2) (Proxy @1) v4,
      return $ bvselectSpec (Proxy @3) (Proxy @1) v4,
      return $ bvselectSpec (Proxy @0) (Proxy @1) v3,
      return $ bvselectSpec (Proxy @1) (Proxy @1) v3,
      return $ bvselectSpec (Proxy @2) (Proxy @1) v3,
      return $ bvselectSpec (Proxy @0) (Proxy @1) v2,
      return $ bvselectSpec (Proxy @1) (Proxy @1) v2,
      return $ bvselectSpec (Proxy @0) (Proxy @1) v1
    ]
dsbv1 _ _ = error "Should never be called"

dsbv2 ::
  forall proxy bv.
  ( forall n. (KnownNat n, 1 <= n) => SupportedPrim (bv n),
    SupportedBV bv 1,
    SupportedBV bv 2,
    SupportedBV bv 3,
    SupportedBV bv 4,
    Typeable bv,
    SizedBV bv
  ) =>
  proxy bv ->
  Int ->
  Gen (DifferentSizeBVSpec bv 2)
dsbv2 _ 0 =
  let s =
        oneof $
          return . symSpec . (++ "bv2")
            <$> ["a", "b", "c", "d", "e", "f", "g"]
      r = conSpec . fromInteger <$> arbitrary
   in oneof [r, s]
dsbv2 p depth | depth > 0 = do
  v1 <- dsbv1 p (depth - 1)
  v1' <- dsbv1 p (depth - 1)
  v2 <- dsbv2 p (depth - 1)
  v2' <- dsbv2 p (depth - 1)
  v3 <- dsbv3 p (depth - 1)
  v4 <- dsbv4 p (depth - 1)
  i <- arbitrary
  oneof
    [ return $ uminusNumSpec v2,
      return $ absNumSpec v2,
      return $ signumNumSpec v2,
      return $ addNumSpec v2 v2',
      return $ timesNumSpec v2 v2',
      return $ andBitsSpec v2 v2',
      return $ orBitsSpec v2 v2',
      return $ xorBitsSpec v2 v2',
      return $ complementBitsSpec v2,
      return $ shiftBitsSpec v2 i,
      return $ rotateBitsSpec v2 i,
      return $ bvselectSpec (Proxy @0) (Proxy @2) v4,
      return $ bvselectSpec (Proxy @1) (Proxy @2) v4,
      return $ bvselectSpec (Proxy @2) (Proxy @2) v4,
      return $ bvselectSpec (Proxy @0) (Proxy @2) v3,
      return $ bvselectSpec (Proxy @1) (Proxy @2) v3,
      return $ bvselectSpec (Proxy @0) (Proxy @2) v2,
      return $ bvconcatSpec v1 v1',
      return $ bvextendSpec False (Proxy @2) v1,
      return $ bvextendSpec True (Proxy @2) v1
    ]
dsbv2 _ _ = error "Should never be called"

dsbv3 ::
  forall proxy bv.
  ( forall n. (KnownNat n, 1 <= n) => SupportedPrim (bv n),
    SupportedBV bv 1,
    SupportedBV bv 2,
    SupportedBV bv 3,
    SupportedBV bv 4,
    Typeable bv,
    SizedBV bv
  ) =>
  proxy bv ->
  Int ->
  Gen (DifferentSizeBVSpec bv 3)
dsbv3 _ 0 =
  let s =
        oneof $
          return . symSpec . (++ "bv3")
            <$> ["a", "b", "c", "d", "e", "f", "g"]
      r = conSpec . fromInteger <$> arbitrary
   in oneof [r, s]
dsbv3 p depth | depth > 0 = do
  v1 <- dsbv1 p (depth - 1)
  v2 <- dsbv2 p (depth - 1)
  v3 <- dsbv3 p (depth - 1)
  v3' <- dsbv3 p (depth - 1)
  v4 <- dsbv4 p (depth - 1)
  i <- arbitrary
  oneof
    [ return $ uminusNumSpec v3,
      return $ absNumSpec v3,
      return $ signumNumSpec v3,
      return $ addNumSpec v3 v3',
      return $ timesNumSpec v3 v3',
      return $ andBitsSpec v3 v3',
      return $ orBitsSpec v3 v3',
      return $ xorBitsSpec v3 v3',
      return $ complementBitsSpec v3,
      return $ shiftBitsSpec v3 i,
      return $ rotateBitsSpec v3 i,
      return $ bvselectSpec (Proxy @0) (Proxy @3) v4,
      return $ bvselectSpec (Proxy @1) (Proxy @3) v4,
      return $ bvselectSpec (Proxy @0) (Proxy @3) v3,
      return $ bvconcatSpec v1 v2,
      return $ bvconcatSpec v2 v1,
      return $ bvextendSpec False (Proxy @3) v1,
      return $ bvextendSpec True (Proxy @3) v1,
      return $ bvextendSpec False (Proxy @3) v2,
      return $ bvextendSpec True (Proxy @3) v2
    ]
dsbv3 _ _ = error "Should never be called"

dsbv4 ::
  forall proxy bv.
  ( forall n. (KnownNat n, 1 <= n) => SupportedPrim (bv n),
    SupportedBV bv 1,
    SupportedBV bv 2,
    SupportedBV bv 3,
    SupportedBV bv 4,
    Typeable bv,
    SizedBV bv
  ) =>
  proxy bv ->
  Int ->
  Gen (DifferentSizeBVSpec bv 4)
dsbv4 _ 0 =
  let s =
        oneof $
          return . symSpec . (++ "bv4")
            <$> ["a", "b", "c", "d", "e", "f", "g"]
      r = conSpec . fromInteger <$> arbitrary
   in oneof [r, s]
dsbv4 p depth | depth > 0 = do
  v1 <- dsbv1 p (depth - 1)
  v2 <- dsbv2 p (depth - 1)
  v2' <- dsbv2 p (depth - 1)
  v3 <- dsbv3 p (depth - 1)
  v4 <- dsbv4 p (depth - 1)
  v4' <- dsbv4 p (depth - 1)
  i <- arbitrary
  oneof
    [ return $ uminusNumSpec v4,
      return $ absNumSpec v4,
      return $ signumNumSpec v4,
      return $ addNumSpec v4 v4',
      return $ timesNumSpec v4 v4',
      return $ andBitsSpec v4 v4',
      return $ orBitsSpec v4 v4',
      return $ xorBitsSpec v4 v4',
      return $ complementBitsSpec v4,
      return $ shiftBitsSpec v4 i,
      return $ rotateBitsSpec v4 i,
      return $ bvselectSpec (Proxy @0) (Proxy @4) v4,
      return $ bvconcatSpec v1 v3,
      return $ bvconcatSpec v2 v2',
      return $ bvconcatSpec v3 v1,
      return $ bvextendSpec False (Proxy @4) v1,
      return $ bvextendSpec True (Proxy @4) v1,
      return $ bvextendSpec False (Proxy @4) v2,
      return $ bvextendSpec True (Proxy @4) v2,
      return $ bvextendSpec False (Proxy @4) v3,
      return $ bvextendSpec True (Proxy @4) v3
    ]
dsbv4 _ _ = error "Should never be called"

instance
  ( forall n. (KnownNat n, 1 <= n) => SupportedPrim (bv n),
    SupportedBV bv 1,
    SupportedBV bv 2,
    SupportedBV bv 3,
    SupportedBV bv 4,
    Typeable bv,
    SizedBV bv
  ) =>
  Arbitrary (DifferentSizeBVSpec bv 4)
  where
  arbitrary = sized (dsbv4 Proxy)

data GeneralSpec s = GeneralSpec (Term s) (Term s)

instance (SupportedPrim s) => Show (GeneralSpec s) where
  show (GeneralSpec n r) = "GeneralSpec { no: " ++ pformat n ++ ", re: " ++ pformat r ++ " }"

instance (SupportedPrim s) => TermRewritingSpec (GeneralSpec s) s where
  norewriteVer (GeneralSpec n _) = n
  rewriteVer (GeneralSpec _ r) = r
  wrap = GeneralSpec
  same s = eqvTerm (norewriteVer s) (rewriteVer s)
