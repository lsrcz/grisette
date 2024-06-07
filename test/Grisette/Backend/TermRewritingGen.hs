{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Grisette.Backend.TermRewritingGen
  ( TermRewritingSpec (..),
    GeneralSpec (..),
    DifferentSizeBVSpec (..),
    FixedSizedBVWithBoolSpec (..),
    BoolWithLIASpec (..),
    LIAWithBoolSpec (..),
    BoolOnlySpec (..),
    constructUnarySpec,
    constructUnarySpec',
    constructBinarySpec,
    constructBinarySpec',
    constructTernarySpec,
    constructTernarySpec',
    divIntegralSpec,
    modIntegralSpec,
    quotIntegralSpec,
    remIntegralSpec,
    negNumSpec,
    mulNumSpec,
    addNumSpec,
    absNumSpec,
    signumNumSpec,
    ltOrdSpec,
    leOrdSpec,
    iteSpec,
    eqvSpec,
    notSpec,
    andSpec,
    orSpec,
    shiftLeftSpec,
    shiftRightSpec,
    rotateLeftSpec,
    rotateRightSpec,
    xorBitsSpec,
    fpTraitSpec,
    fdivSpec,
    recipSpec,
    sqrtSpec,
    IEEEFP32Spec (..),
    IEEEFP32BoolOpSpec (..),
    FPRoundingModeSpec (..),
    FPRoundingModeBoolOpSpec (..),
  )
where

import Data.Bits (FiniteBits)
import Data.Data (Proxy (Proxy), Typeable)
import Data.Functor ((<&>))
import Data.Kind (Type)
import qualified Data.Text as T
import GHC.TypeLits (KnownNat, Nat, type (+), type (<=))
import Grisette (Identifier, SizedBV, SymRotate, SymShift, withInfo)
import Grisette.Internal.SymPrim.FP
  ( FP,
    FP32,
    FPRoundingMode (RNA, RNE, RTN, RTP, RTZ),
    ValidFP,
  )
import Grisette.Internal.SymPrim.Prim.Internal.Term
  ( PEvalFloatingTerm (pevalSqrtTerm),
    PEvalFractionalTerm (pevalRecipTerm),
    fdivTerm,
    sqrtTerm,
  )
import Grisette.Internal.SymPrim.Prim.Term
  ( BinaryOp (pevalBinary),
    FPTrait
      ( FPIsInfinite,
        FPIsNaN,
        FPIsNegative,
        FPIsNegativeInfinite,
        FPIsNegativeZero,
        FPIsNormal,
        FPIsPoint,
        FPIsPositive,
        FPIsPositiveInfinite,
        FPIsPositiveZero,
        FPIsSubnormal,
        FPIsZero
      ),
    PEvalBVTerm
      ( pevalBVConcatTerm,
        pevalBVExtendTerm,
        pevalBVSelectTerm
      ),
    PEvalBitwiseTerm
      ( pevalAndBitsTerm,
        pevalComplementBitsTerm,
        pevalOrBitsTerm,
        pevalXorBitsTerm
      ),
    PEvalDivModIntegralTerm
      ( pevalDivIntegralTerm,
        pevalModIntegralTerm,
        pevalQuotIntegralTerm,
        pevalRemIntegralTerm
      ),
    PEvalFractionalTerm (pevalFdivTerm),
    PEvalNumTerm
      ( pevalAbsNumTerm,
        pevalAddNumTerm,
        pevalMulNumTerm,
        pevalNegNumTerm,
        pevalSignumNumTerm
      ),
    PEvalOrdTerm (pevalLeOrdTerm, pevalLtOrdTerm),
    PEvalRotateTerm
      ( pevalRotateLeftTerm,
        pevalRotateRightTerm
      ),
    PEvalShiftTerm
      ( pevalShiftLeftTerm,
        pevalShiftRightTerm
      ),
    SupportedPrim (pevalITETerm),
    Term,
    TernaryOp (pevalTernary),
    UnaryOp (pevalUnary),
    absNumTerm,
    addNumTerm,
    andBitsTerm,
    andTerm,
    bvconcatTerm,
    bvextendTerm,
    bvselectTerm,
    complementBitsTerm,
    conTerm,
    constructBinary,
    constructTernary,
    constructUnary,
    divIntegralTerm,
    eqTerm,
    fpTraitTerm,
    iteTerm,
    leOrdTerm,
    ltOrdTerm,
    modIntegralTerm,
    mulNumTerm,
    negNumTerm,
    notTerm,
    orBitsTerm,
    orTerm,
    pevalAndTerm,
    pevalEqTerm,
    pevalFPTraitTerm,
    pevalNotTerm,
    pevalOrTerm,
    pformat,
    quotIntegralTerm,
    recipTerm,
    remIntegralTerm,
    rotateLeftTerm,
    rotateRightTerm,
    shiftLeftTerm,
    shiftRightTerm,
    signumNumTerm,
    ssymTerm,
    xorBitsTerm,
  )
import Test.QuickCheck (Arbitrary (arbitrary), Gen, elements, frequency, oneof, sized)

-- import Grisette.Internal.SymPrim.FP (FPRoundingMode(RNE))

class (SupportedPrim b) => TermRewritingSpec a b | a -> b where
  norewriteVer :: a -> Term b
  rewriteVer :: a -> Term b
  wrap :: Term b -> Term b -> a
  same :: a -> Term Bool
  counterExample :: a -> Term Bool
  counterExample = notTerm . same
  symSpec :: Identifier -> a
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
constructUnarySpec' tag = constructUnarySpec @a @av @b @bv (constructUnary tag) (pevalUnary tag)

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
constructBinarySpec' tag = constructBinarySpec @a @av @b @bv @c @cv (constructBinary tag) (pevalBinary tag)

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
    (pevalTernary tag)

notSpec :: (TermRewritingSpec a Bool) => a -> a
notSpec = constructUnarySpec notTerm pevalNotTerm

andSpec :: (TermRewritingSpec a Bool) => a -> a -> a
andSpec = constructBinarySpec andTerm pevalAndTerm

orSpec :: (TermRewritingSpec a Bool) => a -> a -> a
orSpec = constructBinarySpec orTerm pevalOrTerm

eqvSpec :: (TermRewritingSpec a av, TermRewritingSpec b Bool) => a -> a -> b
eqvSpec = constructBinarySpec eqTerm pevalEqTerm

iteSpec :: (TermRewritingSpec a Bool, TermRewritingSpec b bv) => a -> b -> b -> b
iteSpec = constructTernarySpec iteTerm pevalITETerm

addNumSpec :: (TermRewritingSpec a av, PEvalNumTerm av) => a -> a -> a
addNumSpec = constructBinarySpec addNumTerm pevalAddNumTerm

negNumSpec :: (TermRewritingSpec a av, PEvalNumTerm av) => a -> a
negNumSpec = constructUnarySpec negNumTerm pevalNegNumTerm

mulNumSpec :: (TermRewritingSpec a av, PEvalNumTerm av) => a -> a -> a
mulNumSpec = constructBinarySpec mulNumTerm pevalMulNumTerm

absNumSpec :: (TermRewritingSpec a av, PEvalNumTerm av) => a -> a
absNumSpec = constructUnarySpec absNumTerm pevalAbsNumTerm

signumNumSpec :: (TermRewritingSpec a av, PEvalNumTerm av) => a -> a
signumNumSpec = constructUnarySpec signumNumTerm pevalSignumNumTerm

ltOrdSpec ::
  (TermRewritingSpec a av, PEvalOrdTerm av, TermRewritingSpec b Bool) =>
  a ->
  a ->
  b
ltOrdSpec = constructBinarySpec ltOrdTerm pevalLtOrdTerm

leOrdSpec ::
  (TermRewritingSpec a av, PEvalOrdTerm av, TermRewritingSpec b Bool) =>
  a ->
  a ->
  b
leOrdSpec = constructBinarySpec leOrdTerm pevalLeOrdTerm

andBitsSpec :: (TermRewritingSpec a av, PEvalBitwiseTerm av) => a -> a -> a
andBitsSpec = constructBinarySpec andBitsTerm pevalAndBitsTerm

orBitsSpec :: (TermRewritingSpec a av, PEvalBitwiseTerm av) => a -> a -> a
orBitsSpec = constructBinarySpec orBitsTerm pevalOrBitsTerm

xorBitsSpec :: (TermRewritingSpec a av, PEvalBitwiseTerm av) => a -> a -> a
xorBitsSpec = constructBinarySpec xorBitsTerm pevalXorBitsTerm

complementBitsSpec :: (TermRewritingSpec a av, PEvalBitwiseTerm av) => a -> a
complementBitsSpec = constructUnarySpec complementBitsTerm pevalComplementBitsTerm

shiftLeftSpec :: (TermRewritingSpec a av, PEvalShiftTerm av) => a -> a -> a
shiftLeftSpec = constructBinarySpec shiftLeftTerm pevalShiftLeftTerm

shiftRightSpec :: (TermRewritingSpec a av, PEvalShiftTerm av) => a -> a -> a
shiftRightSpec = constructBinarySpec shiftRightTerm pevalShiftRightTerm

rotateLeftSpec :: (TermRewritingSpec a av, PEvalRotateTerm av) => a -> a -> a
rotateLeftSpec = constructBinarySpec rotateLeftTerm pevalRotateLeftTerm

rotateRightSpec :: (TermRewritingSpec a av, PEvalRotateTerm av) => a -> a -> a
rotateRightSpec = constructBinarySpec rotateRightTerm pevalRotateRightTerm

bvconcatSpec ::
  ( TermRewritingSpec a (bv an),
    TermRewritingSpec b (bv bn),
    TermRewritingSpec c (bv (an + bn)),
    PEvalBVTerm bv,
    KnownNat an,
    KnownNat bn,
    KnownNat (an + bn),
    1 <= an,
    1 <= bn,
    1 <= an + bn
  ) =>
  a ->
  b ->
  c
bvconcatSpec = constructBinarySpec bvconcatTerm pevalBVConcatTerm

bvselectSpec ::
  ( TermRewritingSpec a (bv an),
    TermRewritingSpec b (bv bn),
    PEvalBVTerm bv,
    KnownNat an,
    KnownNat ix,
    KnownNat bn,
    1 <= an,
    1 <= bn,
    0 <= ix,
    ix + bn <= an
  ) =>
  proxy ix ->
  proxy bn ->
  a ->
  b
bvselectSpec p1 p2 = constructUnarySpec (bvselectTerm p1 p2) (pevalBVSelectTerm p1 p2)

bvextendSpec ::
  ( TermRewritingSpec a (bv an),
    TermRewritingSpec b (bv bn),
    PEvalBVTerm bv,
    KnownNat an,
    KnownNat bn,
    1 <= an,
    1 <= bn,
    an <= bn
  ) =>
  Bool ->
  proxy bn ->
  a ->
  b
bvextendSpec signed p = constructUnarySpec (bvextendTerm signed p) (pevalBVExtendTerm signed p)

divIntegralSpec :: (TermRewritingSpec a b, PEvalDivModIntegralTerm b) => a -> a -> a
divIntegralSpec = constructBinarySpec divIntegralTerm pevalDivIntegralTerm

modIntegralSpec :: (TermRewritingSpec a b, PEvalDivModIntegralTerm b) => a -> a -> a
modIntegralSpec = constructBinarySpec modIntegralTerm pevalModIntegralTerm

quotIntegralSpec :: (TermRewritingSpec a b, PEvalDivModIntegralTerm b) => a -> a -> a
quotIntegralSpec = constructBinarySpec quotIntegralTerm pevalQuotIntegralTerm

remIntegralSpec :: (TermRewritingSpec a b, PEvalDivModIntegralTerm b) => a -> a -> a
remIntegralSpec = constructBinarySpec remIntegralTerm pevalRemIntegralTerm

fpTraitSpec ::
  ( ValidFP eb fb,
    TermRewritingSpec a (FP eb fb),
    TermRewritingSpec b Bool
  ) =>
  FPTrait ->
  a ->
  b
fpTraitSpec trait = constructUnarySpec (fpTraitTerm trait) (pevalFPTraitTerm trait)

fdivSpec :: (TermRewritingSpec a av, PEvalFractionalTerm av) => a -> a -> a
fdivSpec = constructBinarySpec fdivTerm pevalFdivTerm

recipSpec :: (TermRewritingSpec a av, PEvalFractionalTerm av) => a -> a
recipSpec = constructUnarySpec recipTerm pevalRecipTerm

sqrtSpec :: (TermRewritingSpec a av, PEvalFloatingTerm av) => a -> a
sqrtSpec = constructUnarySpec sqrtTerm pevalSqrtTerm

data BoolOnlySpec = BoolOnlySpec (Term Bool) (Term Bool)

instance Show BoolOnlySpec where
  show (BoolOnlySpec n r) = "BoolOnlySpec { no: " ++ pformat n ++ ", re: " ++ pformat r ++ " }"

instance TermRewritingSpec BoolOnlySpec Bool where
  norewriteVer (BoolOnlySpec n _) = n
  rewriteVer (BoolOnlySpec _ r) = r
  wrap = BoolOnlySpec
  same s = eqTerm (norewriteVer s) (rewriteVer s)

boolonly :: Int -> Gen BoolOnlySpec
boolonly 0 =
  let s =
        oneof $
          return . symSpec . (`withInfo` ("bool" :: T.Text))
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
  same s = eqTerm (norewriteVer s) (rewriteVer s)

data LIAWithBoolSpec = LIAWithBoolSpec (Term Integer) (Term Integer)

instance Show LIAWithBoolSpec where
  show (LIAWithBoolSpec n r) =
    "LIAWithBoolSpec { no: " ++ pformat n ++ ", re: " ++ pformat r ++ " }"

instance TermRewritingSpec LIAWithBoolSpec Integer where
  norewriteVer (LIAWithBoolSpec n _) = n
  rewriteVer (LIAWithBoolSpec _ r) = r
  wrap = LIAWithBoolSpec
  same s = eqTerm (norewriteVer s) (rewriteVer s)

boolWithLIA :: Int -> Gen BoolWithLIASpec
boolWithLIA 0 =
  let s =
        oneof $
          return . symSpec . (`withInfo` ("bool" :: T.Text))
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
      (5, return $ ltOrdSpec v1i v2i),
      (5, return $ leOrdSpec v1i v2i),
      (1, return $ iteSpec v1 v2 v3)
    ]
boolWithLIA _ = error "Should never be called"

liaWithBool :: Int -> Gen LIAWithBoolSpec
liaWithBool 0 =
  let s =
        oneof $
          return . symSpec . (`withInfo` ("int" :: T.Text))
            <$> ["a", "b", "c", "d", "e", "f", "g"]
      r = conSpec <$> arbitrary
   in oneof [r, s]
liaWithBool n | n > 0 = do
  v1b <- boolWithLIA (n - 1)
  v1i <- liaWithBool (n - 1)
  v2i <- liaWithBool (n - 1)
  oneof
    [ return $ negNumSpec v1i,
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

data FixedSizedBVWithBoolSpec (bv :: Nat -> Type) (n :: Nat) = FixedSizedBVWithBoolSpec (Term (bv n)) (Term (bv n))

instance (SupportedPrim (bv n)) => Show (FixedSizedBVWithBoolSpec bv n) where
  show (FixedSizedBVWithBoolSpec n r) = "FixedSizedBVWithBoolSpec { no: " ++ pformat n ++ ", re: " ++ pformat r ++ " }"

instance (SupportedPrim (bv n)) => TermRewritingSpec (FixedSizedBVWithBoolSpec bv n) (bv n) where
  norewriteVer (FixedSizedBVWithBoolSpec n _) = n
  rewriteVer (FixedSizedBVWithBoolSpec _ r) = r
  wrap = FixedSizedBVWithBoolSpec
  same s = eqTerm (norewriteVer s) (rewriteVer s)

data BoolWithFixedSizedBVSpec (bv :: Nat -> Type) (n :: Nat) = BoolWithFixedSizedBVSpec (Term Bool) (Term Bool)

instance Show (BoolWithFixedSizedBVSpec bv n) where
  show (BoolWithFixedSizedBVSpec n r) =
    "BoolWithFixedSizedBVSpec { no: " ++ pformat n ++ ", re: " ++ pformat r ++ " }"

instance TermRewritingSpec (BoolWithFixedSizedBVSpec bv n) Bool where
  norewriteVer (BoolWithFixedSizedBVSpec n _) = n
  rewriteVer (BoolWithFixedSizedBVSpec _ r) = r
  wrap = BoolWithFixedSizedBVSpec
  same s = eqTerm (norewriteVer s) (rewriteVer s)

boolWithFSBV ::
  forall p1 p2 bv n.
  (SupportedBV bv n) =>
  p1 bv ->
  p2 n ->
  Int ->
  Gen (BoolWithFixedSizedBVSpec bv n)
boolWithFSBV _ _ 0 =
  let s =
        oneof $
          return . symSpec . (`withInfo` ("bool" :: T.Text))
            <$> ["a", "b", "c", "d", "e", "f", "g"]
      r = oneof $ return . conSpec <$> [True, False]
   in oneof [r, s]
boolWithFSBV pbv pn n | n > 0 = do
  v1 <- boolWithFSBV pbv pn (n - 1)
  v2 <- boolWithFSBV pbv pn (n - 1)
  v3 <- boolWithFSBV pbv pn (n - 1)
  v1i <- fsbvWithBool pbv pn (n - 1)
  v2i <- fsbvWithBool pbv pn (n - 1)
  frequency
    [ (1, return $ notSpec v1),
      (1, return $ andSpec v1 v2),
      (1, return $ orSpec v1 v2),
      (1, return $ eqvSpec v1 v2),
      (5, return $ eqvSpec v1i v2i),
      (5, return $ ltOrdSpec v1i v2i),
      (5, return $ leOrdSpec v1i v2i),
      (1, return $ iteSpec v1 v2 v3)
    ]
boolWithFSBV _ _ _ = error "Should never be called"

fsbvWithBool ::
  forall p1 p2 bv n.
  (SupportedBV bv n) =>
  p1 bv ->
  p2 n ->
  Int ->
  Gen (FixedSizedBVWithBoolSpec bv n)
fsbvWithBool _ _ 0 =
  let s =
        oneof $
          return . symSpec . (`withInfo` ("int" :: T.Text))
            <$> ["a", "b", "c", "d", "e", "f", "g"]
      r =
        conSpec
          <$> oneof
            [ return minBound,
              return maxBound,
              fromInteger <$> arbitrary
            ]
   in oneof [r, s]
fsbvWithBool pbv pn n | n > 0 = do
  v1b <- boolWithFSBV pbv pn (n - 1)
  v1i <- fsbvWithBool pbv pn (n - 1)
  v2i <- fsbvWithBool pbv pn (n - 1)
  oneof
    [ return $ negNumSpec v1i,
      return $ absNumSpec v1i,
      return $ signumNumSpec v1i,
      return $ addNumSpec v1i v2i,
      return $ mulNumSpec v1i v2i,
      return $ andBitsSpec v1i v2i,
      return $ orBitsSpec v1i v2i,
      return $ xorBitsSpec v1i v2i,
      return $ complementBitsSpec v1i,
      return $ shiftLeftSpec v1i v2i,
      return $ rotateLeftSpec v1i v2i,
      return $ shiftRightSpec v1i v2i,
      return $ rotateRightSpec v1i v2i,
      return $ iteSpec v1b v1i v2i
    ]
fsbvWithBool _ _ _ = error "Should never be called"

instance (SupportedBV bv n) => Arbitrary (BoolWithFixedSizedBVSpec bv n) where
  arbitrary = sized (boolWithFSBV (Proxy @bv) (Proxy @n))

instance (SupportedBV bv n) => Arbitrary (FixedSizedBVWithBoolSpec bv n) where
  arbitrary = sized (fsbvWithBool Proxy Proxy)

data DifferentSizeBVSpec bv (n :: Nat) = DifferentSizeBVSpec (Term (bv n)) (Term (bv n))

instance (SupportedPrim (bv n)) => Show (DifferentSizeBVSpec bv n) where
  show (DifferentSizeBVSpec n r) = "DSizeBVSpec { no: " ++ pformat n ++ ", re: " ++ pformat r ++ " }"

instance (SupportedPrim (bv n)) => TermRewritingSpec (DifferentSizeBVSpec bv n) (bv n) where
  norewriteVer (DifferentSizeBVSpec n _) = n
  rewriteVer (DifferentSizeBVSpec _ r) = r
  wrap = DifferentSizeBVSpec
  same s = eqTerm (norewriteVer s) (rewriteVer s)

type SupportedBV bv (n :: Nat) =
  ( SupportedPrim (bv n),
    Ord (bv n),
    Num (bv n),
    FiniteBits (bv n),
    Integral (bv n),
    Bounded (bv n),
    SymShift (bv n),
    SymRotate (bv n),
    PEvalShiftTerm (bv n),
    PEvalRotateTerm (bv n),
    PEvalNumTerm (bv n),
    PEvalOrdTerm (bv n),
    PEvalBitwiseTerm (bv n),
    PEvalBVTerm bv
  )

dsbv1 ::
  forall proxy bv.
  ( SupportedBV bv 1,
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
          return . symSpec . (`withInfo` ("bv1" :: T.Text))
            <$> ["a", "b", "c", "d", "e", "f", "g"]
      r = conSpec . fromInteger <$> arbitrary
   in oneof [r, s]
dsbv1 p depth | depth > 0 = do
  v1 <- dsbv1 p (depth - 1)
  v1' <- dsbv1 p (depth - 1)
  v2 <- dsbv2 p (depth - 1)
  v3 <- dsbv3 p (depth - 1)
  v4 <- dsbv4 p (depth - 1)
  oneof
    [ return $ negNumSpec v1,
      return $ absNumSpec v1,
      return $ signumNumSpec v1,
      return $ addNumSpec v1 v1',
      return $ mulNumSpec v1 v1',
      return $ andBitsSpec v1 v1',
      return $ orBitsSpec v1 v1',
      return $ xorBitsSpec v1 v1',
      return $ complementBitsSpec v1,
      return $ shiftLeftSpec v1 v1',
      return $ rotateLeftSpec v1 v1',
      return $ shiftRightSpec v1 v1',
      return $ rotateRightSpec v1 v1',
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
  ( SupportedBV bv 1,
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
          return . symSpec . (`withInfo` ("bv2" :: T.Text))
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
  oneof
    [ return $ negNumSpec v2,
      return $ absNumSpec v2,
      return $ signumNumSpec v2,
      return $ addNumSpec v2 v2',
      return $ mulNumSpec v2 v2',
      return $ andBitsSpec v2 v2',
      return $ orBitsSpec v2 v2',
      return $ xorBitsSpec v2 v2',
      return $ complementBitsSpec v2,
      return $ shiftLeftSpec v2 v2',
      return $ rotateLeftSpec v2 v2',
      return $ shiftRightSpec v2 v2',
      return $ rotateRightSpec v2 v2',
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
  ( SupportedBV bv 1,
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
          return . symSpec . (`withInfo` ("bv3" :: T.Text))
            <$> ["a", "b", "c", "d", "e", "f", "g"]
      r = conSpec . fromInteger <$> arbitrary
   in oneof [r, s]
dsbv3 p depth | depth > 0 = do
  v1 <- dsbv1 p (depth - 1)
  v2 <- dsbv2 p (depth - 1)
  v3 <- dsbv3 p (depth - 1)
  v3' <- dsbv3 p (depth - 1)
  v4 <- dsbv4 p (depth - 1)
  oneof
    [ return $ negNumSpec v3,
      return $ absNumSpec v3,
      return $ signumNumSpec v3,
      return $ addNumSpec v3 v3',
      return $ mulNumSpec v3 v3',
      return $ andBitsSpec v3 v3',
      return $ orBitsSpec v3 v3',
      return $ xorBitsSpec v3 v3',
      return $ complementBitsSpec v3,
      return $ shiftLeftSpec v3 v3',
      return $ rotateLeftSpec v3 v3',
      return $ shiftRightSpec v3 v3',
      return $ rotateRightSpec v3 v3',
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
  ( SupportedBV bv 1,
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
          return . symSpec . (`withInfo` ("bv4" :: T.Text))
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
  oneof
    [ return $ negNumSpec v4,
      return $ absNumSpec v4,
      return $ signumNumSpec v4,
      return $ addNumSpec v4 v4',
      return $ mulNumSpec v4 v4',
      return $ andBitsSpec v4 v4',
      return $ orBitsSpec v4 v4',
      return $ xorBitsSpec v4 v4',
      return $ complementBitsSpec v4,
      return $ shiftLeftSpec v4 v4',
      return $ rotateLeftSpec v4 v4',
      return $ shiftRightSpec v4 v4',
      return $ rotateRightSpec v4 v4',
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
  ( SupportedBV bv 1,
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
  same s = eqTerm (norewriteVer s) (rewriteVer s)

data IEEEFP32Spec = IEEEFP32Spec (Term FP32) (Term FP32)

instance Show IEEEFP32Spec where
  show (IEEEFP32Spec n r) =
    "IEEEFP32Spec { no: " ++ pformat n ++ ", re: " ++ pformat r ++ " }"

instance TermRewritingSpec IEEEFP32Spec FP32 where
  norewriteVer (IEEEFP32Spec n _) = n
  rewriteVer (IEEEFP32Spec _ r) = r
  wrap = IEEEFP32Spec
  same s = eqTerm (norewriteVer s) (rewriteVer s)

instance Arbitrary IEEEFP32Spec where
  arbitrary = do
    bool :: BoolOnlySpec <-
      oneof [conSpec <$> arbitrary, return $ symSpec "bool"]
    a <-
      oneof
        [conSpec <$> arbitrary, return $ symSpec "a", return $ symSpec "b"]
    b <-
      oneof
        [conSpec <$> arbitrary, return $ symSpec "a", return $ symSpec "b"]
    oneof $
      return
        <$> [ a,
              iteSpec bool a b,
              addNumSpec a a,
              negNumSpec a,
              mulNumSpec a b,
              absNumSpec a,
              signumNumSpec a,
              fdivSpec a b,
              recipSpec a,
              sqrtSpec a
            ]

data IEEEFP32BoolOpSpec = IEEEFP32BoolOpSpec (Term Bool) (Term Bool)

instance Show IEEEFP32BoolOpSpec where
  show (IEEEFP32BoolOpSpec n r) =
    "IEEEFP32BoolOpSpec { no: " ++ pformat n ++ ", re: " ++ pformat r ++ " }"

instance TermRewritingSpec IEEEFP32BoolOpSpec Bool where
  norewriteVer (IEEEFP32BoolOpSpec n _) = n
  rewriteVer (IEEEFP32BoolOpSpec _ r) = r
  wrap = IEEEFP32BoolOpSpec
  same s = eqTerm (norewriteVer s) (rewriteVer s)

singleFP32BoolOpSpecGen :: Gen IEEEFP32BoolOpSpec
singleFP32BoolOpSpecGen = do
  s0 :: IEEEFP32Spec <- arbitrary
  s1 :: IEEEFP32Spec <- arbitrary
  let traitGens =
        [ FPIsNaN,
          FPIsPositive,
          FPIsNegative,
          FPIsPositiveInfinite,
          FPIsNegativeInfinite,
          FPIsInfinite,
          FPIsPositiveZero,
          FPIsNegativeZero,
          FPIsZero,
          FPIsNormal,
          FPIsSubnormal,
          FPIsPoint
        ]
          <&> (\trait -> return $ fpTraitSpec trait s0)
  let cmpGens = return <$> [eqvSpec s0 s1, ltOrdSpec s0 s1, leOrdSpec s0 s1]
  oneof $ traitGens ++ cmpGens

instance Arbitrary IEEEFP32BoolOpSpec where
  arbitrary = singleFP32BoolOpSpecGen

data FPRoundingModeSpec
  = FPRoundingModeSpec (Term FPRoundingMode) (Term FPRoundingMode)

instance Show FPRoundingModeSpec where
  show (FPRoundingModeSpec n r) =
    "FPRoundingModeSpec { no: " ++ pformat n ++ ", re: " ++ pformat r ++ " }"

instance TermRewritingSpec FPRoundingModeSpec FPRoundingMode where
  norewriteVer (FPRoundingModeSpec n _) = n
  rewriteVer (FPRoundingModeSpec _ r) = r
  wrap = FPRoundingModeSpec
  same s = eqTerm (norewriteVer s) (rewriteVer s)

instance Arbitrary FPRoundingModeSpec where
  arbitrary =
    elements
      [ conSpec RNE,
        conSpec RNA,
        conSpec RTP,
        conSpec RTN,
        conSpec RTZ,
        symSpec "a",
        symSpec "b"
      ]

data FPRoundingModeBoolOpSpec = FPRoundingModeBoolOpSpec (Term Bool) (Term Bool)

instance Show FPRoundingModeBoolOpSpec where
  show (FPRoundingModeBoolOpSpec n r) =
    "FPRoundingModeBoolOpSpec { no: " ++ pformat n ++ ", re: " ++ pformat r ++ " }"

instance TermRewritingSpec FPRoundingModeBoolOpSpec Bool where
  norewriteVer (FPRoundingModeBoolOpSpec n _) = n
  rewriteVer (FPRoundingModeBoolOpSpec _ r) = r
  wrap = FPRoundingModeBoolOpSpec
  same s = eqTerm (norewriteVer s) (rewriteVer s)

instance Arbitrary FPRoundingModeBoolOpSpec where
  arbitrary = do
    l :: FPRoundingModeSpec <- arbitrary
    r <- arbitrary
    elements [eqvSpec l r, ltOrdSpec l r, leOrdSpec l r]
