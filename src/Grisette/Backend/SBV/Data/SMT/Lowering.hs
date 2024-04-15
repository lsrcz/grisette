{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- Module      :   Grisette.Backend.SBV.Data.SMT.Lowering
-- Copyright   :   (c) Sirui Lu 2021-2023
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Backend.SBV.Data.SMT.Lowering
  ( lowerSinglePrim,
    lowerSinglePrimCached,
    parseModel,
    SymBiMap,
  )
where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadTrans (lift), ReaderT)
import Control.Monad.State (StateT)
import Data.Bifunctor (Bifunctor (bimap, first, second))
import Data.Bits
  ( Bits (complement, xor, (.&.), (.|.)),
  )
import Data.Dynamic (Typeable, fromDyn, toDyn)
import Data.Foldable (Foldable (foldl'), asum)
import Data.Kind (Type)
import Data.Maybe (fromMaybe)
import Data.SBV (SIntegral, sRotateLeft, sRotateRight, sShiftLeft, sShiftRight)
import qualified Data.SBV as SBV
import qualified Data.SBV.Internals as SBVI
import qualified Data.SBV.Trans as SBVT
import qualified Data.SBV.Trans.Control as SBVTC
import Data.Type.Equality (type (~~))
import Data.Typeable (Proxy (Proxy), type (:~:) (Refl))
import GHC.Exts (sortWith)
import GHC.Stack (HasCallStack)
import GHC.TypeNats
  ( KnownNat,
    Nat,
    natVal,
    type (+),
    type (-),
    type (<=),
  )
import {-# SOURCE #-} Grisette.Backend.SBV.Data.SMT.Solving
  ( ApproximationConfig (Approx, NoApprox),
    ExtraConfig (integerApprox),
    GrisetteSMTConfig (GrisetteSMTConfig),
    TermTy,
  )
import Grisette.Backend.SBV.Data.SMT.SymBiMap
  ( SymBiMap,
    addBiMap,
    addBiMapIntermediate,
    emptySymBiMap,
    findStringToSymbol,
    lookupTerm,
    sizeBiMap,
  )
import Grisette.Core.Data.BV (IntN (IntN, unIntN), WordN (WordN))
import Grisette.Core.Data.Class.ModelOps
  ( ModelOps (emptyModel, insertValue),
  )
import Grisette.Core.Data.Symbol (Symbol (IndexedSymbol))
import Grisette.IR.SymPrim.Data.Prim.InternedTerm.InternedCtors
  ( conTerm,
    symTerm,
  )
import Grisette.IR.SymPrim.Data.Prim.InternedTerm.SomeTerm
  ( SomeTerm (SomeTerm),
  )
import Grisette.IR.SymPrim.Data.Prim.InternedTerm.Term
  ( SomeTypedSymbol (SomeTypedSymbol),
    SupportedPrim (withPrim),
    Term
      ( AbsNumTerm,
        AddNumTerm,
        AndBitsTerm,
        AndTerm,
        BVConcatTerm,
        BVExtendTerm,
        BVSelectTerm,
        BinaryTerm,
        ComplementBitsTerm,
        ConTerm,
        DivBoundedIntegralTerm,
        DivIntegralTerm,
        EqvTerm,
        GeneralFunApplyTerm,
        ITETerm,
        LENumTerm,
        LTNumTerm,
        ModBoundedIntegralTerm,
        ModIntegralTerm,
        NotTerm,
        OrBitsTerm,
        OrTerm,
        QuotBoundedIntegralTerm,
        QuotIntegralTerm,
        RemBoundedIntegralTerm,
        RemIntegralTerm,
        RotateLeftTerm,
        RotateRightTerm,
        ShiftLeftTerm,
        ShiftRightTerm,
        SignumNumTerm,
        SymTerm,
        TabularFunApplyTerm,
        TernaryTerm,
        TimesNumTerm,
        ToSignedTerm,
        ToUnsignedTerm,
        UMinusNumTerm,
        UnaryTerm,
        XorBitsTerm
      ),
    TypedSymbol (TypedSymbol),
    buildGeneralFun,
    someTypedSymbol,
    withSymbolSupported,
    type (-->),
  )
import Grisette.IR.SymPrim.Data.Prim.InternedTerm.TermUtils
  ( introSupportedPrimConstraint,
  )
import Grisette.IR.SymPrim.Data.Prim.Model as PM (Model)
import Grisette.IR.SymPrim.Data.Prim.PartialEval.Bool
  ( pevalEqvTerm,
    pevalITETerm,
  )
import Grisette.IR.SymPrim.Data.TabularFun
  ( type (=->) (TabularFun),
  )
import Grisette.Utils.Parameterized
  ( KnownProof (KnownProof),
    LeqProof (LeqProof),
    unsafeAxiom,
    unsafeKnownProof,
    unsafeLeqProof,
    withKnownProof,
  )
import qualified Type.Reflection as R

translateTypeError :: (HasCallStack) => R.TypeRep a -> b
translateTypeError ta =
  error $
    "Don't know how to translate the type " ++ show ta ++ " to SMT"

translateUnaryError :: (HasCallStack) => String -> R.TypeRep a -> R.TypeRep b -> c
translateUnaryError op ta tb =
  error $
    "Don't know how to translate the op "
      ++ show op
      ++ " :: "
      ++ show ta
      ++ " -> "
      ++ show tb
      ++ " to SMT"

translateBinaryError :: (HasCallStack) => String -> R.TypeRep a -> R.TypeRep b -> R.TypeRep c -> d
translateBinaryError op ta tb tc =
  error $
    "Don't know how to translate the op "
      ++ show op
      ++ " :: "
      ++ show ta
      ++ " -> "
      ++ show tb
      ++ " -> "
      ++ show tc
      ++ " to SMT"

translateTernaryError :: (HasCallStack) => String -> R.TypeRep a -> R.TypeRep b -> R.TypeRep c -> R.TypeRep d -> e
translateTernaryError op ta tb tc td =
  error $
    "Don't know how to translate the op "
      ++ show op
      ++ " :: "
      ++ show ta
      ++ " -> "
      ++ show tb
      ++ " -> "
      ++ show tc
      ++ " -> "
      ++ show td
      ++ " to SMT"

lowerValue ::
  forall integerBitWidth a.
  (SupportedPrim a, Typeable a) =>
  GrisetteSMTConfig integerBitWidth ->
  a ->
  TermTy integerBitWidth a
lowerValue config@ResolvedConfig {} v =
  case R.typeRep @a of
    BoolType -> if v then SBV.sTrue else SBV.sFalse
    IntegerType -> fromInteger v
    SignedBVType _ -> case v of
      IntN x -> fromInteger x
    UnsignedBVType _ -> case v of
      WordN x -> fromInteger x
    TFunType (l :: a1) (r :: a2) ->
      case ((config, l), (config, r)) of
        (ResolvedSimpleType, ResolvedMergeableType) ->
          lowerTFunCon config v
        _ -> translateTypeError (R.typeRep @a)
    _ -> translateTypeError (R.typeRep @a)
lowerValue _ _ = translateTypeError (R.typeRep @a)

lowerTFunCon ::
  forall integerBitWidth a b.
  (SupportedPrim a, SupportedPrim b, SBV.EqSymbolic (TermTy integerBitWidth a), SBV.Mergeable (TermTy integerBitWidth b)) =>
  GrisetteSMTConfig integerBitWidth ->
  (a =-> b) ->
  (TermTy integerBitWidth a -> TermTy integerBitWidth b)
lowerTFunCon config@ResolvedConfig {} (TabularFun l d) = go l d
  where
    go [] d _ = lowerValue config d
    go ((x, r) : xs) d v = SBV.ite (lowerValue config x SBV..== v) (lowerValue config r) (go xs d v)
lowerTFunCon _ TabularFun {} = translateTypeError (R.typeRep @a)

buildUTFun11 ::
  forall integerBitWidth s1 s2 a.
  (SupportedPrim a, SupportedPrim s1, SupportedPrim s2) =>
  GrisetteSMTConfig integerBitWidth ->
  R.TypeRep s1 ->
  R.TypeRep s2 ->
  Term a ->
  SymBiMap ->
  Maybe (SymBiMap, TermTy integerBitWidth (s1 =-> s2))
buildUTFun11 config ta tb term@(SymTerm _ ts) m = case ((config, ta), (config, tb)) of
  (ResolvedSimpleType, ResolvedSimpleType) ->
    let name = "ufunc_" ++ show (sizeBiMap m)
        f = SBV.uninterpret @(TermTy integerBitWidth s1 -> TermTy integerBitWidth s2) name
     in Just (addBiMap (SomeTerm term) (toDyn f) name (someTypedSymbol ts) m, f)
  _ -> Nothing
buildUTFun11 _ _ _ _ _ = error "Should only be called on SymTerm"

buildUTFun111 ::
  forall integerBitWidth s1 s2 s3 a.
  (SupportedPrim a, SupportedPrim s1, SupportedPrim s2, SupportedPrim s3) =>
  GrisetteSMTConfig integerBitWidth ->
  R.TypeRep s1 ->
  R.TypeRep s2 ->
  R.TypeRep s3 ->
  Term a ->
  SymBiMap ->
  Maybe (SymBiMap, TermTy integerBitWidth (s1 =-> s2 =-> s3))
buildUTFun111 config ta tb tc term@(SymTerm _ ts) m =
  case ((config, ta), (config, tb), (config, tc)) of
    (ResolvedSimpleType, ResolvedSimpleType, ResolvedSimpleType) ->
      let name = "ufunc_" ++ show (sizeBiMap m)
          f =
            SBV.uninterpret @(TermTy integerBitWidth s1 -> TermTy integerBitWidth s2 -> TermTy integerBitWidth s3)
              name
       in Just (addBiMap (SomeTerm term) (toDyn f) name (someTypedSymbol ts) m, f)
    _ -> Nothing
buildUTFun111 _ _ _ _ _ _ = error "Should only be called on SymTerm"

buildUTFun1111 ::
  forall integerBitWidth s1 s2 s3 s4 a.
  (SupportedPrim a, SupportedPrim s1, SupportedPrim s2, SupportedPrim s3, SupportedPrim s4) =>
  GrisetteSMTConfig integerBitWidth ->
  R.TypeRep s1 ->
  R.TypeRep s2 ->
  R.TypeRep s3 ->
  R.TypeRep s4 ->
  Term a ->
  SymBiMap ->
  Maybe (SymBiMap, TermTy integerBitWidth (s1 =-> s2 =-> s3 =-> s4))
buildUTFun1111 config ta tb tc td term@(SymTerm _ ts) m =
  case ((config, ta), (config, tb), (config, tc), (config, td)) of
    (ResolvedSimpleType, ResolvedSimpleType, ResolvedSimpleType, ResolvedSimpleType) ->
      let name = "ufunc_" ++ show (sizeBiMap m)
          f =
            SBV.uninterpret @(TermTy integerBitWidth s1 -> TermTy integerBitWidth s2 -> TermTy integerBitWidth s3 -> TermTy integerBitWidth s4)
              name
       in Just (addBiMap (SomeTerm term) (toDyn f) name (someTypedSymbol ts) m, f)
    _ -> Nothing
buildUTFun1111 _ _ _ _ _ _ _ = error "Should only be called on SymTerm"

buildUTFun11111 ::
  forall integerBitWidth s1 s2 s3 s4 s5 a.
  (SupportedPrim a, SupportedPrim s1, SupportedPrim s2, SupportedPrim s3, SupportedPrim s4, SupportedPrim s5) =>
  GrisetteSMTConfig integerBitWidth ->
  R.TypeRep s1 ->
  R.TypeRep s2 ->
  R.TypeRep s3 ->
  R.TypeRep s4 ->
  R.TypeRep s5 ->
  Term a ->
  SymBiMap ->
  Maybe (SymBiMap, TermTy integerBitWidth (s1 =-> s2 =-> s3 =-> s4 =-> s5))
buildUTFun11111 config ta tb tc td te term@(SymTerm _ ts) m =
  case ((config, ta), (config, tb), (config, tc), (config, td), (config, te)) of
    (ResolvedSimpleType, ResolvedSimpleType, ResolvedSimpleType, ResolvedSimpleType, ResolvedSimpleType) ->
      let name = "ufunc_" ++ show (sizeBiMap m)
          f =
            SBV.uninterpret @(TermTy integerBitWidth (s1 =-> s2 =-> s3 =-> s4 =-> s5))
              name
       in Just (addBiMap (SomeTerm term) (toDyn f) name (someTypedSymbol ts) m, f)
    _ -> Nothing
buildUTFun11111 _ _ _ _ _ _ _ _ = error "Should only be called on SymTerm"

buildUTFun111111 ::
  forall integerBitWidth s1 s2 s3 s4 s5 s6 a.
  ( SupportedPrim a,
    SupportedPrim s1,
    SupportedPrim s2,
    SupportedPrim s3,
    SupportedPrim s4,
    SupportedPrim s5,
    SupportedPrim s6
  ) =>
  GrisetteSMTConfig integerBitWidth ->
  R.TypeRep s1 ->
  R.TypeRep s2 ->
  R.TypeRep s3 ->
  R.TypeRep s4 ->
  R.TypeRep s5 ->
  R.TypeRep s6 ->
  Term a ->
  SymBiMap ->
  Maybe (SymBiMap, TermTy integerBitWidth (s1 =-> s2 =-> s3 =-> s4 =-> s5 =-> s6))
buildUTFun111111 config ta tb tc td te tf term@(SymTerm _ ts) m =
  case ((config, ta), (config, tb), (config, tc), (config, td), (config, te), (config, tf)) of
    (ResolvedSimpleType, ResolvedSimpleType, ResolvedSimpleType, ResolvedSimpleType, ResolvedSimpleType, ResolvedSimpleType) ->
      let name = "ufunc_" ++ show (sizeBiMap m)
          f =
            SBV.uninterpret @(TermTy integerBitWidth (s1 =-> s2 =-> s3 =-> s4 =-> s5 =-> s6))
              name
       in Just (addBiMap (SomeTerm term) (toDyn f) name (someTypedSymbol ts) m, f)
    _ -> Nothing
buildUTFun111111 _ _ _ _ _ _ _ _ _ = error "Should only be called on SymTerm"

buildUTFun1111111 ::
  forall integerBitWidth s1 s2 s3 s4 s5 s6 s7 a.
  ( SupportedPrim a,
    SupportedPrim s1,
    SupportedPrim s2,
    SupportedPrim s3,
    SupportedPrim s4,
    SupportedPrim s5,
    SupportedPrim s6,
    SupportedPrim s7
  ) =>
  GrisetteSMTConfig integerBitWidth ->
  R.TypeRep s1 ->
  R.TypeRep s2 ->
  R.TypeRep s3 ->
  R.TypeRep s4 ->
  R.TypeRep s5 ->
  R.TypeRep s6 ->
  R.TypeRep s7 ->
  Term a ->
  SymBiMap ->
  Maybe (SymBiMap, TermTy integerBitWidth (s1 =-> s2 =-> s3 =-> s4 =-> s5 =-> s6 =-> s7))
buildUTFun1111111 config ta tb tc td te tf tg term@(SymTerm _ ts) m =
  case ((config, ta), (config, tb), (config, tc), (config, td), (config, te), (config, tf), (config, tg)) of
    (ResolvedSimpleType, ResolvedSimpleType, ResolvedSimpleType, ResolvedSimpleType, ResolvedSimpleType, ResolvedSimpleType, ResolvedSimpleType) ->
      let name = "ufunc_" ++ show (sizeBiMap m)
          f =
            SBV.uninterpret @(TermTy integerBitWidth (s1 =-> s2 =-> s3 =-> s4 =-> s5 =-> s6 =-> s7))
              name
       in Just (addBiMap (SomeTerm term) (toDyn f) name (someTypedSymbol ts) m, f)
    _ -> Nothing
buildUTFun1111111 _ _ _ _ _ _ _ _ _ _ = error "Should only be called on SymTerm"

buildUTFun11111111 ::
  forall integerBitWidth s1 s2 s3 s4 s5 s6 s7 s8 a.
  ( SupportedPrim a,
    SupportedPrim s1,
    SupportedPrim s2,
    SupportedPrim s3,
    SupportedPrim s4,
    SupportedPrim s5,
    SupportedPrim s6,
    SupportedPrim s7,
    SupportedPrim s8
  ) =>
  GrisetteSMTConfig integerBitWidth ->
  R.TypeRep s1 ->
  R.TypeRep s2 ->
  R.TypeRep s3 ->
  R.TypeRep s4 ->
  R.TypeRep s5 ->
  R.TypeRep s6 ->
  R.TypeRep s7 ->
  R.TypeRep s8 ->
  Term a ->
  SymBiMap ->
  Maybe (SymBiMap, TermTy integerBitWidth (s1 =-> s2 =-> s3 =-> s4 =-> s5 =-> s6 =-> s7 =-> s8))
buildUTFun11111111 config ta tb tc td te tf tg th term@(SymTerm _ ts) m =
  case ((config, ta), (config, tb), (config, tc), (config, td), (config, te), (config, tf), (config, tg), (config, th)) of
    (ResolvedSimpleType, ResolvedSimpleType, ResolvedSimpleType, ResolvedSimpleType, ResolvedSimpleType, ResolvedSimpleType, ResolvedSimpleType, ResolvedSimpleType) ->
      let name = "ufunc_" ++ show (sizeBiMap m)
          f =
            SBV.uninterpret @(TermTy integerBitWidth (s1 =-> s2 =-> s3 =-> s4 =-> s5 =-> s6 =-> s7 =-> s8))
              name
       in Just (addBiMap (SomeTerm term) (toDyn f) name (someTypedSymbol ts) m, f)
    _ -> Nothing
buildUTFun11111111 _ _ _ _ _ _ _ _ _ _ _ = error "Should only be called on SymTerm"

buildUGFun11 ::
  forall integerBitWidth s1 s2 a.
  (SupportedPrim a, SupportedPrim s1, SupportedPrim s2) =>
  GrisetteSMTConfig integerBitWidth ->
  R.TypeRep s1 ->
  R.TypeRep s2 ->
  Term a ->
  SymBiMap ->
  Maybe (SymBiMap, TermTy integerBitWidth (s1 --> s2))
buildUGFun11 config ta tb term@(SymTerm _ ts) m = case ((config, ta), (config, tb)) of
  (ResolvedSimpleType, ResolvedSimpleType) ->
    let name = "ufunc_" ++ show (sizeBiMap m)
        f = SBV.uninterpret @(TermTy integerBitWidth s1 -> TermTy integerBitWidth s2) name
     in Just (addBiMap (SomeTerm term) (toDyn f) name (someTypedSymbol ts) m, f)
  _ -> Nothing
buildUGFun11 _ _ _ _ _ = error "Should only be called on SymTerm"

buildUGFun111 ::
  forall integerBitWidth s1 s2 s3 a.
  (SupportedPrim a, SupportedPrim s1, SupportedPrim s2, SupportedPrim s3) =>
  GrisetteSMTConfig integerBitWidth ->
  R.TypeRep s1 ->
  R.TypeRep s2 ->
  R.TypeRep s3 ->
  Term a ->
  SymBiMap ->
  Maybe (SymBiMap, TermTy integerBitWidth (s1 --> s2 --> s3))
buildUGFun111 config ta tb tc term@(SymTerm _ ts) m = case ((config, ta), (config, tb), (config, tc)) of
  (ResolvedSimpleType, ResolvedSimpleType, ResolvedSimpleType) ->
    let name = "ufunc_" ++ show (sizeBiMap m)
        f =
          SBV.uninterpret @(TermTy integerBitWidth s1 -> TermTy integerBitWidth s2 -> TermTy integerBitWidth s3)
            name
     in Just (addBiMap (SomeTerm term) (toDyn f) name (someTypedSymbol ts) m, f)
  _ -> Nothing
buildUGFun111 _ _ _ _ _ _ = error "Should only be called on SymTerm"

buildUGFun1111 ::
  forall integerBitWidth s1 s2 s3 s4 a.
  (SupportedPrim a, SupportedPrim s1, SupportedPrim s2, SupportedPrim s3, SupportedPrim s4) =>
  GrisetteSMTConfig integerBitWidth ->
  R.TypeRep s1 ->
  R.TypeRep s2 ->
  R.TypeRep s3 ->
  R.TypeRep s4 ->
  Term a ->
  SymBiMap ->
  Maybe (SymBiMap, TermTy integerBitWidth (s1 --> s2 --> s3 --> s4))
buildUGFun1111 config ta tb tc td term@(SymTerm _ ts) m =
  case ((config, ta), (config, tb), (config, tc), (config, td)) of
    (ResolvedSimpleType, ResolvedSimpleType, ResolvedSimpleType, ResolvedSimpleType) ->
      let name = "ufunc_" ++ show (sizeBiMap m)
          f =
            SBV.uninterpret @(TermTy integerBitWidth s1 -> TermTy integerBitWidth s2 -> TermTy integerBitWidth s3 -> TermTy integerBitWidth s4)
              name
       in Just (addBiMap (SomeTerm term) (toDyn f) name (someTypedSymbol ts) m, f)
    _ -> Nothing
buildUGFun1111 _ _ _ _ _ _ _ = error "Should only be called on SymTerm"

buildUGFun11111 ::
  forall integerBitWidth s1 s2 s3 s4 s5 a.
  (SupportedPrim a, SupportedPrim s1, SupportedPrim s2, SupportedPrim s3, SupportedPrim s4, SupportedPrim s5) =>
  GrisetteSMTConfig integerBitWidth ->
  R.TypeRep s1 ->
  R.TypeRep s2 ->
  R.TypeRep s3 ->
  R.TypeRep s4 ->
  R.TypeRep s5 ->
  Term a ->
  SymBiMap ->
  Maybe (SymBiMap, TermTy integerBitWidth (s1 --> s2 --> s3 --> s4 --> s5))
buildUGFun11111 config ta tb tc td te term@(SymTerm _ ts) m =
  case ((config, ta), (config, tb), (config, tc), (config, td), (config, te)) of
    (ResolvedSimpleType, ResolvedSimpleType, ResolvedSimpleType, ResolvedSimpleType, ResolvedSimpleType) ->
      let name = "ufunc_" ++ show (sizeBiMap m)
          f =
            SBV.uninterpret @(TermTy integerBitWidth (s1 --> s2 --> s3 --> s4 --> s5))
              name
       in Just (addBiMap (SomeTerm term) (toDyn f) name (someTypedSymbol ts) m, f)
    _ -> Nothing
buildUGFun11111 _ _ _ _ _ _ _ _ = error "Should only be called on SymTerm"

buildUGFun111111 ::
  forall integerBitWidth s1 s2 s3 s4 s5 s6 a.
  ( SupportedPrim a,
    SupportedPrim s1,
    SupportedPrim s2,
    SupportedPrim s3,
    SupportedPrim s4,
    SupportedPrim s5,
    SupportedPrim s6
  ) =>
  GrisetteSMTConfig integerBitWidth ->
  R.TypeRep s1 ->
  R.TypeRep s2 ->
  R.TypeRep s3 ->
  R.TypeRep s4 ->
  R.TypeRep s5 ->
  R.TypeRep s6 ->
  Term a ->
  SymBiMap ->
  Maybe (SymBiMap, TermTy integerBitWidth (s1 --> s2 --> s3 --> s4 --> s5 --> s6))
buildUGFun111111 config ta tb tc td te tf term@(SymTerm _ ts) m =
  case ((config, ta), (config, tb), (config, tc), (config, td), (config, te), (config, tf)) of
    (ResolvedSimpleType, ResolvedSimpleType, ResolvedSimpleType, ResolvedSimpleType, ResolvedSimpleType, ResolvedSimpleType) ->
      let name = "ufunc_" ++ show (sizeBiMap m)
          f =
            SBV.uninterpret @(TermTy integerBitWidth (s1 --> s2 --> s3 --> s4 --> s5 --> s6))
              name
       in Just (addBiMap (SomeTerm term) (toDyn f) name (someTypedSymbol ts) m, f)
    _ -> Nothing
buildUGFun111111 _ _ _ _ _ _ _ _ _ = error "Should only be called on SymTerm"

buildUGFun1111111 ::
  forall integerBitWidth s1 s2 s3 s4 s5 s6 s7 a.
  ( SupportedPrim a,
    SupportedPrim s1,
    SupportedPrim s2,
    SupportedPrim s3,
    SupportedPrim s4,
    SupportedPrim s5,
    SupportedPrim s6,
    SupportedPrim s7
  ) =>
  GrisetteSMTConfig integerBitWidth ->
  R.TypeRep s1 ->
  R.TypeRep s2 ->
  R.TypeRep s3 ->
  R.TypeRep s4 ->
  R.TypeRep s5 ->
  R.TypeRep s6 ->
  R.TypeRep s7 ->
  Term a ->
  SymBiMap ->
  Maybe (SymBiMap, TermTy integerBitWidth (s1 --> s2 --> s3 --> s4 --> s5 --> s6 --> s7))
buildUGFun1111111 config ta tb tc td te tf tg term@(SymTerm _ ts) m =
  case ((config, ta), (config, tb), (config, tc), (config, td), (config, te), (config, tf), (config, tg)) of
    (ResolvedSimpleType, ResolvedSimpleType, ResolvedSimpleType, ResolvedSimpleType, ResolvedSimpleType, ResolvedSimpleType, ResolvedSimpleType) ->
      let name = "ufunc_" ++ show (sizeBiMap m)
          f =
            SBV.uninterpret @(TermTy integerBitWidth (s1 --> s2 --> s3 --> s4 --> s5 --> s6 --> s7))
              name
       in Just (addBiMap (SomeTerm term) (toDyn f) name (someTypedSymbol ts) m, f)
    _ -> Nothing
buildUGFun1111111 _ _ _ _ _ _ _ _ _ _ = error "Should only be called on SymTerm"

buildUGFun11111111 ::
  forall integerBitWidth s1 s2 s3 s4 s5 s6 s7 s8 a.
  ( SupportedPrim a,
    SupportedPrim s1,
    SupportedPrim s2,
    SupportedPrim s3,
    SupportedPrim s4,
    SupportedPrim s5,
    SupportedPrim s6,
    SupportedPrim s7,
    SupportedPrim s8
  ) =>
  GrisetteSMTConfig integerBitWidth ->
  R.TypeRep s1 ->
  R.TypeRep s2 ->
  R.TypeRep s3 ->
  R.TypeRep s4 ->
  R.TypeRep s5 ->
  R.TypeRep s6 ->
  R.TypeRep s7 ->
  R.TypeRep s8 ->
  Term a ->
  SymBiMap ->
  Maybe (SymBiMap, TermTy integerBitWidth (s1 --> s2 --> s3 --> s4 --> s5 --> s6 --> s7 --> s8))
buildUGFun11111111 config ta tb tc td te tf tg th term@(SymTerm _ ts) m =
  case ((config, ta), (config, tb), (config, tc), (config, td), (config, te), (config, tf), (config, tg), (config, th)) of
    (ResolvedSimpleType, ResolvedSimpleType, ResolvedSimpleType, ResolvedSimpleType, ResolvedSimpleType, ResolvedSimpleType, ResolvedSimpleType, ResolvedSimpleType) ->
      let name = "ufunc_" ++ show (sizeBiMap m)
          f =
            SBV.uninterpret @(TermTy integerBitWidth (s1 --> s2 --> s3 --> s4 --> s5 --> s6 --> s7 --> s8))
              name
       in Just (addBiMap (SomeTerm term) (toDyn f) name (someTypedSymbol ts) m, f)
    _ -> Nothing
buildUGFun11111111 _ _ _ _ _ _ _ _ _ _ _ = error "Should only be called on SymTerm"

lowerSinglePrimUFun ::
  forall integerBitWidth a.
  GrisetteSMTConfig integerBitWidth ->
  Term a ->
  SymBiMap ->
  Maybe (SymBiMap, TermTy integerBitWidth a)
lowerSinglePrimUFun config t@(SymTerm _ _) m =
  case R.typeRep @a of
    TFun8Type t1 t2 t3 t4 t5 t6 t7 t8 -> buildUTFun11111111 config t1 t2 t3 t4 t5 t6 t7 t8 t m
    TFun7Type t1 t2 t3 t4 t5 t6 t7 -> buildUTFun1111111 config t1 t2 t3 t4 t5 t6 t7 t m
    TFun6Type t1 t2 t3 t4 t5 t6 -> buildUTFun111111 config t1 t2 t3 t4 t5 t6 t m
    TFun5Type t1 t2 t3 t4 t5 -> buildUTFun11111 config t1 t2 t3 t4 t5 t m
    TFun4Type t1 t2 t3 t4 -> buildUTFun1111 config t1 t2 t3 t4 t m
    TFun3Type t1 t2 t3 -> buildUTFun111 config t1 t2 t3 t m
    TFunType t1 t2 -> buildUTFun11 config t1 t2 t m
    GFun8Type t1 t2 t3 t4 t5 t6 t7 t8 -> buildUGFun11111111 config t1 t2 t3 t4 t5 t6 t7 t8 t m
    GFun7Type t1 t2 t3 t4 t5 t6 t7 -> buildUGFun1111111 config t1 t2 t3 t4 t5 t6 t7 t m
    GFun6Type t1 t2 t3 t4 t5 t6 -> buildUGFun111111 config t1 t2 t3 t4 t5 t6 t m
    GFun5Type t1 t2 t3 t4 t5 -> buildUGFun11111 config t1 t2 t3 t4 t5 t m
    GFun4Type t1 t2 t3 t4 -> buildUGFun1111 config t1 t2 t3 t4 t m
    GFun3Type t1 t2 t3 -> buildUGFun111 config t1 t2 t3 t m
    GFunType t1 t2 -> buildUGFun11 config t1 t2 t m
    _ -> Nothing
lowerSinglePrimUFun _ _ _ = error "Should not call this function"

class (Monad m) => SBVFreshMonad m where
  sbvFresh :: (SBV.SymVal a) => String -> m (SBV.SBV a)

instance (MonadIO m) => SBVFreshMonad (SBVT.SymbolicT m) where
  sbvFresh = SBVT.free

instance (MonadIO m) => SBVFreshMonad (SBVTC.QueryT m) where
  sbvFresh = SBVTC.freshVar

instance (SBVFreshMonad m) => SBVFreshMonad (ReaderT r m) where
  sbvFresh = lift . sbvFresh

instance (SBVFreshMonad m) => SBVFreshMonad (StateT s m) where
  sbvFresh = lift . sbvFresh

lowerUnaryTerm ::
  forall integerBitWidth a a1 x x1 m.
  (Typeable x1, a1 ~ TermTy integerBitWidth a, SupportedPrim x, HasCallStack, SBVFreshMonad m) =>
  GrisetteSMTConfig integerBitWidth ->
  Term x ->
  Term a ->
  (a1 -> x1) ->
  SymBiMap ->
  m (SymBiMap, x1)
lowerUnaryTerm config orig t1 f m = do
  (m1, l1) <- lowerSinglePrimCached config t1 m
  let g = f l1
  return (addBiMapIntermediate (SomeTerm orig) (toDyn g) m1, g)

lowerBinaryTerm ::
  forall integerBitWidth a b a1 b1 x x1 m.
  (Typeable x1, a1 ~ TermTy integerBitWidth a, b1 ~ TermTy integerBitWidth b, SupportedPrim x, HasCallStack, SBVFreshMonad m) =>
  GrisetteSMTConfig integerBitWidth ->
  Term x ->
  Term a ->
  Term b ->
  (a1 -> b1 -> x1) ->
  SymBiMap ->
  m (SymBiMap, x1)
lowerBinaryTerm config orig t1 t2 f m = do
  (m1, l1) <- lowerSinglePrimCached config t1 m
  (m2, l2) <- lowerSinglePrimCached config t2 m1
  let g = f l1 l2
  return (addBiMapIntermediate (SomeTerm orig) (toDyn g) m2, g)

lowerSinglePrimCached ::
  forall integerBitWidth a m.
  (HasCallStack, SBVFreshMonad m) =>
  GrisetteSMTConfig integerBitWidth ->
  Term a ->
  SymBiMap ->
  m (SymBiMap, TermTy integerBitWidth a)
lowerSinglePrimCached config t m =
  introSupportedPrimConstraint t $
    case (config, R.typeRep @a) of
      ResolvedDeepType ->
        case lookupTerm (SomeTerm t) m of
          Just x -> return (m, fromDyn x undefined)
          Nothing -> lowerSinglePrimImpl config t m
      _ -> translateTypeError (R.typeRep @a)

lowerSinglePrim ::
  forall integerBitWidth a m.
  (HasCallStack, SBVFreshMonad m) =>
  GrisetteSMTConfig integerBitWidth ->
  Term a ->
  m (SymBiMap, TermTy integerBitWidth a)
lowerSinglePrim config t = lowerSinglePrimCached config t emptySymBiMap

lowerSinglePrimImpl ::
  forall integerBitWidth a m.
  (HasCallStack, SBVFreshMonad m) =>
  GrisetteSMTConfig integerBitWidth ->
  Term a ->
  SymBiMap ->
  m (SymBiMap, TermTy integerBitWidth a)
lowerSinglePrimImpl config@ResolvedConfig {} (ConTerm _ v) m = return (m, lowerValue config v)
lowerSinglePrimImpl config t@(SymTerm _ ts) m =
  fromMaybe errorMsg $ asum [simple, ufunc]
  where
    errorMsg :: forall x. x
    errorMsg = translateTypeError (R.typeRep @a)
    simple :: Maybe (m (SymBiMap, TermTy integerBitWidth a))
    simple = case (config, R.typeRep @a) of
      ResolvedSimpleType -> Just $ do
        let name = show ts
        (g :: TermTy integerBitWidth a) <- sbvFresh name
        return (addBiMap (SomeTerm t) (toDyn g) name (someTypedSymbol ts) m, g)
      _ -> Nothing
    ufunc :: (Maybe (m (SymBiMap, TermTy integerBitWidth a)))
    ufunc = return <$> lowerSinglePrimUFun config t m
lowerSinglePrimImpl _ (UnaryTerm _ op (_ :: Term x)) _ = errorMsg
  where
    errorMsg :: forall t1. t1
    errorMsg = translateUnaryError (show op) (R.typeRep @x) (R.typeRep @a)
lowerSinglePrimImpl _ (BinaryTerm _ op (_ :: Term x) (_ :: Term y)) _ = errorMsg
  where
    errorMsg :: forall t1. t1
    errorMsg = translateBinaryError (show op) (R.typeRep @x) (R.typeRep @y) (R.typeRep @a)
lowerSinglePrimImpl ResolvedConfig {} (TernaryTerm _ op (_ :: Term x) (_ :: Term y) (_ :: Term z)) _ = errorMsg
  where
    errorMsg :: forall t1. t1
    errorMsg = translateTernaryError (show op) (R.typeRep @x) (R.typeRep @y) (R.typeRep @z) (R.typeRep @a)
lowerSinglePrimImpl config t@(NotTerm _ arg) m = lowerUnaryTerm config t arg SBV.sNot m
lowerSinglePrimImpl config t@(OrTerm _ arg1 arg2) m = lowerBinaryTerm config t arg1 arg2 (SBV..||) m
lowerSinglePrimImpl config t@(AndTerm _ arg1 arg2) m = lowerBinaryTerm config t arg1 arg2 (SBV..&&) m
lowerSinglePrimImpl config t@(EqvTerm _ (arg1 :: Term x) arg2) m =
  case (config, R.typeRep @x) of
    ResolvedSimpleType -> lowerBinaryTerm config t arg1 arg2 (SBV..==) m
    _ -> translateBinaryError "(==)" (R.typeRep @x) (R.typeRep @x) (R.typeRep @a)
lowerSinglePrimImpl config t@(ITETerm _ cond arg1 arg2) m =
  case (config, R.typeRep @a) of
    ResolvedMergeableType -> do
      (m1, l1) <- lowerSinglePrimCached config cond m
      (m2, l2) <- lowerSinglePrimCached config arg1 m1
      (m3, l3) <- lowerSinglePrimCached config arg2 m2
      let g = SBV.ite l1 l2 l3
      return (addBiMapIntermediate (SomeTerm t) (toDyn g) m3, g)
    _ -> translateBinaryError "ite" (R.typeRep @Bool) (R.typeRep @a) (R.typeRep @a) (R.typeRep @a)
lowerSinglePrimImpl config t@(AddNumTerm _ arg1 arg2) m =
  case (config, R.typeRep @a) of
    ResolvedNumType -> lowerBinaryTerm config t arg1 arg2 (+) m
    _ -> translateBinaryError "(+)" (R.typeRep @a) (R.typeRep @a) (R.typeRep @a)
lowerSinglePrimImpl config t@(UMinusNumTerm _ arg) m =
  case (config, R.typeRep @a) of
    ResolvedNumType -> lowerUnaryTerm config t arg negate m
    _ -> translateUnaryError "negate" (R.typeRep @a) (R.typeRep @a)
lowerSinglePrimImpl config t@(TimesNumTerm _ arg1 arg2) m =
  case (config, R.typeRep @a) of
    ResolvedNumType -> lowerBinaryTerm config t arg1 arg2 (*) m
    _ -> translateBinaryError "(*)" (R.typeRep @a) (R.typeRep @a) (R.typeRep @a)
lowerSinglePrimImpl config t@(AbsNumTerm _ arg) m =
  case (config, R.typeRep @a) of
    ResolvedNumType -> lowerUnaryTerm config t arg abs m
    _ -> translateUnaryError "abs" (R.typeRep @a) (R.typeRep @a)
lowerSinglePrimImpl config t@(SignumNumTerm _ arg) m =
  case (config, R.typeRep @a) of
    ResolvedNumType -> lowerUnaryTerm config t arg signum m
    _ -> translateUnaryError "signum" (R.typeRep @a) (R.typeRep @a)
lowerSinglePrimImpl config t@(LTNumTerm _ (arg1 :: Term arg) arg2) m =
  case (config, R.typeRep @arg) of
    ResolvedNumOrdType -> lowerBinaryTerm config t arg1 arg2 (SBV..<) m
    _ -> translateBinaryError "(<)" (R.typeRep @a) (R.typeRep @a) (R.typeRep @Bool)
lowerSinglePrimImpl config t@(LENumTerm _ (arg1 :: Term arg) arg2) m =
  case (config, R.typeRep @arg) of
    ResolvedNumOrdType -> lowerBinaryTerm config t arg1 arg2 (SBV..<=) m
    _ -> translateBinaryError "(<=)" (R.typeRep @a) (R.typeRep @a) (R.typeRep @Bool)
lowerSinglePrimImpl config t@(AndBitsTerm _ arg1 arg2) m =
  case (config, R.typeRep @a) of
    ResolvedBitsType -> lowerBinaryTerm config t arg1 arg2 (.&.) m
    _ -> translateBinaryError "(.&.)" (R.typeRep @a) (R.typeRep @a) (R.typeRep @a)
lowerSinglePrimImpl config t@(OrBitsTerm _ arg1 arg2) m =
  case (config, R.typeRep @a) of
    ResolvedBitsType -> lowerBinaryTerm config t arg1 arg2 (.|.) m
    _ -> translateBinaryError "(.|.)" (R.typeRep @a) (R.typeRep @a) (R.typeRep @a)
lowerSinglePrimImpl config t@(XorBitsTerm _ arg1 arg2) m =
  case (config, R.typeRep @a) of
    ResolvedBitsType -> lowerBinaryTerm config t arg1 arg2 xor m
    _ -> translateBinaryError "xor" (R.typeRep @a) (R.typeRep @a) (R.typeRep @a)
lowerSinglePrimImpl config t@(ComplementBitsTerm _ arg) m =
  case (config, R.typeRep @a) of
    ResolvedBitsType -> lowerUnaryTerm config t arg complement m
    _ -> translateUnaryError "complement" (R.typeRep @a) (R.typeRep @a)
lowerSinglePrimImpl config t@(ShiftLeftTerm _ arg n) m =
  case (config, R.typeRep @a) of
    ResolvedBitsType -> lowerBinaryTerm config t arg n sShiftLeft m
    _ -> translateBinaryError "shiftLeft" (R.typeRep @a) (R.typeRep @Int) (R.typeRep @a)
lowerSinglePrimImpl config t@(ShiftRightTerm _ arg n) m =
  case (config, R.typeRep @a) of
    ResolvedBitsType -> lowerBinaryTerm config t arg n sShiftRight m
    _ -> translateBinaryError "shiftRight" (R.typeRep @a) (R.typeRep @Int) (R.typeRep @a)
-- SBV's rotateLeft and rotateRight are broken for signed values, so we have to
-- do this
-- https://github.com/LeventErkok/sbv/issues/673
lowerSinglePrimImpl config t@(RotateLeftTerm _ arg n) m =
  case (config, R.typeRep @a) of
    (_, SignedBVType (Proxy :: Proxy n)) ->
      lowerBinaryTerm
        config
        t
        arg
        n
        ( \x y ->
            SBV.sFromIntegral $
              sRotateLeft
                (SBV.sFromIntegral x :: SBV.SWord n)
                (SBV.sFromIntegral y :: SBV.SWord n)
        )
        m
    ResolvedBitsType -> lowerBinaryTerm config t arg n sRotateLeft m
    _ -> translateBinaryError "rotateLeft" (R.typeRep @a) (R.typeRep @Int) (R.typeRep @a)
lowerSinglePrimImpl config t@(RotateRightTerm _ arg n) m =
  case (config, R.typeRep @a) of
    (_, SignedBVType (Proxy :: Proxy n)) ->
      lowerBinaryTerm
        config
        t
        arg
        n
        ( \x y ->
            SBV.sFromIntegral $
              sRotateRight
                (SBV.sFromIntegral x :: SBV.SWord n)
                (SBV.sFromIntegral y :: SBV.SWord n)
        )
        m
    ResolvedBitsType -> lowerBinaryTerm config t arg n sRotateRight m
    _ -> translateBinaryError "rotateRight" (R.typeRep @a) (R.typeRep @Int) (R.typeRep @a)
lowerSinglePrimImpl config t@(ToSignedTerm _ (bv :: Term x)) m =
  case (R.typeRep @a, R.typeRep @x) of
    (SignedBVType (_ :: Proxy na), UnsignedBVType (_ :: Proxy nx)) ->
      case R.eqTypeRep (R.typeRep @na) (R.typeRep @nx) of
        Just R.HRefl ->
          lowerUnaryTerm config t bv SBV.sFromIntegral m
        _ -> translateUnaryError "u2s" (R.typeRep @x) (R.typeRep @a)
    _ -> translateUnaryError "u2s" (R.typeRep @x) (R.typeRep @a)
lowerSinglePrimImpl config t@(ToUnsignedTerm _ (bv :: Term x)) m =
  case (R.typeRep @a, R.typeRep @x) of
    (UnsignedBVType (_ :: Proxy na), SignedBVType (_ :: Proxy nx)) ->
      case R.eqTypeRep (R.typeRep @na) (R.typeRep @nx) of
        Just R.HRefl ->
          lowerUnaryTerm config t bv SBV.sFromIntegral m
        _ -> translateUnaryError "s2u" (R.typeRep @x) (R.typeRep @a)
    _ -> translateUnaryError "s2u" (R.typeRep @x) (R.typeRep @a)
lowerSinglePrimImpl config t@(BVConcatTerm _ (bv1 :: Term x) (bv2 :: Term y)) m =
  case (R.typeRep @a, R.typeRep @x, R.typeRep @y) of
    (UnsignedBVType (_ :: Proxy na), UnsignedBVType (_ :: Proxy nx), UnsignedBVType (_ :: Proxy ny)) ->
      case (unsafeAxiom @(nx + ny) @na) of
        Refl -> lowerBinaryTerm config t bv1 bv2 (SBV.#) m
    (SignedBVType (_ :: Proxy na), SignedBVType (_ :: Proxy nx), SignedBVType (_ :: Proxy ny)) ->
      case (unsafeAxiom @(nx + ny) @na) of
        Refl ->
          lowerBinaryTerm
            config
            t
            bv1
            bv2
            ( \(x :: SBV.SInt xn) (y :: SBV.SInt yn) ->
                SBV.sFromIntegral $
                  (SBV.sFromIntegral x :: SBV.SWord xn) SBV.# (SBV.sFromIntegral y :: SBV.SWord yn)
            )
            m
    _ -> translateBinaryError "bvconcat" (R.typeRep @x) (R.typeRep @y) (R.typeRep @a)
lowerSinglePrimImpl config t@(BVSelectTerm _ (ix :: R.TypeRep ix) w (bv :: Term x)) m =
  case (R.typeRep @a, R.typeRep @x) of
    (UnsignedBVType (_ :: Proxy na), UnsignedBVType (_ :: Proxy xn)) ->
      withKnownProof (unsafeKnownProof @(na + ix - 1) (natVal (Proxy @na) + natVal (Proxy @ix) - 1)) $
        case ( unsafeAxiom @(na + ix - 1 - ix + 1) @na,
               unsafeLeqProof @(na + ix - 1 + 1) @xn,
               unsafeLeqProof @ix @(na + ix - 1)
             ) of
          (Refl, LeqProof, LeqProof) ->
            lowerUnaryTerm config t bv (SBV.bvExtract (Proxy @(na + ix - 1)) (Proxy @ix)) m
    (SignedBVType (_ :: Proxy na), SignedBVType (_ :: Proxy xn)) ->
      withKnownProof (unsafeKnownProof @(na + ix - 1) (natVal (Proxy @na) + natVal (Proxy @ix) - 1)) $
        case ( unsafeAxiom @(na + ix - 1 - ix + 1) @na,
               unsafeLeqProof @(na + ix - 1 + 1) @xn,
               unsafeLeqProof @ix @(na + ix - 1)
             ) of
          (Refl, LeqProof, LeqProof) ->
            lowerUnaryTerm config t bv (SBV.bvExtract (Proxy @(na + ix - 1)) (Proxy @ix)) m
    _ -> translateTernaryError "bvselect" ix w (R.typeRep @x) (R.typeRep @a)
lowerSinglePrimImpl config t@(BVExtendTerm _ signed (n :: R.TypeRep n) (bv :: Term x)) m =
  case (R.typeRep @a, R.typeRep @x) of
    (UnsignedBVType (_ :: Proxy na), UnsignedBVType (_ :: Proxy nx)) ->
      withKnownProof (unsafeKnownProof @(na - nx) (natVal (Proxy @na) - natVal (Proxy @nx))) $
        case (unsafeLeqProof @(nx + 1) @na, unsafeLeqProof @1 @(na - nx)) of
          (LeqProof, LeqProof) ->
            bvIsNonZeroFromGEq1 @(na - nx) $
              lowerUnaryTerm config t bv (if signed then SBV.signExtend else SBV.zeroExtend) m
    (SignedBVType (_ :: Proxy na), SignedBVType (_ :: Proxy nx)) ->
      withKnownProof (unsafeKnownProof @(na - nx) (natVal (Proxy @na) - natVal (Proxy @nx))) $
        case (unsafeLeqProof @(nx + 1) @na, unsafeLeqProof @1 @(na - nx)) of
          (LeqProof, LeqProof) ->
            bvIsNonZeroFromGEq1 @(na - nx) $
              lowerUnaryTerm
                config
                t
                bv
                ( if signed
                    then SBV.signExtend
                    else \x ->
                      SBV.sFromIntegral
                        (SBV.zeroExtend (SBV.sFromIntegral x :: SBV.SBV (SBV.WordN nx)) :: SBV.SBV (SBV.WordN na))
                )
                m
    _ -> translateTernaryError "bvextend" (R.typeRep @Bool) n (R.typeRep @x) (R.typeRep @a)
lowerSinglePrimImpl config t@(TabularFunApplyTerm _ (f :: Term (b =-> a)) (arg :: Term b)) m =
  case (config, R.typeRep @a) of
    ResolvedDeepType -> do
      (m1, l1) <- lowerSinglePrimCached config f m
      (m2, l2) <- lowerSinglePrimCached config arg m1
      let g = l1 l2
      return (addBiMapIntermediate (SomeTerm t) (toDyn g) m2, g)
    _ -> translateBinaryError "tabularApply" (R.typeRep @(b =-> a)) (R.typeRep @b) (R.typeRep @a)
lowerSinglePrimImpl config t@(GeneralFunApplyTerm _ (f :: Term (b --> a)) (arg :: Term b)) m =
  case (config, R.typeRep @a) of
    ResolvedDeepType -> do
      (m1, l1) <- lowerSinglePrimCached config f m
      (m2, l2) <- lowerSinglePrimCached config arg m1
      let g = l1 l2
      return (addBiMapIntermediate (SomeTerm t) (toDyn g) m2, g)
    _ -> translateBinaryError "generalApply" (R.typeRep @(b --> a)) (R.typeRep @b) (R.typeRep @a)
lowerSinglePrimImpl config t@(DivIntegralTerm _ arg1 arg2) m =
  case (config, R.typeRep @a) of
    ResolvedSDivisibleType -> lowerBinaryTerm config t arg1 arg2 SBV.sDiv m
    _ -> translateBinaryError "div" (R.typeRep @a) (R.typeRep @a) (R.typeRep @a)
lowerSinglePrimImpl config t@(ModIntegralTerm _ arg1 arg2) m =
  case (config, R.typeRep @a) of
    ResolvedSDivisibleType -> lowerBinaryTerm config t arg1 arg2 SBV.sMod m
    _ -> translateBinaryError "mod" (R.typeRep @a) (R.typeRep @a) (R.typeRep @a)
lowerSinglePrimImpl config t@(QuotIntegralTerm _ arg1 arg2) m =
  case (config, R.typeRep @a) of
    ResolvedSDivisibleType -> lowerBinaryTerm config t arg1 arg2 SBV.sQuot m
    _ -> translateBinaryError "quot" (R.typeRep @a) (R.typeRep @a) (R.typeRep @a)
lowerSinglePrimImpl config t@(RemIntegralTerm _ arg1 arg2) m =
  case (config, R.typeRep @a) of
    ResolvedSDivisibleType -> lowerBinaryTerm config t arg1 arg2 SBV.sRem m
    _ -> translateBinaryError "rem" (R.typeRep @a) (R.typeRep @a) (R.typeRep @a)
lowerSinglePrimImpl config t@(DivBoundedIntegralTerm _ arg1 arg2) m =
  case (config, R.typeRep @a) of
    ResolvedSDivisibleType -> lowerBinaryTerm config t arg1 arg2 SBV.sDiv m
    _ -> translateBinaryError "div" (R.typeRep @a) (R.typeRep @a) (R.typeRep @a)
lowerSinglePrimImpl config t@(ModBoundedIntegralTerm _ arg1 arg2) m =
  case (config, R.typeRep @a) of
    ResolvedSDivisibleType -> lowerBinaryTerm config t arg1 arg2 SBV.sMod m
    _ -> translateBinaryError "mod" (R.typeRep @a) (R.typeRep @a) (R.typeRep @a)
lowerSinglePrimImpl config t@(QuotBoundedIntegralTerm _ arg1 arg2) m =
  case (config, R.typeRep @a) of
    ResolvedSDivisibleType -> lowerBinaryTerm config t arg1 arg2 SBV.sQuot m
    _ -> translateBinaryError "quot" (R.typeRep @a) (R.typeRep @a) (R.typeRep @a)
lowerSinglePrimImpl config t@(RemBoundedIntegralTerm _ arg1 arg2) m =
  case (config, R.typeRep @a) of
    ResolvedSDivisibleType -> lowerBinaryTerm config t arg1 arg2 SBV.sRem m
    _ -> translateBinaryError "rem" (R.typeRep @a) (R.typeRep @a) (R.typeRep @a)
lowerSinglePrimImpl _ _ _ = error "Should never happen"

bvIsNonZeroFromGEq1 :: forall w r. (1 <= w) => ((SBV.BVIsNonZero w) => r) -> r
bvIsNonZeroFromGEq1 r1 = case unsafeAxiom :: w :~: 1 of
  Refl -> r1

#if MIN_VERSION_sbv(10,3,0)
preprocessUIFuncs ::
  [(String, (Bool, SBVI.SBVType, Either String ([([SBVI.CV], SBVI.CV)], SBVI.CV)))] ->
  Maybe [(String, (SBVI.SBVType, ([([SBVI.CV], SBVI.CV)], SBVI.CV)))]
preprocessUIFuncs =
  traverse
    (\case
      (a, (_, b, Right c)) -> Just (a, (b, c))
      _ -> Nothing)
#elif MIN_VERSION_sbv(10,0,0)
preprocessUIFuncs ::
  [(String, (SBVI.SBVType, Either String ([([SBVI.CV], SBVI.CV)], SBVI.CV)))] ->
  Maybe [(String, (SBVI.SBVType, ([([SBVI.CV], SBVI.CV)], SBVI.CV)))]
preprocessUIFuncs =
  traverse
    (\case
      (a, (b, Right c)) -> Just (a, (b, c))
      _ -> Nothing)
#else
preprocessUIFuncs ::
  [(String, (SBVI.SBVType, ([([SBVI.CV], SBVI.CV)], SBVI.CV)))] ->
  Maybe [(String, (SBVI.SBVType, ([([SBVI.CV], SBVI.CV)], SBVI.CV)))]
preprocessUIFuncs = Just
#endif

parseModel :: forall integerBitWidth. GrisetteSMTConfig integerBitWidth -> SBVI.SMTModel -> SymBiMap -> PM.Model
parseModel _ (SBVI.SMTModel _ _ assoc orguifuncs) mp =
  case preprocessUIFuncs orguifuncs of
    Just uifuncs -> foldr gouifuncs (foldr goassoc emptyModel assoc) uifuncs
    _ -> error "SBV Failed to parse model"
  where
    goassoc :: (String, SBVI.CV) -> PM.Model -> PM.Model
    goassoc (name, cv) m = case findStringToSymbol name mp of
      Just (SomeTypedSymbol tr s) ->
        insertValue s (resolveSingle tr cv) m
      Nothing -> error "Bad"
    resolveSingle :: R.TypeRep a -> SBVI.CV -> a
    resolveSingle t (SBVI.CV SBVI.KBool (SBVI.CInteger n)) =
      case R.eqTypeRep t (R.typeRep @Bool) of
        Just R.HRefl -> n /= 0
        Nothing -> error "Bad type"
    resolveSingle t (SBVI.CV SBVI.KUnbounded (SBVI.CInteger i)) =
      case R.eqTypeRep t (R.typeRep @Integer) of
        Just R.HRefl -> i
        Nothing -> error "Bad type"
    resolveSingle t (SBVI.CV (SBVI.KBounded _ bitWidth) (SBVI.CInteger i)) =
      case R.eqTypeRep t (R.typeRep @Integer) of
        Just R.HRefl -> i
        _ -> case t of
          R.App a (n :: R.TypeRep w) ->
            case R.eqTypeRep (R.typeRepKind n) (R.typeRep @Nat) of
              Just R.HRefl ->
                case (unsafeKnownProof @w (fromIntegral bitWidth), unsafeLeqProof @1 @w) of
                  (KnownProof, LeqProof) ->
                    case (R.eqTypeRep a (R.typeRep @IntN), R.eqTypeRep a (R.typeRep @WordN)) of
                      (Just R.HRefl, _) ->
                        fromInteger i
                      (_, Just R.HRefl) -> fromInteger i
                      _ -> error "Bad type"
              _ -> error "Bad type"
          _ -> error "Bad type"
    resolveSingle _ _ = error "Unknown cv"

    buildConstFun :: (SupportedPrim a, SupportedPrim r) => R.TypeRep a -> R.TypeRep r -> SBVI.CV -> a =-> r
    buildConstFun _ tr v = case tr of
      TFunType (ta2' :: R.TypeRep a2) (tr2' :: R.TypeRep r2) -> TabularFun [] $ buildConstFun ta2' tr2' v
      _ -> TabularFun [] $ resolveSingle tr v

    goutfuncResolve ::
      forall a r.
      (SupportedPrim a, SupportedPrim r) =>
      R.TypeRep a ->
      R.TypeRep r ->
      ([([SBVI.CV], SBVI.CV)], SBVI.CV) ->
      (a =-> r)
    goutfuncResolve ta1 ta2 (l, s) =
      case ta2 of
        TFunType (ta2' :: R.TypeRep a2) (tr2' :: R.TypeRep r2) ->
          TabularFun
            (second (\r -> goutfuncResolve ta2' tr2' (r, s)) <$> partition ta1 l)
            (buildConstFun ta2' tr2' s)
        _ ->
          TabularFun
            (bimap (resolveSingle ta1 . head) (resolveSingle ta2) <$> l)
            (resolveSingle ta2 s)

    gougfuncResolve ::
      forall a r.
      (SupportedPrim a, SupportedPrim r) =>
      Int ->
      R.TypeRep a ->
      R.TypeRep r ->
      ([([SBVI.CV], SBVI.CV)], SBVI.CV) ->
      (a --> r)
    gougfuncResolve idx ta1 ta2 (l, s) =
      case ta2 of
        GFunType (ta2' :: R.TypeRep a2) (tr2' :: R.TypeRep r2) ->
          let sym = IndexedSymbol "arg" idx
              funs = second (\r -> gougfuncResolve (idx + 1) ta2' tr2' (r, s)) <$> partition ta1 l
              def = gougfuncResolve (idx + 1) ta2' tr2' ([], s)
              body =
                foldl'
                  ( \acc (v, f) ->
                      pevalITETerm
                        (pevalEqvTerm (symTerm sym) (conTerm v))
                        (conTerm f)
                        acc
                  )
                  (conTerm def)
                  funs
           in buildGeneralFun (TypedSymbol sym) body
        _ ->
          let sym = IndexedSymbol "arg" idx
              vs = bimap (resolveSingle ta1 . head) (resolveSingle ta2) <$> l
              def = resolveSingle ta2 s
              body =
                foldl'
                  ( \acc (v, a) ->
                      pevalITETerm
                        (pevalEqvTerm (symTerm sym) (conTerm v))
                        (conTerm a)
                        acc
                  )
                  (conTerm def)
                  vs
           in buildGeneralFun (TypedSymbol sym) body
    partition :: R.TypeRep a -> [([SBVI.CV], SBVI.CV)] -> [(a, [([SBVI.CV], SBVI.CV)])]
    partition t = case (R.eqTypeRep t (R.typeRep @Bool), R.eqTypeRep t (R.typeRep @Integer)) of
      (Just R.HRefl, _) -> partitionWithOrd . resolveFirst t
      (_, Just R.HRefl) -> partitionWithOrd . resolveFirst t
      _ -> case t of
        R.App bv _ -> case (R.eqTypeRep bv (R.typeRep @IntN), R.eqTypeRep bv (R.typeRep @WordN)) of
          (Just R.HRefl, _) -> fmap (first IntN) . partitionWithOrd . fmap (first unIntN) . resolveFirst t
          (_, Just R.HRefl) -> partitionWithOrd . resolveFirst t
          _ -> error "Unknown type"
        _ -> error "Unknown type"

    resolveFirst :: R.TypeRep a -> [([SBVI.CV], SBVI.CV)] -> [(a, [([SBVI.CV], SBVI.CV)])]
    resolveFirst tf = fmap (\case (x : xs, v) -> (resolveSingle tf x, [(xs, v)]); _ -> error "impossible")

    partitionWithOrd :: forall a. (Ord a) => [(a, [([SBVI.CV], SBVI.CV)])] -> [(a, [([SBVI.CV], SBVI.CV)])]
    partitionWithOrd v = go sorted
      where
        sorted = sortWith fst v
        go (x : x1 : xs) =
          if fst x == fst x1
            then go $ (fst x, snd x ++ snd x1) : xs
            else x : go (x1 : xs)
        go x = x

    gouifuncs :: (String, (SBVI.SBVType, ([([SBVI.CV], SBVI.CV)], SBVI.CV))) -> PM.Model -> PM.Model
    gouifuncs (name, (SBVI.SBVType _, l)) m = case findStringToSymbol name mp of
      Just (SomeTypedSymbol tr s) -> withSymbolSupported s $ case tr of
        t@(TFunType a r) -> R.withTypeable t $ insertValue s (goutfuncResolve a r l) m
        t@(GFunType a r) -> R.withTypeable t $ insertValue s (gougfuncResolve 0 a r l) m
        _ -> error "Bad"
      Nothing -> error "Bad"

-- helpers

data BVTypeContainer bv k where
  BVTypeContainer :: (SBV.BVIsNonZero n, KnownNat n, 1 <= n, k ~ bv n) => Proxy n -> BVTypeContainer bv k

signedBVTypeView :: forall t. (SupportedPrim t) => R.TypeRep t -> Maybe (BVTypeContainer IntN t)
signedBVTypeView t = case t of
  R.App s (n :: R.TypeRep w) ->
    case (R.eqTypeRep s (R.typeRep @IntN), R.eqTypeRep (R.typeRepKind n) (R.typeRep @Nat)) of
      (Just R.HRefl, Just R.HRefl) ->
        Just $ unsafeBVIsNonZero @w $ withPrim (Proxy @t) (BVTypeContainer Proxy)
      _ -> Nothing
  _ -> Nothing
  where
    unsafeBVIsNonZero :: forall w r. ((SBV.BVIsNonZero w) => r) -> r
    unsafeBVIsNonZero r1 = case unsafeAxiom :: w :~: 1 of
      Refl -> r1

pattern SignedBVType ::
  forall t.
  (SupportedPrim t) =>
  forall (n :: Nat).
  (t ~~ IntN n, KnownNat n, 1 <= n, SBV.BVIsNonZero n) =>
  Proxy n ->
  R.TypeRep t
pattern SignedBVType p <- (signedBVTypeView @t -> Just (BVTypeContainer p))

unsignedBVTypeView :: forall t. (SupportedPrim t) => R.TypeRep t -> Maybe (BVTypeContainer WordN t)
unsignedBVTypeView t = case t of
  R.App s (n :: R.TypeRep w) ->
    case (R.eqTypeRep s (R.typeRep @WordN), R.eqTypeRep (R.typeRepKind n) (R.typeRep @Nat)) of
      (Just R.HRefl, Just R.HRefl) ->
        Just $ unsafeBVIsNonZero @w $ withPrim (Proxy @t) (BVTypeContainer Proxy)
      _ -> Nothing
  _ -> Nothing
  where
    unsafeBVIsNonZero :: forall w r. ((SBV.BVIsNonZero w) => r) -> r
    unsafeBVIsNonZero r1 = case unsafeAxiom :: w :~: 1 of
      Refl -> r1

pattern UnsignedBVType ::
  forall t.
  (SupportedPrim t) =>
  forall (n :: Nat).
  (t ~~ WordN n, KnownNat n, 1 <= n, SBV.BVIsNonZero n) =>
  Proxy n ->
  R.TypeRep t
pattern UnsignedBVType p <- (unsignedBVTypeView @t -> Just (BVTypeContainer p))

data TFunTypeContainer :: forall k. k -> Type where
  TFunTypeContainer :: (SupportedPrim a, SupportedPrim b) => R.TypeRep a -> R.TypeRep b -> TFunTypeContainer (a =-> b)

tFunTypeView :: forall t. (SupportedPrim t) => R.TypeRep t -> Maybe (TFunTypeContainer t)
tFunTypeView t = case t of
  R.App (R.App arr (ta2' :: R.TypeRep a2)) (tr2' :: R.TypeRep r2) ->
    case R.eqTypeRep arr (R.typeRep @(=->)) of
      Just R.HRefl -> Just $ withPrim (Proxy @t) $ TFunTypeContainer ta2' tr2'
      Nothing -> Nothing
  _ -> Nothing

pattern TFunType ::
  forall t.
  (SupportedPrim t) =>
  forall (a :: Type) (b :: Type).
  (t ~~ (a =-> b), SupportedPrim a, SupportedPrim b) =>
  R.TypeRep a ->
  R.TypeRep b ->
  R.TypeRep t
pattern TFunType a b <-
  (tFunTypeView -> Just (TFunTypeContainer a b))
  where
    TFunType a b = R.App (R.App (R.typeRep @(=->)) a) b

pattern TFun3Type ::
  forall t.
  (SupportedPrim t) =>
  forall (a :: Type) (b :: Type) (c :: Type).
  (t ~~ (a =-> b =-> c), SupportedPrim a, SupportedPrim b, SupportedPrim c) =>
  R.TypeRep a ->
  R.TypeRep b ->
  R.TypeRep c ->
  R.TypeRep t
pattern TFun3Type a b c = TFunType a (TFunType b c)

pattern TFun4Type ::
  forall t.
  (SupportedPrim t) =>
  forall (a :: Type) (b :: Type) (c :: Type) (d :: Type).
  ( t ~~ (a =-> b =-> c =-> d),
    SupportedPrim a,
    SupportedPrim b,
    SupportedPrim c,
    SupportedPrim d
  ) =>
  R.TypeRep a ->
  R.TypeRep b ->
  R.TypeRep c ->
  R.TypeRep d ->
  R.TypeRep t
pattern TFun4Type a b c d = TFunType a (TFunType b (TFunType c d))

pattern TFun5Type ::
  forall t.
  (SupportedPrim t) =>
  forall (a :: Type) (b :: Type) (c :: Type) (d :: Type) (e :: Type).
  ( t ~~ (a =-> b =-> c =-> d =-> e),
    SupportedPrim a,
    SupportedPrim b,
    SupportedPrim c,
    SupportedPrim d,
    SupportedPrim e
  ) =>
  R.TypeRep a ->
  R.TypeRep b ->
  R.TypeRep c ->
  R.TypeRep d ->
  R.TypeRep e ->
  R.TypeRep t
pattern TFun5Type a b c d e =
  TFunType
    a
    ( TFunType
        b
        ( TFunType
            c
            (TFunType d e)
          )
      )

pattern TFun6Type ::
  forall t.
  (SupportedPrim t) =>
  forall
    (a :: Type)
    (b :: Type)
    (c :: Type)
    (d :: Type)
    (e :: Type)
    (f :: Type).
  ( t ~~ (a =-> b =-> c =-> d =-> e =-> f),
    SupportedPrim a,
    SupportedPrim b,
    SupportedPrim c,
    SupportedPrim d,
    SupportedPrim e,
    SupportedPrim f
  ) =>
  R.TypeRep a ->
  R.TypeRep b ->
  R.TypeRep c ->
  R.TypeRep d ->
  R.TypeRep e ->
  R.TypeRep f ->
  R.TypeRep t
pattern TFun6Type a b c d e f =
  TFunType
    a
    ( TFunType
        b
        ( TFunType
            c
            ( TFunType
                d
                (TFunType e f)
              )
          )
      )

pattern TFun7Type ::
  forall t.
  (SupportedPrim t) =>
  forall
    (a :: Type)
    (b :: Type)
    (c :: Type)
    (d :: Type)
    (e :: Type)
    (f :: Type)
    (g :: Type).
  ( t ~~ (a =-> b =-> c =-> d =-> e =-> f =-> g),
    SupportedPrim a,
    SupportedPrim b,
    SupportedPrim c,
    SupportedPrim d,
    SupportedPrim e,
    SupportedPrim f,
    SupportedPrim g
  ) =>
  R.TypeRep a ->
  R.TypeRep b ->
  R.TypeRep c ->
  R.TypeRep d ->
  R.TypeRep e ->
  R.TypeRep f ->
  R.TypeRep g ->
  R.TypeRep t
pattern TFun7Type a b c d e f g =
  TFunType
    a
    ( TFunType
        b
        ( TFunType
            c
            ( TFunType
                d
                ( TFunType
                    e
                    (TFunType f g)
                  )
              )
          )
      )

pattern TFun8Type ::
  forall t.
  (SupportedPrim t) =>
  forall
    (a :: Type)
    (b :: Type)
    (c :: Type)
    (d :: Type)
    (e :: Type)
    (f :: Type)
    (g :: Type)
    (h :: Type).
  ( t ~~ (a =-> b =-> c =-> d =-> e =-> f =-> g =-> h),
    SupportedPrim a,
    SupportedPrim b,
    SupportedPrim c,
    SupportedPrim d,
    SupportedPrim e,
    SupportedPrim f,
    SupportedPrim g,
    SupportedPrim h
  ) =>
  R.TypeRep a ->
  R.TypeRep b ->
  R.TypeRep c ->
  R.TypeRep d ->
  R.TypeRep e ->
  R.TypeRep f ->
  R.TypeRep g ->
  R.TypeRep h ->
  R.TypeRep t
pattern TFun8Type a b c d e f g h =
  TFunType
    a
    ( TFunType
        b
        ( TFunType
            c
            ( TFunType
                d
                ( TFunType
                    e
                    ( TFunType
                        f
                        (TFunType g h)
                      )
                  )
              )
          )
      )

data GFunTypeContainer :: forall k. k -> Type where
  GFunTypeContainer :: (SupportedPrim a, SupportedPrim b) => R.TypeRep a -> R.TypeRep b -> GFunTypeContainer (a --> b)

gFunTypeView :: forall t. (SupportedPrim t) => R.TypeRep t -> Maybe (GFunTypeContainer t)
gFunTypeView t = case t of
  R.App (R.App arr (ta2' :: R.TypeRep a2)) (tr2' :: R.TypeRep r2) ->
    case R.eqTypeRep arr (R.typeRep @(-->)) of
      Just R.HRefl -> Just $ withPrim (Proxy @t) $ GFunTypeContainer ta2' tr2'
      Nothing -> Nothing
  _ -> Nothing

pattern GFunType ::
  forall t.
  (SupportedPrim t) =>
  forall (a :: Type) (b :: Type).
  (t ~~ (a --> b), SupportedPrim a, SupportedPrim b) =>
  R.TypeRep a ->
  R.TypeRep b ->
  R.TypeRep t
pattern GFunType a b <-
  (gFunTypeView -> Just (GFunTypeContainer a b))
  where
    GFunType a b = R.App (R.App (R.typeRep @(-->)) a) b

pattern GFun3Type ::
  forall t.
  (SupportedPrim t) =>
  forall (a :: Type) (b :: Type) (c :: Type).
  (t ~~ (a --> b --> c), SupportedPrim a, SupportedPrim b, SupportedPrim c) =>
  R.TypeRep a ->
  R.TypeRep b ->
  R.TypeRep c ->
  R.TypeRep t
pattern GFun3Type a b c = GFunType a (GFunType b c)

pattern GFun4Type ::
  forall t.
  (SupportedPrim t) =>
  forall (a :: Type) (b :: Type) (c :: Type) (d :: Type).
  ( t ~~ (a --> b --> c --> d),
    SupportedPrim a,
    SupportedPrim b,
    SupportedPrim c,
    SupportedPrim d
  ) =>
  R.TypeRep a ->
  R.TypeRep b ->
  R.TypeRep c ->
  R.TypeRep d ->
  R.TypeRep t
pattern GFun4Type a b c d = GFunType a (GFunType b (GFunType c d))

pattern GFun5Type ::
  forall t.
  (SupportedPrim t) =>
  forall (a :: Type) (b :: Type) (c :: Type) (d :: Type) (e :: Type).
  ( t ~~ (a --> b --> c --> d --> e),
    SupportedPrim a,
    SupportedPrim b,
    SupportedPrim c,
    SupportedPrim d,
    SupportedPrim e
  ) =>
  R.TypeRep a ->
  R.TypeRep b ->
  R.TypeRep c ->
  R.TypeRep d ->
  R.TypeRep e ->
  R.TypeRep t
pattern GFun5Type a b c d e =
  GFunType
    a
    ( GFunType
        b
        ( GFunType
            c
            (GFunType d e)
          )
      )

pattern GFun6Type ::
  forall t.
  (SupportedPrim t) =>
  forall
    (a :: Type)
    (b :: Type)
    (c :: Type)
    (d :: Type)
    (e :: Type)
    (f :: Type).
  ( t ~~ (a --> b --> c --> d --> e --> f),
    SupportedPrim a,
    SupportedPrim b,
    SupportedPrim c,
    SupportedPrim d,
    SupportedPrim e,
    SupportedPrim f
  ) =>
  R.TypeRep a ->
  R.TypeRep b ->
  R.TypeRep c ->
  R.TypeRep d ->
  R.TypeRep e ->
  R.TypeRep f ->
  R.TypeRep t
pattern GFun6Type a b c d e f =
  GFunType
    a
    ( GFunType
        b
        ( GFunType
            c
            ( GFunType
                d
                (GFunType e f)
              )
          )
      )

pattern GFun7Type ::
  forall t.
  (SupportedPrim t) =>
  forall
    (a :: Type)
    (b :: Type)
    (c :: Type)
    (d :: Type)
    (e :: Type)
    (f :: Type)
    (g :: Type).
  ( t ~~ (a --> b --> c --> d --> e --> f --> g),
    SupportedPrim a,
    SupportedPrim b,
    SupportedPrim c,
    SupportedPrim d,
    SupportedPrim e,
    SupportedPrim f,
    SupportedPrim g
  ) =>
  R.TypeRep a ->
  R.TypeRep b ->
  R.TypeRep c ->
  R.TypeRep d ->
  R.TypeRep e ->
  R.TypeRep f ->
  R.TypeRep g ->
  R.TypeRep t
pattern GFun7Type a b c d e f g =
  GFunType
    a
    ( GFunType
        b
        ( GFunType
            c
            ( GFunType
                d
                ( GFunType
                    e
                    (GFunType f g)
                  )
              )
          )
      )

pattern GFun8Type ::
  forall t.
  (SupportedPrim t) =>
  forall
    (a :: Type)
    (b :: Type)
    (c :: Type)
    (d :: Type)
    (e :: Type)
    (f :: Type)
    (g :: Type)
    (h :: Type).
  ( t ~~ (a --> b --> c --> d --> e --> f --> g --> h),
    SupportedPrim a,
    SupportedPrim b,
    SupportedPrim c,
    SupportedPrim d,
    SupportedPrim e,
    SupportedPrim f,
    SupportedPrim g,
    SupportedPrim h
  ) =>
  R.TypeRep a ->
  R.TypeRep b ->
  R.TypeRep c ->
  R.TypeRep d ->
  R.TypeRep e ->
  R.TypeRep f ->
  R.TypeRep g ->
  R.TypeRep h ->
  R.TypeRep t
pattern GFun8Type a b c d e f g h =
  GFunType
    a
    ( GFunType
        b
        ( GFunType
            c
            ( GFunType
                d
                ( GFunType
                    e
                    ( GFunType
                        f
                        (GFunType g h)
                      )
                  )
              )
          )
      )

pattern BoolType ::
  forall t.
  () =>
  (t ~~ Bool) =>
  R.TypeRep t
pattern BoolType <- (R.eqTypeRep (R.typeRep @Bool) -> Just R.HRefl)

pattern IntegerType ::
  forall t.
  () =>
  (t ~~ Integer) =>
  R.TypeRep t
pattern IntegerType <- (R.eqTypeRep (R.typeRep @Integer) -> Just R.HRefl)

type ConfigConstraint integerBitWidth s =
  ( SBV.SBV s ~ TermTy integerBitWidth Integer,
    SBV.SymVal s,
    SBV.HasKind s,
    Typeable s,
    Num (SBV.SBV s),
    Num s,
    SBV.OrdSymbolic (SBV.SBV s),
    Ord s,
    SBV.SDivisible (SBV.SBV s),
    SBV.OrdSymbolic (SBV.SBV s),
    SBV.Mergeable (SBV.SBV s)
  )

data DictConfig integerBitWidth where
  DictConfig ::
    forall s integerBitWidth.
    (ConfigConstraint integerBitWidth s) =>
    SBV.SMTConfig ->
    DictConfig integerBitWidth

resolveConfigView ::
  forall integerBitWidth.
  GrisetteSMTConfig integerBitWidth ->
  DictConfig integerBitWidth
resolveConfigView config = case config of
  GrisetteSMTConfig c extra ->
    case integerApprox extra of
      NoApprox -> DictConfig c
      Approx _ -> DictConfig c

pattern ResolvedConfig ::
  forall integerBitWidth.
  () =>
  forall s.
  (ConfigConstraint integerBitWidth s) =>
  SBV.SMTConfig ->
  GrisetteSMTConfig integerBitWidth
pattern ResolvedConfig c <- (resolveConfigView -> DictConfig c)

type MergeableTypeConstraint integerBitWidth s =
  ( Typeable (TermTy integerBitWidth s),
    SBV.Mergeable (TermTy integerBitWidth s)
  )

-- has to declare this because GHC does not support impredicative polymorphism
data DictMergeableType integerBitWidth s where
  DictMergeableType ::
    forall integerBitWidth s.
    (MergeableTypeConstraint integerBitWidth s) =>
    DictMergeableType integerBitWidth s

resolveMergeableTypeView :: TypeResolver DictMergeableType
resolveMergeableTypeView (config@ResolvedConfig {}, s) = case s of
  BoolType -> Just DictMergeableType
  IntegerType -> Just DictMergeableType
  SignedBVType _ -> Just DictMergeableType
  UnsignedBVType _ -> Just DictMergeableType
  TFunType l r ->
    case (resolveSimpleTypeView (config, l), resolveMergeableTypeView (config, r)) of
      (Just DictSimpleType, Just DictMergeableType) -> Just DictMergeableType
      _ -> Nothing
  GFunType l r ->
    case (resolveSimpleTypeView (config, l), resolveMergeableTypeView (config, r)) of
      (Just DictSimpleType, Just DictMergeableType) -> Just DictMergeableType
      _ -> Nothing
  _ -> Nothing
resolveMergeableTypeView _ = error "Should never happen, make compiler happy"

pattern ResolvedMergeableType ::
  forall integerBitWidth s.
  (SupportedPrim s) =>
  (MergeableTypeConstraint integerBitWidth s) =>
  (GrisetteSMTConfig integerBitWidth, R.TypeRep s)
pattern ResolvedMergeableType <- (resolveMergeableTypeView -> Just DictMergeableType)

type SimpleTypeConstraint integerBitWidth s s' =
  ( SBV.SBV s' ~ TermTy integerBitWidth s,
    SBV.SymVal s',
    SBV.HasKind s',
    Typeable s',
    SBV.OrdSymbolic (SBV.SBV s'),
    SBV.Mergeable (SBV.SBV s')
  )

type TypeResolver dictType =
  forall integerBitWidth s.
  (SupportedPrim s) =>
  (GrisetteSMTConfig integerBitWidth, R.TypeRep s) ->
  Maybe (dictType integerBitWidth s)

-- has to declare this because GHC does not support impredicative polymorphism
data DictSimpleType integerBitWidth s where
  DictSimpleType ::
    forall integerBitWidth s s'.
    (SimpleTypeConstraint integerBitWidth s s') =>
    DictSimpleType integerBitWidth s

resolveSimpleTypeView :: TypeResolver DictSimpleType
resolveSimpleTypeView (ResolvedConfig {}, s) = case s of
  BoolType -> Just DictSimpleType
  IntegerType -> Just DictSimpleType
  SignedBVType _ -> Just DictSimpleType
  UnsignedBVType _ -> Just DictSimpleType
  _ -> Nothing
resolveSimpleTypeView _ = error "Should never happen, make compiler happy"

pattern ResolvedSimpleType ::
  forall integerBitWidth s.
  (SupportedPrim s) =>
  forall s'.
  (SimpleTypeConstraint integerBitWidth s s') =>
  (GrisetteSMTConfig integerBitWidth, R.TypeRep s)
pattern ResolvedSimpleType <- (resolveSimpleTypeView -> Just DictSimpleType)

type DeepTypeConstraint integerBitWidth s s' =
  ( s' ~ TermTy integerBitWidth s,
    Typeable s',
    SBV.Mergeable s'
  )

data DictDeepType integerBitWidth s where
  DictDeepType ::
    forall integerBitWidth s s'.
    (DeepTypeConstraint integerBitWidth s s') =>
    DictDeepType integerBitWidth s

resolveDeepTypeView :: TypeResolver DictDeepType
resolveDeepTypeView r = case r of
  ResolvedSimpleType -> Just DictDeepType
  (config, TFunType (ta :: R.TypeRep a) (tb :: R.TypeRep b)) ->
    case (resolveDeepTypeView (config, ta), resolveDeepTypeView (config, tb)) of
      (Just DictDeepType, Just DictDeepType) -> Just DictDeepType
      _ -> Nothing
  (config, GFunType (ta :: R.TypeRep a) (tb :: R.TypeRep b)) ->
    case (resolveDeepTypeView (config, ta), resolveDeepTypeView (config, tb)) of
      (Just DictDeepType, Just DictDeepType) -> Just DictDeepType
      _ -> Nothing
  _ -> Nothing

pattern ResolvedDeepType ::
  forall integerBitWidth s.
  (SupportedPrim s) =>
  forall s'.
  (DeepTypeConstraint integerBitWidth s s') =>
  (GrisetteSMTConfig integerBitWidth, R.TypeRep s)
pattern ResolvedDeepType <- (resolveDeepTypeView -> Just DictDeepType)

type NumTypeConstraint integerBitWidth s s' =
  ( SimpleTypeConstraint integerBitWidth s s',
    Num (SBV.SBV s'),
    Num s',
    Num s
  )

data DictNumType integerBitWidth s where
  DictNumType ::
    forall integerBitWidth s s'.
    (NumTypeConstraint integerBitWidth s s') =>
    DictNumType integerBitWidth s

resolveNumTypeView :: TypeResolver DictNumType
resolveNumTypeView (ResolvedConfig {}, s) = case s of
  IntegerType -> Just DictNumType
  SignedBVType _ -> Just DictNumType
  UnsignedBVType _ -> Just DictNumType
  _ -> Nothing
resolveNumTypeView _ = error "Should never happen, make compiler happy"

pattern ResolvedNumType ::
  forall integerBitWidth s.
  (SupportedPrim s) =>
  forall s'.
  (NumTypeConstraint integerBitWidth s s') =>
  (GrisetteSMTConfig integerBitWidth, R.TypeRep s)
pattern ResolvedNumType <- (resolveNumTypeView -> Just DictNumType)

type SDivisibleTypeConstraint integerBitWidth s s' =
  ( SimpleTypeConstraint integerBitWidth s s',
    SBV.SDivisible (SBV.SBV s'),
    Integral s
  )

data DictSDivisibleType integerBitWidth s where
  DictSDivisibleType ::
    forall integerBitWidth s s'.
    (SDivisibleTypeConstraint integerBitWidth s s') =>
    DictSDivisibleType integerBitWidth s

resolveSDivisibleTypeView :: TypeResolver DictSDivisibleType
resolveSDivisibleTypeView (ResolvedConfig {}, s) = case s of
  IntegerType -> Just DictSDivisibleType
  SignedBVType _ -> Just DictSDivisibleType
  UnsignedBVType _ -> Just DictSDivisibleType
  _ -> Nothing
resolveSDivisibleTypeView _ = error "Should never happen, make compiler happy"

pattern ResolvedSDivisibleType ::
  forall integerBitWidth s.
  (SupportedPrim s) =>
  forall s'.
  (SDivisibleTypeConstraint integerBitWidth s s') =>
  (GrisetteSMTConfig integerBitWidth, R.TypeRep s)
pattern ResolvedSDivisibleType <- (resolveSDivisibleTypeView -> Just DictSDivisibleType)

type NumOrdTypeConstraint integerBitWidth s s' =
  ( NumTypeConstraint integerBitWidth s s',
    SBV.OrdSymbolic (SBV.SBV s'),
    Ord s',
    Ord s
  )

data DictNumOrdType integerBitWidth s where
  DictNumOrdType ::
    forall integerBitWidth s s'.
    (NumOrdTypeConstraint integerBitWidth s s') =>
    DictNumOrdType integerBitWidth s

resolveNumOrdTypeView :: TypeResolver DictNumOrdType
resolveNumOrdTypeView (ResolvedConfig {}, s) = case s of
  IntegerType -> Just DictNumOrdType
  SignedBVType _ -> Just DictNumOrdType
  UnsignedBVType _ -> Just DictNumOrdType
  _ -> Nothing
resolveNumOrdTypeView _ = error "Should never happen, make compiler happy"

pattern ResolvedNumOrdType ::
  forall integerBitWidth s.
  (SupportedPrim s) =>
  forall s'.
  (NumOrdTypeConstraint integerBitWidth s s') =>
  (GrisetteSMTConfig integerBitWidth, R.TypeRep s)
pattern ResolvedNumOrdType <- (resolveNumOrdTypeView -> Just DictNumOrdType)

type BitsTypeConstraint integerBitWidth s s' =
  ( SimpleTypeConstraint integerBitWidth s s',
    Bits (SBV.SBV s'),
    Bits s',
    Bits s,
    SIntegral s',
    Integral s
  )

data DictBitsType integerBitWidth s where
  DictBitsType ::
    forall integerBitWidth s s'.
    (BitsTypeConstraint integerBitWidth s s') =>
    DictBitsType integerBitWidth s

resolveBitsTypeView :: TypeResolver DictBitsType
resolveBitsTypeView (ResolvedConfig {}, s) = case s of
  SignedBVType _ -> Just DictBitsType
  UnsignedBVType _ -> Just DictBitsType
  _ -> Nothing
resolveBitsTypeView _ = error "Should never happen, make compiler happy"

pattern ResolvedBitsType ::
  forall integerBitWidth s.
  (SupportedPrim s) =>
  forall s'.
  (BitsTypeConstraint integerBitWidth s s') =>
  (GrisetteSMTConfig integerBitWidth, R.TypeRep s)
pattern ResolvedBitsType <- (resolveBitsTypeView -> Just DictBitsType)
