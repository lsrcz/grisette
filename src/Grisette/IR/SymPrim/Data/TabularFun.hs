{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Eta reduce" #-}

-- |
-- Module      :   Grisette.IR.SymPrim.Data.TabularFun
-- Copyright   :   (c) Sirui Lu 2021-2023
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.IR.SymPrim.Data.TabularFun
  ( type (=->) (..),
  )
where

import Control.DeepSeq (NFData, NFData1)
import Data.Hashable (Hashable)
import qualified Data.SBV as SBV
import GHC.Generics (Generic, Generic1)
import Grisette.Core.Data.Class.Function (Function ((#)))
import Grisette.IR.SymPrim.Data.Prim.Internal.IsZero (KnownIsZero)
import Grisette.IR.SymPrim.Data.Prim.Internal.PartialEval (totalize2)
import Grisette.IR.SymPrim.Data.Prim.Internal.Term
  ( NonFuncSBVRep (NonFuncSBVBaseType),
    PEvalApplyTerm (pevalApplyTerm, sbvApplyTerm),
    SBVRep (SBVType),
    SupportedNonFuncPrim (conNonFuncSBVTerm, withNonFuncPrim),
    SupportedPrim
      ( conSBVTerm,
        defaultValue,
        pevalITETerm,
        sbvEq,
        sbvIte,
        symSBVName,
        symSBVTerm,
        withPrim
      ),
    SupportedPrimConstraint (PrimConstraint),
    Term (ConTerm),
    applyTerm,
    conTerm,
    pevalDefaultEqTerm,
    pevalEqTerm,
    pevalITEBasicTerm,
    translateTypeError,
  )
import Language.Haskell.TH.Syntax (Lift)
import Type.Reflection (typeRep)

-- $setup
-- >>> import Grisette.Core
-- >>> import Grisette.IR.SymPrim

-- |
-- Functions as a table. Use the `#` operator to apply the function.
--
-- >>> :set -XTypeOperators
-- >>> let f = TabularFun [(1, 2), (3, 4)] 0 :: Int =-> Int
-- >>> f # 1
-- 2
-- >>> f # 2
-- 0
-- >>> f # 3
-- 4
data (=->) a b = TabularFun {funcTable :: [(a, b)], defaultFuncValue :: b}
  deriving (Show, Eq, Generic, Generic1, Lift, NFData, NFData1)

infixr 0 =->

instance (Eq a) => Function (a =-> b) a b where
  (TabularFun table d) # a = go table
    where
      go [] = d
      go ((av, bv) : s)
        | a == av = bv
        | otherwise = go s

instance (Hashable a, Hashable b) => Hashable (a =-> b)

instance
  (SupportedNonFuncPrim a, SupportedPrim b) =>
  SupportedPrimConstraint (a =-> b)
  where
  type
    PrimConstraint n (a =-> b) =
      ( SupportedNonFuncPrim a,
        SupportedPrim b,
        PrimConstraint n b
      )

instance
  (SupportedNonFuncPrim a, SupportedPrim b) =>
  SBVRep (a =-> b)
  where
  type
    SBVType n (a =-> b) =
      SBV.SBV (NonFuncSBVBaseType n a) ->
      SBVType n b

instance
  (SupportedNonFuncPrim a, SupportedNonFuncPrim b) =>
  SupportedPrim (a =-> b)
  where
  defaultValue = TabularFun [] defaultValue
  pevalITETerm = pevalITEBasicTerm
  pevalEqTerm = pevalDefaultEqTerm
  conSBVTerm p f =
    withNonFuncPrim @b p $
      lowerTFunCon p f
  symSBVName _ num = "tfunc2" <> show num
  symSBVTerm (p :: proxy n) name =
    withNonFuncPrim @a p $
      withNonFuncPrim @b p $
        return $
          SBV.uninterpret name
  withPrim p r = withNonFuncPrim @a p $ withNonFuncPrim @b p r
  sbvIte p = withNonFuncPrim @b p SBV.ite
  sbvEq _ _ =
    translateTypeError
      ( Just $
          "BUG. Please send a bug report. TabularFun is not supported for "
            <> "equality comparison."
      )
      (typeRep @(a =-> b))

instance
  {-# OVERLAPPING #-}
  ( SupportedNonFuncPrim a,
    SupportedNonFuncPrim b,
    SupportedNonFuncPrim c,
    SupportedPrim a,
    SupportedPrim b,
    SupportedPrim c
  ) =>
  SupportedPrim (a =-> b =-> c)
  where
  defaultValue = TabularFun [] defaultValue
  pevalITETerm = pevalITEBasicTerm
  pevalEqTerm = pevalDefaultEqTerm
  conSBVTerm p f =
    withNonFuncPrim @c p $
      lowerTFunCon p f
  symSBVName _ num = "tfunc3" <> show num
  symSBVTerm (p :: proxy n) name =
    withNonFuncPrim @a p $
      withNonFuncPrim @b p $
        withNonFuncPrim @c p $
          return $
            SBV.uninterpret name
  withPrim p r =
    withNonFuncPrim @a p $
      withNonFuncPrim @b p $
        withNonFuncPrim @c p r
  sbvIte p = withNonFuncPrim @c p SBV.ite
  sbvEq _ _ =
    translateTypeError
      ( Just $
          "BUG. Please send a bug report. TabularFun is not supported for "
            <> "equality comparison."
      )
      (typeRep @(a =-> b =-> c))

instance
  {-# OVERLAPPING #-}
  ( SupportedNonFuncPrim a,
    SupportedNonFuncPrim b,
    SupportedNonFuncPrim c,
    SupportedNonFuncPrim d,
    SupportedPrim a,
    SupportedPrim b,
    SupportedPrim c,
    SupportedPrim d
  ) =>
  SupportedPrim (a =-> b =-> c =-> d)
  where
  defaultValue = TabularFun [] defaultValue
  pevalITETerm = pevalITEBasicTerm
  pevalEqTerm = pevalDefaultEqTerm
  conSBVTerm p f =
    withNonFuncPrim @d p $
      lowerTFunCon p f
  symSBVName _ num = "tfunc4" <> show num
  symSBVTerm (p :: proxy n) name =
    withNonFuncPrim @a p $
      withNonFuncPrim @b p $
        withNonFuncPrim @c p $
          withNonFuncPrim @d p $
            return $
              SBV.uninterpret name
  withPrim p r =
    withNonFuncPrim @a p $
      withNonFuncPrim @b p $
        withNonFuncPrim @c p $
          withNonFuncPrim @d p r
  sbvIte p = withNonFuncPrim @d p SBV.ite
  sbvEq _ _ =
    translateTypeError
      ( Just $
          "BUG. Please send a bug report. TabularFun is not supported for "
            <> "equality comparison."
      )
      (typeRep @(a =-> b =-> c =-> d))

instance
  {-# OVERLAPPING #-}
  ( SupportedNonFuncPrim a,
    SupportedNonFuncPrim b,
    SupportedNonFuncPrim c,
    SupportedNonFuncPrim d,
    SupportedNonFuncPrim e,
    SupportedPrim a,
    SupportedPrim b,
    SupportedPrim c,
    SupportedPrim d,
    SupportedPrim e
  ) =>
  SupportedPrim (a =-> b =-> c =-> d =-> e)
  where
  defaultValue = TabularFun [] defaultValue
  pevalITETerm = pevalITEBasicTerm
  pevalEqTerm = pevalDefaultEqTerm
  conSBVTerm p f =
    withNonFuncPrim @e p $
      lowerTFunCon p f
  symSBVName _ num = "tfunc5" <> show num
  symSBVTerm (p :: proxy n) name =
    withNonFuncPrim @a p $
      withNonFuncPrim @b p $
        withNonFuncPrim @c p $
          withNonFuncPrim @d p $
            withNonFuncPrim @e p $
              return $
                SBV.uninterpret name
  withPrim p r =
    withNonFuncPrim @a p $
      withNonFuncPrim @b p $
        withNonFuncPrim @c p $
          withNonFuncPrim @d p $
            withNonFuncPrim @e p r
  sbvIte p = withNonFuncPrim @e p SBV.ite
  sbvEq _ _ =
    translateTypeError
      ( Just $
          "BUG. Please send a bug report. TabularFun is not supported for "
            <> "equality comparison."
      )
      (typeRep @(a =-> b =-> c =-> d =-> e))

instance
  {-# OVERLAPPING #-}
  ( SupportedNonFuncPrim a,
    SupportedNonFuncPrim b,
    SupportedNonFuncPrim c,
    SupportedNonFuncPrim d,
    SupportedNonFuncPrim e,
    SupportedNonFuncPrim f,
    SupportedPrim a,
    SupportedPrim b,
    SupportedPrim c,
    SupportedPrim d,
    SupportedPrim e,
    SupportedPrim f
  ) =>
  SupportedPrim (a =-> b =-> c =-> d =-> e =-> f)
  where
  defaultValue = TabularFun [] defaultValue
  pevalITETerm = pevalITEBasicTerm
  pevalEqTerm = pevalDefaultEqTerm
  conSBVTerm p f =
    withNonFuncPrim @f p $
      lowerTFunCon p f
  symSBVName _ num = "tfunc6" <> show num
  symSBVTerm (p :: proxy n) name =
    withNonFuncPrim @a p $
      withNonFuncPrim @b p $
        withNonFuncPrim @c p $
          withNonFuncPrim @d p $
            withNonFuncPrim @e p $
              withNonFuncPrim @f p $
                return $
                  SBV.uninterpret name
  withPrim p r =
    withNonFuncPrim @a p $
      withNonFuncPrim @b p $
        withNonFuncPrim @c p $
          withNonFuncPrim @d p $
            withNonFuncPrim @e p $
              withNonFuncPrim @f p r
  sbvIte p = withNonFuncPrim @f p SBV.ite
  sbvEq _ _ =
    translateTypeError
      ( Just $
          "BUG. Please send a bug report. TabularFun is not supported for "
            <> "equality comparison."
      )
      (typeRep @(a =-> b =-> c =-> d =-> e =-> f))

-- 7 arguments
instance
  {-# OVERLAPPING #-}
  ( SupportedNonFuncPrim a,
    SupportedNonFuncPrim b,
    SupportedNonFuncPrim c,
    SupportedNonFuncPrim d,
    SupportedNonFuncPrim e,
    SupportedNonFuncPrim f,
    SupportedNonFuncPrim g,
    SupportedPrim a,
    SupportedPrim b,
    SupportedPrim c,
    SupportedPrim d,
    SupportedPrim e,
    SupportedPrim f,
    SupportedPrim g
  ) =>
  SupportedPrim (a =-> b =-> c =-> d =-> e =-> f =-> g)
  where
  defaultValue = TabularFun [] defaultValue
  pevalITETerm = pevalITEBasicTerm
  pevalEqTerm = pevalDefaultEqTerm
  conSBVTerm p f =
    withNonFuncPrim @g p $
      lowerTFunCon p f
  symSBVName _ num = "tfunc7" <> show num
  symSBVTerm (p :: proxy n) name =
    withNonFuncPrim @a p $
      withNonFuncPrim @b p $
        withNonFuncPrim @c p $
          withNonFuncPrim @d p $
            withNonFuncPrim @e p $
              withNonFuncPrim @f p $
                withNonFuncPrim @g p $
                  return $
                    SBV.uninterpret name
  withPrim p r =
    withNonFuncPrim @a p $
      withNonFuncPrim @b p $
        withNonFuncPrim @c p $
          withNonFuncPrim @d p $
            withNonFuncPrim @e p $
              withNonFuncPrim @f p $
                withNonFuncPrim @g p r
  sbvIte p = withNonFuncPrim @g p SBV.ite
  sbvEq _ _ =
    translateTypeError
      ( Just $
          "BUG. Please send a bug report. TabularFun is not supported for "
            <> "equality comparison."
      )
      (typeRep @(a =-> b =-> c =-> d =-> e =-> f =-> g))

-- 8 arguments
instance
  {-# OVERLAPPING #-}
  ( SupportedNonFuncPrim a,
    SupportedNonFuncPrim b,
    SupportedNonFuncPrim c,
    SupportedNonFuncPrim d,
    SupportedNonFuncPrim e,
    SupportedNonFuncPrim f,
    SupportedNonFuncPrim g,
    SupportedNonFuncPrim h,
    SupportedPrim a,
    SupportedPrim b,
    SupportedPrim c,
    SupportedPrim d,
    SupportedPrim e,
    SupportedPrim f,
    SupportedPrim g,
    SupportedPrim h
  ) =>
  SupportedPrim (a =-> b =-> c =-> d =-> e =-> f =-> g =-> h)
  where
  defaultValue = TabularFun [] defaultValue
  pevalITETerm = pevalITEBasicTerm
  pevalEqTerm = pevalDefaultEqTerm
  conSBVTerm p f =
    withNonFuncPrim @h p $
      lowerTFunCon p f
  symSBVName _ num = "tfunc8" <> show num
  symSBVTerm (p :: proxy n) name =
    withNonFuncPrim @a p $
      withNonFuncPrim @b p $
        withNonFuncPrim @c p $
          withNonFuncPrim @d p $
            withNonFuncPrim @e p $
              withNonFuncPrim @f p $
                withNonFuncPrim @g p $
                  withNonFuncPrim @h p $
                    return $
                      SBV.uninterpret name
  withPrim p r =
    withNonFuncPrim @a p $
      withNonFuncPrim @b p $
        withNonFuncPrim @c p $
          withNonFuncPrim @d p $
            withNonFuncPrim @e p $
              withNonFuncPrim @f p $
                withNonFuncPrim @g p $
                  withNonFuncPrim @h p r
  sbvIte p = withNonFuncPrim @h p SBV.ite
  sbvEq _ _ =
    translateTypeError
      ( Just $
          "BUG. Please send a bug report. TabularFun is not supported for "
            <> "equality comparison."
      )
      (typeRep @(a =-> b =-> c =-> d =-> e =-> f =-> g =-> h))

instance
  (SupportedPrim a, SupportedPrim b, SupportedPrim (a =-> b)) =>
  PEvalApplyTerm (a =-> b) a b
  where
  pevalApplyTerm = totalize2 doPevalApplyTerm applyTerm
    where
      doPevalApplyTerm ::
        (SupportedPrim a, SupportedPrim b) =>
        Term (a =-> b) ->
        Term a ->
        Maybe (Term b)
      doPevalApplyTerm (ConTerm _ f) (ConTerm _ a) = Just $ conTerm $ f # a
      doPevalApplyTerm (ConTerm _ (TabularFun f d)) a = Just $ go f
        where
          go [] = conTerm d
          go ((x, y) : xs) =
            pevalITETerm (pevalEqTerm a (conTerm x)) (conTerm y) (go xs)
      doPevalApplyTerm _ _ = Nothing
  sbvApplyTerm p f a =
    withPrim @(a =-> b) p $ withNonFuncPrim @a p $ f a

lowerTFunCon ::
  forall proxy integerBitWidth a b.
  ( SupportedNonFuncPrim a,
    SupportedPrim b,
    SBV.Mergeable (SBVType integerBitWidth b),
    KnownIsZero integerBitWidth
  ) =>
  proxy integerBitWidth ->
  (a =-> b) ->
  ( SBV.SBV (NonFuncSBVBaseType integerBitWidth a) ->
    SBVType integerBitWidth b
  )
lowerTFunCon proxy (TabularFun l d) = go l d
  where
    go [] d _ = conSBVTerm proxy d
    go ((x, r) : xs) d v =
      SBV.ite
        (conNonFuncSBVTerm proxy x SBV..== v)
        (conSBVTerm proxy r)
        (go xs d v)