{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- Module      :   Grisette.IR.SymPrim.Data.Prim.PartialEval.Bool
-- Copyright   :   (c) Sirui Lu 2021-2023
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.IR.SymPrim.Data.Prim.PartialEval.Bool
  ( trueTerm,
    falseTerm,
    pattern BoolConTerm,
    pattern TrueTerm,
    pattern FalseTerm,
    pattern BoolTerm,
    pevalNotTerm,
    pevalEqvTerm,
    pevalNotEqvTerm,
    pevalOrTerm,
    pevalAndTerm,
    pevalITETerm,
    pevalImplyTerm,
    pevalXorTerm,
  )
where

import Control.Monad (msum)
import Data.Maybe (fromMaybe)
import Data.Typeable (cast, eqT, type (:~:) (Refl))
import Grisette.IR.SymPrim.Data.Prim.InternedTerm.Term
  ( SupportedPrim,
    Term
      ( AddNumTerm,
        AndTerm,
        ConTerm,
        EqvTerm,
        ITETerm,
        NotTerm,
        OrTerm
      ),
    andTerm,
    conTerm,
    eqvTerm,
    iteTerm,
    notTerm,
    orTerm,
  )
import Grisette.IR.SymPrim.Data.Prim.InternedTerm.TermUtils
  ( castTerm,
  )
import Grisette.IR.SymPrim.Data.Prim.Utils (pattern Dyn)
import Unsafe.Coerce (unsafeCoerce)

trueTerm :: Term Bool
trueTerm = conTerm True
{-# INLINE trueTerm #-}

falseTerm :: Term Bool
falseTerm = conTerm False
{-# INLINE falseTerm #-}

boolConTermView :: forall a. Term a -> Maybe Bool
boolConTermView (ConTerm _ b) = cast b
boolConTermView _ = Nothing
{-# INLINE boolConTermView #-}

pattern BoolConTerm :: Bool -> Term a
pattern BoolConTerm b <- (boolConTermView -> Just b)

pattern TrueTerm :: Term a
pattern TrueTerm <- BoolConTerm True

pattern FalseTerm :: Term a
pattern FalseTerm <- BoolConTerm False

pattern BoolTerm :: Term Bool -> Term a
pattern BoolTerm b <- (castTerm -> Just b)

-- Not
pevalNotTerm :: Term Bool -> Term Bool
pevalNotTerm (NotTerm _ tm) = tm
pevalNotTerm (ConTerm _ a) = if a then falseTerm else trueTerm
pevalNotTerm (OrTerm _ (NotTerm _ n1) n2) = pevalAndTerm n1 (pevalNotTerm n2)
pevalNotTerm (OrTerm _ n1 (NotTerm _ n2)) = pevalAndTerm (pevalNotTerm n1) n2
pevalNotTerm (AndTerm _ (NotTerm _ n1) n2) = pevalOrTerm n1 (pevalNotTerm n2)
pevalNotTerm (AndTerm _ n1 (NotTerm _ n2)) = pevalOrTerm (pevalNotTerm n1) n2
pevalNotTerm tm = notTerm tm
{-# INLINEABLE pevalNotTerm #-}

-- Eqv
pevalEqvTerm :: forall a. (SupportedPrim a) => Term a -> Term a -> Term Bool
pevalEqvTerm l@ConTerm {} r@ConTerm {} = conTerm $ l == r
pevalEqvTerm l@ConTerm {} r = pevalEqvTerm r l
pevalEqvTerm l (BoolConTerm rv) = if rv then unsafeCoerce l else pevalNotTerm $ unsafeCoerce l
pevalEqvTerm (NotTerm _ lv) r
  | lv == unsafeCoerce r = falseTerm
pevalEqvTerm l (NotTerm _ rv)
  | unsafeCoerce l == rv = falseTerm
{-
pevalBinary _ (ConTerm l) (ConTerm r) =
  if l == r then trueTerm else falseTerm
  -}
pevalEqvTerm
  ( AddNumTerm
      _
      (ConTerm _ c :: Term a)
      (Dyn (v :: Term a))
    )
  (Dyn (ConTerm _ c2 :: Term a)) =
    pevalEqvTerm v (conTerm $ c2 - c)
pevalEqvTerm
  (Dyn (ConTerm _ c2 :: Term a))
  ( AddNumTerm
      _
      (Dyn (ConTerm _ c :: Term a))
      (Dyn (v :: Term a))
    ) =
    pevalEqvTerm v (conTerm $ c2 - c)
pevalEqvTerm l (ITETerm _ c t f)
  | l == t = pevalOrTerm c (pevalEqvTerm l f)
  | l == f = pevalOrTerm (pevalNotTerm c) (pevalEqvTerm l t)
pevalEqvTerm (ITETerm _ c t f) r
  | t == r = pevalOrTerm c (pevalEqvTerm f r)
  | f == r = pevalOrTerm (pevalNotTerm c) (pevalEqvTerm t r)
pevalEqvTerm l r
  | l == r = trueTerm
  | otherwise = eqvTerm l r
{-# INLINEABLE pevalEqvTerm #-}

pevalNotEqvTerm :: (SupportedPrim a) => Term a -> Term a -> Term Bool
pevalNotEqvTerm l r = pevalNotTerm $ pevalEqvTerm l r
{-# INLINE pevalNotEqvTerm #-}

pevalImpliesTerm :: Term Bool -> Term Bool -> Bool
pevalImpliesTerm (ConTerm _ False) _ = True
pevalImpliesTerm _ (ConTerm _ True) = True
pevalImpliesTerm
  (EqvTerm _ (e1 :: Term a) (ec1@(ConTerm _ _) :: Term b))
  (NotTerm _ (EqvTerm _ (Dyn (e2 :: Term a)) (Dyn (ec2@(ConTerm _ _) :: Term b))))
    | e1 == e2 && ec1 /= ec2 = True
pevalImpliesTerm a b
  | a == b = True
  | otherwise = False
{-# INLINE pevalImpliesTerm #-}

orEqFirst :: Term Bool -> Term Bool -> Bool
orEqFirst _ (ConTerm _ False) = True
orEqFirst
  (NotTerm _ (EqvTerm _ (e1 :: Term a) (ec1@(ConTerm _ _) :: Term b)))
  (EqvTerm _ (Dyn (e2 :: Term a)) (Dyn (ec2@(ConTerm _ _) :: Term b)))
    | e1 == e2 && ec1 /= ec2 = True
orEqFirst x y
  | x == y = True
  | otherwise = False
{-# INLINE orEqFirst #-}

orEqTrue :: Term Bool -> Term Bool -> Bool
orEqTrue (ConTerm _ True) _ = True
orEqTrue _ (ConTerm _ True) = True
-- orEqTrue (NotTerm _ e1) (NotTerm _ e2) = andEqFalse e1 e2
orEqTrue
  (NotTerm _ (EqvTerm _ (e1 :: Term a) (ec1@(ConTerm _ _) :: Term b)))
  (NotTerm _ (EqvTerm _ (Dyn (e2 :: Term a)) (Dyn (ec2@(ConTerm _ _) :: Term b))))
    | e1 == e2 && ec1 /= ec2 = True
orEqTrue (NotTerm _ l) r | l == r = True
orEqTrue l (NotTerm _ r) | l == r = True
orEqTrue _ _ = False
{-# INLINE orEqTrue #-}

andEqFirst :: Term Bool -> Term Bool -> Bool
andEqFirst _ (ConTerm _ True) = True
-- andEqFirst x (NotTerm _ y) = andEqFalse x y
andEqFirst
  (EqvTerm _ (e1 :: Term a) (ec1@(ConTerm _ _) :: Term b))
  (NotTerm _ (EqvTerm _ (Dyn (e2 :: Term a)) (Dyn (ec2@(ConTerm _ _) :: Term b))))
    | e1 == e2 && ec1 /= ec2 = True
andEqFirst x y
  | x == y = True
  | otherwise = False
{-# INLINE andEqFirst #-}

andEqFalse :: Term Bool -> Term Bool -> Bool
andEqFalse (ConTerm _ False) _ = True
andEqFalse _ (ConTerm _ False) = True
-- andEqFalse (NotTerm _ e1) (NotTerm _ e2) = orEqTrue e1 e2
andEqFalse
  (EqvTerm _ (e1 :: Term a) (ec1@(ConTerm _ _) :: Term b))
  (EqvTerm _ (Dyn (e2 :: Term a)) (Dyn (ec2@(ConTerm _ _) :: Term b)))
    | e1 == e2 && ec1 /= ec2 = True
andEqFalse (NotTerm _ x) y | x == y = True
andEqFalse x (NotTerm _ y) | x == y = True
andEqFalse _ _ = False
{-# INLINE andEqFalse #-}

-- Or
pevalOrTerm :: Term Bool -> Term Bool -> Term Bool
pevalOrTerm l r
  | orEqTrue l r = trueTerm
  | orEqFirst l r = l
  | orEqFirst r l = r
pevalOrTerm l r@(OrTerm _ r1 r2)
  | orEqTrue l r1 = trueTerm
  | orEqTrue l r2 = trueTerm
  | orEqFirst r1 l = r
  | orEqFirst r2 l = r
  | orEqFirst l r1 = pevalOrTerm l r2
  | orEqFirst l r2 = pevalOrTerm l r1
pevalOrTerm l@(OrTerm _ l1 l2) r
  | orEqTrue l1 r = trueTerm
  | orEqTrue l2 r = trueTerm
  | orEqFirst l1 r = l
  | orEqFirst l2 r = l
  | orEqFirst r l1 = pevalOrTerm l2 r
  | orEqFirst r l2 = pevalOrTerm l1 r
pevalOrTerm l (AndTerm _ r1 r2)
  | orEqFirst l r1 = l
  | orEqFirst l r2 = l
  | orEqTrue l r1 = pevalOrTerm l r2
  | orEqTrue l r2 = pevalOrTerm l r1
pevalOrTerm (AndTerm _ l1 l2) r
  | orEqFirst r l1 = r
  | orEqFirst r l2 = r
  | orEqTrue l1 r = pevalOrTerm l2 r
  | orEqTrue l2 r = pevalOrTerm l1 r
pevalOrTerm (NotTerm _ nl) (NotTerm _ nr) = pevalNotTerm $ pevalAndTerm nl nr
pevalOrTerm l r = orTerm l r
{-# INLINEABLE pevalOrTerm #-}

pevalAndTerm :: Term Bool -> Term Bool -> Term Bool
pevalAndTerm l r
  | andEqFalse l r = falseTerm
  | andEqFirst l r = l
  | andEqFirst r l = r
pevalAndTerm l r@(AndTerm _ r1 r2)
  | andEqFalse l r1 = falseTerm
  | andEqFalse l r2 = falseTerm
  | andEqFirst r1 l = r
  | andEqFirst r2 l = r
  | andEqFirst l r1 = pevalAndTerm l r2
  | andEqFirst l r2 = pevalAndTerm l r1
pevalAndTerm l@(AndTerm _ l1 l2) r
  | andEqFalse l1 r = falseTerm
  | andEqFalse l2 r = falseTerm
  | andEqFirst l1 r = l
  | andEqFirst l2 r = l
  | andEqFirst r l1 = pevalAndTerm l2 r
  | andEqFirst r l2 = pevalAndTerm l1 r
pevalAndTerm l (OrTerm _ r1 r2)
  | andEqFirst l r1 = l
  | andEqFirst l r2 = l
  | andEqFalse l r1 = pevalAndTerm l r2
  | andEqFalse l r2 = pevalAndTerm l r1
pevalAndTerm (OrTerm _ l1 l2) r
  | andEqFirst r l1 = r
  | andEqFirst r l2 = r
  | andEqFalse l1 r = pevalAndTerm l2 r
  | andEqFalse l2 r = pevalAndTerm l1 r
pevalAndTerm (NotTerm _ nl) (NotTerm _ nr) = pevalNotTerm $ pevalOrTerm nl nr
pevalAndTerm l r = andTerm l r
{-# INLINEABLE pevalAndTerm #-}

pevalITEBoolLeftNot :: Term Bool -> Term Bool -> Term Bool -> Maybe (Term Bool)
pevalITEBoolLeftNot cond nIfTrue ifFalse
  | cond == nIfTrue = Just $ pevalAndTerm (pevalNotTerm cond) ifFalse -- need test
  | otherwise = case nIfTrue of
      AndTerm _ nt1 nt2 -> ra
        where
          ra
            | pevalImpliesTerm cond nt1 = Just $ pevalITETerm cond (pevalNotTerm nt2) ifFalse
            | pevalImpliesTerm cond nt2 = Just $ pevalITETerm cond (pevalNotTerm nt1) ifFalse
            | pevalImpliesTerm cond (pevalNotTerm nt1) || pevalImpliesTerm cond (pevalNotTerm nt2) =
                Just $ pevalOrTerm cond ifFalse
            | otherwise = Nothing
      OrTerm _ nt1 nt2 -> ra
        where
          ra
            | pevalImpliesTerm cond nt1 || pevalImpliesTerm cond nt2 = Just $ pevalAndTerm (pevalNotTerm cond) ifFalse
            | pevalImpliesTerm cond (pevalNotTerm nt1) = Just $ pevalITETerm cond (pevalNotTerm nt2) ifFalse
            | pevalImpliesTerm cond (pevalNotTerm nt2) = Just $ pevalITETerm cond (pevalNotTerm nt1) ifFalse
            | otherwise = Nothing
      _ -> Nothing

pevalITEBoolBothNot :: Term Bool -> Term Bool -> Term Bool -> Maybe (Term Bool)
pevalITEBoolBothNot cond nIfTrue nIfFalse = Just $ pevalNotTerm $ pevalITETerm cond nIfTrue nIfFalse

pevalITEBoolRightNot :: Term Bool -> Term Bool -> Term Bool -> Maybe (Term Bool)
pevalITEBoolRightNot cond ifTrue nIfFalse
  | cond == nIfFalse = Just $ pevalOrTerm (pevalNotTerm cond) ifTrue -- need test
  | otherwise = Nothing -- need work

pevalInferImplies :: Term Bool -> Term Bool -> Term Bool -> Term Bool -> Maybe (Term Bool)
pevalInferImplies cond (NotTerm _ nt1) trueRes falseRes
  | cond == nt1 = Just falseRes
  | otherwise = case (cond, nt1) of
      ( EqvTerm _ (e1 :: Term a) (ec1@(ConTerm _ _) :: Term b),
        EqvTerm _ (Dyn (e2 :: Term a)) (Dyn (ec2@(ConTerm _ _) :: Term b))
        )
          | e1 == e2 && ec1 /= ec2 -> Just trueRes
      _ -> Nothing
pevalInferImplies
  (EqvTerm _ (e1 :: Term a) (ec1@(ConTerm _ _) :: Term b))
  (EqvTerm _ (Dyn (e2 :: Term a)) (Dyn (ec2@(ConTerm _ _) :: Term b)))
  _
  falseRes
    | e1 == e2 && ec1 /= ec2 = Just falseRes
pevalInferImplies _ _ _ _ = Nothing

pevalITEBoolLeftAnd :: Term Bool -> Term Bool -> Term Bool -> Term Bool -> Maybe (Term Bool)
pevalITEBoolLeftAnd cond t1 t2 ifFalse
  | t1 == ifFalse = Just $ pevalAndTerm t1 $ pevalImplyTerm cond t2
  | t2 == ifFalse = Just $ pevalAndTerm t2 $ pevalImplyTerm cond t1
  | cond == t1 = Just $ pevalITETerm cond t2 ifFalse
  | cond == t2 = Just $ pevalITETerm cond t1 ifFalse
  | otherwise =
      msum
        [ pevalInferImplies cond t1 (pevalITETerm cond t2 ifFalse) (pevalAndTerm (pevalNotTerm cond) ifFalse),
          pevalInferImplies cond t2 (pevalITETerm cond t1 ifFalse) (pevalAndTerm (pevalNotTerm cond) ifFalse)
        ]

pevalITEBoolBothAnd :: Term Bool -> Term Bool -> Term Bool -> Term Bool -> Term Bool -> Maybe (Term Bool)
pevalITEBoolBothAnd cond t1 t2 f1 f2
  | t1 == f1 = Just $ pevalAndTerm t1 $ pevalITETerm cond t2 f2
  | t1 == f2 = Just $ pevalAndTerm t1 $ pevalITETerm cond t2 f1
  | t2 == f1 = Just $ pevalAndTerm t2 $ pevalITETerm cond t1 f2
  | t2 == f2 = Just $ pevalAndTerm t2 $ pevalITETerm cond t1 f1
  | otherwise = Nothing

pevalITEBoolRightAnd :: Term Bool -> Term Bool -> Term Bool -> Term Bool -> Maybe (Term Bool)
pevalITEBoolRightAnd cond ifTrue f1 f2
  | f1 == ifTrue = Just $ pevalAndTerm f1 $ pevalOrTerm cond f2
  | f2 == ifTrue = Just $ pevalAndTerm f2 $ pevalOrTerm cond f1
  | otherwise = Nothing

pevalITEBoolLeftOr :: Term Bool -> Term Bool -> Term Bool -> Term Bool -> Maybe (Term Bool)
pevalITEBoolLeftOr cond t1 t2 ifFalse
  | t1 == ifFalse = Just $ pevalOrTerm t1 $ pevalAndTerm cond t2
  | t2 == ifFalse = Just $ pevalOrTerm t2 $ pevalAndTerm cond t1
  | cond == t1 = Just $ pevalOrTerm cond ifFalse
  | cond == t2 = Just $ pevalOrTerm cond ifFalse
  | otherwise =
      msum
        [ pevalInferImplies cond t1 (pevalOrTerm cond ifFalse) (pevalITETerm cond t2 ifFalse),
          pevalInferImplies cond t2 (pevalOrTerm cond ifFalse) (pevalITETerm cond t1 ifFalse)
        ]

pevalITEBoolBothOr :: Term Bool -> Term Bool -> Term Bool -> Term Bool -> Term Bool -> Maybe (Term Bool)
pevalITEBoolBothOr cond t1 t2 f1 f2
  | t1 == f1 = Just $ pevalOrTerm t1 $ pevalITETerm cond t2 f2
  | t1 == f2 = Just $ pevalOrTerm t1 $ pevalITETerm cond t2 f1
  | t2 == f1 = Just $ pevalOrTerm t2 $ pevalITETerm cond t1 f2
  | t2 == f2 = Just $ pevalOrTerm t2 $ pevalITETerm cond t1 f1
  | otherwise = Nothing

pevalITEBoolRightOr :: Term Bool -> Term Bool -> Term Bool -> Term Bool -> Maybe (Term Bool)
pevalITEBoolRightOr cond ifTrue f1 f2
  | f1 == ifTrue = Just $ pevalOrTerm f1 $ pevalAndTerm (pevalNotTerm cond) f2
  | f2 == ifTrue = Just $ pevalOrTerm f2 $ pevalAndTerm (pevalNotTerm cond) f1
  | otherwise = Nothing

pevalITEBoolLeft :: Term Bool -> Term Bool -> Term Bool -> Maybe (Term Bool)
pevalITEBoolLeft cond (AndTerm _ t1 t2) ifFalse =
  msum
    [ pevalITEBoolLeftAnd cond t1 t2 ifFalse,
      case ifFalse of
        AndTerm _ f1 f2 -> pevalITEBoolBothAnd cond t1 t2 f1 f2
        _ -> Nothing
    ]
pevalITEBoolLeft cond (OrTerm _ t1 t2) ifFalse =
  msum
    [ pevalITEBoolLeftOr cond t1 t2 ifFalse,
      case ifFalse of
        OrTerm _ f1 f2 -> pevalITEBoolBothOr cond t1 t2 f1 f2
        _ -> Nothing
    ]
pevalITEBoolLeft cond (NotTerm _ nIfTrue) ifFalse =
  msum
    [ pevalITEBoolLeftNot cond nIfTrue ifFalse,
      case ifFalse of
        NotTerm _ nIfFalse ->
          pevalITEBoolBothNot cond nIfTrue nIfFalse
        _ -> Nothing
    ]
pevalITEBoolLeft _ _ _ = Nothing

pevalITEBoolNoLeft :: Term Bool -> Term Bool -> Term Bool -> Maybe (Term Bool)
pevalITEBoolNoLeft cond ifTrue (AndTerm _ f1 f2) = pevalITEBoolRightAnd cond ifTrue f1 f2
pevalITEBoolNoLeft cond ifTrue (OrTerm _ f1 f2) = pevalITEBoolRightOr cond ifTrue f1 f2
pevalITEBoolNoLeft cond ifTrue (NotTerm _ nIfFalse) = pevalITEBoolRightNot cond ifTrue nIfFalse
pevalITEBoolNoLeft _ _ _ = Nothing

pevalITEBasic :: (SupportedPrim a) => Term Bool -> Term a -> Term a -> Maybe (Term a)
pevalITEBasic (ConTerm _ True) ifTrue _ = Just ifTrue
pevalITEBasic (ConTerm _ False) _ ifFalse = Just ifFalse
pevalITEBasic (NotTerm _ ncond) ifTrue ifFalse = Just $ pevalITETerm ncond ifFalse ifTrue
pevalITEBasic _ ifTrue ifFalse | ifTrue == ifFalse = Just ifTrue
pevalITEBasic (ITETerm _ cc ct cf) (ITETerm _ tc tt tf) (ITETerm _ fc ft ff) -- later
  | cc == tc && cc == fc = Just $ pevalITETerm cc (pevalITETerm ct tt ft) (pevalITETerm cf tf ff)
pevalITEBasic cond (ITETerm _ tc tt tf) ifFalse -- later
  | cond == tc = Just $ pevalITETerm cond tt ifFalse
  | tt == ifFalse = Just $ pevalITETerm (pevalOrTerm (pevalNotTerm cond) tc) tt tf
  | tf == ifFalse = Just $ pevalITETerm (pevalAndTerm cond tc) tt tf
pevalITEBasic cond ifTrue (ITETerm _ fc ft ff) -- later
  | ifTrue == ft = Just $ pevalITETerm (pevalOrTerm cond fc) ifTrue ff
  | ifTrue == ff = Just $ pevalITETerm (pevalOrTerm cond (pevalNotTerm fc)) ifTrue ft
  | pevalImpliesTerm fc cond = Just $ pevalITETerm cond ifTrue ff
pevalITEBasic _ _ _ = Nothing

pevalITEBoolBasic :: Term Bool -> Term Bool -> Term Bool -> Maybe (Term Bool)
pevalITEBoolBasic cond ifTrue ifFalse
  | cond == ifTrue = Just $ pevalOrTerm cond ifFalse
  | cond == ifFalse = Just $ pevalAndTerm cond ifTrue
pevalITEBoolBasic cond (ConTerm _ v) ifFalse
  | v = Just $ pevalOrTerm cond ifFalse
  | otherwise = Just $ pevalAndTerm (pevalNotTerm cond) ifFalse
pevalITEBoolBasic cond ifTrue (ConTerm _ v)
  | v = Just $ pevalOrTerm (pevalNotTerm cond) ifTrue
  | otherwise = Just $ pevalAndTerm cond ifTrue
pevalITEBoolBasic _ _ _ = Nothing

pevalITEBool :: Term Bool -> Term Bool -> Term Bool -> Maybe (Term Bool)
pevalITEBool cond ifTrue ifFalse =
  msum
    [ pevalITEBasic cond ifTrue ifFalse,
      pevalITEBoolBasic cond ifTrue ifFalse,
      pevalITEBoolLeft cond ifTrue ifFalse,
      pevalITEBoolNoLeft cond ifTrue ifFalse
    ]

pevalITETerm :: forall a. (SupportedPrim a) => Term Bool -> Term a -> Term a -> Term a
pevalITETerm cond ifTrue ifFalse = fromMaybe (iteTerm cond ifTrue ifFalse) $
  case eqT @a @Bool of
    Nothing -> pevalITEBasic cond ifTrue ifFalse
    Just Refl -> pevalITEBool cond ifTrue ifFalse

pevalImplyTerm :: Term Bool -> Term Bool -> Term Bool
pevalImplyTerm l = pevalOrTerm (pevalNotTerm l)

pevalXorTerm :: Term Bool -> Term Bool -> Term Bool
pevalXorTerm l r = pevalOrTerm (pevalAndTerm (pevalNotTerm l) r) (pevalAndTerm l (pevalNotTerm r))
