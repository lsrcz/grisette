{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module      :   Grisette.Internal.SymPrim.Prim.PartialEval.Unfold
-- Copyright   :   (c) Sirui Lu 2021-2023
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Internal.SymPrim.Prim.Internal.Unfold
  ( unaryUnfoldOnce,
    binaryUnfoldOnce,
    generalUnaryUnfolded,
    generalBinaryUnfolded,
  )
where

import Control.Monad.Except (MonadError (catchError))
import Data.Typeable (Typeable)
import Grisette.Internal.SymPrim.Prim.Internal.PartialEval
  ( PartialRuleBinary,
    PartialRuleUnary,
    TotalRuleBinary,
    TotalRuleUnary,
    totalize,
    totalize2,
  )
import Grisette.Internal.SymPrim.Prim.Internal.Term
  ( SupportedPrim (pevalITETerm),
    Term,
    conTerm,
    pattern ConTerm,
    pattern ITETerm,
  )

unaryPartialUnfoldOnce ::
  forall a b.
  (SupportedPrim b) =>
  PartialRuleUnary a b ->
  TotalRuleUnary a b ->
  PartialRuleUnary a b
unaryPartialUnfoldOnce partial fallback = ret
  where
    oneLevel :: TotalRuleUnary a b -> PartialRuleUnary a b
    oneLevel fallback' x = case (x, partial x) of
      (ITETerm cond vt vf, pr) ->
        let pt = partial vt
            pf = partial vf
         in case (pt, pf) of
              (Nothing, Nothing) -> pr
              (mt, mf) ->
                pevalITETerm cond
                  <$> catchError mt (\_ -> Just $ totalize (oneLevel fallback') fallback' vt)
                  <*> catchError mf (\_ -> Just $ totalize (oneLevel fallback') fallback vf)
      (_, pr) -> pr
    ret :: PartialRuleUnary a b
    ret = oneLevel (totalize @(Term a) @(Term b) partial fallback)

-- | Unfold a unary operation once.
unaryUnfoldOnce ::
  forall a b.
  (SupportedPrim b) =>
  PartialRuleUnary a b ->
  TotalRuleUnary a b ->
  TotalRuleUnary a b
unaryUnfoldOnce partial fallback = totalize (unaryPartialUnfoldOnce partial fallback) fallback

binaryPartialUnfoldOnce ::
  forall a b c.
  (SupportedPrim c) =>
  PartialRuleBinary a b c ->
  TotalRuleBinary a b c ->
  PartialRuleBinary a b c
binaryPartialUnfoldOnce partial fallback = ret
  where
    oneLevel :: PartialRuleBinary x y c -> TotalRuleBinary x y c -> PartialRuleBinary x y c
    oneLevel partial' fallback' x y =
      catchError
        (partial' x y)
        ( \_ ->
            case (x, y) of
              (ITETerm _ ITETerm {} _, ITETerm {}) -> Nothing
              (ITETerm _ _ ITETerm {}, ITETerm {}) -> Nothing
              (ITETerm {}, ITETerm _ ITETerm {} _) -> Nothing
              (ITETerm {}, ITETerm _ _ ITETerm {}) -> Nothing
              (ITETerm cond vt vf, _) ->
                left cond vt vf y partial' fallback'
              (_, ITETerm cond vt vf) ->
                left cond vt vf x (flip partial') (flip fallback')
              _ -> Nothing
        )
    left ::
      Term Bool ->
      Term x ->
      Term x ->
      Term y ->
      PartialRuleBinary x y c ->
      TotalRuleBinary x y c ->
      Maybe (Term c)
    left cond vt vf y partial' fallback' =
      let pt = partial' vt y
          pf = partial' vf y
       in case (pt, pf) of
            (Nothing, Nothing) -> Nothing
            (mt, mf) ->
              pevalITETerm cond
                <$> catchError mt (\_ -> Just $ totalize2 (oneLevel partial' fallback') fallback' vt y)
                <*> catchError mf (\_ -> Just $ totalize2 (oneLevel partial' fallback') fallback' vf y)
    ret :: PartialRuleBinary a b c
    ret = oneLevel partial (totalize2 @(Term a) @(Term b) @(Term c) partial fallback)

-- | Unfold a binary operation once.
binaryUnfoldOnce ::
  forall a b c.
  (SupportedPrim c) =>
  PartialRuleBinary a b c ->
  TotalRuleBinary a b c ->
  TotalRuleBinary a b c
binaryUnfoldOnce partial fallback = totalize2 (binaryPartialUnfoldOnce partial fallback) fallback

-- | Unfold a unary operation once.
generalUnaryUnfolded ::
  forall a b.
  (Typeable a, SupportedPrim b) =>
  (a -> b) ->
  (Term a -> Term b) ->
  Term a ->
  Term b
generalUnaryUnfolded compute =
  unaryUnfoldOnce
    ( \case
        ConTerm lv -> Just $ conTerm $ compute lv
        _ -> Nothing
    )

-- | Unfold a binary operation once.
generalBinaryUnfolded ::
  forall a b c.
  (Typeable a, Typeable b, SupportedPrim c) =>
  (a -> b -> c) ->
  (Term a -> Term b -> Term c) ->
  Term a ->
  Term b ->
  Term c
generalBinaryUnfolded compute =
  binaryUnfoldOnce
    ( \l r -> case (l, r) of
        (ConTerm lv, ConTerm rv) -> Just $ conTerm $ compute lv rv
        _ -> Nothing
    )
