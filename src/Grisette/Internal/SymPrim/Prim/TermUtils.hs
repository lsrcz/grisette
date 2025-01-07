{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module      :   Grisette.Internal.SymPrim.Prim.TermUtils
-- Copyright   :   (c) Sirui Lu 2021-2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Internal.SymPrim.Prim.TermUtils
  ( extractTerm,
    castTerm,
    someTermsSize,
    someTermSize,
    termSize,
    termsSize,
  )
where

import Control.Monad.State
  ( State,
    execState,
    gets,
    modify',
  )
import Data.Data (cast)
import Data.Foldable (traverse_)
import qualified Data.HashSet as HS
import Grisette.Internal.Core.Data.MemoUtils (htmemo)
import Grisette.Internal.SymPrim.GeneralFun (type (-->) (GeneralFun))
import Grisette.Internal.SymPrim.Prim.Internal.Term
  ( IsSymbolKind (SymbolKindConstraint),
    SomeTypedConstantSymbol,
    SomeTypedSymbol (SomeTypedSymbol),
    SupportedPrim (castTypedSymbol, primTypeRep),
    Term,
    TypedAnySymbol,
    someTypedSymbol,
    pattern ConTerm,
    pattern ExistsTerm,
    pattern ForallTerm,
    pattern SupportedTerm,
    pattern SymTerm,
  )
import Grisette.Internal.SymPrim.Prim.Pattern (pattern SubTerms)
import Grisette.Internal.SymPrim.Prim.SomeTerm
  ( SomeTerm (SomeTerm),
    someTerm,
  )
import Type.Reflection
  ( TypeRep,
    Typeable,
    eqTypeRep,
    typeRep,
    pattern App,
    type (:~~:) (HRefl),
  )

{-# NOINLINE extractSymSomeTerm #-}
extractSymSomeTerm ::
  forall knd.
  (IsSymbolKind knd) =>
  HS.HashSet (SomeTypedConstantSymbol) ->
  SomeTerm ->
  Maybe (HS.HashSet (SomeTypedSymbol knd))
extractSymSomeTerm initialBounded = go initialMemo initialBounded
  where
    gotyped ::
      ( SomeTerm ->
        Maybe (HS.HashSet (SomeTypedSymbol knd))
      ) ->
      Term a ->
      Maybe (HS.HashSet (SomeTypedSymbol knd))
    gotyped memo a = memo (someTerm a)
    initialMemo ::
      SomeTerm ->
      Maybe (HS.HashSet (SomeTypedSymbol knd))
    initialMemo = htmemo (go initialMemo initialBounded)
    {-# NOINLINE initialMemo #-}

    go ::
      ( SomeTerm ->
        Maybe (HS.HashSet (SomeTypedSymbol knd))
      ) ->
      HS.HashSet (SomeTypedConstantSymbol) ->
      SomeTerm ->
      Maybe (HS.HashSet (SomeTypedSymbol knd))
    go _ bs (SomeTerm (SymTerm (sym :: TypedAnySymbol a))) =
      case (castTypedSymbol sym, castTypedSymbol sym) of
        (Just sym', _) | HS.member (someTypedSymbol sym') bs -> return HS.empty
        (_, Just sym') ->
          return $ HS.singleton $ SomeTypedSymbol sym'
        _ -> Nothing
    go _ bs (SomeTerm (ConTerm cv :: Term v)) =
      case (primTypeRep :: TypeRep v) of
        App (App gf _) _ ->
          case eqTypeRep (typeRep @(-->)) gf of
            Just HRefl -> case cv of
              GeneralFun sym (tm :: Term r) ->
                let newBounded = HS.union (HS.singleton (someTypedSymbol sym)) bs
                    newmemo = htmemo (go newmemo newBounded)
                    {-# NOINLINE newmemo #-}
                 in gotyped newmemo tm
            Nothing -> return HS.empty
        _ -> return HS.empty
    go _ bs (SomeTerm (ForallTerm sym arg)) =
      let newBounded = HS.insert (someTypedSymbol sym) bs
          newmemo = htmemo (go newmemo newBounded)
          {-# NOINLINE newmemo #-}
       in gotyped newmemo arg
    go _ bs (SomeTerm (ExistsTerm sym arg)) =
      let newBounded = HS.insert (someTypedSymbol sym) bs
          newmemo = htmemo (go newmemo newBounded)
          {-# NOINLINE newmemo #-}
       in gotyped newmemo arg
    go memo _ (SomeTerm (SubTerms ts)) = combineAllSets $ map memo ts
    combineSet (Just a) (Just b) = Just $ HS.union a b
    combineSet _ _ = Nothing
    combineAllSets = foldl1 combineSet

-- | Extract all the symbols in a term.
extractTerm ::
  (IsSymbolKind knd, SymbolKindConstraint knd a, SupportedPrim a) =>
  HS.HashSet (SomeTypedConstantSymbol) ->
  Term a ->
  Maybe (HS.HashSet (SomeTypedSymbol knd))
extractTerm initialBoundedSymbols t =
  extractSymSomeTerm initialBoundedSymbols (SomeTerm t)
{-# NOINLINE extractTerm #-}

-- | Cast a term to another type.
castTerm :: forall a b. (Typeable b) => Term a -> Maybe (Term b)
castTerm t@SupportedTerm = cast t
{-# INLINE castTerm #-}

-- | Compute the size of a list of terms. Do not count the same term twice.
someTermsSize :: [SomeTerm] -> Int
someTermsSize terms = HS.size $ execState (traverse goSome terms) HS.empty
  where
    exists t = gets (HS.member (someTerm t))
    add t = modify' (HS.insert (someTerm t))
    goSome :: SomeTerm -> State (HS.HashSet SomeTerm) ()
    goSome (SomeTerm b) = go b
    go :: forall b. Term b -> State (HS.HashSet SomeTerm) ()
    go t@ConTerm {} = add t
    go t@SymTerm {} = add t
    go t@(SubTerms ts) = do
      b <- exists t
      if b
        then return ()
        else do
          add t
          traverse_ goSome ts
{-# INLINEABLE someTermsSize #-}

-- | Compute the size of a list of terms. Do not count the same term twice.
someTermSize :: SomeTerm -> Int
someTermSize term = someTermsSize [term]
{-# INLINE someTermSize #-}

-- | Compute the size of a list of terms. Do not count the same term twice.
termsSize :: [Term a] -> Int
termsSize terms = someTermsSize $ someTerm <$> terms
{-# INLINEABLE termsSize #-}

-- | Compute the size of a term.
termSize :: Term a -> Int
termSize term = termsSize [term]
{-# INLINE termSize #-}
