{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- Module      :   Grisette.Internal.SymPrim.Prim.PartialEval.PartialEval
-- Copyright   :   (c) Sirui Lu 2021-2023
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Internal.SymPrim.Prim.Internal.PartialEval
  ( PartialFun,
    PartialRuleUnary,
    TotalRuleUnary,
    PartialRuleBinary,
    TotalRuleBinary,
    totalize,
    totalize2,
    UnaryPartialStrategy (..),
    unaryPartial,
    BinaryCommPartialStrategy (..),
    BinaryPartialStrategy (..),
    binaryPartial,
  )
where

import Control.Monad.Except (MonadError (catchError))
import Grisette.Internal.SymPrim.Prim.Internal.Term (Term)

-- | A partial function from a to b.
type PartialFun a b = a -> Maybe b

-- | A partial rule for unary operations.
type PartialRuleUnary a b = PartialFun (Term a) (Term b)

-- | A total rule for unary operations.
type TotalRuleUnary a b = Term a -> Term b

-- | A partial rule for binary operations.
type PartialRuleBinary a b c = Term a -> PartialFun (Term b) (Term c)

-- | A total rule for binary operations.
type TotalRuleBinary a b c = Term a -> Term b -> Term c

-- | Totalize a partial function with a fallback function.
totalize :: PartialFun a b -> (a -> b) -> a -> b
totalize partial fallback a =
  case partial a of
    Just b -> b
    Nothing -> fallback a

-- | Totalize a binary partial function with a fallback function.
totalize2 :: (a -> PartialFun b c) -> (a -> b -> c) -> a -> b -> c
totalize2 partial fallback a b =
  case partial a b of
    Just c -> c
    Nothing -> fallback a b

-- | A strategy for partially evaluating unary operations.
class UnaryPartialStrategy tag a b | tag a -> b where
  extractor :: tag -> Term a -> Maybe a
  constantHandler :: tag -> a -> Maybe (Term b)
  nonConstantHandler :: tag -> Term a -> Maybe (Term b)

-- | Partially evaluate a unary operation.
unaryPartial :: forall tag a b. (UnaryPartialStrategy tag a b) => tag -> PartialRuleUnary a b
unaryPartial tag a = case extractor tag a of
  Nothing -> nonConstantHandler tag a
  Just a' -> constantHandler tag a'

-- | A strategy for partially evaluating commutative binary operations.
class BinaryCommPartialStrategy tag a c | tag a -> c where
  singleConstantHandler :: tag -> a -> Term a -> Maybe (Term c)

-- | A strategy for partially evaluating operations.
class BinaryPartialStrategy tag a b c | tag a b -> c where
  extractora :: tag -> Term a -> Maybe a
  extractorb :: tag -> Term b -> Maybe b
  allConstantHandler :: tag -> a -> b -> Maybe (Term c)
  leftConstantHandler :: tag -> a -> Term b -> Maybe (Term c)
  default leftConstantHandler :: (a ~ b, BinaryCommPartialStrategy tag a c) => tag -> a -> Term b -> Maybe (Term c)
  leftConstantHandler = singleConstantHandler @tag @a
  rightConstantHandler :: tag -> Term a -> b -> Maybe (Term c)
  default rightConstantHandler :: (a ~ b, BinaryCommPartialStrategy tag a c) => tag -> Term a -> b -> Maybe (Term c)
  rightConstantHandler tag = flip $ singleConstantHandler @tag @a tag
  nonBinaryConstantHandler :: tag -> Term a -> Term b -> Maybe (Term c)

-- | Partially evaluate a binary operation.
binaryPartial :: forall tag a b c. (BinaryPartialStrategy tag a b c) => tag -> PartialRuleBinary a b c
binaryPartial tag a b = case (extractora @tag @a @b @c tag a, extractorb @tag @a @b @c tag b) of
  (Nothing, Nothing) -> nonBinaryConstantHandler @tag @a @b @c tag a b
  (Just a', Nothing) ->
    leftConstantHandler @tag @a @b @c tag a' b
      `catchError` \_ -> nonBinaryConstantHandler @tag @a @b @c tag a b
  (Nothing, Just b') ->
    rightConstantHandler @tag @a @b @c tag a b'
      `catchError` \_ -> nonBinaryConstantHandler @tag @a @b @c tag a b
  (Just a', Just b') ->
    allConstantHandler @tag @a @b @c tag a' b'
