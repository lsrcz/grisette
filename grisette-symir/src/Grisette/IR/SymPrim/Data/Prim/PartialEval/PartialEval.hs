{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Grisette.IR.SymPrim.Data.Prim.PartialEval.PartialEval
  ( PartialFunc,
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

import Control.Monad.Except
import Grisette.IR.SymPrim.Data.Prim.InternedTerm.Term

type PartialFunc a b = a -> Maybe b

type PartialRuleUnary a b = PartialFunc (Term a) (Term b)

type TotalRuleUnary a b = Term a -> Term b

type PartialRuleBinary a b c = Term a -> PartialFunc (Term b) (Term c)

type TotalRuleBinary a b c = Term a -> Term b -> Term c

totalize :: PartialFunc a b -> (a -> b) -> a -> b
totalize partial fallback a =
  case partial a of
    Just b -> b
    Nothing -> fallback a

totalize2 :: (a -> PartialFunc b c) -> (a -> b -> c) -> a -> b -> c
totalize2 partial fallback a b =
  case partial a b of
    Just c -> c
    Nothing -> fallback a b

class UnaryPartialStrategy tag a b | tag a -> b where
  extractor :: tag -> Term a -> Maybe a
  constantHandler :: tag -> a -> Maybe (Term b)
  nonConstantHandler :: tag -> Term a -> Maybe (Term b)

unaryPartial :: forall tag a b. (UnaryPartialStrategy tag a b) => tag -> PartialRuleUnary a b
unaryPartial tag a = case extractor tag a of
  Nothing -> nonConstantHandler tag a
  Just a' -> constantHandler tag a'

class BinaryCommPartialStrategy tag a c | tag a -> c where
  singleConstantHandler :: tag -> a -> Term a -> Maybe (Term c)

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
