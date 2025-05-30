{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      :   Grisette.Lib.Data.List
-- Copyright   :   (c) Sirui Lu 2021-2023
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Lib.Data.List
  ( -- * Special folds
    symAnd,
    symOr,
    symAny,
    symAll,
    mrgMaximum,
    symMaximum,
    mrgMinimum,
    symMinimum,

    -- * Sublists

    -- ** Extracting sublists
    mrgTake,
    mrgDrop,
    mrgSplitAt,
    mrgTakeWhile,
    mrgDropWhile,
    mrgDropWhileEnd,
    mrgSpan,
    mrgBreak,
    mrgStripPrefix,
    mrgGroup,

    -- ** Predicates
    symIsPrefixOf,
    symIsSuffixOf,
    symIsInfixOf,
    symIsSubsequenceOf,

    -- * Searching lists

    -- ** Searching by equality
    symElem,
    symNotElem,
    mrgLookup,

    -- ** Searching with a predicate
    mrgFind,
    mrgFilter,
    mrgPartition,

    -- * Indexing lists
    (.!?),
    mrgElemIndex,
    mrgElemIndices,
    mrgFindIndex,
    mrgFindIndices,

    -- * Special lists

    -- ** "Set" operations
    mrgNub,
    mrgDelete,
    (.\\),
    mrgUnion,
    mrgIntersect,

    -- ** Ordered lists (sorting not supported yet)
    mrgInsert,

    -- * Generalized functions

    -- ** The "By" operations

    -- *** User-supplied equality (replacing an 'SymEq' context)
    mrgNubBy,
    mrgDeleteBy,
    mrgDeleteFirstsBy,
    mrgUnionBy,
    mrgIntersectBy,
    mrgGroupBy,

    -- *** User-supplied comparison (replacing an 'SymOrd' context)
    mrgInsertBy,
    mrgMaximumBy,
    symMaximumBy,
    mrgMinimumBy,
    symMinimumBy,
  )
where

import Data.Bifunctor (Bifunctor (first, second))
import Data.List (tails)
import Data.Maybe (listToMaybe)
import Grisette.Internal.Core.Control.Monad.Class.Union (MonadUnion)
import Grisette.Internal.Core.Control.Monad.Union (Union)
import Grisette.Internal.Core.Data.Class.ITEOp (ITEOp (symIte))
import Grisette.Internal.Core.Data.Class.LogicalOp (LogicalOp (symNot, (.&&), (.||)))
import Grisette.Internal.Core.Data.Class.Mergeable (Mergeable)
import Grisette.Internal.Core.Data.Class.SimpleMergeable (SymBranching, mrgIf)
import Grisette.Internal.Core.Data.Class.Solvable (Solvable (con))
import Grisette.Internal.Core.Data.Class.SymEq (SymEq ((./=), (.==)))
import Grisette.Internal.Core.Data.Class.SymOrd (SymOrd ((.<=), (.>=)))
import Grisette.Internal.Core.Data.Class.UnionView (liftUnion)
import Grisette.Internal.SymPrim.SymBool (SymBool)
import Grisette.Lib.Control.Applicative (mrgPure)
import Grisette.Lib.Control.Monad (mrgReturn)
import Grisette.Lib.Data.Foldable
  ( mrgFind,
    mrgFoldlM,
    mrgMaximum,
    mrgMaximumBy,
    mrgMinimum,
    mrgMinimumBy,
    symAll,
    symAnd,
    symAny,
    symElem,
    symMaximum,
    symMaximumBy,
    symMinimum,
    symMinimumBy,
    symNotElem,
    symOr,
  )
import Grisette.Lib.Data.Functor (mrgFmap)

symListOpOnSymInt ::
  (Applicative u, SymBranching u, Mergeable b, Num int, SymOrd int) =>
  Bool ->
  (Int -> [a] -> b) ->
  int ->
  [a] ->
  u b
symListOpOnSymInt reversed f x vs = do
  let zipped =
        (\n -> (fromIntegral n, mrgPure $ f n vs))
          <$> (if reversed then reverse else id) [1 .. length vs - 1]
  let outerMostX = if reversed then length vs else 0
  let innerMostX = if reversed then 0 else length vs
  let guardCond =
        if reversed then (x .>= fromIntegral (length vs)) else (x .<= 0)
  mrgIf guardCond (mrgPure $ f outerMostX vs) $
    foldr
      (\(n, l) acc -> mrgIf (x .== n) l acc)
      (mrgPure $ f innerMostX vs)
      zipped

-- | Symbolic version of 'Data.List.take', the result would be merged and
-- propagate the mergeable knowledge.
--
-- Can generate O(n) cases and O(n) sized branch constraints.
mrgTake ::
  (Applicative u, SymBranching u, Mergeable a, Num int, SymOrd int) =>
  int ->
  [a] ->
  u [a]
mrgTake = symListOpOnSymInt False take

-- | Symbolic version of 'Data.List.drop', the result would be merged and
-- propagate the mergeable knowledge.
--
-- Can generate O(n) cases and O(n) sized branch constraints.
mrgDrop ::
  (Applicative u, SymBranching u, Mergeable a, Num int, SymOrd int) =>
  int ->
  [a] ->
  u [a]
mrgDrop = symListOpOnSymInt True drop

-- | Symbolic version of 'Data.List.splitAt', the result would be merged and
-- propagate the mergeable knowledge.
--
-- Can generate O(n) cases and O(n) sized branch constraints.
mrgSplitAt ::
  forall a int u.
  (MonadUnion u, Mergeable a, Num int, SymOrd int) =>
  int ->
  [a] ->
  u ([a], [a])
mrgSplitAt = symListOpOnSymInt False splitAt

-- | Symbolic version of 'Data.List.takeWhile', the result would be merged and
-- propagate the mergeable knowledge.
--
-- Can generate O(n) cases and O(n) sized branch constraints.
mrgTakeWhile ::
  (Applicative u, SymBranching u, Mergeable a) =>
  (a -> SymBool) ->
  [a] ->
  u [a]
mrgTakeWhile _ [] = mrgPure []
mrgTakeWhile p (x : xs) =
  mrgIf (p x) (mrgFmap (x :) $ mrgTakeWhile p xs) (mrgPure [])

-- | Symbolic version of 'Data.List.dropWhile', the result would be merged and
-- propagate the mergeable knowledge.
--
-- Can generate O(n) cases and O(n) sized branch constraints.
mrgDropWhile ::
  (Applicative u, SymBranching u, Mergeable a) =>
  (a -> SymBool) ->
  [a] ->
  u [a]
mrgDropWhile _ [] = mrgPure []
mrgDropWhile p r = do
  let allConds = reverse $ scanl1 (.&&) $ p <$> r
  foldr (\(cond, l) acc -> mrgIf cond (pure l) acc) (pure r) $
    zip allConds $
      reverse $
        tails r

-- | Symbolic version of 'Data.List.dropWhileEnd', the result would be merged
-- and propagate the mergeable knowledge.
--
-- Can generate O(n) cases and O(n) sized branch constraints.
mrgDropWhileEnd ::
  (MonadUnion u, Mergeable a) =>
  (a -> SymBool) ->
  [a] ->
  u [a]
mrgDropWhileEnd p =
  foldr
    ( \x xs -> do
        xsv <- xs
        mrgIf (p x .&& con (null xsv)) (mrgPure []) (mrgPure $ x : xsv)
    )
    (mrgPure [])

-- | Symbolic version of 'Data.List.span', the result would be merged and
-- propagate the mergeable knowledge.
--
-- Can generate O(n) cases and O(n) sized branch constraints.
mrgSpan ::
  (Applicative u, SymBranching u, Mergeable a) =>
  (a -> SymBool) ->
  [a] ->
  u ([a], [a])
mrgSpan _ [] = mrgPure ([], [])
mrgSpan p xs@(x : xs') =
  mrgIf (p x) (mrgFmap (first (x :)) $ mrgSpan p xs') (mrgPure ([], xs))

-- | Symbolic version of 'Data.List.break', the result would be merged and
-- propagate the mergeable knowledge.
--
-- Can generate O(n) cases and O(n) sized branch constraints.
mrgBreak ::
  (Applicative u, SymBranching u, Mergeable a) =>
  (a -> SymBool) ->
  [a] ->
  u ([a], [a])
mrgBreak p = mrgSpan (symNot . p)

-- | Symbolic version of 'Data.List.stripPrefix', the result would be merged and
-- propagate the mergeable knowledge.
--
-- Generate O(1) cases and O(len(prefix)) sized branch constraints.
mrgStripPrefix ::
  (Applicative u, SymBranching u, Mergeable a, SymEq a) =>
  [a] ->
  [a] ->
  u (Maybe [a])
mrgStripPrefix [] ys = mrgPure $ Just ys
mrgStripPrefix (x : xs) (y : ys) =
  mrgIf (x .== y) (mrgStripPrefix xs ys) (mrgPure Nothing)
mrgStripPrefix _ _ = mrgPure Nothing

-- | Symbolic version of 'Data.List.group', the result would be merged and
-- propagate the mergeable knowledge.
--
-- This function can be very inefficient on large symbolic lists and generate
-- O(2^n) cases. Use with caution.
mrgGroup ::
  (MonadUnion u, Mergeable a, SymEq a) =>
  [a] ->
  u [[a]]
mrgGroup = mrgGroupBy (.==)

-- | Symbolic version of 'Data.List.isPrefixOf'.
--
-- Generate O(len(prefix)) sized constraints.
symIsPrefixOf :: (SymEq a) => [a] -> [a] -> SymBool
symIsPrefixOf [] _ = con True
symIsPrefixOf _ [] = con False
symIsPrefixOf (x : xs) (y : ys) =
  x .== y .&& symIsPrefixOf xs ys

-- | Symbolic version of 'Data.List.isSuffixOf'.
--
-- Generate O(len(suffix)) sized constraints.
symIsSuffixOf :: (SymEq a) => [a] -> [a] -> SymBool
symIsSuffixOf ns hs = symIsPrefixOf (reverse ns) (reverse hs)

-- | Symbolic version of 'Data.List.isInfixOf'.
--
-- Generate O(len(haystack) * len(needle)) sized constraints.
symIsInfixOf :: (SymEq a) => [a] -> [a] -> SymBool
symIsInfixOf needle haystack = symAny (symIsPrefixOf needle) (tails haystack)

-- | Symbolic version of 'Data.List.isSubsequenceOf'.
--
-- Generate O(len(haystack) * len(needle)) sized constraints.
symIsSubsequenceOf :: (SymEq a) => [a] -> [a] -> SymBool
symIsSubsequenceOf [] _ = con True
symIsSubsequenceOf _ [] = con False
symIsSubsequenceOf a@(x : a') (y : b) =
  symIte (x .== y) (symIsSubsequenceOf a' b) (symIsSubsequenceOf a b)

-- | Symbolic version of 'Data.List.lookup', the result would be merged and
-- propagate the mergeable knowledge.
--
-- Can generate O(n) cases and O(n) sized branch constraints.
mrgLookup ::
  forall a b u.
  (Applicative u, SymBranching u, Mergeable b, SymEq a) =>
  a ->
  [(a, b)] ->
  u (Maybe b)
mrgLookup _ [] = mrgPure Nothing
mrgLookup key l =
  mrgIf (symAll (key ./=) (fst <$> l)) (mrgPure Nothing) $
    mrgLookup' l
  where
    mrgLookup' :: [(a, b)] -> u (Maybe b)
    mrgLookup' [] = error "mrgLookup: impossible"
    mrgLookup' [(_, y)] = mrgPure $ Just y
    mrgLookup' ((x, y) : xys) =
      mrgIf (key .== x) (mrgPure $ Just y) (mrgLookup' xys)

-- | Symbolic version of 'Data.List.filter', the result would be merged and
-- propagate the mergeable knowledge.
--
-- This function can be very inefficient on large symbolic lists and generate
-- O(2^n) cases. Use with caution.
mrgFilter ::
  (Applicative u, SymBranching u, Mergeable a) =>
  (a -> SymBool) ->
  [a] ->
  u [a]
mrgFilter _ [] = mrgPure []
mrgFilter p (x : xs) =
  mrgIf (p x) (mrgFmap (x :) $ mrgFilter p xs) (mrgFilter p xs)

-- | Symbolic version of 'Data.List.partition', the result would be merged and
-- propagate the mergeable knowledge.
--
-- This function can be very inefficient on large symbolic lists and generate
-- O(2^n) cases. Use with caution.
mrgPartition ::
  forall u a.
  (Applicative u, SymBranching u, Mergeable a) =>
  (a -> SymBool) ->
  [a] ->
  u ([a], [a])
mrgPartition _ [] = mrgPure ([], [])
mrgPartition p (x : xs) =
  mrgIf
    (p x)
    (mrgFmap (first (x :)) partitioned)
    (mrgFmap (second (x :)) partitioned)
  where
    partitioned :: u ([a], [a])
    partitioned = mrgPartition p xs

-- | Symbolic version of 'Data.List.!?', the result would be merged and
-- propagate the mergeable knowledge.
--
-- Can generate O(1) cases and O(n) sized branch constraints.
(.!?) ::
  forall a uf int.
  ( MonadUnion uf,
    Mergeable a,
    Num int,
    SymEq int
  ) =>
  [a] ->
  int ->
  uf (Maybe a)
l .!? p = go l p 0
  where
    go :: [a] -> int -> int -> uf (Maybe a)
    go [] _ _ = mrgReturn Nothing
    go (x : xs) p1 i = mrgIf (p1 .== i) (mrgReturn $ Just x) (go xs p1 $ i + 1)

-- | Symbolic version of 'Data.List.elemIndex', the result would be merged and
-- propagate the mergeable knowledge.
--
-- Can generate O(n) cases (or O(1) if int is merged), and O(n^2) sized
-- constraints.
mrgElemIndex ::
  (MonadUnion u, Mergeable int, SymEq a, Num int) =>
  a ->
  [a] ->
  u (Maybe int)
mrgElemIndex x = mrgFindIndex (x .==)

-- | Symbolic version of 'Data.List.elemIndices', the result would be merged and
-- propagate the mergeable knowledge.
--
-- Can generate O(n) cases, and O(n^3) sized constraints.
mrgElemIndices ::
  (MonadUnion u, Mergeable int, SymEq a, Num int) =>
  a ->
  [a] ->
  u [int]
mrgElemIndices x = mrgFindIndices (x .==)

-- | Symbolic version of 'Data.List.findIndex', the result would be merged and
-- propagate the mergeable knowledge.
--
-- Can generate O(n) cases (or O(1) if int is merged), and O(n^2) sized
-- constraints, assuming the predicate only generates O(1) constraints.
mrgFindIndex ::
  (Applicative u, SymBranching u, Mergeable int, SymEq a, Num int) =>
  (a -> SymBool) ->
  [a] ->
  u (Maybe int)
mrgFindIndex p l = mrgFmap listToMaybe $ mrgFindIndices p l

-- | Symbolic version of 'Data.List.findIndices', the result would be merged and
-- propagate the mergeable knowledge.
--
-- Can generate O(n) cases, and O(n^3) sized constraints, assuming the predicate
-- only generates O(1) constraints.
mrgFindIndices ::
  forall u a int.
  (Applicative u, SymBranching u, Mergeable int, SymEq a, Num int) =>
  (a -> SymBool) ->
  [a] ->
  u [int]
mrgFindIndices p xs = go $ zip xs $ fromIntegral <$> [0 ..]
  where
    go :: [(a, int)] -> u [int]
    go [] = mrgPure []
    go ((x, y) : xys) = mrgIf (p x) (mrgFmap (y :) $ go xys) (go xys)

-- | Symbolic version of 'Data.List.nub', the result would be merged and
-- propagate the mergeable knowledge.
--
-- Can generate O(n) cases, and O(n^3) sized constraints.
mrgNub ::
  (Applicative u, SymBranching u, Mergeable a, SymEq a) =>
  [a] ->
  u [a]
mrgNub = mrgNubBy (.==)

-- | Symbolic version of 'Data.List.delete', the result would be merged and
-- propagate the mergeable knowledge.
--
-- Can generate O(n) cases, and O(n^2) sized constraints.
mrgDelete ::
  (Applicative u, SymBranching u, Mergeable a, SymEq a) =>
  a ->
  [a] ->
  u [a]
mrgDelete = mrgDeleteBy (.==)

-- | Symbolic version of 'Data.List.\\', the result would be merged and
-- propagate the mergeable knowledge.
--
-- Can generate O(len(lhs)) cases, and O(len(lhs)^2 * len(rhs)) sized
-- constraints.
(.\\) ::
  (MonadUnion u, Mergeable a, SymEq a) =>
  [a] ->
  [a] ->
  u [a]
(.\\) = mrgDeleteFirstsBy (.==)

-- | Symbolic version of 'Data.List.union', the result would be merged and
-- propagate the mergeable knowledge.
--
-- Can generate O(len(rhs)) cases, and O(len(rhs)^5 * len(lhs)) sized
-- constraints.
--
-- Should be improvable.
mrgUnion ::
  (MonadUnion u, Mergeable a, SymEq a) =>
  [a] ->
  [a] ->
  u [a]
mrgUnion = mrgUnionBy (.==)

-- | Symbolic version of 'Data.List.intersect', the result would be merged and
-- propagate the mergeable knowledge.
--
-- Can generate O(len(rhs)) cases, and O(len(lhs) * len(rhs)) constraints.
mrgIntersect ::
  (MonadUnion u, Mergeable a, SymEq a) =>
  [a] ->
  [a] ->
  u [a]
mrgIntersect = mrgIntersectBy (.==)

-- | Symbolic version of 'Data.List.nubBy', the result would be merged and
-- propagate the mergeable knowledge.
--
-- Can generate O(n) cases, and O(n^3) sized constraints, assuming the predicate
-- only generates O(1) constraints.
mrgNubBy ::
  forall a u.
  (Applicative u, SymBranching u, Mergeable a) =>
  (a -> a -> SymBool) ->
  [a] ->
  u [a]
mrgNubBy eq l = mrgNubBy' l []
  where
    mrgNubBy' :: [a] -> [a] -> u [a]
    mrgNubBy' [] _ = mrgPure []
    mrgNubBy' (y : ys) xs =
      mrgIf
        (mrgElemBy y xs)
        (mrgNubBy' ys xs)
        (mrgFmap (y :) $ mrgNubBy' ys (y : xs))
    mrgElemBy _ [] = con False
    mrgElemBy x (y : ys) = eq x y .|| mrgElemBy x ys

-- | Symbolic version of 'Data.List.deleteBy', the result would be merged and
-- propagate the mergeable knowledge.
--
-- Can generate O(n) cases, and O(n^2) sized constraints, assuming the predicate
-- only generates O(1) constraints.
mrgDeleteBy ::
  (Applicative u, SymBranching u, Mergeable a) =>
  (a -> a -> SymBool) ->
  a ->
  [a] ->
  u [a]
mrgDeleteBy _ _ [] = mrgPure []
mrgDeleteBy eq x (y : ys) =
  mrgIf (eq x y) (mrgPure ys) (mrgFmap (y :) $ mrgDeleteBy eq x ys)

-- | Symbolic version of 'Data.List.deleteFirstsBy', the result would be merged
-- and propagate the mergeable knowledge.
--
-- Can generate O(len(lhs)) cases, and O(len(lhs)^2 * len(rhs)) sized
-- constraints, assuming the predicate only generates O(1) constraints.
mrgDeleteFirstsBy ::
  (MonadUnion u, Mergeable a) =>
  (a -> a -> SymBool) ->
  [a] ->
  [a] ->
  u [a]
mrgDeleteFirstsBy eq = mrgFoldlM (flip $ mrgDeleteBy eq)

-- | Symbolic version of 'Data.List.unionBy', the result would be merged and
-- propagate the mergeable knowledge.
--
-- Can generate O(len(rhs)) cases, and O(len(rhs)^5 * len(lhs)) sized
-- constraints, assuming the predicate only generates O(1) constraints.
--
-- Should be improvable.
mrgUnionBy ::
  (MonadUnion u, Mergeable a) =>
  (a -> a -> SymBool) ->
  [a] ->
  [a] ->
  u [a]
mrgUnionBy eq xs ys =
  mrgFmap (xs ++) $
    (mrgNubBy eq ys)
      >>= \nubbed -> mrgFoldlM (flip $ mrgDeleteBy eq) nubbed xs

-- | Symbolic version of 'Data.List.intersectBy', the result would be merged and
-- propagate the mergeable knowledge.
--
-- Can generate O(len(rhs)) cases, and O(len(lhs) * len(rhs)) constraints,
-- assuming the predicate only generates O(1) constraints.
mrgIntersectBy ::
  (MonadUnion u, Mergeable a) =>
  (a -> a -> SymBool) ->
  [a] ->
  [a] ->
  u [a]
mrgIntersectBy _ [] _ = mrgPure []
mrgIntersectBy _ _ [] = mrgPure []
mrgIntersectBy eq xs ys = do
  tl <- mrgIntersectBy eq (tail xs) ys
  mrgIf (symAny (eq (head xs)) ys) (mrgReturn $ head xs : tl) (mrgPure tl)

-- | This function can be very inefficient on large symbolic lists and generate
-- O(2^n) cases. Use with caution.
mrgGroupBy ::
  (MonadUnion u, Mergeable a) =>
  (a -> a -> SymBool) ->
  [a] ->
  u [[a]]
mrgGroupBy _ [] = mrgPure []
mrgGroupBy eq (x : xs) = do
  (ys, zs) <- mrgSpan (eq x) xs
  tl <- mrgGroupBy eq zs
  mrgReturn $ (x : ys) : tl

-- | Symbolic version of 'Data.List.insert', the result would be merged and
-- propagate the mergeable knowledge.
--
-- Can generate 1 case, and O(n^2) sized constraints.
mrgInsert ::
  (MonadUnion m, Mergeable a, SymOrd a) =>
  a ->
  [a] ->
  m [a]
mrgInsert x [] = mrgPure [x]
mrgInsert x ys@(y : ys') =
  mrgIf (x .<= y) (mrgReturn $ x : ys) (mrgFmap (y :) $ mrgInsert x ys')

-- | Symbolic version of 'Data.List.insertBy', the result would be merged and
-- propagate the mergeable knowledge.
--
-- Can generate 1 case, and O(n^2) sized constraints, assuming the ordering
-- function only generates O(1) constraints.
mrgInsertBy ::
  (MonadUnion m, Mergeable a) =>
  (a -> a -> Union Ordering) ->
  a ->
  [a] ->
  m [a]
mrgInsertBy _ x [] = mrgPure [x]
mrgInsertBy cmp x ys@(y : ys') = do
  r <- liftUnion $ cmp x y
  case r of
    GT -> mrgFmap (y :) $ mrgInsertBy cmp x ys'
    _ -> mrgReturn $ x : ys
