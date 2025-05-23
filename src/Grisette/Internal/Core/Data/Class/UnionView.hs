{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- Module      :   Grisette.Internal.Core.Data.Class.UnionView
-- Copyright   :   (c) Sirui Lu 2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Internal.Core.Data.Class.UnionView
  ( UnionView (..),
    IfViewResult (..),
    pattern Single,
    pattern If,
    simpleMerge,
    symIteMerge,
    (.#),
    onUnion,
    onUnion2,
    onUnion3,
    onUnion4,
    unionToCon,
    liftUnion,
    liftToMonadUnion,
  )
where

import Control.Monad.Identity (Identity (runIdentity))
import Data.Bifunctor (Bifunctor (first))
import Data.Kind (Type)
import Grisette.Internal.Core.Data.Class.AsKey (AsKey1 (AsKey1))
import Grisette.Internal.Core.Data.Class.Function (Function ((#)))
import Grisette.Internal.Core.Data.Class.ITEOp (ITEOp (symIte))
import Grisette.Internal.Core.Data.Class.LogicalOp
  ( LogicalOp (symNot, (.&&)),
  )
import Grisette.Internal.Core.Data.Class.Solvable (Solvable (con))
import Grisette.Internal.Internal.Decl.Core.Data.Class.Mergeable
  ( Mergeable,
  )
import Grisette.Internal.Internal.Decl.Core.Data.Class.SimpleMergeable
  ( SimpleMergeable,
    SymBranching,
    mrgIf,
  )
import Grisette.Internal.Internal.Decl.Core.Data.Class.ToCon (ToCon (toCon))
import Grisette.Internal.Internal.Decl.Core.Data.Class.TryMerge
  ( TryMerge,
    mrgSingle,
    tryMerge,
  )
import Grisette.Internal.SymPrim.SymBool (SymBool)

-- $setup
-- >>> import Grisette.Core
-- >>> import Grisette.SymPrim

-- | The result of 'ifView'.
data IfViewResult u a where
  IfViewResult :: (SymBranching u) => SymBool -> u a -> u a -> IfViewResult u a

instance (Show (u a)) => Show (IfViewResult u a) where
  showsPrec d (IfViewResult c l r) =
    showParen (d > 10) $
      showString "IfViewResult "
        . showsPrec 11 c
        . showString " "
        . showsPrec 11 l
        . showString " "
        . showsPrec 11 r

-- | Containers that can be projected back into single value or if-guarded
-- values. 'Identity' is an instance of this class as we can always project to
-- single value.
class (Applicative u, TryMerge u) => UnionView (u :: Type -> Type) where
  -- | Pattern match to extract single values.
  --
  -- >>> singleView (return 1 :: Union Integer)
  -- Just 1
  -- >>> singleView (mrgIfPropagatedStrategy "a" (return 1) (return 2) :: Union Integer)
  -- Nothing
  singleView :: u a -> Maybe a

  -- | Pattern match to extract if values.
  --
  -- >>> ifView (return 1 :: Union Integer)
  -- Nothing
  -- >>> ifView (mrgIfPropagatedStrategy "a" (return 1) (return 2) :: Union Integer)
  -- Just (IfViewResult a <1> <2>)
  -- >>> ifView (mrgIf "a" (return 1) (return 2) :: Union Integer)
  -- Just (IfViewResult a {1} {2})
  ifView :: u a -> Maybe (IfViewResult u a)

  -- | Convert the union to a guarded list.
  --
  -- >>> toGuardedList (mrgIf "a" (return 1) (mrgIf "b" (return 2) (return 3)) :: Union Integer)
  -- [(a,1),((&& b (! a)),2),((! (|| b a)),3)]
  toGuardedList :: u a -> [(SymBool, a)]
  toGuardedList u =
    case (singleView u, ifView u) of
      (Just x, _) -> [(con True, x)]
      (_, Just (IfViewResult c l r)) ->
        fmap (first (.&& c)) (toGuardedList l)
          ++ fmap (first (.&& symNot c)) (toGuardedList r)
      _ -> error "Should not happen"

  -- | Return all possible values in the union. Drop the path conditions.
  --
  -- >>> overestimateUnionValues (return 1 :: Union Integer)
  -- [1]
  --
  -- >>> overestimateUnionValues (mrgIf "a" (return 1) (return 2) :: Union Integer)
  -- [1,2]
  overestimateUnionValues :: (Mergeable a) => u a -> [a]
  overestimateUnionValues (Single v) = [v]
  overestimateUnionValues (If _ l r) =
    overestimateUnionValues l ++ overestimateUnionValues r

instance UnionView Identity where
  singleView = Just . runIdentity
  ifView _ = Nothing

-- | Pattern match to extract single values with 'singleView'.
--
-- >>> case (return 1 :: Union Integer) of Single v -> v
-- 1
pattern Single :: (UnionView u, Mergeable a) => a -> u a
pattern Single x <-
  (singleView -> Just x)
  where
    Single x = mrgSingle x

-- | Pattern match to extract guard values with 'ifView'
--
-- >>> case (mrgIfPropagatedStrategy "a" (return 1) (return 2) :: Union Integer) of If c t f -> (c,t,f)
-- (a,<1>,<2>)
pattern If :: (UnionView u, Mergeable a) => (SymBranching u) => SymBool -> u a -> u a -> u a
pattern If c t f <-
  (ifView -> Just (IfViewResult c t f))
  where
    If c t f = mrgIf c t f

#if MIN_VERSION_base(4, 16, 4)
{-# COMPLETE Single, If #-}
#endif

-- | Merge the simply mergeable values in a union, and extract the merged value.
--
-- In the following example,
-- 'Grisette.Internal.Core.Data.Class.SimpleMergeable.mrgIfPropagatedStrategy'
-- will not merge the results, and 'simpleMerge' will merge it and extract the
-- single merged value.
--
-- >>> mrgIfPropagatedStrategy (ssym "a") (return $ ssym "b") (return $ ssym "c") :: Union SymBool
-- <If a b c>
-- >>> simpleMerge $ (mrgIfPropagatedStrategy (ssym "a") (return $ ssym "b") (return $ ssym "c") :: Union SymBool)
-- (ite a b c)
simpleMerge :: forall u a. (SimpleMergeable a, UnionView u) => u a -> a
simpleMerge u = case tryMerge u of
  Single x -> x
  _ -> error "Should not happen"
{-# INLINE simpleMerge #-}

-- | Merge the mergeable values in a union, using `symIte`, and extract the
-- merged value.
--
-- The reason why we provide this class is that for some types, we only have
-- `ITEOp` (which may throw an error), and we don't have a `SimpleMergeable`
-- instance. In this case, we can use `symIteMerge` to merge the values.
symIteMerge :: (ITEOp a, Mergeable a, UnionView u) => u a -> a
symIteMerge (Single x) = x
symIteMerge (If cond l r) = symIte cond (symIteMerge l) (symIteMerge r)
{-# INLINE symIteMerge #-}

-- | Helper for applying functions on 'UnionView' and 'SimpleMergeable'.
--
-- >>> let f :: Integer -> Union Integer = \x -> mrgIf (ssym "a") (mrgSingle $ x + 1) (mrgSingle $ x + 2)
-- >>> f .# (mrgIf (ssym "b" :: SymBool) (mrgSingle 0) (mrgSingle 2) :: Union Integer)
-- {If (&& b a) 1 (If b 2 (If a 3 4))}
(.#) ::
  (Function f a r, SimpleMergeable r, UnionView u) =>
  f ->
  u a ->
  r
(.#) f u = simpleMerge $ fmap (f #) u
{-# INLINE (.#) #-}

infixl 9 .#

-- | Lift a function to work on union values.
--
-- >>> sumU = onUnion sum :: Union [SymInteger] -> SymInteger
-- >>> sumU (mrgIfPropagatedStrategy "cond" (return ["a"]) (return ["b","c"]) :: Union [SymInteger])
-- (ite cond a (+ b c))
onUnion ::
  forall u a r.
  (SimpleMergeable r, SymBranching u, UnionView u, Mergeable a) =>
  (a -> r) ->
  (u a -> r)
onUnion f = simpleMerge . fmap f . tryMerge

-- | Lift a function to work on union values.
onUnion2 ::
  forall u a b r.
  ( SimpleMergeable r,
    SymBranching u,
    UnionView u,
    Mergeable a,
    Mergeable b
  ) =>
  (a -> b -> r) ->
  (u a -> u b -> r)
onUnion2 f ua ub = simpleMerge $ f <$> tryMerge ua <*> tryMerge ub

-- | Lift a function to work on union values.
onUnion3 ::
  forall u a b c r.
  ( SimpleMergeable r,
    SymBranching u,
    UnionView u,
    Mergeable a,
    Mergeable b,
    Mergeable c
  ) =>
  (a -> b -> c -> r) ->
  (u a -> u b -> u c -> r)
onUnion3 f ua ub uc =
  simpleMerge $ f <$> tryMerge ua <*> tryMerge ub <*> tryMerge uc

-- | Lift a function to work on union values.
onUnion4 ::
  forall u a b c d r.
  ( SimpleMergeable r,
    SymBranching u,
    UnionView u,
    Mergeable a,
    Mergeable b,
    Mergeable c,
    Mergeable d
  ) =>
  (a -> b -> c -> d -> r) ->
  (u a -> u b -> u c -> u d -> r)
onUnion4 f ua ub uc ud =
  simpleMerge $
    f <$> tryMerge ua <*> tryMerge ub <*> tryMerge uc <*> tryMerge ud

-- | Convert a plain union to concrete values.
--
-- >>> unionToCon (return 1 :: Union SymInteger) :: Maybe Integer
-- Just 1
-- >>> unionToCon (mrgIf "a" (return 1) (return 2) :: Union SymInteger) :: Maybe Integer
-- Nothing
-- >>> unionToCon (return "a" :: Union SymInteger) :: Maybe Integer
-- Nothing
unionToCon :: (ToCon a b, UnionView u) => u a -> Maybe b
unionToCon u =
  case (singleView u, ifView u) of
    (Just x, _) -> toCon x
    (_, Just (IfViewResult c l r)) -> do
      cl <- toCon c
      if cl then unionToCon l else unionToCon r
    _ -> Nothing
{-# INLINE unionToCon #-}

-- | Lift the 'UnionView' value to any Applicative 'SymBranching'.
liftUnion ::
  (Mergeable a, UnionView u, Applicative m, SymBranching m) => u a -> m a
liftUnion u = case u of
  Single x -> mrgSingle x
  If c l r -> mrgIf c (liftUnion l) (liftUnion r)

-- | Alias for 'liftUnion', but for monads.
liftToMonadUnion ::
  (Mergeable a, UnionView u, Monad m, SymBranching m) => u a -> m a
liftToMonadUnion = liftUnion

#if MIN_VERSION_base(4,16,0)
instance (UnionView u) => UnionView (AsKey1 u) where
  singleView (AsKey1 u) = singleView u
  ifView (AsKey1 u) = case ifView u of
    Just (IfViewResult c l r) -> Just (IfViewResult c (AsKey1 l) (AsKey1 r))
    Nothing -> Nothing
  toGuardedList (AsKey1 u) = toGuardedList u
  overestimateUnionValues (AsKey1 u) = overestimateUnionValues u
#endif
