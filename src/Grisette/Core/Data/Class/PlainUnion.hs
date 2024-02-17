{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}

module Grisette.Core.Data.Class.PlainUnion
  ( PlainUnion (..),
    pattern Single,
    pattern If,
    simpleMerge,
    (.#),
    onUnion,
    onUnion2,
    onUnion3,
    onUnion4,
  )
where

import Data.Bifunctor (Bifunctor (first))
import Data.Kind (Type)
import Grisette.Core.Data.Class.Function (Function (Arg, Ret, (#)))
import Grisette.Core.Data.Class.LogicalOp
  ( LogicalOp (symNot, (.&&)),
  )
import Grisette.Core.Data.Class.Mergeable (Mergeable)
import Grisette.Core.Data.Class.SimpleMergeable
  ( SimpleMergeable,
    UnionMergeable1,
    mrgIf,
  )
import Grisette.Core.Data.Class.Solvable (Solvable (con))
import Grisette.Core.Data.Class.TryMerge
  ( mrgPure,
    tryMerge,
  )
import Grisette.IR.SymPrim.Data.SymPrim (SymBool)

-- | Plain union containers that can be projected back into single value or
-- if-guarded values.
class (Applicative u, UnionMergeable1 u) => PlainUnion (u :: Type -> Type) where
  -- | Pattern match to extract single values.
  --
  -- >>> singleView (single 1 :: UnionM Integer)
  -- Just 1
  -- >>> singleView (unionIf "a" (single 1) (single 2) :: UnionM Integer)
  -- Nothing
  singleView :: u a -> Maybe a

  -- | Pattern match to extract if values.
  --
  -- >>> ifView (single 1 :: UnionM Integer)
  -- Nothing
  -- >>> ifView (unionIf "a" (single 1) (single 2) :: UnionM Integer)
  -- Just (a,<1>,<2>)
  -- >>> ifView (mrgIf "a" (single 1) (single 2) :: UnionM Integer)
  -- Just (a,{1},{2})
  ifView :: u a -> Maybe (SymBool, u a, u a)

  -- | Convert the union to a guarded list.
  --
  -- >>> toGuardedList (mrgIf "a" (single 1) (mrgIf "b" (single 2) (single 3)) :: UnionM Integer)
  -- [(a,1),((&& b (! a)),2),((! (|| b a)),3)]
  toGuardedList :: u a -> [(SymBool, a)]
  toGuardedList u =
    case (singleView u, ifView u) of
      (Just x, _) -> [(con True, x)]
      (_, Just (c, l, r)) ->
        fmap (first (.&& c)) (toGuardedList l)
          ++ fmap (first (.&& symNot c)) (toGuardedList r)
      _ -> error "Should not happen"

-- | Pattern match to extract single values with 'singleView'.
--
-- >>> case (single 1 :: UnionM Integer) of Single v -> v
-- 1
pattern Single :: (PlainUnion u, Mergeable a) => a -> u a
pattern Single x <-
  (singleView -> Just x)
  where
    Single x = mrgPure x

-- | Pattern match to extract guard values with 'ifView'
-- >>> case (unionIf "a" (single 1) (single 2) :: UnionM Integer) of If c t f -> (c,t,f)
-- (a,<1>,<2>)
pattern If :: (PlainUnion u, Mergeable a) => SymBool -> u a -> u a -> u a
pattern If c t f <-
  (ifView -> Just (c, t, f))
  where
    If c t f = mrgIf c t f

-- | Merge the simply mergeable values in a union, and extract the merged value.
--
-- In the following example, 'unionIf' will not merge the results, and
-- 'simpleMerge' will merge it and extract the single merged value.
--
-- >>> unionIf (ssym "a") (return $ ssym "b") (return $ ssym "c") :: UnionM SymBool
-- <If a b c>
-- >>> simpleMerge $ (unionIf (ssym "a") (return $ ssym "b") (return $ ssym "c") :: UnionM SymBool)
-- (ite a b c)
simpleMerge :: forall u a. (SimpleMergeable a, PlainUnion u) => u a -> a
simpleMerge u = case tryMerge u of
  Single x -> x
  _ -> error "Should not happen"
{-# INLINE simpleMerge #-}

-- | Helper for applying functions on 'UnionLike' and 'SimpleMergeable'.
--
-- >>> let f :: Integer -> UnionM Integer = \x -> mrgIf (ssym "a") (mrgSingle $ x + 1) (mrgSingle $ x + 2)
-- >>> f .# (mrgIf (ssym "b" :: SymBool) (mrgSingle 0) (mrgSingle 2) :: UnionM Integer)
-- {If (&& b a) 1 (If b 2 (If a 3 4))}
(.#) ::
  (Function f, SimpleMergeable (Ret f), PlainUnion u) =>
  f ->
  u (Arg f) ->
  Ret f
(.#) f u = simpleMerge $ fmap (f #) u
{-# INLINE (.#) #-}

infixl 9 .#

-- | Lift a function to work on union values.
--
-- >>> sumU = onUnion sum
-- >>> sumU (unionIf "cond" (return ["a"]) (return ["b","c"]) :: UnionM [SymInteger])
-- (ite cond a (+ b c))
onUnion ::
  forall u a r.
  (SimpleMergeable r, UnionMergeable1 u, PlainUnion u, Mergeable a) =>
  (a -> r) ->
  (u a -> r)
onUnion f = simpleMerge . fmap f . tryMerge

-- | Lift a function to work on union values.
onUnion2 ::
  forall u a b r.
  ( SimpleMergeable r,
    UnionMergeable1 u,
    PlainUnion u,
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
    UnionMergeable1 u,
    PlainUnion u,
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
    UnionMergeable1 u,
    PlainUnion u,
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
