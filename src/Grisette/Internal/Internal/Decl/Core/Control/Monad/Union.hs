{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <&>" #-}

-- |
-- Module      :   Grisette.Internal.Internal.Decl.Core.Control.Monad.Union
-- Copyright   :   (c) Sirui Lu 2021-2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Internal.Internal.Decl.Core.Control.Monad.Union
  ( -- * Union and helpers
    Union (..),
    pattern UAny,
    pattern UMrg,
  )
where

import Data.String (IsString (fromString))
import Grisette.Internal.Core.Data.Class.PlainUnion
  ( PlainUnion (ifView, singleView, toGuardedList),
  )
import Grisette.Internal.Core.Data.Class.Solvable
  ( Solvable (con, conView, sym),
    pattern Con,
  )
import Grisette.Internal.Internal.Decl.Core.Data.Class.Mergeable
  ( Mergeable (rootStrategy, sortIndices),
    Mergeable1 (liftRootStrategy),
    MergingStrategy (SimpleStrategy),
    resolveStrategy,
    rootStrategy1,
  )
import Grisette.Internal.Internal.Decl.Core.Data.Class.SimpleMergeable
  ( SimpleMergeable (mrgIte),
    SimpleMergeable1 (liftMrgIte),
    SymBranching (mrgIfPropagatedStrategy, mrgIfWithStrategy),
    mrgIf,
  )
import Grisette.Internal.Internal.Decl.Core.Data.Class.TryMerge
  ( TryMerge (tryMergeWithStrategy),
    mrgSingle,
    tryMerge,
  )
import Grisette.Internal.Internal.Decl.Core.Data.UnionBase
  ( UnionBase (UnionIf, UnionSingle),
    ifWithLeftMost,
  )

-- $setup
-- >>> import Grisette.Core
-- >>> import Grisette.SymPrim

-- | t'Union' is the 'UnionBase' container (hidden) enhanced with
-- 'MergingStrategy'
-- [knowledge propagation](https://okmij.org/ftp/Haskell/set-monad.html#PE).
--
-- The 'UnionBase' models the underlying semantics evaluation semantics for
-- unsolvable types with the nested if-then-else tree semantics, and can be
-- viewed as the following structure:
--
-- > data UnionBase a
-- >   = UnionSingle a
-- >   | UnionIf bool (Union a) (Union a)
--
-- The 'UnionSingle' constructor is for a single value with the path condition
-- @true@, and the 'UnionIf' constructor is the if operator in an if-then-else
-- tree.
-- For clarity, when printing a t'Union' value, we will omit the 'UnionSingle'
-- constructor. The following two representations has the same semantics.
--
-- > If      c1    (If c11 v11 (If c12 v12 v13))
-- >   (If   c2    v2
-- >               v3)
--
-- \[
--   \left\{\begin{aligned}&t_1&&\mathrm{if}&&c_1\\&v_2&&\mathrm{else if}&&c_2\\&v_3&&\mathrm{otherwise}&&\end{aligned}\right.\hspace{2em}\mathrm{where}\hspace{2em}t_1 = \left\{\begin{aligned}&v_{11}&&\mathrm{if}&&c_{11}\\&v_{12}&&\mathrm{else if}&&c_{12}\\&v_{13}&&\mathrm{otherwise}&&\end{aligned}\right.
-- \]
--
-- To reduce the size of the if-then-else tree to reduce the number of paths to
-- execute, Grisette would merge the branches in a 'UnionBase' container and
-- maintain a representation invariant for them. To perform this merging
-- procedure, Grisette relies on a type class called 'Mergeable' and the
-- merging strategy defined by it.
--
-- 'UnionBase' is a monad, so we can easily write code with the do-notation and
-- monadic combinators. However, the standard monadic operators cannot
-- resolve any extra constraints, including the 'Mergeable' constraint (see
-- [The constrained-monad
-- problem](https://dl.acm.org/doi/10.1145/2500365.2500602)
-- by Sculthorpe et al.).
-- This prevents the standard do-notations to merge the results automatically,
-- and would result in bad performance or very verbose code.
--
-- To reduce this boilerplate, Grisette provide another monad, t'Union' that
-- would try to cache the merging strategy.
-- The t'Union' has a data constructor (hidden intentionally) that maintains
-- an optional 'MergingStrategy' and a 'UnionBase'.
-- When the optional 'MergingStrategy' presents (printed as @{...}@), the
-- 'UnionBase' must have already been merged. When the optional
-- 'MergingStrategy' is absent (printed as @<...>@), the t'UnionBase' does not
-- guarantee to be merged.
-- When used in monadic context, Grisette would try to use this cached merging
-- strategy to merge the result, as the '>>=' operator itself cannot resolve the
-- 'Mergeable' constraint.
--
-- __/Examples:/__
--
-- 'return' cannot resolve the 'Mergeable' constraint.
--
-- >>> return 1 :: Union Integer
-- <1>
--
-- 'Grisette.Lib.Control.Monad.mrgReturn' can resolve the 'Mergeable' constraint.
--
-- >>> import Grisette.Lib.Base
-- >>> mrgReturn 1 :: Union Integer
-- {1}
--
-- 'mrgIfPropagatedStrategy' does not try to 'Mergeable' constraint.
--
-- >>> mrgIfPropagatedStrategy "a" (return 1) (mrgIfPropagatedStrategy "b" (return 1) (return 2)) :: Union Integer
-- <If a 1 (If b 1 2)>
--
-- But 'mrgIfPropagatedStrategy' is able to merge the result if some of the
-- branches are merged and have a cached merging strategy:
--
-- >>> mrgIfPropagatedStrategy "a" (return 1) (mrgIfPropagatedStrategy "b" (mrgReturn 1) (return 2)) :: Union Integer
-- {If (|| a b) 1 2}
--
-- The '>>=' operator uses 'mrgIfPropagatedStrategy' internally. When the final
-- statement in a do-block merges the values, the system can then merge the
-- final result.
--
-- >>> :{
--   do
--     x <- mrgIfPropagatedStrategy (ssym "a") (return 1) (mrgIfPropagatedStrategy (ssym "b") (return 1) (return 2))
--     mrgSingle $ x + 1 :: Union Integer
-- :}
-- {If (|| a b) 2 3}
--
-- Calling a function that merges a result at the last line of a do-notation
-- will also merge the whole block. If you stick to these @mrg*@ combinators and
-- all the functions will merge the results, the whole program can be
-- symbolically evaluated efficiently.
--
-- >>> f x y = mrgIf "c" x y :: Union Integer
-- >>> :{
--   do
--     x <- mrgIfPropagatedStrategy (ssym "a") (return 1) (mrgIfPropagatedStrategy (ssym "b") (return 1) (return 2))
--     f x (x + 1)
-- :}
-- {If (&& c (|| a b)) 1 (If (|| a (|| b c)) 2 3)}
--
-- In "Grisette.Lib.Base", "Grisette.Lib.Mtl", we also provided more @mrg*@
-- variants of other combinators. You should stick to these combinators to
-- ensure efficient merging by Grisette.
data Union a = Union
  { unionMergingStrategy :: Maybe (MergingStrategy a),
    unionBase :: UnionBase a
  }

-- | Pattern synonym for Union with no MergingStrategy (backwards compatibility)
pattern UAny :: UnionBase a -> Union a
pattern UAny u <- Union Nothing u
  where
    UAny u = Union Nothing u

-- | Pattern synonym for Union with MergingStrategy (backwards compatibility)
pattern UMrg :: MergingStrategy a -> UnionBase a -> Union a
pattern UMrg s u <- Union (Just s) u
  where
    UMrg s u = Union (Just s) u

#if MIN_VERSION_base(4, 16, 4)
{-# COMPLETE UAny, UMrg #-}
#endif

instance Functor Union where
  fmap f fa = fa >>= return . f
  {-# INLINE fmap #-}

instance Applicative Union where
  pure = UAny . pure
  {-# INLINE pure #-}
  f <*> a = f >>= (\xf -> a >>= (return . xf))
  {-# INLINE (<*>) #-}

bindUnionBase :: UnionBase a -> (a -> Union b) -> Union b
bindUnionBase (UnionSingle a') f' = f' a'
bindUnionBase (UnionIf _ _ cond ifTrue ifFalse) f' =
  mrgIfPropagatedStrategy
    cond
    (bindUnionBase ifTrue f')
    (bindUnionBase ifFalse f')
{-# INLINE bindUnionBase #-}

instance Monad Union where
  a >>= f = bindUnionBase (unionBase a) f
  {-# INLINE (>>=) #-}

instance TryMerge Union where
  tryMergeWithStrategy _ m@(Union Just {} _) = m
  tryMergeWithStrategy s (Union Nothing u) =
    Union (Just s) $ tryMergeWithStrategy s u
  {-# INLINE tryMergeWithStrategy #-}

instance (IsString a, Mergeable a) => IsString (Union a) where
  fromString = mrgSingle . fromString

instance (Solvable c t, Mergeable t) => Solvable c (Union t) where
  con = mrgSingle . con
  {-# INLINE con #-}
  sym = mrgSingle . sym
  {-# INLINE sym #-}
  conView v = do
    c <- singleView $ tryMerge v
    conView c
  {-# INLINE conView #-}

instance (Mergeable a) => Mergeable (Union a) where
  rootStrategy = rootStrategy1
  {-# INLINE rootStrategy #-}
  sortIndices = fst . resolveStrategy rootStrategy . snd . head . toGuardedList

instance (Mergeable a) => SimpleMergeable (Union a) where
  mrgIte = mrgIf
  {-# INLINE mrgIte #-}

instance Mergeable1 Union where
  liftRootStrategy m = SimpleStrategy $ mrgIfWithStrategy m
  {-# INLINE liftRootStrategy #-}

instance SimpleMergeable1 Union where
  liftMrgIte m = mrgIfWithStrategy (SimpleStrategy m)
  {-# INLINE liftMrgIte #-}

instance SymBranching Union where
  mrgIfWithStrategy s (Con c) l r =
    if c then tryMergeWithStrategy s l else tryMergeWithStrategy s r
  mrgIfWithStrategy s cond l r =
    Union (Just s) $
      mrgIfWithStrategy
        s
        cond
        (unionBase l)
        (unionBase r)
  {-# INLINE mrgIfWithStrategy #-}
  mrgIfPropagatedStrategy cond (Union Nothing t) (Union Nothing f) =
    Union Nothing $ ifWithLeftMost False cond t f
  mrgIfPropagatedStrategy cond t@(Union (Just m) _) f = mrgIfWithStrategy m cond t f
  mrgIfPropagatedStrategy cond t f@(Union (Just m) _) = mrgIfWithStrategy m cond t f
  {-# INLINE mrgIfPropagatedStrategy #-}

instance PlainUnion Union where
  singleView = singleView . unionBase
  {-# INLINE singleView #-}
  ifView (Union Nothing u) = case ifView u of
    Just (c, t, f) -> Just (c, Union Nothing t, Union Nothing f)
    Nothing -> Nothing
  ifView (Union (Just m) u) = case ifView u of
    Just (c, t, f) -> Just (c, Union (Just m) t, Union (Just m) f)
    Nothing -> Nothing
  {-# INLINE ifView #-}
