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
    unionBase,
  )
where

import Data.String (IsString (fromString))
import Grisette.Internal.Core.Data.Class.PlainUnion
  ( PlainUnion (ifView, singleView),
  )
import Grisette.Internal.Core.Data.Class.Solvable
  ( Solvable (con, conView, sym),
    pattern Con,
  )
import Grisette.Internal.Internal.Decl.Core.Data.Class.Mergeable
  ( Mergeable (rootStrategy),
    Mergeable1 (liftRootStrategy),
    MergingStrategy (SimpleStrategy),
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

-- | 'Union' is the 'UnionBase' container (hidden) enhanced with
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
-- For clarity, when printing a 'Union' value, we will omit the 'UnionSingle'
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
-- To reduce this boilerplate, Grisette provide another monad, 'Union' that
-- would try to cache the merging strategy.
-- The 'Union' has two data constructors (hidden intentionally), 'UAny' and
-- 'UMrg'. The 'UAny' data constructor (printed as @<@@...@@>@) wraps an
-- arbitrary (probably unmerged) 'UnionBase'. It is constructed when no
-- 'Mergeable' knowledge is available (for example, when constructed with
-- Haskell\'s 'return'). The 'UMrg' data constructor (printed as @{...}@) wraps
-- a merged 'UnionBase' along with the 'Mergeable' constraint. This constraint
-- can be propagated to the contexts without 'Mergeable' knowledge, and helps
-- the system to merge the resulting 'UnionBase'.
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
data Union a where
  -- | 'Union' with no 'Mergeable' knowledge.
  UAny ::
    -- | Original 'UnionBase'.
    UnionBase a ->
    Union a
  -- | 'Union' with 'Mergeable' knowledge.
  UMrg ::
    -- | Cached merging strategy.
    MergingStrategy a ->
    -- | Merged 'UnionBase'
    UnionBase a ->
    Union a

-- | Extract the underlying Union. May be unmerged.
unionBase :: Union a -> UnionBase a
unionBase (UAny a) = a
unionBase (UMrg _ a) = a
{-# INLINE unionBase #-}

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
  tryMergeWithStrategy _ m@(UMrg _ _) = m
  tryMergeWithStrategy s (UAny u) = UMrg s $ tryMergeWithStrategy s u
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
    UMrg s $
      mrgIfWithStrategy
        s
        cond
        (unionBase l)
        (unionBase r)
  {-# INLINE mrgIfWithStrategy #-}
  mrgIfPropagatedStrategy cond (UAny t) (UAny f) =
    UAny $ ifWithLeftMost False cond t f
  mrgIfPropagatedStrategy cond t@(UMrg m _) f = mrgIfWithStrategy m cond t f
  mrgIfPropagatedStrategy cond t f@(UMrg m _) = mrgIfWithStrategy m cond t f
  {-# INLINE mrgIfPropagatedStrategy #-}

instance PlainUnion Union where
  singleView = singleView . unionBase
  {-# INLINE singleView #-}
  ifView (UAny u) = case ifView u of
    Just (c, t, f) -> Just (c, UAny t, UAny f)
    Nothing -> Nothing
  ifView (UMrg m u) = case ifView u of
    Just (c, t, f) -> Just (c, UMrg m t, UMrg m f)
    Nothing -> Nothing
  {-# INLINE ifView #-}
