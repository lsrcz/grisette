{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -fno-cse #-}

{-# HLINT ignore "Use <&>" #-}

-- {-# OPTIONS_GHC -fno-full-laziness #-}

-- |
-- Module      :   Grisette.Core.Control.Monad.UnionMBase
-- Copyright   :   (c) Sirui Lu 2021-2022
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Core.Control.Monad.UnionMBase
  ( -- * Note for the examples

    --

    -- | This module does not contain the implementation for solvable (see "Grisette.Core#solvable")
    -- types, and the examples in this module rely on the implementations in
    -- the [grisette-symir](https://hackage.haskell.org/package/grisette-symir) package.

    -- * UnionMBase and helpers
    UnionMBase (..),
    liftToGMonadUnion,
    underlyingUnion,
    isMerged,
    (#~),
    IsConcrete,
  )
where

import Control.DeepSeq
import Control.Monad.Identity (Identity (..))
import Data.Functor.Classes
import qualified Data.HashMap.Lazy as HML
import Data.Hashable
import Data.IORef
import Data.String
import GHC.IO hiding (evaluate)
import Grisette.Core.Control.Monad.CBMCExcept
import Grisette.Core.Control.Monad.Union
import Grisette.Core.Data.Class.Bool
import Grisette.Core.Data.Class.Evaluate
import Grisette.Core.Data.Class.ExtractSymbolics
import Grisette.Core.Data.Class.Function
import Grisette.Core.Data.Class.GenSym
import Grisette.Core.Data.Class.Mergeable
import Grisette.Core.Data.Class.SOrd
import Grisette.Core.Data.Class.SimpleMergeable
import Grisette.Core.Data.Class.Solvable
import Grisette.Core.Data.Class.Solver
import Grisette.Core.Data.Class.ToCon
import Grisette.Core.Data.Class.ToSym
import Grisette.Core.Data.UnionBase
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Syntax.Compat (unTypeSplice)

-- $setup
-- >>> import Grisette.Core
-- >>> import Grisette.IR.SymPrim
-- >>> :set -XScopedTypeVariables

-- | 'UnionMBase' is the 'UnionBase' container (hidden) enhanced with
-- 'GMergingStrategy'
-- [knowledge propagation](https://okmij.org/ftp/Haskell/set-monad.html#PE).
--
-- The 'UnionBase' models the underlying semantics evaluation semantics for
-- unsolvable types with the nested if-then-else tree semantics, and can be
-- viewed as the following structure:
--
-- > data UnionBase bool a
-- >   = Single a
-- >   | If bool (UnionBase bool a) (UnionBase bool a)
--
-- The 'Single' constructor is for a single value with the path condition
-- @true@, and the 'If' constructor is the if operator in an if-then-else
-- tree. The following two representations has the same semantics.
--
-- > If      c1    (If c11 (Single v11) (If c12 (Single v12) (Single v13)))
-- >   (If   c2    (Single v2)
-- >               (Single v3))
--
-- \[
--   \left\{\begin{aligned}&t_1&&\mathrm{if}&&c_1\\&v_2&&\mathrm{else if}&&c_2\\&v_3&&\mathrm{otherwise}&&\end{aligned}\right.\hspace{2em}\mathrm{where}\hspace{2em}t_1 = \left\{\begin{aligned}&v_{11}&&\mathrm{if}&&c_{11}\\&v_{12}&&\mathrm{else if}&&c_{12}\\&v_{13}&&\mathrm{otherwise}&&\end{aligned}\right.
-- \]
--
-- To reduce the size of the if-then-else tree to reduce the number of paths to
-- execute, Grisette would merge the branches in a 'UnionBase' container and
-- maintain a representation invariant for them. To perform this merging
-- procedure, Grisette relies on a type class called 'GMergeable' and the
-- merging strategy defined by it.
--
-- 'UnionBase' is a monad, so we can easily write code with the do-notation and
-- monadic combinators. However, the standard monadic operators cannot
-- resolve any extra constraints, including the 'GMergeable' constraint (see
-- [The constrained-monad
-- problem](https://dl.acm.org/doi/10.1145/2500365.2500602)
-- by Sculthorpe et al.).
-- This prevents the standard do-notations to merge the results automatically,
-- and would result in bad performance or very verbose code.
--
-- To reduce this boilerplate, Grisette provide another monad, 'UnionMBase' that
-- would try to cache the merging strategy.
-- The 'UnionMBase' has two data constructors (hidden intentionally), 'UAny' and 'UMrg'.
-- The 'UAny' data constructor wraps an arbitrary (probably
-- unmerged) 'UnionBase'. It is constructed when no 'GMergeable' knowledge is
-- available (for example, when constructed with Haskell\'s 'return').
-- The 'UMrg' data constructor wraps a merged 'UnionMBase' along with the
-- 'GMergeable' constraint. This constraint can be propagated to the contexts
-- without 'GMergeable' knowledge, and helps the system to merge the resulting
-- 'UnionBase'.
--
-- __/Examples:/__
--
-- 'return' cannot resolve the 'GMergeable' constraint.
--
-- >>> return 1 :: UnionM Integer
-- UAny (Single 1)
--
-- 'Grisette.Lib.Control.Monad.mrgReturn' can resolve the 'GMergeable' constraint.
--
-- >>> import Grisette.Lib.Base
-- >>> mrgReturn 1 :: UnionM Integer
-- UMrg (Single 1)
--
-- 'unionIf' cannot resolve the 'GMergeable' constraint.
--
-- >>> unionIf "a" (return 1) (unionIf "b" (return 1) (return 2)) :: UnionM Integer
-- UAny (If a (Single 1) (If b (Single 1) (Single 2)))
--
-- But 'unionIf' is able to merge the result if some of the branches are merged:
--
-- >>> unionIf "a" (return 1) (unionIf "b" (mrgReturn 1) (return 2)) :: UnionM Integer
-- UMrg (If (|| a b) (Single 1) (Single 2))
--
-- The '>>=' operator uses 'unionIf' internally. When the final statement in a do-block
-- merges the values, the system can then merge the final result.
--
-- >>> :{
--   do
--     x <- unionIf (ssymb "a") (return 1) (unionIf (ssymb "b") (return 1) (return 2))
--     mrgSingle $ x + 1 :: UnionM Integer
-- :}
-- UMrg (If (|| a b) (Single 2) (Single 3))
--
-- Calling a function that merges a result at the last line of a do-notation
-- will also merge the whole block. If you stick to these @mrg*@ combinators and
-- all the functions will merge the results, the whole program can be
-- symbolically evaluated efficiently.
--
-- >>> f x y = mrgIf "c" x y
-- >>> :{
--   do
--     x <- unionIf (ssymb "a") (return 1) (unionIf (ssymb "b") (return 1) (return 2))
--     f x (x + 1) :: UnionM Integer
-- :}
-- UMrg (If (&& c (|| a b)) (Single 1) (If (|| a (|| b c)) (Single 2) (Single 3)))
--
-- In "Grisette.Lib.Base", "Grisette.Lib.Mtl", we also provided more @mrg*@
-- variants of other combinators. You should stick to these combinators to
-- ensure efficient merging by Grisette.
data UnionMBase bool a where
  -- | 'UnionMBase' with no 'Mergeable' knowledge.
  UAny ::
    -- | (Possibly) cached merging result.
    IORef (Either (UnionBase bool a) (UnionMBase bool a)) ->
    -- | Original 'UnionBase'.
    UnionBase bool a ->
    UnionMBase bool a
  -- | 'UnionMBase' with 'Mergeable' knowledge.
  UMrg ::
    -- | Cached merging strategy.
    GMergingStrategy bool a ->
    -- | Merged UnionBase
    UnionBase bool a ->
    UnionMBase bool a

instance (NFData bool, NFData a) => NFData (UnionMBase bool a) where
  rnf = rnf1

instance (NFData bool) => NFData1 (UnionMBase bool) where
  liftRnf = liftRnf2 rnf

instance NFData2 UnionMBase where
  liftRnf2 _bool _a (UAny i m) = rnf i `seq` liftRnf2 _bool _a m
  liftRnf2 _bool _a (UMrg _ m) = liftRnf2 _bool _a m

instance (Lift bool, Lift a) => Lift (UnionMBase bool a) where
  liftTyped (UAny _ v) = [||freshUAny v||]
  liftTyped (UMrg _ v) = [||freshUAny v||]
  lift = unTypeSplice . liftTyped

freshUAny :: UnionBase bool a -> UnionMBase bool a
freshUAny v = UAny (unsafeDupablePerformIO $ newIORef $ Left v) v
{-# NOINLINE freshUAny #-}

instance (Show a, Show bool) => (Show (UnionMBase bool a)) where
  showsPrec = showsPrec1

instance (Show b) => Show1 (UnionMBase b) where
  liftShowsPrec sp sl i (UAny _ a) = showsUnaryWith (liftShowsPrec sp sl) "UAny" i a
  liftShowsPrec sp sl i (UMrg _ a) = showsUnaryWith (liftShowsPrec sp sl) "UMrg" i a

-- | Extract the underlying UnionBase. May be unmerged.
underlyingUnion :: UnionMBase bool a -> UnionBase bool a
underlyingUnion (UAny _ a) = a
underlyingUnion (UMrg _ a) = a
{-# INLINE underlyingUnion #-}

-- | Check if a UnionMBase is already merged.
isMerged :: UnionMBase bool a -> Bool
isMerged UAny {} = False
isMerged UMrg {} = True
{-# INLINE isMerged #-}

instance SymBoolOp bool => GUnionPrjOp bool (UnionMBase bool) where
  singleView = singleView . underlyingUnion
  {-# INLINE singleView #-}
  ifView (UAny _ u) = case ifView u of
    Just (c, t, f) -> Just (c, freshUAny t, freshUAny f)
    Nothing -> Nothing
  ifView (UMrg m u) = case ifView u of
    Just (c, t, f) -> Just (c, UMrg m t, UMrg m f)
    Nothing -> Nothing
  {-# INLINE ifView #-}
  leftMost = leftMost . underlyingUnion
  {-# INLINE leftMost #-}

instance (SymBoolOp bool) => Functor (UnionMBase bool) where
  fmap f fa = fa >>= return . f
  {-# INLINE fmap #-}

instance (SymBoolOp bool) => Applicative (UnionMBase bool) where
  pure = single
  {-# INLINE pure #-}
  f <*> a = f >>= (\xf -> a >>= (return . xf))
  {-# INLINE (<*>) #-}

bindUnion :: SymBoolOp bool => UnionBase bool a -> (a -> UnionMBase bool b) -> UnionMBase bool b
bindUnion (Single a') f' = f' a'
bindUnion (If _ _ cond ifTrue ifFalse) f' =
  unionIf cond (bindUnion ifTrue f') (bindUnion ifFalse f')
{-# INLINE bindUnion #-}

instance (SymBoolOp bool) => Monad (UnionMBase bool) where
  a >>= f = bindUnion (underlyingUnion a) f
  {-# INLINE (>>=) #-}

instance (SymBoolOp bool, GMergeable bool a) => GMergeable bool (UnionMBase bool a) where
  gmergingStrategy = SimpleStrategy $ \cond t f -> unionIf cond t f >>= mrgSingle
  {-# INLINE gmergingStrategy #-}

instance (SymBoolOp bool, GMergeable bool a) => GSimpleMergeable bool (UnionMBase bool a) where
  gmrgIte = mrgIf
  {-# INLINE gmrgIte #-}

instance (SymBoolOp bool) => GMergeable1 bool (UnionMBase bool) where
  liftGMergingStrategy m = SimpleStrategy $ \cond t f -> unionIf cond t f >>= (UMrg m . Single)
  {-# INLINE liftGMergingStrategy #-}

instance SymBoolOp bool => GSimpleMergeable1 bool (UnionMBase bool) where
  liftGMrgIte m = mrgIfWithStrategy (SimpleStrategy m)
  {-# INLINE liftGMrgIte #-}

instance SymBoolOp bool => GUnionLike bool (UnionMBase bool) where
  mergeWithStrategy _ m@(UMrg _ _) = m
  mergeWithStrategy s (UAny ref u) = unsafeDupablePerformIO $
    atomicModifyIORef' ref $ \case
      x@(Right r) -> (x, r)
      Left _ -> (Right r, r)
        where
          !r = UMrg s $ fullReconstruct s u -- m >>= mrgSingle
  {-# NOINLINE mergeWithStrategy #-}
  mrgIfWithStrategy s (Conc c) l r = if c then mergeWithStrategy s l else mergeWithStrategy s r
  mrgIfWithStrategy s cond l r =
    mergeWithStrategy s $ unionIf cond l r
  {-# INLINE mrgIfWithStrategy #-}
  single = freshUAny . single
  {-# INLINE single #-}
  unionIf cond (UAny _ a) (UAny _ b) = freshUAny $ unionIf cond a b
  unionIf cond (UMrg m a) (UAny _ b) = UMrg m $ ifWithStrategy m cond a b
  unionIf cond a (UMrg m b) = UMrg m $ ifWithStrategy m cond (underlyingUnion a) b
  {-# INLINE unionIf #-}

instance (SymBoolOp bool, GSEq bool a) => GSEq bool (UnionMBase bool a) where
  x `gsymeq` y = getSingle $ do
    x1 <- x
    y1 <- y
    mrgSingle $ x1 `gsymeq` y1

-- | Lift the 'UnionMBase' to any 'GMonadUnion'.
liftToGMonadUnion :: (SymBoolOp bool, GMergeable bool a, GMonadUnion bool u) => UnionMBase bool a -> u a
liftToGMonadUnion u = go (underlyingUnion u)
  where
    go (Single v) = mrgSingle v
    go (If _ _ c t f) = mrgIf c (go t) (go f)

instance (SymBoolOp bool, GSOrd bool a) => GSOrd bool (UnionMBase bool a) where
  x `gsymle` y = getSingle $ do
    x1 <- x
    y1 <- y
    mrgSingle $ x1 `gsymle` y1
  x `gsymlt` y = getSingle $ do
    x1 <- x
    y1 <- y
    mrgSingle $ x1 `gsymlt` y1
  x `gsymge` y = getSingle $ do
    x1 <- x
    y1 <- y
    mrgSingle $ x1 `gsymge` y1
  x `gsymgt` y = getSingle $ do
    x1 <- x
    y1 <- y
    mrgSingle $ x1 `gsymgt` y1
  x `gsymCompare` y = liftToGMonadUnion $ do
    x1 <- x
    y1 <- y
    x1 `gsymCompare` y1

instance {-# OVERLAPPABLE #-} (SymBoolOp bool, ToSym a b, GMergeable bool b) => ToSym a (UnionMBase bool b) where
  toSym = mrgSingle . toSym

instance {-# OVERLAPPING #-} (SymBoolOp bool, ToSym a b, GMergeable bool b) => ToSym (UnionMBase bool a) (UnionMBase bool b) where
  toSym = merge . fmap toSym

instance {-# OVERLAPPING #-} (SymBoolOp bool, ToSym a b, GMergeable bool b) => ToSym (Identity a) (UnionMBase bool b) where
  toSym (Identity x) = toSym x

instance (SymBoolOp bool, ToCon a b) => ToCon (UnionMBase bool a) b where
  toCon v = go $ underlyingUnion v
    where
      go (Single x) = toCon x
      go _ = Nothing

instance (SymBoolOp bool, GMergeable bool a, GEvaluateSym model a, GEvaluateSym model bool) => GEvaluateSym model (UnionMBase bool a) where
  gevaluateSym fillDefault model x = go $ underlyingUnion x
    where
      go :: UnionBase bool a -> UnionMBase bool a
      go (Single v) = mrgSingle $ gevaluateSym fillDefault model v
      go (If _ _ cond t f) =
        mrgIf
          (gevaluateSym fillDefault model cond)
          (go t)
          (go f)

instance
  (Monoid symbolSet, SymBoolOp bool, GExtractSymbolics symbolSet a, GExtractSymbolics symbolSet bool) =>
  GExtractSymbolics symbolSet (UnionMBase bool a)
  where
  gextractSymbolics v = go $ underlyingUnion v
    where
      go (Single x) = gextractSymbolics x
      go (If _ _ cond t f) = gextractSymbolics cond <> go t <> go f

instance (Hashable bool, Hashable a) => Hashable (UnionMBase bool a) where
  s `hashWithSalt` (UAny _ u) = s `hashWithSalt` (0 :: Int) `hashWithSalt` u
  s `hashWithSalt` (UMrg _ u) = s `hashWithSalt` (1 :: Int) `hashWithSalt` u

instance (Eq bool, Eq a) => Eq (UnionMBase bool a) where
  UAny _ l == UAny _ r = l == r
  UMrg _ l == UMrg _ r = l == r
  _ == _ = False

instance (Eq bool) => Eq1 (UnionMBase bool) where
  liftEq e l r = liftEq e (underlyingUnion l) (underlyingUnion r)

instance (SymBoolOp bool, Num a, GMergeable bool a) => Num (UnionMBase bool a) where
  fromInteger = mrgSingle . fromInteger
  negate x = x >>= (mrgSingle . negate)
  x + y = x >>= \x1 -> y >>= \y1 -> mrgSingle $ x1 + y1
  x - y = x >>= \x1 -> y >>= \y1 -> mrgSingle $ x1 - y1
  x * y = x >>= \x1 -> y >>= \y1 -> mrgSingle $ x1 * y1
  abs x = x >>= mrgSingle . abs
  signum x = x >>= mrgSingle . signum

instance (SymBoolOp bool, ITEOp bool a, GMergeable bool a) => ITEOp bool (UnionMBase bool a) where
  ites = mrgIf

instance (SymBoolOp bool, LogicalOp a, GMergeable bool a) => LogicalOp (UnionMBase bool a) where
  a ||~ b = do
    a1 <- a
    b1 <- b
    mrgSingle $ a1 ||~ b1
  a &&~ b = do
    a1 <- a
    b1 <- b
    mrgSingle $ a1 &&~ b1
  nots x = do
    x1 <- x
    mrgSingle $ nots x1
  xors a b = do
    a1 <- a
    b1 <- b
    mrgSingle $ a1 `xors` b1
  implies a b = do
    a1 <- a
    b1 <- b
    mrgSingle $ a1 `implies` b1

instance (SymBoolOp bool, Solvable c t, GMergeable bool t) => Solvable c (UnionMBase bool t) where
  conc = mrgSingle . conc
  {-# INLINE conc #-}
  ssymb = mrgSingle . ssymb
  {-# INLINE ssymb #-}
  isymb i s = mrgSingle $ isymb i s
  {-# INLINE isymb #-}
  sinfosymb s info = mrgSingle $ sinfosymb s info
  {-# INLINE sinfosymb #-}
  iinfosymb i s info = mrgSingle $ iinfosymb i s info
  {-# INLINE iinfosymb #-}
  concView v = do
    c <- singleView v
    concView c
  {-# INLINE concView #-}

instance
  (SymBoolOp bool, Function f, GMergeable bool f, GMergeable bool a, Ret f ~ a) =>
  Function (UnionMBase bool f)
  where
  type Arg (UnionMBase bool f) = Arg f
  type Ret (UnionMBase bool f) = UnionMBase bool (Ret f)
  f # a = do
    f1 <- f
    mrgSingle $ f1 # a

instance (SymBoolOp bool, IsString a, GMergeable bool a) => IsString (UnionMBase bool a) where
  fromString = mrgSingle . fromString

{-
foldMapUnion :: (Monoid m) => (a -> m) -> UnionBase bool a -> m
foldMapUnion f (Single v) = f v
foldMapUnion f (If _ _ _ l r) = foldMapUnion f l <> foldMapUnion f r

instance Foldable (UnionMBase bool) where
  foldMap f u = foldMapUnion f (underlyingUnion u)

sequenceAUnion :: (Applicative m, SymBoolOp bool) => UnionBase bool (m a) -> m (UnionBase bool a)
sequenceAUnion (Single v) = single <$> v
sequenceAUnion (If _ _ cond l r) = unionIf cond <$> sequenceAUnion l <*> sequenceAUnion r

instance (SymBoolOp bool) => Traversable (UnionMBase bool) where
  sequenceA u = freshUAny <$> sequenceAUnion (underlyingUnion u)
  -}

-- GenSym
instance (SymBoolOp bool, GenSym bool spec a, GMergeable bool a) => GenSym bool spec (UnionMBase bool a)

instance (SymBoolOp bool, GenSym bool spec a) => GenSymSimple spec (UnionMBase bool a) where
  simpleFresh spec = do
    res <- fresh spec
    if not (isMerged res) then error "Not merged" else return res

instance
  (SymBoolOp bool, GenSym bool a a, GenSymSimple () bool, GMergeable bool a) =>
  GenSym bool (UnionMBase bool a) a
  where
  fresh spec = go (underlyingUnion $ merge spec)
    where
      go (Single x) = fresh x
      go (If _ _ _ t f) = mrgIf <$> simpleFresh () <*> go t <*> go f

-- Concrete Key HashMaps

-- | Tag for concrete types.
-- Useful for specifying the merge strategy for some parametrized types where we should have different
-- merge strategy for symbolic and concrete ones.
class (Eq t, Ord t, Hashable t) => IsConcrete t

instance IsConcrete Bool

instance IsConcrete Integer

instance (SymBoolOp bool, IsConcrete k, GMergeable bool t) => GMergeable bool (HML.HashMap k (UnionMBase bool (Maybe t))) where
  gmergingStrategy = SimpleStrategy gmrgIte

instance (SymBoolOp bool, IsConcrete k, GMergeable bool t) => GSimpleMergeable bool (HML.HashMap k (UnionMBase bool (Maybe t))) where
  gmrgIte cond l r =
    HML.unionWith (mrgIf cond) ul ur
    where
      ul =
        foldr
          ( \k m -> case HML.lookup k m of
              Nothing -> HML.insert k (mrgSingle Nothing) m
              _ -> m
          )
          l
          (HML.keys r)
      ur =
        foldr
          ( \k m -> case HML.lookup k m of
              Nothing -> HML.insert k (mrgSingle Nothing) m
              _ -> m
          )
          r
          (HML.keys l)

instance UnionWithExcept (UnionMBase bool (Either e v)) (UnionMBase bool) e v where
  extractUnionExcept = id

instance SymBoolOp bool => UnionWithExcept (UnionMBase bool (CBMCEither e v)) (UnionMBase bool) e v where
  extractUnionExcept = fmap runCBMCEither
