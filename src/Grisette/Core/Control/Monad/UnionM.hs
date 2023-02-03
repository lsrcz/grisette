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
-- Module      :   Grisette.Core.Control.Monad.UnionM
-- Copyright   :   (c) Sirui Lu 2021-2023
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Core.Control.Monad.UnionM
  ( -- * UnionM and helpers
    UnionM (..),
    liftToMonadUnion,
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
import Grisette.Core.Data.Class.Substitute
import Grisette.Core.Data.Class.ToCon
import Grisette.Core.Data.Class.ToSym
import Grisette.Core.Data.Union
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Syntax.Compat (unTypeSplice)

-- $setup
-- >>> import Grisette.Core
-- >>> import Grisette.IR.SymPrim
-- >>> :set -XScopedTypeVariables

-- | 'UnionM' is the 'Union' container (hidden) enhanced with
-- 'MergingStrategy'
-- [knowledge propagation](https://okmij.org/ftp/Haskell/set-monad.html#PE).
--
-- The 'Union' models the underlying semantics evaluation semantics for
-- unsolvable types with the nested if-then-else tree semantics, and can be
-- viewed as the following structure:
--
-- > data Union a
-- >   = Single a
-- >   | If bool (Union a) (Union a)
--
-- The 'Single' constructor is for a single value with the path condition
-- @true@, and the 'If' constructor is the if operator in an if-then-else
-- tree.
-- For clarity, when printing a 'UnionM' value, we will omit the 'Single'
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
-- execute, Grisette would merge the branches in a 'Union' container and
-- maintain a representation invariant for them. To perform this merging
-- procedure, Grisette relies on a type class called 'Mergeable' and the
-- merging strategy defined by it.
--
-- 'Union' is a monad, so we can easily write code with the do-notation and
-- monadic combinators. However, the standard monadic operators cannot
-- resolve any extra constraints, including the 'Mergeable' constraint (see
-- [The constrained-monad
-- problem](https://dl.acm.org/doi/10.1145/2500365.2500602)
-- by Sculthorpe et al.).
-- This prevents the standard do-notations to merge the results automatically,
-- and would result in bad performance or very verbose code.
--
-- To reduce this boilerplate, Grisette provide another monad, 'UnionM' that
-- would try to cache the merging strategy.
-- The 'UnionM' has two data constructors (hidden intentionally), 'UAny' and 'UMrg'.
-- The 'UAny' data constructor (printed as @<@@...@@>@) wraps an arbitrary (probably
-- unmerged) 'Union'. It is constructed when no 'Mergeable' knowledge is
-- available (for example, when constructed with Haskell\'s 'return').
-- The 'UMrg' data constructor (printed as @{...}@) wraps a merged 'UnionM' along with the
-- 'Mergeable' constraint. This constraint can be propagated to the contexts
-- without 'Mergeable' knowledge, and helps the system to merge the resulting
-- 'Union'.
--
-- __/Examples:/__
--
-- 'return' cannot resolve the 'Mergeable' constraint.
--
-- >>> return 1 :: UnionM Integer
-- <1>
--
-- 'Grisette.Lib.Control.Monad.mrgReturn' can resolve the 'Mergeable' constraint.
--
-- >>> import Grisette.Lib.Base
-- >>> mrgReturn 1 :: UnionM Integer
-- {1}
--
-- 'unionIf' cannot resolve the 'Mergeable' constraint.
--
-- >>> unionIf "a" (return 1) (unionIf "b" (return 1) (return 2)) :: UnionM Integer
-- <If a 1 (If b 1 2)>
--
-- But 'unionIf' is able to merge the result if some of the branches are merged:
--
-- >>> unionIf "a" (return 1) (unionIf "b" (mrgReturn 1) (return 2)) :: UnionM Integer
-- {If (|| a b) 1 2}
--
-- The '>>=' operator uses 'unionIf' internally. When the final statement in a do-block
-- merges the values, the system can then merge the final result.
--
-- >>> :{
--   do
--     x <- unionIf (ssym "a") (return 1) (unionIf (ssym "b") (return 1) (return 2))
--     mrgSingle $ x + 1 :: UnionM Integer
-- :}
-- {If (|| a b) 2 3}
--
-- Calling a function that merges a result at the last line of a do-notation
-- will also merge the whole block. If you stick to these @mrg*@ combinators and
-- all the functions will merge the results, the whole program can be
-- symbolically evaluated efficiently.
--
-- >>> f x y = mrgIf "c" x y
-- >>> :{
--   do
--     x <- unionIf (ssym "a") (return 1) (unionIf (ssym "b") (return 1) (return 2))
--     f x (x + 1) :: UnionM Integer
-- :}
-- {If (&& c (|| a b)) 1 (If (|| a (|| b c)) 2 3)}
--
-- In "Grisette.Lib.Base", "Grisette.Lib.Mtl", we also provided more @mrg*@
-- variants of other combinators. You should stick to these combinators to
-- ensure efficient merging by Grisette.
data UnionM a where
  -- | 'UnionM' with no 'Mergeable' knowledge.
  UAny ::
    -- | (Possibly) cached merging result.
    IORef (Either (Union a) (UnionM a)) ->
    -- | Original 'Union'.
    Union a ->
    UnionM a
  -- | 'UnionM' with 'Mergeable' knowledge.
  UMrg ::
    -- | Cached merging strategy.
    MergingStrategy a ->
    -- | Merged Union
    Union a ->
    UnionM a

instance (NFData a) => NFData (UnionM a) where
  rnf = rnf1

instance NFData1 UnionM where
  liftRnf _a (UAny i m) = rnf i `seq` liftRnf _a m
  liftRnf _a (UMrg _ m) = liftRnf _a m

instance (Lift a) => Lift (UnionM a) where
  liftTyped (UAny _ v) = [||freshUAny v||]
  liftTyped (UMrg _ v) = [||freshUAny v||]
  lift = unTypeSplice . liftTyped

freshUAny :: Union a -> UnionM a
freshUAny v = UAny (unsafeDupablePerformIO $ newIORef $ Left v) v
{-# NOINLINE freshUAny #-}

instance (Show a) => (Show (UnionM a)) where
  showsPrec = showsPrec1

liftShowsPrecUnion ::
  forall a.
  (Int -> a -> ShowS) ->
  ([a] -> ShowS) ->
  Int ->
  Union a ->
  ShowS
liftShowsPrecUnion sp _ i (Single a) = sp i a
liftShowsPrecUnion sp sl i (If _ _ cond t f) =
  showParen (i > 10) $
    showString "If"
      . showChar ' '
      . showsPrec 11 cond
      . showChar ' '
      . sp1 11 t
      . showChar ' '
      . sp1 11 f
  where
    sp1 = liftShowsPrecUnion sp sl

wrapBracket :: Char -> Char -> ShowS -> ShowS
wrapBracket l r p = showChar l . p . showChar r

instance Show1 UnionM where
  liftShowsPrec sp sl i (UAny _ a) =
    wrapBracket '<' '>'
      . liftShowsPrecUnion sp sl 0
      $ a
  liftShowsPrec sp sl i (UMrg _ a) =
    wrapBracket '{' '}'
      . liftShowsPrecUnion sp sl 0
      $ a

-- | Extract the underlying Union. May be unmerged.
underlyingUnion :: UnionM a -> Union a
underlyingUnion (UAny _ a) = a
underlyingUnion (UMrg _ a) = a
{-# INLINE underlyingUnion #-}

-- | Check if a UnionM is already merged.
isMerged :: UnionM a -> Bool
isMerged UAny {} = False
isMerged UMrg {} = True
{-# INLINE isMerged #-}

instance UnionPrjOp UnionM where
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

instance Functor UnionM where
  fmap f fa = fa >>= return . f
  {-# INLINE fmap #-}

instance Applicative UnionM where
  pure = single
  {-# INLINE pure #-}
  f <*> a = f >>= (\xf -> a >>= (return . xf))
  {-# INLINE (<*>) #-}

bindUnion :: Union a -> (a -> UnionM b) -> UnionM b
bindUnion (Single a') f' = f' a'
bindUnion (If _ _ cond ifTrue ifFalse) f' =
  unionIf cond (bindUnion ifTrue f') (bindUnion ifFalse f')
{-# INLINE bindUnion #-}

instance Monad UnionM where
  a >>= f = bindUnion (underlyingUnion a) f
  {-# INLINE (>>=) #-}

instance (Mergeable a) => Mergeable (UnionM a) where
  rootStrategy = SimpleStrategy $ \cond t f -> unionIf cond t f >>= mrgSingle
  {-# INLINE rootStrategy #-}

instance (Mergeable a) => SimpleMergeable (UnionM a) where
  mrgIte = mrgIf
  {-# INLINE mrgIte #-}

instance Mergeable1 UnionM where
  liftRootStrategy m = SimpleStrategy $ \cond t f -> unionIf cond t f >>= (UMrg m . Single)
  {-# INLINE liftRootStrategy #-}

instance SimpleMergeable1 UnionM where
  liftMrgIte m = mrgIfWithStrategy (SimpleStrategy m)
  {-# INLINE liftMrgIte #-}

instance UnionLike UnionM where
  mergeWithStrategy _ m@(UMrg _ _) = m
  mergeWithStrategy s (UAny ref u) = unsafeDupablePerformIO $
    atomicModifyIORef' ref $ \case
      x@(Right r) -> (x, r)
      Left _ -> (Right r, r)
        where
          !r = UMrg s $ fullReconstruct s u -- m >>= mrgSingle
  {-# NOINLINE mergeWithStrategy #-}
  mrgIfWithStrategy s (Con c) l r = if c then mergeWithStrategy s l else mergeWithStrategy s r
  mrgIfWithStrategy s cond l r =
    mergeWithStrategy s $ unionIf cond l r
  {-# INLINE mrgIfWithStrategy #-}
  single = freshUAny . single
  {-# INLINE single #-}
  unionIf cond (UAny _ a) (UAny _ b) = freshUAny $ unionIf cond a b
  unionIf cond (UMrg m a) (UAny _ b) = UMrg m $ ifWithStrategy m cond a b
  unionIf cond a (UMrg m b) = UMrg m $ ifWithStrategy m cond (underlyingUnion a) b
  {-# INLINE unionIf #-}

instance (SEq a) => SEq (UnionM a) where
  x ==~ y = simpleMerge $ do
    x1 <- x
    y1 <- y
    mrgSingle $ x1 ==~ y1

-- | Lift the 'UnionM' to any 'MonadUnion'.
liftToMonadUnion :: (Mergeable a, MonadUnion u) => UnionM a -> u a
liftToMonadUnion u = go (underlyingUnion u)
  where
    go (Single v) = mrgSingle v
    go (If _ _ c t f) = mrgIf c (go t) (go f)

instance (SOrd a) => SOrd (UnionM a) where
  x <=~ y = simpleMerge $ do
    x1 <- x
    y1 <- y
    mrgSingle $ x1 <=~ y1
  x <~ y = simpleMerge $ do
    x1 <- x
    y1 <- y
    mrgSingle $ x1 <~ y1
  x >=~ y = simpleMerge $ do
    x1 <- x
    y1 <- y
    mrgSingle $ x1 >=~ y1
  x >~ y = simpleMerge $ do
    x1 <- x
    y1 <- y
    mrgSingle $ x1 >~ y1
  x `symCompare` y = liftToMonadUnion $ do
    x1 <- x
    y1 <- y
    x1 `symCompare` y1

instance {-# OVERLAPPABLE #-} (ToSym a b, Mergeable b) => ToSym a (UnionM b) where
  toSym = mrgSingle . toSym

instance {-# OVERLAPPING #-} (ToSym a b, Mergeable b) => ToSym (UnionM a) (UnionM b) where
  toSym = merge . fmap toSym

instance {-# OVERLAPPABLE #-} (ToCon a b) => ToCon (UnionM a) b where
  toCon v = go $ underlyingUnion v
    where
      go (Single x) = toCon x
      go _ = Nothing

instance {-# OVERLAPPING #-} (ToCon a b, Mergeable b) => ToCon (UnionM a) (UnionM b) where
  toCon v = go $ underlyingUnion v
    where
      go (Single x) = case toCon x of
        Nothing -> Nothing
        Just v -> Just $ mrgSingle v
      go (If _ _ c t f) = do
        t' <- go t
        f' <- go f
        return $ mrgIf c t' f'

instance (Mergeable a, EvaluateSym a) => EvaluateSym (UnionM a) where
  evaluateSym fillDefault model x = go $ underlyingUnion x
    where
      go :: Union a -> UnionM a
      go (Single v) = mrgSingle $ evaluateSym fillDefault model v
      go (If _ _ cond t f) =
        mrgIf
          (evaluateSym fillDefault model cond)
          (go t)
          (go f)

{-
instance (Mergeable a, SubstituteSym a) => SubstituteSym (UnionM a) where
  substituteSym sym val x = go $ underlyingUnion x
    where
      go :: Union a -> UnionM a
      go (Single v) = mrgSingle $ substituteSym sym val v
      go (If _ _ cond t f) =
        mrgIf
          (substituteSym sym val cond)
          (go t)
          (go f)
          -}

instance
  (ExtractSymbolics a) =>
  ExtractSymbolics (UnionM a)
  where
  extractSymbolics v = go $ underlyingUnion v
    where
      go (Single x) = extractSymbolics x
      go (If _ _ cond t f) = extractSymbolics cond <> go t <> go f

instance (Hashable a) => Hashable (UnionM a) where
  s `hashWithSalt` (UAny _ u) = s `hashWithSalt` (0 :: Int) `hashWithSalt` u
  s `hashWithSalt` (UMrg _ u) = s `hashWithSalt` (1 :: Int) `hashWithSalt` u

instance (Eq a) => Eq (UnionM a) where
  UAny _ l == UAny _ r = l == r
  UMrg _ l == UMrg _ r = l == r
  _ == _ = False

instance Eq1 UnionM where
  liftEq e l r = liftEq e (underlyingUnion l) (underlyingUnion r)

instance (Num a, Mergeable a) => Num (UnionM a) where
  fromInteger = mrgSingle . fromInteger
  negate x = x >>= (mrgSingle . negate)
  x + y = x >>= \x1 -> y >>= \y1 -> mrgSingle $ x1 + y1
  x - y = x >>= \x1 -> y >>= \y1 -> mrgSingle $ x1 - y1
  x * y = x >>= \x1 -> y >>= \y1 -> mrgSingle $ x1 * y1
  abs x = x >>= mrgSingle . abs
  signum x = x >>= mrgSingle . signum

instance (ITEOp a, Mergeable a) => ITEOp (UnionM a) where
  ites = mrgIf

instance (LogicalOp a, Mergeable a) => LogicalOp (UnionM a) where
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

instance (Solvable c t, Mergeable t) => Solvable c (UnionM t) where
  con = mrgSingle . con
  {-# INLINE con #-}
  ssym = mrgSingle . ssym
  {-# INLINE ssym #-}
  isym i s = mrgSingle $ isym i s
  {-# INLINE isym #-}
  sinfosym s info = mrgSingle $ sinfosym s info
  {-# INLINE sinfosym #-}
  iinfosym i s info = mrgSingle $ iinfosym i s info
  {-# INLINE iinfosym #-}
  conView v = do
    c <- singleView v
    conView c
  {-# INLINE conView #-}

instance
  (Function f, Mergeable f, Mergeable a, Ret f ~ a) =>
  Function (UnionM f)
  where
  type Arg (UnionM f) = Arg f
  type Ret (UnionM f) = UnionM (Ret f)
  f # a = do
    f1 <- f
    mrgSingle $ f1 # a

instance (IsString a, Mergeable a) => IsString (UnionM a) where
  fromString = mrgSingle . fromString

{-
foldMapUnion :: (Monoid m) => (a -> m) -> Union a -> m
foldMapUnion f (Single v) = f v
foldMapUnion f (If _ _ _ l r) = foldMapUnion f l <> foldMapUnion f r

instance Foldable UnionM where
  foldMap f u = foldMapUnion f (underlyingUnion u)

sequenceAUnion :: (Applicative m, SymBoolOp bool) => Union (m a) -> m (Union a)
sequenceAUnion (Single v) = single <$> v
sequenceAUnion (If _ _ cond l r) = unionIf cond <$> sequenceAUnion l <*> sequenceAUnion r

instance  Traversable UnionM where
  sequenceA u = freshUAny <$> sequenceAUnion (underlyingUnion u)
  -}

-- GenSym
instance (GenSym spec a, Mergeable a) => GenSym spec (UnionM a)

instance (GenSym spec a) => GenSymSimple spec (UnionM a) where
  simpleFresh spec = do
    res <- fresh spec
    if not (isMerged res) then error "Not merged" else return res

instance
  (GenSym a a, Mergeable a) =>
  GenSym (UnionM a) a
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

instance (IsConcrete k, Mergeable t) => Mergeable (HML.HashMap k (UnionM (Maybe t))) where
  rootStrategy = SimpleStrategy mrgIte

instance (IsConcrete k, Mergeable t) => SimpleMergeable (HML.HashMap k (UnionM (Maybe t))) where
  mrgIte cond l r =
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

instance UnionWithExcept (UnionM (Either e v)) UnionM e v where
  extractUnionExcept = id

instance UnionWithExcept (UnionM (CBMCEither e v)) UnionM e v where
  extractUnionExcept = fmap runCBMCEither
