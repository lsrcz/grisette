{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# HLINT ignore "Use <&>" #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

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
    unionSize,
  )
where

import Control.DeepSeq (NFData (rnf), NFData1 (liftRnf), force, rnf1)
import Control.Parallel.Strategies (rpar, rseq, runEval)
import Data.Functor.Classes
  ( Eq1 (liftEq),
    Show1 (liftShowsPrec),
    showsPrec1,
  )
import qualified Data.HashMap.Lazy as HML
import Data.Hashable (Hashable (hashWithSalt))
import Data.String (IsString (fromString))
import GHC.TypeNats (KnownNat, type (<=))
import Grisette.Core.Control.Monad.CBMCExcept
  ( CBMCEither (runCBMCEither),
  )
import Grisette.Core.Control.Monad.Class.MonadParallelUnion
  ( MonadParallelUnion (parBindUnion),
  )
import Grisette.Core.Control.Monad.Union (MonadUnion)
import Grisette.Core.Data.BV (IntN, WordN)
import Grisette.Core.Data.Class.Bool
  ( ITEOp (ites),
    LogicalOp (implies, nots, xors, (&&~), (||~)),
    SEq ((==~)),
  )
import Grisette.Core.Data.Class.Evaluate (EvaluateSym (evaluateSym))
import Grisette.Core.Data.Class.ExtractSymbolics
  ( ExtractSymbolics (extractSymbolics),
  )
import Grisette.Core.Data.Class.Function (Function (Arg, Ret, (#)))
import Grisette.Core.Data.Class.GPretty
  ( GPretty (gpretty),
    groupedEnclose,
  )
import Grisette.Core.Data.Class.GenSym
  ( GenSym (fresh),
    GenSymSimple (simpleFresh),
  )
import Grisette.Core.Data.Class.Mergeable
  ( Mergeable (rootStrategy),
    Mergeable1 (liftRootStrategy),
    MergingStrategy (SimpleStrategy),
  )
import Grisette.Core.Data.Class.SOrd
  ( SOrd (symCompare, (<=~), (<~), (>=~), (>~)),
  )
import Grisette.Core.Data.Class.SimpleMergeable
  ( SimpleMergeable (mrgIte),
    SimpleMergeable1 (liftMrgIte),
    UnionLike (mergeWithStrategy, mrgIfWithStrategy, single, unionIf),
    UnionPrjOp (ifView, leftMost, singleView),
    merge,
    mrgIf,
    mrgSingle,
    simpleMerge,
    (#~),
  )
import Grisette.Core.Data.Class.Solvable
  ( Solvable (con, conView, iinfosym, isym, sinfosym, ssym),
    pattern Con,
  )
import Grisette.Core.Data.Class.Solver (UnionWithExcept (extractUnionExcept))
import Grisette.Core.Data.Class.Substitute (SubstituteSym (substituteSym))
import Grisette.Core.Data.Class.ToCon (ToCon (toCon))
import Grisette.Core.Data.Class.ToSym (ToSym (toSym))
import Grisette.Core.Data.Union
  ( Union (If, Single),
    fullReconstruct,
    ifWithStrategy,
  )
import Grisette.IR.SymPrim.Data.Prim.InternedTerm.Term
  ( LinkedRep,
    SupportedPrim,
    type (-->),
  )
import Grisette.IR.SymPrim.Data.SymPrim
  ( AllSyms (allSymsS),
    SymBool,
    SymIntN,
    SymInteger,
    SymWordN,
    type (-~>),
    type (=~>),
  )
import Grisette.IR.SymPrim.Data.TabularFun (type (=->))
import Language.Haskell.TH.Syntax (Lift (lift, liftTyped))
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
  liftRnf _a (UAny m) = liftRnf _a m
  liftRnf _a (UMrg _ m) = liftRnf _a m

instance (Lift a) => Lift (UnionM a) where
  liftTyped (UAny v) = [||UAny v||]
  liftTyped (UMrg _ v) = [||UAny v||]
  lift = unTypeSplice . liftTyped

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
  liftShowsPrec sp sl _ (UAny a) =
    wrapBracket '<' '>'
      . liftShowsPrecUnion sp sl 0
      $ a
  liftShowsPrec sp sl _ (UMrg _ a) =
    wrapBracket '{' '}'
      . liftShowsPrecUnion sp sl 0
      $ a

instance (GPretty a) => GPretty (UnionM a) where
  gpretty = \case
    (UAny a) -> groupedEnclose "<" ">" $ gpretty a
    (UMrg _ a) -> groupedEnclose "{" "}" $ gpretty a

-- | Extract the underlying Union. May be unmerged.
underlyingUnion :: UnionM a -> Union a
underlyingUnion (UAny a) = a
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
  ifView (UAny u) = case ifView u of
    Just (c, t, f) -> Just (c, UAny t, UAny f)
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

parBindUnion'' :: (Mergeable b, NFData b) => Union a -> (a -> UnionM b) -> UnionM b
parBindUnion'' (Single a) f = merge $ f a
parBindUnion'' u f = parBindUnion' u f

parBindUnion' :: (Mergeable b, NFData b) => Union a -> (a -> UnionM b) -> UnionM b
parBindUnion' (Single a') f' = f' a'
parBindUnion' (If _ _ cond ifTrue ifFalse) f' = runEval $ do
  l <- rpar $ force $ parBindUnion' ifTrue f'
  r <- rpar $ force $ parBindUnion' ifFalse f'
  l' <- rseq l
  r' <- rseq r
  rseq $ mrgIf cond l' r'
{-# INLINE parBindUnion' #-}

instance MonadParallelUnion UnionM where
  parBindUnion = parBindUnion'' . underlyingUnion
  {-# INLINE parBindUnion #-}

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
  mergeWithStrategy s (UAny u) = UMrg s $ fullReconstruct s u
  {-# INLINE mergeWithStrategy #-}
  mrgIfWithStrategy s (Con c) l r = if c then mergeWithStrategy s l else mergeWithStrategy s r
  mrgIfWithStrategy s cond l r =
    mergeWithStrategy s $ unionIf cond l r
  {-# INLINE mrgIfWithStrategy #-}
  single = UAny . single
  {-# INLINE single #-}
  unionIf cond (UAny a) (UAny b) = UAny $ unionIf cond a b
  unionIf cond (UMrg m a) (UAny b) = UMrg m $ ifWithStrategy m cond a b
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

instance {-# INCOHERENT #-} (ToSym a b, Mergeable b) => ToSym a (UnionM b) where
  toSym = mrgSingle . toSym

instance (ToSym a b, Mergeable b) => ToSym (UnionM a) (UnionM b) where
  toSym = merge . fmap toSym

#define TO_SYM_FROM_UNION_CON_SIMPLE(contype, symtype) \
instance ToSym (UnionM contype) symtype where \
  toSym = simpleMerge . fmap con

#define TO_SYM_FROM_UNION_CON_BV(contype, symtype) \
instance (KnownNat n, 1 <= n) => ToSym (UnionM (contype n)) (symtype n) where \
  toSym = simpleMerge . fmap con

#define TO_SYM_FROM_UNION_CON_FUN(conop, symop) \
instance (SupportedPrim ca, SupportedPrim cb, LinkedRep ca sa, LinkedRep cb sb) => ToSym (UnionM (conop ca cb)) (symop sa sb) where \
  toSym = simpleMerge . fmap con

#define TO_SYM_FROM_UNION_CON_BV_SOME(contype, symtype) \
instance ToSym (UnionM contype) symtype where \
  toSym = simpleMerge . fmap (toSym :: contype -> symtype)

#if 1
TO_SYM_FROM_UNION_CON_SIMPLE(Bool, SymBool)
TO_SYM_FROM_UNION_CON_SIMPLE(Integer, SymInteger)
TO_SYM_FROM_UNION_CON_BV(IntN, SymIntN)
TO_SYM_FROM_UNION_CON_BV(WordN, SymWordN)
TO_SYM_FROM_UNION_CON_FUN((=->), (=~>))
TO_SYM_FROM_UNION_CON_FUN((-->), (-~>))
#endif

instance {-# INCOHERENT #-} (ToCon a b) => ToCon (UnionM a) b where
  toCon v = go $ underlyingUnion v
    where
      go (Single x) = toCon x
      go _ = Nothing

instance (ToCon a b, Mergeable b) => ToCon (UnionM a) (UnionM b) where
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

instance
  (ExtractSymbolics a) =>
  ExtractSymbolics (UnionM a)
  where
  extractSymbolics v = go $ underlyingUnion v
    where
      go (Single x) = extractSymbolics x
      go (If _ _ cond t f) = extractSymbolics cond <> go t <> go f

instance (Hashable a) => Hashable (UnionM a) where
  s `hashWithSalt` (UAny u) = s `hashWithSalt` (0 :: Int) `hashWithSalt` u
  s `hashWithSalt` (UMrg _ u) = s `hashWithSalt` (1 :: Int) `hashWithSalt` u

instance (Eq a) => Eq (UnionM a) where
  UAny l == UAny r = l == r
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

-- AllSyms
instance (AllSyms a) => AllSyms (UnionM a) where
  allSymsS = allSymsS . underlyingUnion

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

-- | The size of a union is defined as the number of branches.
-- For example,
--
-- >>> unionSize (single True)
-- 1
-- >>> unionSize (mrgIf "a" (single 1) (single 2) :: UnionM Integer)
-- 2
-- >>> unionSize (choose [1..7] "a" :: UnionM Integer)
-- 7
unionSize :: UnionM a -> Int
unionSize = unionSize' . underlyingUnion
  where
    unionSize' (Single _) = 1
    unionSize' (If _ _ _ l r) = unionSize' l + unionSize' r
