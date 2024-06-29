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

-- |
-- Module      :   Grisette.Internal.Core.Control.Monad.Union
-- Copyright   :   (c) Sirui Lu 2021-2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Internal.Core.Control.Monad.Union
  ( -- * Union and helpers
    Union (..),
    unionUnaryOp,
    unionBinOp,
    liftUnion,
    liftToMonadUnion,
    unionBase,
    isMerged,
    unionSize,
    IsConcrete,
  )
where

import Control.DeepSeq (NFData (rnf), NFData1 (liftRnf), rnf1)
import Data.Functor.Classes
  ( Eq1 (liftEq),
    Show1 (liftShowsPrec),
    showsPrec1,
  )
import qualified Data.HashMap.Lazy as HML
import Data.Hashable (Hashable (hashWithSalt))
import Data.String (IsString (fromString))
import GHC.TypeNats (KnownNat, type (<=))
import Grisette.Internal.Core.Control.Monad.Class.Union (MonadUnion)
import Grisette.Internal.Core.Data.Class.EvalSym (EvalSym (evalSym))
import Grisette.Internal.Core.Data.Class.ExtractSym
  ( ExtractSym (extractSym),
  )
import Grisette.Internal.Core.Data.Class.Function (Function ((#)))
import Grisette.Internal.Core.Data.Class.GPretty
  ( GPretty (gpretty),
    groupedEnclose,
  )
import Grisette.Internal.Core.Data.Class.ITEOp (ITEOp (symIte))
import Grisette.Internal.Core.Data.Class.LogicalOp
  ( LogicalOp (false, symImplies, symNot, symXor, true, (.&&), (.||)),
  )
import Grisette.Internal.Core.Data.Class.Mergeable
  ( Mergeable (rootStrategy),
    Mergeable1 (liftRootStrategy),
    MergingStrategy (SimpleStrategy),
  )
import Grisette.Internal.Core.Data.Class.PlainUnion
  ( PlainUnion (ifView, singleView),
    simpleMerge,
  )
import Grisette.Internal.Core.Data.Class.SEq (SEq ((.==)))
import Grisette.Internal.Core.Data.Class.SimpleMergeable
  ( SimpleMergeable (mrgIte),
    SimpleMergeable1 (liftMrgIte),
    SymBranching (mrgIfPropagatedStrategy, mrgIfWithStrategy),
    mrgIf,
  )
import Grisette.Internal.Core.Data.Class.Solvable
  ( Solvable (con, conView, sym),
    pattern Con,
  )
import Grisette.Internal.Core.Data.Class.Solver
  ( UnionWithExcept (extractUnionExcept),
  )
import Grisette.Internal.Core.Data.Class.SubstSym
  ( SubstSym (substSym),
  )
import Grisette.Internal.Core.Data.Class.ToCon (ToCon (toCon))
import Grisette.Internal.Core.Data.Class.ToSym (ToSym (toSym))
import Grisette.Internal.Core.Data.Class.TryMerge
  ( TryMerge (tryMergeWithStrategy),
    mrgSingle,
    tryMerge,
  )
import Grisette.Internal.Core.Data.UnionBase
  ( UnionBase (UnionIf, UnionSingle),
    ifWithLeftMost,
  )
import Grisette.Internal.SymPrim.AllSyms
  ( AllSyms (allSymsS),
  )
import Grisette.Internal.SymPrim.BV (IntN, WordN)
import Grisette.Internal.SymPrim.GeneralFun
  ( type (-->),
  )
import Grisette.Internal.SymPrim.Prim.Term
  ( LinkedRep,
    SupportedPrim,
  )
import Grisette.Internal.SymPrim.SymBV
  ( SymIntN,
    SymWordN,
  )
import Grisette.Internal.SymPrim.SymBool (SymBool)
import Grisette.Internal.SymPrim.SymGeneralFun (type (-~>))
import Grisette.Internal.SymPrim.SymInteger (SymInteger)
import Grisette.Internal.SymPrim.SymTabularFun (type (=~>))
import Grisette.Internal.SymPrim.TabularFun (type (=->))
import Language.Haskell.TH.Syntax (Lift (lift, liftTyped))
import Language.Haskell.TH.Syntax.Compat (unTypeSplice)

-- $setup
-- >>> import Grisette.Core
-- >>> import Grisette.SymPrim
-- >>> :set -XScopedTypeVariables

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

instance (NFData a) => NFData (Union a) where
  rnf = rnf1

instance NFData1 Union where
  liftRnf _a (UAny m) = liftRnf _a m
  liftRnf _a (UMrg _ m) = liftRnf _a m

instance (Lift a) => Lift (Union a) where
  liftTyped (UAny v) = [||UAny v||]
  liftTyped (UMrg _ v) = [||UAny v||]
  lift = unTypeSplice . liftTyped

instance (Show a) => (Show (Union a)) where
  showsPrec = showsPrec1

liftShowsPrecUnion ::
  forall a.
  (Int -> a -> ShowS) ->
  ([a] -> ShowS) ->
  Int ->
  UnionBase a ->
  ShowS
liftShowsPrecUnion sp _ i (UnionSingle a) = sp i a
liftShowsPrecUnion sp sl i (UnionIf _ _ cond t f) =
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

instance Show1 Union where
  liftShowsPrec sp sl _ (UAny a) =
    wrapBracket '<' '>'
      . liftShowsPrecUnion sp sl 0
      $ a
  liftShowsPrec sp sl _ (UMrg _ a) =
    wrapBracket '{' '}'
      . liftShowsPrecUnion sp sl 0
      $ a

instance (GPretty a) => GPretty (Union a) where
  gpretty = \case
    (UAny a) -> groupedEnclose "<" ">" $ gpretty a
    (UMrg _ a) -> groupedEnclose "{" "}" $ gpretty a

-- | Extract the underlying Union. May be unmerged.
unionBase :: Union a -> UnionBase a
unionBase (UAny a) = a
unionBase (UMrg _ a) = a
{-# INLINE unionBase #-}

-- | Check if a 'Union' is already merged.
isMerged :: Union a -> Bool
isMerged UAny {} = False
isMerged UMrg {} = True
{-# INLINE isMerged #-}

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

-- | Lift a unary operation to 'Union'.
unionUnaryOp :: (Mergeable a, Mergeable b) => (a -> b) -> Union a -> Union b
unionUnaryOp f a = do
  a1 <- tryMerge a
  mrgSingle $ f a1
{-# INLINE unionUnaryOp #-}

-- | Lift a binary operation to 'Union'.
unionBinOp ::
  (Mergeable a, Mergeable b, Mergeable c) =>
  (a -> b -> c) ->
  Union a ->
  Union b ->
  Union c
unionBinOp f a b = do
  a1 <- tryMerge a
  b1 <- tryMerge b
  mrgSingle $ f a1 b1
{-# INLINE unionBinOp #-}

instance (Mergeable a) => Mergeable (Union a) where
  rootStrategy = SimpleStrategy mrgIf
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

instance TryMerge Union where
  tryMergeWithStrategy _ m@(UMrg _ _) = m
  tryMergeWithStrategy s (UAny u) = UMrg s $ tryMergeWithStrategy s u
  {-# INLINE tryMergeWithStrategy #-}

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

instance (Mergeable a, SEq a) => SEq (Union a) where
  x .== y = simpleMerge $ unionBinOp (.==) x y
  {-# INLINE (.==) #-}

-- | Lift the 'Union' to any Applicative 'SymBranching'.
liftUnion :: forall u a. (Mergeable a, SymBranching u, Applicative u) => Union a -> u a
liftUnion u = go (unionBase u)
  where
    go :: UnionBase a -> u a
    go (UnionSingle v) = mrgSingle v
    go (UnionIf _ _ c t f) = mrgIf c (go t) (go f)

-- | Alias for `liftUnion`, but for monads.
liftToMonadUnion :: (Mergeable a, MonadUnion u) => Union a -> u a
liftToMonadUnion = liftUnion

instance {-# INCOHERENT #-} (ToSym a b, Mergeable b) => ToSym a (Union b) where
  toSym = mrgSingle . toSym

instance (ToSym a b, Mergeable b) => ToSym (Union a) (Union b) where
  toSym = tryMerge . fmap toSym

#define TO_SYM_FROM_UNION_CON_SIMPLE(contype, symtype) \
instance ToSym (Union contype) symtype where \
  toSym = simpleMerge . fmap con

#define TO_SYM_FROM_UNION_CON_BV(contype, symtype) \
instance (KnownNat n, 1 <= n) => ToSym (Union (contype n)) (symtype n) where \
  toSym = simpleMerge . fmap con

#define TO_SYM_FROM_UNION_CON_FUN(conop, symop) \
instance (SupportedPrim (conop ca cb), LinkedRep ca sa, LinkedRep cb sb) => ToSym (Union (conop ca cb)) (symop sa sb) where \
  toSym = simpleMerge . fmap con

#define TO_SYM_FROM_UNION_CON_BV_SOME(contype, symtype) \
instance ToSym (Union contype) symtype where \
  toSym = simpleMerge . fmap (toSym :: contype -> symtype)

#if 1
TO_SYM_FROM_UNION_CON_SIMPLE(Bool, SymBool)
TO_SYM_FROM_UNION_CON_SIMPLE(Integer, SymInteger)
TO_SYM_FROM_UNION_CON_BV(IntN, SymIntN)
TO_SYM_FROM_UNION_CON_BV(WordN, SymWordN)
TO_SYM_FROM_UNION_CON_FUN((=->), (=~>))
TO_SYM_FROM_UNION_CON_FUN((-->), (-~>))
#endif

instance {-# INCOHERENT #-} (ToCon a b, Mergeable a) => ToCon (Union a) b where
  toCon v = go $ unionBase $ tryMerge v
    where
      go (UnionSingle x) = toCon x
      go _ = Nothing

instance
  (ToCon a b, Mergeable a, Mergeable b) =>
  ToCon (Union a) (Union b)
  where
  toCon v = go $ unionBase $ tryMerge v
    where
      go :: UnionBase a -> Maybe (Union b)
      go (UnionSingle x) = case toCon x of
        Nothing -> Nothing
        Just v -> Just $ mrgSingle v
      go (UnionIf _ _ c t f) = do
        t' <- go t
        f' <- go f
        return $ mrgIf c t' f'

instance (Mergeable a, EvalSym a) => EvalSym (Union a) where
  evalSym fillDefault model x = go $ unionBase x
    where
      go :: UnionBase a -> Union a
      go (UnionSingle v) = mrgSingle $ evalSym fillDefault model v
      go (UnionIf _ _ cond t f) =
        mrgIf
          (evalSym fillDefault model cond)
          (go t)
          (go f)

instance (Mergeable a, SubstSym a) => SubstSym (Union a) where
  substSym sym val x = go $ unionBase x
    where
      go :: UnionBase a -> Union a
      go (UnionSingle v) = mrgSingle $ substSym sym val v
      go (UnionIf _ _ cond t f) =
        mrgIf
          (substSym sym val cond)
          (go t)
          (go f)

instance
  (ExtractSym a) =>
  ExtractSym (Union a)
  where
  extractSym v = go $ unionBase v
    where
      go (UnionSingle x) = extractSym x
      go (UnionIf _ _ cond t f) = extractSym cond <> go t <> go f

instance (Hashable a) => Hashable (Union a) where
  s `hashWithSalt` (UAny u) = s `hashWithSalt` (0 :: Int) `hashWithSalt` u
  s `hashWithSalt` (UMrg _ u) = s `hashWithSalt` (1 :: Int) `hashWithSalt` u

instance (Eq a) => Eq (Union a) where
  UAny l == UAny r = l == r
  UMrg _ l == UMrg _ r = l == r
  _ == _ = False

instance Eq1 Union where
  liftEq e l r = liftEq e (unionBase l) (unionBase r)

instance (Num a, Mergeable a) => Num (Union a) where
  fromInteger = mrgSingle . fromInteger
  negate = unionUnaryOp negate
  (+) = unionBinOp (+)
  (*) = unionBinOp (*)
  (-) = unionBinOp (-)
  abs = unionUnaryOp abs
  signum = unionUnaryOp signum

instance (ITEOp a, Mergeable a) => ITEOp (Union a) where
  symIte = mrgIf

instance (LogicalOp a, Mergeable a) => LogicalOp (Union a) where
  true = mrgSingle true
  false = mrgSingle false
  (.||) = unionBinOp (.||)
  (.&&) = unionBinOp (.&&)
  symNot = unionUnaryOp symNot
  symXor = unionBinOp symXor
  symImplies = unionBinOp symImplies

instance (Solvable c t, Mergeable t) => Solvable c (Union t) where
  con = mrgSingle . con
  {-# INLINE con #-}
  sym = mrgSingle . sym
  {-# INLINE sym #-}
  conView v = do
    c <- singleView $ tryMerge v
    conView c
  {-# INLINE conView #-}

instance
  (Function f arg ret, Mergeable f, Mergeable ret) =>
  Function (Union f) arg (Union ret)
  where
  f # a = do
    f1 <- f
    mrgSingle $ f1 # a

instance (IsString a, Mergeable a) => IsString (Union a) where
  fromString = mrgSingle . fromString

-- AllSyms
instance (AllSyms a) => AllSyms (Union a) where
  allSymsS = allSymsS . unionBase

-- Concrete Key HashMaps

-- | Tag for concrete types.
-- Useful for specifying the merge strategy for some parametrized types where we should have different
-- merge strategy for symbolic and concrete ones.
class (Eq t, Ord t, Hashable t) => IsConcrete t

instance IsConcrete Bool

instance IsConcrete Integer

instance (IsConcrete k, Mergeable t) => Mergeable (HML.HashMap k (Union (Maybe t))) where
  rootStrategy = SimpleStrategy mrgIte

instance (IsConcrete k, Mergeable t) => SimpleMergeable (HML.HashMap k (Union (Maybe t))) where
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

instance UnionWithExcept (Union (Either e v)) Union e v where
  extractUnionExcept = id

-- | The size of a union is defined as the number of branches.
-- For example,
--
-- >>> unionSize (return True)
-- 1
-- >>> unionSize (mrgIf "a" (return 1) (return 2) :: Union Integer)
-- 2
-- >>> unionSize (choose [1..7] "a" :: Union Integer)
-- 7
unionSize :: Union a -> Int
unionSize = unionSize' . unionBase
  where
    unionSize' (UnionSingle _) = 1
    unionSize' (UnionIf _ _ _ l r) = unionSize' l + unionSize' r
