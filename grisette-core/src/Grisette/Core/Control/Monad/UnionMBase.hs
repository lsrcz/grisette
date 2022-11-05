{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -fno-cse #-}

{-# HLINT ignore "Use <&>" #-}

-- {-# OPTIONS_GHC -fno-full-laziness #-}

module Grisette.Core.Control.Monad.UnionMBase
  ( UnionMBase (..),
    IsConcrete,
    liftToMonadUnion,
    underlyingUnion,
    isMerged,
    (#~),
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
import Grisette.Core.Data.Class.PrimWrapper
import Grisette.Core.Data.Class.SOrd
import Grisette.Core.Data.Class.SimpleMergeable
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

-- | 'UnionBase' enhanced with 'Mergeable' knowledge propagation.
--
-- The 'UnionMBase' has two data constructors, 'UAny' and 'UMrg' (hidden intentionally).
--
-- The 'UAny' data constructor wraps an arbitrary 'UnionMBase'.
-- It is constructed when no 'Mergeable' knowledge is available (for example, when constructed with Haskell\'s 'return').
--
-- The 'UMrg' data constructor wraps a merged 'UnionMBase' along with the 'Mergeable' constraint.
-- This constraint can be propagated to the context without 'Mergeable' knowledge,
-- and helps the system to merge the resulting 'UnionBase'.
--
-- /Examples:/
--
-- 'return' cannot resolve the 'Mergeable' constraint.
--
-- >>> return 1 :: UnionM Integer
-- UAny (Single 1)
--
-- 'unionIf' cannot resolve the 'Mergeable' constraint.
--
-- >>> unionIf (ssymb "a") (return 1) (unionIf (ssymb "b") (return 1) (return 2)) :: UnionM Integer
-- UAny (If a (Single 1) (If b (Single 1) (Single 2)))
--
-- The system can merge the final result if the 'Mergeable' knowledge is introduced by 'mrgSingle':
--
-- >>> unionIf (ssymb "a") (return 1) (unionIf (ssymb "b") (return 1) (return 2)) >>= \x -> mrgSingle $ x + 1 :: UnionM Integer
-- UMrg (If (|| a b) (Single 2) (Single 3))
data UnionMBase bool a where
  -- | 'UnionMBase' with no 'Mergeable' knowledge.
  UAny ::
    -- | Cached merging result.
    IORef (Either (UnionBase bool a) (UnionMBase bool a)) ->
    -- | Original 'UnionBase'.
    UnionBase bool a ->
    UnionMBase bool a
  -- | 'UnionMBase' with 'Mergeable' knowledge.
  UMrg ::
    GMergingStrategy bool a ->
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

underlyingUnion :: UnionMBase bool a -> UnionBase bool a
underlyingUnion (UAny _ a) = a
underlyingUnion (UMrg _ a) = a
{-# INLINE underlyingUnion #-}

isMerged :: UnionMBase bool a -> Bool
isMerged UAny {} = False
isMerged UMrg {} = True
{-# INLINE isMerged #-}

instance SymBoolOp bool => UnionPrjOp bool (UnionMBase bool) where
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

instance (SymBoolOp bool, GMergeable bool a) => SimpleMergeable bool (UnionMBase bool a) where
  mrgIte = mrgIf
  {-# INLINE mrgIte #-}

instance (SymBoolOp bool) => GMergeable1 bool (UnionMBase bool) where
  liftGMergingStrategy m = SimpleStrategy $ \cond t f -> unionIf cond t f >>= (UMrg m . Single)
  {-# INLINE liftGMergingStrategy #-}

instance SymBoolOp bool => SimpleMergeable1 bool (UnionMBase bool) where
  liftMrgIte m = mrgIfWithStrategy (SimpleStrategy m)
  {-# INLINE liftMrgIte #-}

instance SymBoolOp bool => UnionLike bool (UnionMBase bool) where
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

-- | Lift the 'UnionMBase' to any 'MonadUnion'.
liftToMonadUnion :: (SymBoolOp bool, GMergeable bool a, MonadUnion bool u) => UnionMBase bool a -> u a
liftToMonadUnion u = go (underlyingUnion u)
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
  x `gsymCompare` y = liftToMonadUnion $ do
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

instance (SymBoolOp bool, PrimWrapper t c, GMergeable bool t) => PrimWrapper (UnionMBase bool t) c where
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
  genSymSimpleFresh spec = do
    res <- genSymFresh spec
    if not (isMerged res) then error "Not merged" else return res

instance
  (SymBoolOp bool, GenSym bool a a, GenSymSimple () bool, GMergeable bool a) =>
  GenSym bool (UnionMBase bool a) a
  where
  genSymFresh spec = go (underlyingUnion $ merge spec)
    where
      go (Single x) = genSymFresh x
      go (If _ _ _ t f) = mrgIf <$> genSymSimpleFresh () <*> go t <*> go f

-- Concrete Key HashMaps

-- | Tag for concrete types.
-- Useful for specifying the merge strategy for some parametrized types where we should have different
-- merge strategy for symbolic and concrete ones.
class (Eq t, Ord t, Hashable t) => IsConcrete t

instance IsConcrete Bool

instance IsConcrete Integer

instance (SymBoolOp bool, IsConcrete k, GMergeable bool t) => GMergeable bool (HML.HashMap k (UnionMBase bool (Maybe t))) where
  gmergingStrategy = SimpleStrategy mrgIte

instance (SymBoolOp bool, IsConcrete k, GMergeable bool t) => SimpleMergeable bool (HML.HashMap k (UnionMBase bool (Maybe t))) where
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

instance ExtractUnionEither (UnionMBase bool (Either e v)) (UnionMBase bool) e v where
  extractUnionEither = id

instance SymBoolOp bool => ExtractUnionEither (UnionMBase bool (CBMCEither e v)) (UnionMBase bool) e v where
  extractUnionEither = fmap runCBMCEither
