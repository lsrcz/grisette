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
-- Module      :   Grisette.Internal.Internal.Impl.Core.Control.Monad.Union
-- Copyright   :   (c) Sirui Lu 2021-2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Internal.Internal.Impl.Core.Control.Monad.Union
  ( -- * Union and helpers
    unionUnaryOp,
    unionBinOp,
    liftUnion,
    liftToMonadUnion,
    unionMergingStrategy,
    isMerged,
    unionSize,
  )
where

import Control.Applicative (Alternative ((<|>)))
import Control.DeepSeq (NFData (rnf), NFData1 (liftRnf), rnf1)
import Control.Monad.Identity (Identity (Identity, runIdentity))
import qualified Data.Binary as Binary
import Data.Bytes.Serial (Serial (deserialize, serialize))
import Data.Functor.Classes
  ( Eq1 (liftEq),
    Show1 (liftShowsPrec),
    showsPrec1,
  )
import Data.Hashable (Hashable (hashWithSalt))
import qualified Data.Serialize as Cereal
import GHC.TypeNats (KnownNat, type (<=))
import Grisette.Internal.Core.Control.Monad.Class.Union (MonadUnion)
import Grisette.Internal.Core.Data.Class.AsKey
  ( KeyEq (keyEq),
    KeyEq1 (liftKeyEq),
    KeyHashable (keyHashWithSalt),
    KeyHashable1 (liftKeyHashWithSalt),
    shouldUseAsKeyHasSymbolicVersionError,
  )
import Grisette.Internal.Core.Data.Class.EvalSym
  ( EvalSym (evalSym),
    EvalSym1 (liftEvalSym),
    evalSym1,
  )
import Grisette.Internal.Core.Data.Class.ExtractSym
  ( ExtractSym (extractSymMaybe),
    ExtractSym1 (liftExtractSymMaybe),
    extractSymMaybe1,
  )
import Grisette.Internal.Core.Data.Class.Function (Function ((#)))
import Grisette.Internal.Core.Data.Class.ITEOp (ITEOp (symIte))
import Grisette.Internal.Core.Data.Class.LogicalOp
  ( LogicalOp (false, symImplies, symNot, symXor, true, (.&&), (.||)),
  )
import Grisette.Internal.Core.Data.Class.Mergeable
  ( Mergeable (rootStrategy),
    MergingStrategy,
  )
import Grisette.Internal.Core.Data.Class.PPrint
  ( PPrint (pformatPrec),
    PPrint1 (liftPFormatPrec),
    groupedEnclose,
    pformatPrec1,
  )
import Grisette.Internal.Core.Data.Class.PlainUnion
  ( simpleMerge,
  )
import Grisette.Internal.Core.Data.Class.SimpleMergeable
  ( SymBranching (mrgIfPropagatedStrategy, mrgIfWithStrategy),
    mrgIf,
  )
import Grisette.Internal.Core.Data.Class.Solvable
  ( Solvable (con),
  )
import Grisette.Internal.Core.Data.Class.Solver
  ( UnionWithExcept (extractUnionExcept),
  )
import Grisette.Internal.Core.Data.Class.SubstSym
  ( SubstSym (substSym),
    SubstSym1 (liftSubstSym),
    substSym1,
  )
import Grisette.Internal.Core.Data.Class.ToCon
  ( ToCon (toCon),
    ToCon1 (liftToCon),
    toCon1,
  )
import Grisette.Internal.Core.Data.Class.ToSym
  ( ToSym (toSym),
    ToSym1 (liftToSym),
    toSym1,
  )
import Grisette.Internal.Core.Data.Class.TryMerge
  ( mrgSingle,
    mrgSingleWithStrategy,
    tryMerge,
  )
import Grisette.Internal.Internal.Decl.Core.Control.Monad.Union
  ( Union (UAny, UMrg),
    unionBase,
  )
import Grisette.Internal.Internal.Decl.Core.Data.Class.SymEq
  ( SymEq ((.==)),
    SymEq1 (liftSymEq),
    symEq1,
  )
import Grisette.Internal.Internal.Decl.Core.Data.UnionBase
  ( UnionBase (UnionIf, UnionSingle),
  )
import Grisette.Internal.Internal.Impl.Core.Data.UnionBase ()
import Grisette.Internal.SymPrim.AllSyms
  ( AllSyms (allSymsS),
    AllSyms1 (liftAllSymsS),
    allSymsS1,
  )
import Grisette.Internal.SymPrim.BV (IntN, WordN)
import Grisette.Internal.SymPrim.GeneralFun
  ( type (-->),
  )
import Grisette.Internal.SymPrim.Prim.Term
  ( LinkedRep,
    SupportedNonFuncPrim,
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

instance (Mergeable a, Serial a) => Serial (Union a) where
  serialize = serialize . unionBase
  deserialize = UMrg rootStrategy <$> deserialize

instance (Mergeable a, Serial a) => Cereal.Serialize (Union a) where
  put = serialize
  get = deserialize

instance (Mergeable a, Serial a) => Binary.Binary (Union a) where
  put = serialize
  get = deserialize

-- | Get the (possibly empty) cached merging strategy.
unionMergingStrategy :: Union a -> Maybe (MergingStrategy a)
unionMergingStrategy (UMrg s _) = Just s
unionMergingStrategy _ = Nothing

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

instance (PPrint a) => PPrint (Union a) where
  pformatPrec = pformatPrec1

instance PPrint1 Union where
  liftPFormatPrec fa fl _ = \case
    (UAny a) -> groupedEnclose "<" ">" $ liftPFormatPrec fa fl 0 a
    (UMrg _ a) -> groupedEnclose "{" "}" $ liftPFormatPrec fa fl 0 a

-- | Check if a 'Union' is already merged.
isMerged :: Union a -> Bool
isMerged UAny {} = False
isMerged UMrg {} = True
{-# INLINE isMerged #-}

-- | Lift a unary operation to 'Union'.
unionUnaryOp :: (a -> a) -> Union a -> Union a
unionUnaryOp f a = do
  a1 <- a
  maybe return mrgSingleWithStrategy (unionMergingStrategy a) $ f a1
{-# INLINE unionUnaryOp #-}

-- | Lift a binary operation to 'Union'.
unionBinOp ::
  (a -> a -> a) ->
  Union a ->
  Union a ->
  Union a
unionBinOp f a b = do
  a1 <- a
  b1 <- b
  maybe
    return
    mrgSingleWithStrategy
    (unionMergingStrategy a <|> unionMergingStrategy b)
    $ f a1 b1
{-# INLINE unionBinOp #-}

instance (SymEq a) => SymEq (Union a) where
  (.==) = symEq1
  {-# INLINE (.==) #-}

instance SymEq1 Union where
  liftSymEq f x y = simpleMerge $ f <$> x <*> y
  {-# INLINE liftSymEq #-}

-- | Lift the 'Union' to any Applicative 'SymBranching'.
liftUnion ::
  forall u a. (Mergeable a, SymBranching u, Applicative u) => Union a -> u a
liftUnion u = go (unionBase u)
  where
    go :: UnionBase a -> u a
    go (UnionSingle v) = mrgSingle v
    go (UnionIf _ _ c t f) = mrgIf c (go t) (go f)

-- | Alias for `liftUnion`, but for monads.
liftToMonadUnion :: (Mergeable a, MonadUnion u) => Union a -> u a
liftToMonadUnion = liftUnion

instance (ToSym a b) => ToSym (Union a) (Union b) where
  toSym = toSym1

instance ToSym1 Union Union where
  liftToSym = fmap

instance (ToSym a b) => ToSym (Identity a) (Union b) where
  toSym = toSym1

instance ToSym1 Identity Union where
  liftToSym f v = return $ runIdentity $ fmap f v

instance ToSym (Union Bool) SymBool where
  toSym = simpleMerge . fmap con

instance ToSym (Union Integer) SymInteger where
  toSym = simpleMerge . fmap con

instance (KnownNat n, 1 <= n) => ToSym (Union (IntN n)) (SymIntN n) where
  toSym = simpleMerge . fmap con

instance (KnownNat n, 1 <= n) => ToSym (Union (WordN n)) (SymWordN n) where
  toSym = simpleMerge . fmap con

instance
  ( SupportedPrim ((=->) ca cb),
    SupportedNonFuncPrim ca,
    LinkedRep ca sa,
    LinkedRep cb sb
  ) =>
  ToSym (Union ((=->) ca cb)) ((=~>) sa sb)
  where
  toSym = simpleMerge . fmap con

instance
  ( SupportedPrim ((-->) ca cb),
    SupportedNonFuncPrim ca,
    LinkedRep ca sa,
    LinkedRep cb sb
  ) =>
  ToSym (Union ((-->) ca cb)) ((-~>) sa sb)
  where
  toSym = simpleMerge . fmap con

instance (ToCon a b) => ToCon (Union a) (Identity b) where
  toCon = toCon1

instance ToCon1 Union Identity where
  liftToCon f v = go $ unionBase v
    where
      go (UnionSingle x) = Identity <$> f x
      go (UnionIf _ _ c t f) =
        case toCon c of
          Nothing -> Nothing
          Just True -> go t
          Just False -> go f

instance (ToCon a b) => ToCon (Union a) (Union b) where
  toCon = toCon1

instance ToCon1 Union Union where
  liftToCon f v = go $ unionBase v
    where
      go (UnionSingle x) = case f x of
        Nothing -> Nothing
        Just v -> Just $ return v
      go (UnionIf _ _ c t f) = do
        t' <- go t
        f' <- go f
        return $ mrgIfPropagatedStrategy c t' f'

instance (EvalSym a) => EvalSym (Union a) where
  evalSym = evalSym1

instance EvalSym1 Union where
  liftEvalSym f fillDefault model x = go $ unionBase x
    where
      go (UnionSingle v) = single $ f fillDefault model v
      go (UnionIf _ _ cond t f) =
        unionIf (evalSym fillDefault model cond) (go t) (go f)
      strategy = unionMergingStrategy x
      single = maybe return mrgSingleWithStrategy strategy
      unionIf = maybe mrgIfPropagatedStrategy mrgIfWithStrategy strategy

instance (SubstSym a) => SubstSym (Union a) where
  substSym = substSym1

instance SubstSym1 Union where
  liftSubstSym f sym val x = go $ unionBase x
    where
      go (UnionSingle v) = single $ f sym val v
      go (UnionIf _ _ cond t f) =
        unionIf
          (substSym sym val cond)
          (go t)
          (go f)
      strategy = unionMergingStrategy x
      single = maybe return mrgSingleWithStrategy strategy
      unionIf = maybe mrgIfPropagatedStrategy mrgIfWithStrategy strategy

instance (ExtractSym a) => ExtractSym (Union a) where
  extractSymMaybe = extractSymMaybe1

instance ExtractSym1 Union where
  liftExtractSymMaybe e v = go $ unionBase v
    where
      go (UnionSingle x) = e x
      go (UnionIf _ _ cond t f) = extractSymMaybe cond <> go t <> go f

instance (Hashable a) => KeyHashable (Union a) where
  keyHashWithSalt = liftKeyHashWithSalt hashWithSalt
  {-# INLINE keyHashWithSalt #-}

instance KeyHashable1 Union where
  liftKeyHashWithSalt f s (UAny a) =
    liftKeyHashWithSalt f s a `hashWithSalt` (0 :: Int)
  liftKeyHashWithSalt f s (UMrg _ a) =
    liftKeyHashWithSalt f s a `hashWithSalt` (1 :: Int)

instance (Eq a) => KeyEq (Union a) where
  keyEq = liftKeyEq (==)

instance KeyEq1 Union where
  liftKeyEq f (UAny l) (UAny r) = liftKeyEq f l r
  liftKeyEq f (UMrg _ l) (UMrg _ r) = liftKeyEq f l r
  liftKeyEq _ _ _ = False

instance (Eq a) => Eq (Union a) where
  (==) = shouldUseAsKeyHasSymbolicVersionError "Union" "(==)" "(.==)"

instance Eq1 Union where
  liftEq e l r = liftEq e (unionBase l) (unionBase r)

instance (Num a, Mergeable a) => Num (Union a) where
  fromInteger = mrgSingle . fromInteger
  negate = tryMerge . unionUnaryOp negate
  l + r = tryMerge $ unionBinOp (+) l r
  l * r = tryMerge $ unionBinOp (*) l r
  l - r = tryMerge $ unionBinOp (-) l r
  abs = tryMerge . unionUnaryOp abs
  signum = tryMerge . unionUnaryOp signum

instance (Mergeable a) => ITEOp (Union a) where
  symIte = mrgIf

instance (LogicalOp a, Mergeable a) => LogicalOp (Union a) where
  true = mrgSingle true
  false = mrgSingle false
  l .|| r = tryMerge $ unionBinOp (.||) l r
  l .&& r = tryMerge $ unionBinOp (.&&) l r
  symNot = tryMerge . unionUnaryOp symNot
  symXor l r = tryMerge $ unionBinOp symXor l r
  symImplies l r = tryMerge $ unionBinOp symImplies l r

instance
  (Function f arg ret, Mergeable f, Mergeable ret) =>
  Function (Union f) arg (Union ret)
  where
  f # a = do
    f1 <- f
    mrgSingle $ f1 # a

-- AllSyms
instance (AllSyms a) => AllSyms (Union a) where
  allSymsS = allSymsS1

instance AllSyms1 Union where
  liftAllSymsS f = liftAllSymsS f . unionBase

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
