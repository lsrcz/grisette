{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Grisette.IR.SymPrim.Data.Prim.Helpers
  ( pattern UnaryTermPatt,
    pattern BinaryTermPatt,
    pattern TernaryTermPatt,
    pattern UnsafeUnaryTermPatt,
    pattern UnsafeBinaryTermPatt,
    pattern Unsafe1t21BinaryTermPatt,
    pattern Unsafe1u2t32TernaryTermPatt,
  )
where

import Data.Typeable
import Grisette.IR.SymPrim.Data.Prim.InternedTerm.Term
import Grisette.IR.SymPrim.Data.Prim.InternedTerm.TermUtils
import Unsafe.Coerce

unsafeUnaryTermView :: forall a b tag. (Typeable tag) => Term a -> Maybe (tag, Term b)
unsafeUnaryTermView (UnaryTerm _ (tag :: tagt) t1) =
  case cast tag of
    Just t -> Just (t, unsafeCoerce t1)
    Nothing -> Nothing
-- (,) <$> cast tag <*> castTerm t1
unsafeUnaryTermView _ = Nothing

pattern UnsafeUnaryTermPatt :: forall a b tag. (Typeable tag) => tag -> Term b -> Term a
pattern UnsafeUnaryTermPatt tag t <- (unsafeUnaryTermView @a @b @tag -> Just (tag, t))

unaryTermView :: forall a b tag. (Typeable tag, Typeable b) => Term a -> Maybe (tag, Term b)
unaryTermView (UnaryTerm _ (tag :: tagt) t1) =
  (,) <$> cast tag <*> castTerm t1
unaryTermView _ = Nothing

pattern UnaryTermPatt :: forall a b tag. (Typeable tag, Typeable b) => tag -> Term b -> Term a
pattern UnaryTermPatt tag t <- (unaryTermView @a @b @tag -> Just (tag, t))

unsafeBinaryTermView :: forall a b c tag. (Typeable tag) => Term a -> Maybe (tag, Term b, Term c)
unsafeBinaryTermView (BinaryTerm _ (tag :: tagt) t1 t2) =
  case cast tag of
    Just t -> Just (t, unsafeCoerce t1, unsafeCoerce t2)
    Nothing -> Nothing
-- (,) <$> cast tag <*> castTerm t1
unsafeBinaryTermView _ = Nothing

pattern UnsafeBinaryTermPatt :: forall a b c tag. (Typeable tag) => tag -> Term b -> Term c -> Term a
pattern UnsafeBinaryTermPatt tag t1 t2 <- (unsafeBinaryTermView @a @b @c @tag -> Just (tag, t1, t2))

unsafe1t21BinaryTermView :: forall a b tag. (Typeable tag, Typeable b) => Term a -> Maybe (tag, Term b, Term b)
unsafe1t21BinaryTermView (BinaryTerm _ (tag :: tagt) t1 t2) =
  case (cast tag, cast t1) of
    (Just tg, Just t1') -> Just (tg, t1', unsafeCoerce t2)
    _ -> Nothing
-- (,) <$> cast tag <*> castTerm t1
unsafe1t21BinaryTermView _ = Nothing

pattern Unsafe1t21BinaryTermPatt :: forall a b tag. (Typeable tag, Typeable b) => tag -> Term b -> Term b -> Term a
pattern Unsafe1t21BinaryTermPatt tag t1 t2 <- (unsafe1t21BinaryTermView @a @b @tag -> Just (tag, t1, t2))

binaryTermView :: forall a b c tag. (Typeable tag, Typeable b, Typeable c) => Term a -> Maybe (tag, Term b, Term c)
binaryTermView (BinaryTerm _ (tag :: tagt) t1 t2) =
  (,,) <$> cast tag <*> castTerm t1 <*> castTerm t2
binaryTermView _ = Nothing

pattern BinaryTermPatt :: forall a b c tag. (Typeable tag, Typeable b, Typeable c) => tag -> Term b -> Term c -> Term a
pattern BinaryTermPatt tag l r <- (binaryTermView @a @b @c @tag -> Just (tag, l, r))

unsafe1u2t32TernaryTermView ::
  forall a b c tag.
  (Typeable tag, Typeable c) =>
  Term a ->
  Maybe (tag, Term b, Term c, Term c)
unsafe1u2t32TernaryTermView (TernaryTerm _ (tag :: tagt) t1 t2 t3) =
  case (cast tag, castTerm t2) of
    (Just tg, Just t2') -> Just (tg, unsafeCoerce t1, t2', unsafeCoerce t3)
    _ -> Nothing
unsafe1u2t32TernaryTermView _ = Nothing

pattern Unsafe1u2t32TernaryTermPatt ::
  forall a b c tag.
  (Typeable tag, Typeable c) =>
  tag ->
  Term b ->
  Term c ->
  Term c ->
  Term a
pattern Unsafe1u2t32TernaryTermPatt tag a b c <-
  (unsafe1u2t32TernaryTermView @a @b @c @tag -> Just (tag, a, b, c))

ternaryTermView ::
  forall a b c d tag.
  (Typeable tag, Typeable b, Typeable c, Typeable d) =>
  Term a ->
  Maybe (tag, Term b, Term c, Term d)
ternaryTermView (TernaryTerm _ (tag :: tagt) t1 t2 t3) =
  (,,,) <$> cast tag <*> castTerm t1 <*> castTerm t2 <*> castTerm t3
ternaryTermView _ = Nothing

pattern TernaryTermPatt ::
  forall a b c d tag.
  (Typeable tag, Typeable b, Typeable c, Typeable d) =>
  tag ->
  Term b ->
  Term c ->
  Term d ->
  Term a
pattern TernaryTermPatt tag a b c <- (ternaryTermView @a @b @c @d @tag -> Just (tag, a, b, c))
