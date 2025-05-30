{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module      :   Grisette.Internal.SymPrim.Prim.SomeTerm
-- Copyright   :   (c) Sirui Lu 2021-2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Internal.SymPrim.Prim.SomeTerm
  ( SomeTerm (..),
    someTerm,
    someTermId,
  )
where

import Data.Hashable (Hashable (hashWithSalt))
import Data.Typeable (eqT, type (:~:) (Refl))
import Grisette.Internal.SymPrim.Prim.Internal.Caches (Id)
import Grisette.Internal.SymPrim.Prim.Internal.Term
  ( SupportedPrim (primTypeRep),
    Term,
    termId,
    pattern SupportedTerm,
  )

-- | Existential wrapper for symbolic Grisette terms.
data SomeTerm where
  SomeTerm :: forall a. (SupportedPrim a) => Term a -> SomeTerm

instance Eq SomeTerm where
  (SomeTerm (t1 :: Term a)) == (SomeTerm (t2 :: Term b)) =
    case eqT @a @b of
      Just Refl -> t1 == t2
      Nothing -> False

instance Hashable SomeTerm where
  hashWithSalt s (SomeTerm t) = hashWithSalt s t

instance Show SomeTerm where
  show (SomeTerm (t :: Term a)) =
    "<<" ++ show t ++ " :: " ++ show (primTypeRep @a) ++ ">>"

-- | Wrap a symbolic term into t'SomeTerm'.
someTerm :: Term a -> SomeTerm
someTerm v@SupportedTerm = SomeTerm v
{-# INLINE someTerm #-}

-- | Get the unique identifier of a symbolic term.
someTermId :: SomeTerm -> Id
someTermId (SomeTerm t) = termId t
{-# INLINE someTermId #-}
