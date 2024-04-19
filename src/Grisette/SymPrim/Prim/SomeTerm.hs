{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module      :   Grisette.SymPrim.Prim.SomeTerm
-- Copyright   :   (c) Sirui Lu 2021-2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.SymPrim.Prim.SomeTerm (SomeTerm (..)) where

import Data.Hashable (Hashable (hashWithSalt))
import Data.Typeable (Proxy (Proxy), typeRep)
import Grisette.SymPrim.Prim.Internal.Term
  ( SupportedPrim,
    Term,
    identityWithTypeRep,
  )

data SomeTerm where
  SomeTerm :: forall a. (SupportedPrim a) => Term a -> SomeTerm

instance Eq SomeTerm where
  (SomeTerm t1) == (SomeTerm t2) =
    identityWithTypeRep t1 == identityWithTypeRep t2

instance Hashable SomeTerm where
  hashWithSalt s (SomeTerm t) = hashWithSalt s $ identityWithTypeRep t

instance Show SomeTerm where
  show (SomeTerm (t :: Term a)) =
    "<<" ++ show t ++ " :: " ++ show (typeRep (Proxy @a)) ++ ">>"
