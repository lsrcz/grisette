{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Unused LANGUAGE pragma" #-}

-- |
-- Module      :   Grisette.IR.SymPrim.Data.SymGeneralFun
-- Copyright   :   (c) Sirui Lu 2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.IR.SymPrim.Data.SymGeneralFun
  ( type (-~>) (..),
    (-->),
  )
where

import Control.DeepSeq (NFData (rnf))
import Data.Hashable (Hashable (hashWithSalt))
import Data.String (IsString (fromString))
import GHC.Generics (Generic)
import Grisette.Core.Data.Class.Function
  ( Apply (FunType, apply),
    Function ((#)),
  )
import Grisette.Core.Data.Class.Solvable
  ( Solvable (con, conView, ssym, sym),
  )
import Grisette.IR.SymPrim.Data.GeneralFun (buildGeneralFun, type (-->))
import Grisette.IR.SymPrim.Data.Prim.Term
  ( ConRep (ConType),
    LinkedRep (underlyingTerm, wrapTerm),
    PEvalApplyTerm (pevalApplyTerm),
    SupportedNonFuncPrim,
    SupportedPrim,
    SymRep (SymType),
    Term (ConTerm),
    TypedSymbol,
    conTerm,
    pformat,
    symTerm,
  )
import Grisette.IR.SymPrim.Data.AllSyms (AllSyms (allSymsS), SomeSym (SomeSym))
import Language.Haskell.TH.Syntax (Lift (liftTyped))

-- $setup
-- >>> import Grisette.Core
-- >>> import Grisette.IR.SymPrim
-- >>> import Grisette.Backend.SBV
-- >>> import Data.Proxy

-- |
-- Symbolic general function type.
--
-- >>> :set -XTypeOperators -XOverloadedStrings
-- >>> f' = "f" :: SymInteger -~> SymInteger
-- >>> f = (f' #)
-- >>> f 1
-- (apply f 1)
--
-- >>> f' = con ("a" --> "a" + 1) :: SymInteger -~> SymInteger
-- >>> f'
-- \(a:ARG :: Integer) -> (+ 1 a:ARG)
-- >>> f = (f' #)
-- >>> f 1
-- 2
-- >>> f 2
-- 3
-- >>> f 3
-- 4
-- >>> f "b"
-- (+ 1 b)
data sa -~> sb where
  SymGeneralFun :: (LinkedRep ca sa, LinkedRep cb sb) => Term (ca --> cb) -> sa -~> sb

infixr 0 -~>

-- | Construction of general symbolic functions.
--
-- >>> f = "a" --> "a" + 1 :: Integer --> Integer
-- >>> f
-- \(a:ARG :: Integer) -> (+ 1 a:ARG)
--
-- This general symbolic function needs to be applied to symbolic values:
-- >>> f # ("a" :: SymInteger)
-- (+ 1 a)
(-->) ::
  (SupportedPrim ca, SupportedPrim cb, LinkedRep cb sb) =>
  TypedSymbol ca ->
  sb ->
  ca --> cb
(-->) arg = buildGeneralFun arg . underlyingTerm

infixr 0 -->

data ARG = ARG
  deriving (Eq, Ord, Lift, Show, Generic)

instance NFData ARG where
  rnf ARG = ()

instance Hashable ARG where
  hashWithSalt s ARG = s `hashWithSalt` (0 :: Int)

instance Lift (sa -~> sb) where
  liftTyped (SymGeneralFun t) = [||SymGeneralFun t||]

instance NFData (sa -~> sb) where
  rnf (SymGeneralFun t) = rnf t

instance (ConRep a, ConRep b) => ConRep (a -~> b) where
  type ConType (a -~> b) = ConType a --> ConType b

instance
  ( SymRep ca,
    SymRep cb,
    SupportedPrim (ca --> cb)
  ) =>
  SymRep (ca --> cb)
  where
  type SymType (ca --> cb) = SymType ca -~> SymType cb

instance
  ( LinkedRep ca sa,
    LinkedRep cb sb,
    SupportedPrim ca,
    SupportedPrim cb,
    SupportedPrim (ca --> cb)
  ) =>
  LinkedRep (ca --> cb) (sa -~> sb)
  where
  underlyingTerm (SymGeneralFun a) = a
  wrapTerm = SymGeneralFun

instance
  ( SupportedNonFuncPrim ca,
    SupportedPrim cb,
    LinkedRep ca sa,
    LinkedRep cb sb,
    SupportedPrim (ca --> cb)
  ) =>
  Function (sa -~> sb) sa sb
  where
  (SymGeneralFun f) # t = wrapTerm $ pevalApplyTerm f (underlyingTerm t)

instance
  ( LinkedRep ca sa,
    LinkedRep ct st,
    Apply st,
    SupportedNonFuncPrim ca,
    SupportedPrim ct,
    SupportedPrim (ca --> ct)
  ) =>
  Apply (sa -~> st)
  where
  type FunType (sa -~> st) = sa -> FunType st
  apply uf a = apply (uf # a)

instance
  ( SupportedPrim ca,
    SupportedPrim cb,
    LinkedRep ca sa,
    LinkedRep cb sb,
    SupportedPrim (ca --> cb)
  ) =>
  Solvable (ca --> cb) (sa -~> sb)
  where
  con = SymGeneralFun . conTerm
  sym = SymGeneralFun . symTerm
  conView (SymGeneralFun (ConTerm _ t)) = Just t
  conView _ = Nothing

instance
  ( SupportedPrim (ca --> cb),
    LinkedRep ca sa,
    LinkedRep cb sb
  ) =>
  IsString (sa -~> sb)
  where
  fromString = ssym . fromString

instance
  (SupportedPrim (ca --> cb), LinkedRep ca sa, LinkedRep cb sb) =>
  Show (sa -~> sb)
  where
  show (SymGeneralFun t) = pformat t

instance
  (SupportedPrim (ca --> cb), LinkedRep ca sa, LinkedRep cb sb) =>
  Eq (sa -~> sb)
  where
  SymGeneralFun l == SymGeneralFun r = l == r

instance
  (SupportedPrim (ca --> cb), LinkedRep ca sa, LinkedRep cb sb) =>
  Hashable (sa -~> sb)
  where
  hashWithSalt s (SymGeneralFun v) = s `hashWithSalt` v

instance
  (SupportedPrim (ca --> cb), LinkedRep ca sa, LinkedRep cb sb) =>
  AllSyms (sa -~> sb)
  where
  allSymsS v = (SomeSym v :)
