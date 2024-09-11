{-# HLINT ignore "Unused LANGUAGE pragma" #-}
{-# LANGUAGE DataKinds #-}
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

-- |
-- Module      :   Grisette.Internal.SymPrim.SymGeneralFun
-- Copyright   :   (c) Sirui Lu 2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Internal.SymPrim.SymGeneralFun
  ( type (-~>) (SymGeneralFun),
    (-->),
  )
where

import Control.DeepSeq (NFData (rnf))
import qualified Data.Binary as Binary
import Data.Bytes.Serial (Serial (deserialize, serialize))
import Data.Hashable (Hashable (hashWithSalt))
import qualified Data.Serialize as Cereal
import Data.String (IsString (fromString))
import GHC.Generics (Generic)
import Grisette.Internal.Core.Data.Class.Function
  ( Apply (FunType, apply),
    Function ((#)),
  )
import Grisette.Internal.Core.Data.Class.Solvable
  ( Solvable (con, conView, ssym, sym),
  )
import Grisette.Internal.SymPrim.AllSyms (AllSyms (allSymsS), SomeSym (SomeSym))
import Grisette.Internal.SymPrim.GeneralFun (buildGeneralFun, type (-->))
import Grisette.Internal.SymPrim.Prim.Term
  ( ConRep (ConType),
    LinkedRep (underlyingTerm, wrapTerm),
    PEvalApplyTerm (pevalApplyTerm),
    SupportedNonFuncPrim,
    SupportedPrim,
    SymRep (SymType),
    Term (ConTerm),
    TypedConstantSymbol,
    conTerm,
    pformatTerm,
    symTerm,
    typedAnySymbol,
  )
import Language.Haskell.TH.Syntax (Lift (liftTyped))

-- $setup
-- >>> import Grisette.Core
-- >>> import Grisette.SymPrim
-- >>> import Grisette.Backend
-- >>> import Data.Proxy

-- |
-- Symbolic general function type.
--
-- >>> f' = "f" :: SymInteger -~> SymInteger
-- >>> f = (f' #)
-- >>> f 1
-- (apply f 1)
--
-- >>> f' = con ("a" --> "a" + 1) :: SymInteger -~> SymInteger
-- >>> f'
-- \(arg@0 :: Integer) -> (+ 1 arg@0)
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
  SymGeneralFun ::
    ( LinkedRep ca sa,
      LinkedRep cb sb,
      SupportedPrim (ca --> cb),
      SupportedNonFuncPrim ca
    ) =>
    Term (ca --> cb) ->
    sa -~> sb

infixr 0 -~>

-- | Construction of general symbolic functions.
--
-- >>> f = "a" --> "a" + 1 :: Integer --> Integer
-- >>> f
-- \(arg@0 :: Integer) -> (+ 1 arg@0)
--
-- This general symbolic function needs to be applied to symbolic values:
--
-- >>> f # ("a" :: SymInteger)
-- (+ 1 a)
-- >>> f # (2 :: SymInteger)
-- 3
(-->) ::
  (SupportedNonFuncPrim ca, SupportedPrim cb, LinkedRep cb sb) =>
  TypedConstantSymbol ca ->
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
    SupportedPrim cb,
    SupportedPrim (ca --> cb),
    SupportedNonFuncPrim ca
  ) =>
  LinkedRep (ca --> cb) (sa -~> sb)
  where
  underlyingTerm (SymGeneralFun a) = a
  wrapTerm = SymGeneralFun

instance Function (sa -~> sb) sa sb where
  (SymGeneralFun f) # t = wrapTerm $ pevalApplyTerm f (underlyingTerm t)

instance (Apply st) => Apply (sa -~> st) where
  type FunType (sa -~> st) = sa -> FunType st
  apply uf a = apply (uf # a)

instance
  ( SupportedNonFuncPrim ca,
    LinkedRep ca sa,
    LinkedRep cb sb,
    SupportedPrim (ca --> cb)
  ) =>
  Solvable (ca --> cb) (sa -~> sb)
  where
  con = SymGeneralFun . conTerm
  sym = SymGeneralFun . symTerm . typedAnySymbol
  conView (SymGeneralFun (ConTerm _ _ _ _ t)) = Just t
  conView _ = Nothing

instance
  ( SupportedPrim (ca --> cb),
    SupportedNonFuncPrim ca,
    LinkedRep ca sa,
    LinkedRep cb sb
  ) =>
  IsString (sa -~> sb)
  where
  fromString = ssym . fromString

instance Show (sa -~> sb) where
  show (SymGeneralFun t) = pformatTerm t

instance Eq (sa -~> sb) where
  SymGeneralFun l == SymGeneralFun r = l == r

instance Hashable (sa -~> sb) where
  hashWithSalt s (SymGeneralFun v) = s `hashWithSalt` v

instance AllSyms (sa -~> sb) where
  allSymsS v@SymGeneralFun {} = (SomeSym v :)

instance
  ( LinkedRep ca sa,
    LinkedRep cb sb,
    SupportedPrim (ca --> cb),
    SupportedNonFuncPrim ca
  ) =>
  Serial (sa -~> sb)
  where
  serialize = serialize . underlyingTerm
  deserialize = SymGeneralFun <$> deserialize

instance
  ( LinkedRep ca sa,
    LinkedRep cb sb,
    SupportedPrim (ca --> cb),
    SupportedNonFuncPrim ca
  ) =>
  Cereal.Serialize (sa -~> sb)
  where
  put = serialize
  get = deserialize

instance
  ( LinkedRep ca sa,
    LinkedRep cb sb,
    SupportedPrim (ca --> cb),
    SupportedNonFuncPrim ca
  ) =>
  Binary.Binary (sa -~> sb)
  where
  put = serialize
  get = deserialize
