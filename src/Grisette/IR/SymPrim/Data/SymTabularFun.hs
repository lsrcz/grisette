{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Unused LANGUAGE pragma" #-}

-- |
-- Module      :   Grisette.IR.SymPrim.Data.SymTabularFun
-- Copyright   :   (c) Sirui Lu 2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.IR.SymPrim.Data.SymTabularFun (type (=~>) (..)) where

import Control.DeepSeq (NFData (rnf))
import Data.Hashable (Hashable (hashWithSalt))
import Data.String (IsString (fromString))
import Grisette.Core.Data.Class.Function
  ( Apply (FunType, apply),
    Function ((#)),
  )
import Grisette.Core.Data.Class.Solvable
  ( Solvable (con, conView, ssym, sym),
  )
import Grisette.IR.SymPrim.Data.Prim.Term
  ( ConRep (ConType),
    LinkedRep (underlyingTerm, wrapTerm),
    PEvalApplyTerm (pevalApplyTerm),
    SupportedPrim,
    SymRep (SymType),
    Term (ConTerm),
    conTerm,
    pformat,
    symTerm,
  )
import Grisette.IR.SymPrim.Data.AllSyms (AllSyms (allSymsS), SomeSym (SomeSym))
import Grisette.IR.SymPrim.Data.TabularFun (type (=->))
import Language.Haskell.TH.Syntax (Lift (liftTyped))

-- $setup
-- >>> import Grisette.Core
-- >>> import Grisette.IR.SymPrim
-- >>> import Grisette.Backend.SBV
-- >>> import Data.Proxy

-- | Symbolic tabular function type.
--
-- >>> :set -XTypeOperators -XOverloadedStrings
-- >>> f' = "f" :: SymInteger =~> SymInteger
-- >>> f = (f' #)
-- >>> f 1
-- (apply f 1)
--
-- >>> f' = con (TabularFun [(1, 2), (2, 3)] 4) :: SymInteger =~> SymInteger
-- >>> f = (f' #)
-- >>> f 1
-- 2
-- >>> f 2
-- 3
-- >>> f 3
-- 4
-- >>> f "b"
-- (ite (= b 1) 2 (ite (= b 2) 3 4))
data sa =~> sb where
  SymTabularFun ::
    (LinkedRep ca sa, LinkedRep cb sb) =>
    Term (ca =-> cb) ->
    sa =~> sb

infixr 0 =~>

instance Lift (sa =~> sb) where
  liftTyped (SymTabularFun t) = [||SymTabularFun t||]

instance NFData (sa =~> sb) where
  rnf (SymTabularFun t) = rnf t

instance (ConRep a, ConRep b) => ConRep (a =~> b) where
  type ConType (a =~> b) = ConType a =-> ConType b

instance (SymRep a, SymRep b, SupportedPrim (a =-> b)) => SymRep (a =-> b) where
  type SymType (a =-> b) = SymType a =~> SymType b

instance
  (LinkedRep ca sa, LinkedRep cb sb, SupportedPrim (ca =-> cb)) =>
  LinkedRep (ca =-> cb) (sa =~> sb)
  where
  underlyingTerm (SymTabularFun a) = a
  wrapTerm = SymTabularFun

instance
  ( SupportedPrim ca,
    SupportedPrim cb,
    LinkedRep ca sa,
    LinkedRep cb sb,
    SupportedPrim (ca =-> cb)
  ) =>
  Function (sa =~> sb) sa sb
  where
  (SymTabularFun f) # t = wrapTerm $ pevalApplyTerm f (underlyingTerm t)

instance
  ( LinkedRep ca sa,
    LinkedRep ct st,
    Apply st,
    SupportedPrim ca,
    SupportedPrim ct,
    SupportedPrim (ca =-> ct)
  ) =>
  Apply (sa =~> st)
  where
  type FunType (sa =~> st) = sa -> FunType st
  apply uf a = apply (uf # a)

instance
  ( SupportedPrim ca,
    SupportedPrim cb,
    LinkedRep ca sa,
    LinkedRep cb sb,
    SupportedPrim (ca =-> cb)
  ) =>
  Solvable (ca =-> cb) (sa =~> sb)
  where
  con = SymTabularFun . conTerm
  sym = SymTabularFun . symTerm
  conView (SymTabularFun (ConTerm _ t)) = Just t
  conView _ = Nothing

instance
  ( SupportedPrim (ca =-> cb),
    LinkedRep ca sa,
    LinkedRep cb sb
  ) =>
  IsString (sa =~> sb)
  where
  fromString = ssym . fromString

instance
  (SupportedPrim (ca =-> cb), LinkedRep ca sa, LinkedRep cb sb) =>
  Show (sa =~> sb)
  where
  show (SymTabularFun t) = pformat t

instance
  (SupportedPrim (ca =-> cb), LinkedRep ca sa, LinkedRep cb sb) =>
  Eq (sa =~> sb)
  where
  SymTabularFun l == SymTabularFun r = l == r

instance
  (SupportedPrim (ca =-> cb), LinkedRep ca sa, LinkedRep cb sb) =>
  Hashable (sa =~> sb)
  where
  hashWithSalt s (SymTabularFun v) = s `hashWithSalt` v

instance
  (SupportedPrim (ca =-> cb), LinkedRep ca sa, LinkedRep cb sb) =>
  AllSyms (sa =~> sb)
  where
  allSymsS v = (SomeSym v :)
