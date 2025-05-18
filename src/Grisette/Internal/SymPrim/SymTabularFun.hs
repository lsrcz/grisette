{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Unused LANGUAGE pragma" #-}

-- |
-- Module      :   Grisette.Internal.SymPrim.SymTabularFun
-- Copyright   :   (c) Sirui Lu 2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Internal.SymPrim.SymTabularFun
  ( type (=~>) (SymTabularFun),
  )
where

import Control.DeepSeq (NFData (rnf))
import qualified Data.Binary as Binary
import Data.Bytes.Serial (Serial (deserialize, serialize))
import Data.Hashable (Hashable (hashWithSalt))
import qualified Data.Serialize as Cereal
import Data.String (IsString (fromString))
import Grisette.Internal.Core.Data.Class.AsKey
  ( KeyEq (keyEq),
    KeyHashable (keyHashWithSalt),
    shouldUseAsKeyHasSymbolicVersionError,
  )
import Grisette.Internal.Core.Data.Class.Function
  ( Apply (FunType, apply),
    Function ((#)),
  )
import Grisette.Internal.Core.Data.Class.Solvable
  ( Solvable (con, conView, ssym, sym),
  )
import Grisette.Internal.Internal.Decl.SymPrim.AllSyms
  ( AllSyms (allSymsS),
    SomeSym (SomeSym),
  )
import Grisette.Internal.SymPrim.Prim.Term
  ( ConRep (ConType),
    LinkedRep (underlyingTerm, wrapTerm),
    PEvalApplyTerm (pevalApplyTerm),
    SupportedNonFuncPrim,
    SupportedPrim,
    SymRep (SymType),
    Term,
    conTerm,
    pformatTerm,
    symTerm,
    typedAnySymbol,
    pattern ConTerm,
  )
import Grisette.Internal.SymPrim.TabularFun (type (=->))
import Language.Haskell.TH.Syntax (Lift (liftTyped))

-- $setup
-- >>> import Grisette.Core
-- >>> import Grisette.SymPrim
-- >>> import Grisette.Backend
-- >>> import Data.Proxy

-- | Symbolic tabular function type.
--
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
    ( LinkedRep ca sa,
      LinkedRep cb sb,
      SupportedPrim (ca =-> cb),
      SupportedNonFuncPrim ca
    ) =>
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
  ( LinkedRep ca sa,
    LinkedRep cb sb,
    SupportedPrim (ca =-> cb),
    SupportedNonFuncPrim ca
  ) =>
  LinkedRep (ca =-> cb) (sa =~> sb)
  where
  underlyingTerm (SymTabularFun a) = a
  wrapTerm = SymTabularFun

instance Function (sa =~> sb) sa sb where
  (SymTabularFun f) # t = wrapTerm $ pevalApplyTerm f (underlyingTerm t)

instance (Apply st) => Apply (sa =~> st) where
  type FunType (sa =~> st) = sa -> FunType st
  apply uf a = apply (uf # a)

instance
  ( LinkedRep ca sa,
    LinkedRep cb sb,
    SupportedPrim (ca =-> cb),
    SupportedNonFuncPrim ca
  ) =>
  Solvable (ca =-> cb) (sa =~> sb)
  where
  con = SymTabularFun . conTerm
  sym = SymTabularFun . symTerm . typedAnySymbol
  conView (SymTabularFun (ConTerm t)) = Just t
  conView _ = Nothing

instance
  ( SupportedPrim (ca =-> cb),
    LinkedRep ca sa,
    LinkedRep cb sb,
    SupportedNonFuncPrim ca
  ) =>
  IsString (sa =~> sb)
  where
  fromString = ssym . fromString

instance Show (sa =~> sb) where
  show (SymTabularFun t) = pformatTerm t

-- | This will crash the program.
--
-- 'SymTabularFun' cannot be compared concretely.
--
-- If you want to use the type as keys in hash maps based on term equality, say
-- memo table, you should use @'AsKey' 'SymTabularFun'@ instead.
instance Eq (sa =~> sb) where
  (==) = shouldUseAsKeyHasSymbolicVersionError "SymTabularFun" "(==)" "(.==)"

instance KeyEq (sa =~> sb) where
  keyEq (SymTabularFun l) (SymTabularFun r) = l == r

instance KeyHashable (sa =~> sb) where
  keyHashWithSalt s (SymTabularFun v) = hashWithSalt s v

instance AllSyms (sa =~> sb) where
  allSymsS v@SymTabularFun {} = (SomeSym v :)

instance
  ( LinkedRep ca sa,
    LinkedRep cb sb,
    SupportedPrim (ca =-> cb),
    SupportedNonFuncPrim ca
  ) =>
  Serial (sa =~> sb)
  where
  serialize = serialize . underlyingTerm
  deserialize = SymTabularFun <$> deserialize

instance
  ( LinkedRep ca sa,
    LinkedRep cb sb,
    SupportedPrim (ca =-> cb),
    SupportedNonFuncPrim ca
  ) =>
  Cereal.Serialize (sa =~> sb)
  where
  put = serialize
  get = deserialize

instance
  ( LinkedRep ca sa,
    LinkedRep cb sb,
    SupportedPrim (ca =-> cb),
    SupportedNonFuncPrim ca
  ) =>
  Binary.Binary (sa =~> sb)
  where
  put = serialize
  get = deserialize
