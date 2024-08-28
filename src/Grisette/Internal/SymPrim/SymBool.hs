{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      :   Grisette.Internal.SymPrim.SymBool
-- Copyright   :   (c) Sirui Lu 2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Internal.SymPrim.SymBool (SymBool (SymBool)) where

import Control.DeepSeq (NFData)
import Data.Hashable (Hashable (hashWithSalt))
import Data.String (IsString (fromString))
import GHC.Generics (Generic)
import Grisette.Internal.Core.Data.Class.Function (Apply (FunType, apply))
import Grisette.Internal.Core.Data.Class.Solvable
  ( Solvable (con, conView, ssym, sym),
  )
import Grisette.Internal.SymPrim.AllSyms (AllSyms (allSymsS), SomeSym (SomeSym))
import Grisette.Internal.SymPrim.Prim.Term
  ( ConRep (ConType),
    LinkedRep (underlyingTerm, wrapTerm),
    SymRep (SymType),
    Term (ConTerm),
    conTerm,
    pformatTerm,
    symTerm,
  )
import Language.Haskell.TH.Syntax (Lift)

-- $setup
-- >>> import Grisette.Core
-- >>> import Grisette.SymPrim
-- >>> import Grisette.Backend
-- >>> import Data.Proxy

-- | Symbolic Boolean type.
--
-- >>> "a" :: SymBool
-- a
-- >>> "a" .&& "b" :: SymBool
-- (&& a b)
--
-- More operations are available. Please refer to "Grisette.Core#g:symops" for
-- more information.
newtype SymBool = SymBool {underlyingBoolTerm :: Term Bool}
  deriving (Lift, NFData, Generic)

instance ConRep SymBool where
  type ConType SymBool = Bool

instance SymRep Bool where
  type SymType Bool = SymBool

instance LinkedRep Bool SymBool where
  underlyingTerm (SymBool a) = a
  wrapTerm = SymBool

instance Apply SymBool where
  type FunType SymBool = SymBool
  apply = id

instance Eq SymBool where
  SymBool l == SymBool r = l == r

instance Hashable SymBool where
  hashWithSalt s (SymBool v) = s `hashWithSalt` v

instance Solvable Bool SymBool where
  con = SymBool . conTerm
  sym = SymBool . symTerm
  conView (SymBool (ConTerm _ _ t)) = Just t
  conView _ = Nothing

instance IsString SymBool where
  fromString = ssym . fromString

instance Show SymBool where
  show (SymBool t) = pformatTerm t

instance AllSyms SymBool where
  allSymsS v = (SomeSym v :)
