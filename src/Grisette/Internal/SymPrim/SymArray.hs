{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      :   Grisette.Internal.SymPrim.Array
-- Copyright   :   (c) Sirui Lu 2021-2023
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Internal.SymPrim.SymArray 
  ( SymArray (..)
  , const
  , select
  , store
  ) where

import Control.DeepSeq (NFData)
import Data.Binary qualified as Binary
import Data.Bytes.Serial (Serial (deserialize, serialize))
import Data.Data (Proxy(Proxy))
import Data.Serialize qualified as Cereal
import Data.String (IsString (fromString))
import Grisette.Internal.SymPrim.Array (Array)
import Grisette.Internal.SymPrim.Prim.Internal.Term
  ( Term
  , SupportedNonFuncPrim
  , ConRep (ConType)
  , SymRep (SymType)
  , LinkedRep (underlyingTerm, wrapTerm)
  , conTerm
  , typedConstantSymbol
  , symTerm
  , pformatTerm
  , pattern ConTerm
  , pattern SelectTerm
  , pattern StoreTerm
  , pattern ConstArrayTerm
  )
import Grisette.Internal.SymPrim.Prim.Internal.Serialize ()
import Grisette.Internal.Core.Data.Class.Solvable (Solvable (con, sym, conView), ssym)
import GHC.Generics (Generic)
import Language.Haskell.TH.Syntax (Lift)
import Prelude (Show (show), Maybe (Just, Nothing), (<$>), ($), (.))

newtype SymArray k v = SymArray { underlyingArrayTerm :: Term (Array (ConType k) (ConType v)) }
  deriving (Lift, NFData, Generic)

instance ConRep (SymArray k v) where
  type ConType (SymArray k v) = Array (ConType k) (ConType v)

instance (SupportedNonFuncPrim k, SupportedNonFuncPrim v) => SymRep (Array k v) where
  type SymType (Array k v) = SymArray (SymType k) (SymType v)

instance
  ( SupportedNonFuncPrim ck,
    SupportedNonFuncPrim cv,
    LinkedRep ck sk,
    LinkedRep cv sv
  ) =>
  LinkedRep (Array ck cv) (SymArray sk sv) where
  underlyingTerm = underlyingArrayTerm
  wrapTerm = SymArray

instance
  ( SupportedNonFuncPrim ck,
    SupportedNonFuncPrim cv,
    LinkedRep ck sk,
    LinkedRep cv sv
  ) =>
  Solvable (Array ck cv) (SymArray sk sv) where
  con = wrapTerm . conTerm
  sym = wrapTerm . symTerm . typedConstantSymbol
  conView v = case underlyingTerm v of
    ConTerm t -> Just t
    _ -> Nothing

instance
  ( SupportedNonFuncPrim ck,
    SupportedNonFuncPrim cv,
    LinkedRep ck sk,
    LinkedRep cv sv
  ) =>
  IsString (SymArray sk sv) where
  fromString = ssym . fromString

instance Show (SymArray sk sv) where
  show = pformatTerm . underlyingArrayTerm

instance
  ( SupportedNonFuncPrim ck,
    SupportedNonFuncPrim cv,
    LinkedRep ck sk,
    LinkedRep cv sv
  ) =>
  Serial (SymArray sk sv) where
  serialize = serialize . underlyingTerm
  deserialize = wrapTerm <$> deserialize

instance
  ( SupportedNonFuncPrim ck,
    SupportedNonFuncPrim cv,
    LinkedRep ck sk,
    LinkedRep cv sv
  ) =>
  Cereal.Serialize (SymArray sk sv) where
  put = serialize
  get = deserialize

instance
  ( SupportedNonFuncPrim ck,
    SupportedNonFuncPrim cv,
    LinkedRep ck sk,
    LinkedRep cv sv
  ) =>
  Binary.Binary (SymArray sk sv) where
  put = serialize
  get = deserialize

const
  :: forall k v
   . SupportedNonFuncPrim (ConType k)
  => SupportedNonFuncPrim (ConType v)
  => LinkedRep (ConType k) k
  => LinkedRep (ConType v) v
  => v
  -> SymArray k v
const val = wrapTerm $ ConstArrayTerm Proxy (underlyingTerm val)

select
  :: forall k v
   . SupportedNonFuncPrim (ConType k)
  => SupportedNonFuncPrim (ConType v)
  => LinkedRep (ConType k) k
  => LinkedRep (ConType v) v
  => SymArray k v
  -> k
  -> v
select arr key = wrapTerm $ SelectTerm (underlyingTerm arr) (underlyingTerm key)

store
  :: forall k v
   . SupportedNonFuncPrim (ConType k)
  => SupportedNonFuncPrim (ConType v)
  => LinkedRep (ConType k) k
  => LinkedRep (ConType v) v
  => SymArray k v
  -> k
  -> v
  -> SymArray k v
store arr key val = do
  wrapTerm $ StoreTerm (underlyingTerm arr) (underlyingTerm key) (underlyingTerm val)
