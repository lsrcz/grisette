{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Grisette.Core.TH.DerivationTest (concreteT, symbolicT) where

import Control.DeepSeq (NFData)
import Data.Hashable (Hashable)
import Data.Maybe (fromJust)
import GHC.Generics (Generic)
import Grisette
  ( Default (Default),
    Mergeable,
    SEq,
    SOrd,
    SymInteger,
    ToCon (toCon),
    ToSym (toSym),
  )
import Grisette.Internal.Core.TH.Derivation
  ( deriveAllGrisette,
    deriveAnyclass,
    deriveAnyclassWithMode,
    deriveConversions,
    deriveNewtype,
    deriveStock,
    deriveStockWithMode,
    deriveViaDefault,
  )
import Grisette.Unified (EvaluationMode (Con, Sym), GetBool, GetData, GetWordN)
import Language.Haskell.TH.Syntax (Lift)

data T mode a n
  = T (GetBool mode) (GetWordN mode n) a (GetData mode (T mode a n))
  | TNil
  deriving (Generic)

deriveViaDefault ''T [''Mergeable, ''SEq, ''SOrd]
deriveStock ''T [''Show, ''Eq, ''Lift]
deriveAnyclass ''T [''NFData]
deriveAnyclassWithMode Sym ''T [''Hashable]
deriveStockWithMode Con ''T [''Ord]
deriveConversions ''T ''T [''ToCon, ''ToSym]

concreteT :: T 'Con Integer 10
concreteT = toSym (T True 10 (10 :: Integer) TNil :: T 'Con Integer 10)

symbolicT :: T 'Sym SymInteger 10
symbolicT = fromJust $ toCon (toSym concreteT :: T 'Sym SymInteger 10)

newtype T1 mode = T1 (GetBool mode)
  deriving (Generic)

deriveNewtype ''T1 [''Mergeable, ''SEq, ''SOrd, ''Show]

data T2 mode a n
  = T2 (GetBool mode) (GetWordN mode n) a (GetData mode (T2 mode a n))
  | T2Nil
  deriving (Generic)

deriveAllGrisette ''T2
